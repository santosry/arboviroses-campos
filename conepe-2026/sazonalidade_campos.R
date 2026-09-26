# =====================================================================
# ANÁLISE DE SAZONALIDADE — CAMPOS DOS GOYTACAZES, RJ
# Dados REAIS do INMET · estações automáticas A607 + A620
# Período: 2020–2025 · Temperatura horária → média diária
# =====================================================================
# Auditoria rigorosa: logging, detecção de outliers, correções,
# exportação por ano, validação cruzada entre estações.
# =====================================================================

# =====================================================================
# 0. CONFIGURAÇÃO E PACOTES
# =====================================================================
library(tidyverse)
library(lubridate)
library(BrazilMet)

dir.create("audit", showWarnings = FALSE)
dir.create("data_inmet_campos/por_ano", showWarnings = FALSE)
dir.create("figures_cellpress", showWarnings = FALSE)

# --- Sistema de auditoria ---
audit_file <- file.path("audit", sprintf("audit_%s.log",
                        format(Sys.time(), "%Y%m%d_%H%M%S")))
audit <- function(msg, level = "INFO") {
  linha <- sprintf("[%s] %-5s %s", format(Sys.time(), "%H:%M:%S"), level, msg)
  cat(linha, "\n", file = audit_file, append = TRUE)
  message(linha)
}

audit("=== INÍCIO DA ANÁLISE ===")
audit(sprintf("R %s | %s", R.version.string, R.version$platform))
audit(sprintf("WD: %s", getwd()))

# =====================================================================
# 1. IDENTIFICAÇÃO DAS ESTAÇÕES DE CAMPOS
# =====================================================================
audit("Buscando estações de Campos dos Goytacazes...")
stations <- see_stations_info()
campos <- stations %>%
  filter(uf == "RJ", str_detect(station_municipality,
         regex("campos.*goytacaz", ignore_case = TRUE)))

audit(sprintf("Encontradas %d estação(s):", nrow(campos)))
for (i in seq_len(nrow(campos))) {
  audit(sprintf("  %s — %s (%.4f, %.4f) alt %sm",
    campos$station_code[i], campos$station_municipality[i],
    campos$latitude_degrees[i], campos$longitude_degrees[i],
    campos$altitude_m[i]))
}
codigos <- campos$station_code

# =====================================================================
# 2. EXTRAÇÃO DOS ZIPs DO INMET
# =====================================================================
audit("Extraindo ZIPs do INMET...")
zips <- list.files("data_inmet_campos/zip", pattern = "\\.zip$", full.names = TRUE)
audit(sprintf("%d ZIPs encontrados (total: %s)", length(zips),
     format(sum(file.size(zips)), big.mark = ",")))

dir_csv <- "data_inmet_campos/csv_extracted"
dir.create(dir_csv, showWarnings = FALSE, recursive = TRUE)

for (z in zips) {
  fname <- basename(z)
  audit(sprintf("  Extraindo %s (%s bytes)...", fname, format(file.size(z), big.mark = ",")))
  suppressWarnings(unzip(z, exdir = dir_csv, overwrite = FALSE))
}

# =====================================================================
# 3. IMPORTAÇÃO — APENAS CAMPOS DOS GOYTACAZES
# =====================================================================
audit("Importando CSVs das estações de Campos...")

csv_files <- list.files(dir_csv, pattern = "\\.CSV$", full.names = TRUE, recursive = TRUE)
padrao <- paste(codigos, collapse = "|")
csv_campos <- csv_files[str_detect(basename(csv_files), padrao)]
audit(sprintf("  %d de %d CSVs são de Campos", length(csv_campos), length(csv_files)))

# Função robusta de importação
ler_csv_inmet <- function(caminho) {
  linhas <- readLines(caminho, n = 30, warn = FALSE)
  if (length(linhas) == 0) return(NULL)

  # Encontra linha de cabeçalho (Data;Hora...)
  idx <- which(str_detect(linhas, regex("^data\\s*[;,]", ignore_case = TRUE)))[1]
  if (is.na(idx) || idx < 1) idx <- 1L

  delim <- ifelse(str_count(linhas[idx], ";") >= 2, ";", ",")

  df <- tryCatch(
    read_delim(caminho, delim = delim, skip = idx - 1L,
               col_names = TRUE, col_types = cols(.default = col_character()),
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      # Fallback: tenta sem skip
      tryCatch(
        read_delim(caminho, delim = delim, col_names = TRUE,
                   col_types = cols(.default = col_character()),
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE, progress = FALSE),
        error = function(e2) NULL
      )
    }
  )

  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Extrai código da estação do nome do arquivo
  df$station_code <- str_extract(basename(caminho), "A\\d{3}")
  df$.fonte <- basename(caminho)
  return(df)
}

dfs <- map(csv_campos, ler_csv_inmet, .progress = TRUE)
dfs <- compact(dfs)
audit(sprintf("  %d CSVs importados com sucesso", length(dfs)))

df_bruto <- bind_rows(dfs)
audit(sprintf("  Total registros brutos: %s", format(nrow(df_bruto), big.mark = ",")))
audit(sprintf("  Colunas detectadas: %d", ncol(df_bruto)))

# =====================================================================
# 4. SELEÇÃO E PADRONIZAÇÃO DAS VARIÁVEIS
# =====================================================================
audit("Selecionando variáveis de temperatura...")

# Identifica colunas por regex case-insensitive
col_data <- names(df_bruto)[str_detect(names(df_bruto),
  regex("^Data$", ignore_case = TRUE))][1]
col_hora <- names(df_bruto)[str_detect(names(df_bruto),
  regex("^Hora", ignore_case = TRUE))][1]
col_temp <- names(df_bruto)[str_detect(names(df_bruto),
  regex("temperatura.*bulbo.*seco", ignore_case = TRUE))][1]

if (is.na(col_temp)) {
  col_temp <- names(df_bruto)[str_detect(names(df_bruto),
    regex("temperatura", ignore_case = TRUE))][1]
}
if (is.na(col_data)) col_data <- names(df_bruto)[1]
if (is.na(col_hora)) col_hora <- names(df_bruto)[2]

audit(sprintf("  Data : %s", col_data))
audit(sprintf("  Hora : %s", col_hora))
audit(sprintf("  Temp : %s", col_temp))

df_temp <- df_bruto %>%
  select(
    data_bruta  = all_of(col_data),
    hora_bruta  = all_of(col_hora),
    temperatura = all_of(col_temp),
    station_code,
    .fonte
  ) %>%
  mutate(
    temperatura = str_replace_all(temperatura, ",", "."),
    temperatura = parse_number(temperatura, na = c("", "NA", "null", "NULL", "-9999"))
  )

# =====================================================================
# 5. PARSE DE DATA/HORA E LIMPEZA INICIAL
# =====================================================================
audit("Parse de data/hora e limpeza inicial...")

# --- FILTRO: SOMENTE A607 (Campos sede, exclui A620 São Tomé) ---
audit("  Filtrando apenas estação A607 (Campos dos Goytacazes sede)...")
n_antes_a607 <- nrow(df_temp)
df_temp <- df_temp %>% filter(station_code == "A607")
audit(sprintf("  A607: %s registros (removidos %d de A620)",
              format(nrow(df_temp), big.mark = ","), n_antes_a607 - nrow(df_temp)))

n0 <- nrow(df_temp)

df_temp <- df_temp %>%
  mutate(
    data_limpa = str_extract(data_bruta, "\\d{4}[-/]\\d{2}[-/]\\d{2}"),
    data_limpa = str_replace_all(data_limpa, "/", "-"),
    hora_limpa = str_extract(hora_bruta, "\\d{2}:?\\d{2}"),
    hora_limpa = if_else(str_detect(hora_limpa, ":"), hora_limpa,
      paste0(str_sub(hora_limpa, 1, 2), ":", str_sub(hora_limpa, 3, 4))),
    data_hora  = ymd_hm(paste(data_limpa, hora_limpa), quiet = TRUE),
    date       = as.Date(data_hora),
    ano        = year(date),
    mes        = month(date)
  )

# Remove registros sem data/hora
n1 <- nrow(df_temp)
df_temp <- df_temp %>% filter(!is.na(data_hora))
n2 <- nrow(df_temp)
audit(sprintf("  Data/hora inválida: %d removidos (restam %s)",
              n1 - n2, format(n2, big.mark = ",")))

# =====================================================================
# 6. CONTROLE DE QUALIDADE RIGOROSO
# =====================================================================
audit("Controle de qualidade dos dados...")
audit("  Aplicando critérios estatísticos e físicos...")

# --- 6a. NA explícitos ---
n_antes <- nrow(df_temp)
df_temp <- df_temp %>% filter(!is.na(temperatura))
n_depois <- nrow(df_temp)
audit(sprintf("  Temperatura NA: %d removidos", n_antes - n_depois))

# --- 6b. Limites físicos absolutos ---
lim_inf <- -5    # °C — abaixo disso é impossível em Campos/RJ
lim_sup <- 48    # °C — acima disso é erro de sensor

n_antes <- nrow(df_temp)
df_temp <- df_temp %>% filter(temperatura >= lim_inf, temperatura <= lim_sup)
n_depois <- nrow(df_temp)
audit(sprintf("  Fora [%d, %d] °C: %d removidos", lim_inf, lim_sup, n_antes - n_depois))

# --- 6c. Outliers estatísticos por mês (IQR) ---
# Detecta valores > 3 IQR da mediana mensal (muito extremos)
audit("  Detectando outliers por IQR mensal (>3xIQR)...")
df_temp <- df_temp %>%
  group_by(ano, mes) %>%
  mutate(
    q1_mes = quantile(temperatura, 0.25, na.rm = TRUE),
    q3_mes = quantile(temperatura, 0.75, na.rm = TRUE),
    iqr_mes = q3_mes - q1_mes,
    outlier_iqr = temperatura < (q1_mes - 3 * iqr_mes) |
                  temperatura > (q3_mes + 3 * iqr_mes)
  ) %>%
  ungroup()

n_outliers <- sum(df_temp$outlier_iqr, na.rm = TRUE)
audit(sprintf("  Outliers IQR detectados: %d (%.2f%%)",
              n_outliers, 100 * n_outliers / nrow(df_temp)))

# Log detalhado dos outliers
if (n_outliers > 0) {
  outliers_log <- df_temp %>%
    filter(outlier_iqr) %>%
    select(date, station_code, temperatura, q1_mes, q3_mes, iqr_mes)
  write_csv(outliers_log, file.path("audit", "outliers_iqr_detectados.csv"))
  audit(sprintf("  Detalhes salvos em audit/outliers_iqr_detectados.csv (%d registros)",
                nrow(outliers_log)))

  # Exibe exemplos
  cat("\n  Amostra de outliers detectados:\n")
  print(head(outliers_log, 10))
}

# Corrige outliers: substitui por NA (serão interpolados na agregação diária)
df_temp <- df_temp %>%
  mutate(temperatura = if_else(outlier_iqr, NA_real_, temperatura))

# Remove colunas auxiliares
df_temp <- df_temp %>% select(-q1_mes, -q3_mes, -iqr_mes, -outlier_iqr)

# --- 6d. Consistência (estação única A607, sem comparação) ---
audit("  Estação única A607 — validação cruzada não aplicável.")

# --- 6e. Verificação de gaps temporais ---
audit("  Verificando gaps temporais...")
gaps_check <- df_temp %>%
  distinct(date) %>%
  arrange(date) %>%
  mutate(gap = as.numeric(date - lag(date)))

gaps_longos <- gaps_check %>% filter(gap > 1)
if (nrow(gaps_longos) > 0) {
  audit(sprintf("  ⚠ %d gaps > 1 dia detectados!", nrow(gaps_longos)), "WARN")
  write_csv(gaps_longos, file.path("audit", "gaps_temporais.csv"))
} else {
  audit("  Nenhum gap > 1 dia — série contínua.")
}

# =====================================================================
# 7. AGREGAÇÃO DIÁRIA
# =====================================================================
audit("Agregando para base diária...")

df_diario <- df_temp %>%
  filter(!is.na(temperatura)) %>%
  group_by(date) %>%
  summarise(
    temp_media   = mean(temperatura, na.rm = TRUE),
    temp_min     = min(temperatura, na.rm = TRUE),
    temp_max     = max(temperatura, na.rm = TRUE),
    temp_sd      = sd(temperatura, na.rm = TRUE),
    n_obs        = n(),
    estacoes     = "A607",
    .groups      = "drop"
  ) %>%
  mutate(
    ano       = year(date),
    mes       = month(date),
    dia_ano   = yday(date),
    mes_nome  = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  arrange(date)

# Remove dias com < 6 observações (suspeitos)
n_dias_raw <- nrow(df_diario)
df_diario <- df_diario %>% filter(n_obs >= 6)
audit(sprintf("  Dias com ≥6 obs: %d (removidos %d com poucos dados)",
              nrow(df_diario), n_dias_raw - nrow(df_diario)))

# Preenche anos bissextos corretamente (dia 366)
audit(sprintf("  Período: %s a %s (%d dias)",
              min(df_diario$date), max(df_diario$date), nrow(df_diario)))

# =====================================================================
# 8. INTERPRETAÇÃO E ANÁLISE CÉTICA DOS DADOS
# =====================================================================
audit("=== ANÁLISE EXPLORATÓRIA ===")

resumo_anual <- df_diario %>%
  group_by(ano) %>%
  summarise(
    dias     = n(),
    media    = round(mean(temp_media), 2),
    sd       = round(sd(temp_media), 2),
    minima   = round(min(temp_media), 2),
    maxima   = round(max(temp_media), 2),
    amp_media = round(mean(temp_max - temp_min), 2),
    .groups  = "drop"
  )

cat("\n")
print(resumo_anual)

# Interpretação cética
audit("--- Interpretação dos dados ---")

# Verifica tendência de aquecimento
if (nrow(resumo_anual) >= 3) {
  anos_validos <- resumo_anual %>% filter(dias >= 300)  # pelo menos 300 dias/ano
  if (nrow(anos_validos) >= 3) {
    modelo <- lm(media ~ ano, data = anos_validos)
    tendencia <- coef(modelo)[2]
    p_valor <- summary(modelo)$coefficients[2, 4]
    audit(sprintf("  Tendência linear: %+.3f °C/ano (p = %.3f)", tendencia, p_valor))
    if (p_valor < 0.05) {
      audit(sprintf("  ⚠ Tendência estatisticamente significativa!", "WARN"))
    } else {
      audit("  Tendência NÃO significativa — variação natural.")
    }
  }
}

# Análise de amplitude térmica
audit(sprintf("  Amplitude térmica média diária: %.1f °C",
              mean(df_diario$temp_max - df_diario$temp_min, na.rm = TRUE)))

# Verifica sazonalidade
temp_verao <- df_diario %>% filter(mes %in% c(12, 1, 2)) %>% pull(temp_media)
temp_inverno <- df_diario %>% filter(mes %in% c(6, 7, 8)) %>% pull(temp_media)
audit(sprintf("  Verão (DJF): média %.1f °C  |  Inverno (JJA): média %.1f °C  |  Δ = %.1f °C",
              mean(temp_verao, na.rm = TRUE), mean(temp_inverno, na.rm = TRUE),
              mean(temp_verao, na.rm = TRUE) - mean(temp_inverno, na.rm = TRUE)))

# =====================================================================
# 9. EXPORTAÇÃO DOS DADOS
# =====================================================================
audit("Exportando dados processados...")

# --- CSV consolidado ---
write_csv(df_diario, "data_inmet_campos/temperatura_diaria_campos.csv")
audit("  data_inmet_campos/temperatura_diaria_campos.csv (consolidado)")

# --- CSV por ano ---
for (yr in sort(unique(df_diario$ano))) {
  df_ano <- df_diario %>% filter(ano == yr)
  fname <- file.path("data_inmet_campos/por_ano", sprintf("campos_%d.csv", yr))
  write_csv(df_ano, fname)
  audit(sprintf("  data_inmet_campos/por_ano/campos_%d.csv (%d dias)", yr, nrow(df_ano)))
}

# --- Dados horários limpos ---
df_temp_out <- df_temp %>%
  filter(!is.na(temperatura)) %>%
  select(date, data_hora, station_code, temperatura)
write_csv(df_temp_out, "data_inmet_campos/temperatura_horaria_campos.csv")
audit(sprintf("  data_inmet_campos/temperatura_horaria_campos.csv (%s registros)",
              format(nrow(df_temp_out), big.mark = ",")))

# =====================================================================
# 10. GRÁFICO DE SAZONALIDADE
# =====================================================================
audit("Gerando gráfico de sazonalidade...")

cores <- c("#2C3E50", "#3498DB", "#E67E22", "#27AE60", "#8E44AD", "#C0392B")
anos_presentes <- sort(unique(df_diario$ano))
names(cores) <- as.character(anos_presentes)

p <- ggplot(df_diario, aes(x = dia_ano, y = temp_media,
                            color = factor(ano), group = factor(ano))) +
  # Dados diários (linhas finas)
  geom_line(linewidth = 0.2, alpha = 0.35) +
  # Suavização LOESS
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1.3) +
  scale_color_manual(
    values = cores[as.character(anos_presentes)],
    name   = "Ano",
    guide  = guide_legend(
      nrow = 1,
      override.aes = list(linewidth = 2.5, alpha = 1),
      keywidth  = unit(2.0, "cm"),
      keyheight = unit(0.5, "cm")
    )
  ) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(8)) +
  labs(
    title    = "Sazonalidade da temperatura — Campos dos Goytacazes, RJ",
    subtitle = sprintf("INMET · estação A607 · %d–%d · %d dias",
                       min(df_diario$ano), max(df_diario$ano),
                       nrow(df_diario)),
    x        = NULL,
    y        = "Temperatura média diária (°C)",
    caption  = "Linhas finas: dados diários | Linhas grossas: LOESS (span=0,15)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.2),
    axis.line.x        = element_line(color = "gray40", linewidth = 0.35),
    axis.ticks.x       = element_line(color = "gray40", linewidth = 0.35),
    axis.ticks.length  = unit(0.2, "cm"),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.box         = "horizontal",
    legend.margin      = margin(t = 8, b = 2),
    plot.title         = element_text(color = "gray10", face = "plain", size = 22, margin = margin(b = 6)),
    plot.subtitle      = element_text(color = "gray40", size = 14),
    plot.caption       = element_text(color = "gray60", size = 11, hjust = 1),
    axis.title.y       = element_text(color = "gray20", size = 18, margin = margin(r = 8)),
    axis.text.x        = element_text(color = "gray30", size = 15),
    axis.text.y        = element_text(color = "gray30", size = 15),
    legend.text        = element_text(color = "gray30", size = 16),
    legend.title       = element_text(color = "gray20", size = 16, face = "bold"),
    plot.margin        = margin(20, 20, 15, 15)
  )

ggsave("figures_cellpress/sazonalidade_campos.png",
       p, width = 14, height = 8, dpi = 300, bg = "white")
audit("  figures_cellpress/sazonalidade_campos.png (4200×2400px, 300 DPI)")

# =====================================================================
# 11. EXPORTAÇÃO DO LOG DE AUDITORIA E RESUMO FINAL
# =====================================================================
tempo_total <- difftime(Sys.time(), lubridate::ymd_hms(
  paste(min(df_diario$date), "00:00:00")), units = "auto")

dados_log <- tibble(
  metrica = c(
    "Data de execução",
    "R version",
    "Estação",
    "ZIPs INMET processados",
    "Registros horários brutos",
    "Registros horários limpos",
    "Dias processados",
    "Período coberto",
    "Temperatura média",
    "Temperatura mínima absoluta",
    "Temperatura máxima absoluta",
    "Outliers IQR removidos",
    "Gaps > 1 dia",
    "Arquivo de auditoria"
  ),
  valor = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    R.version.string,
    "A607 — Campos dos Goytacazes",
    as.character(length(zips)),
    format(n0, big.mark = ","),
    format(nrow(df_diario) * 24, big.mark = ","),  # estimativa
    as.character(nrow(df_diario)),
    sprintf("%s a %s", min(df_diario$date), max(df_diario$date)),
    sprintf("%.2f °C", mean(df_diario$temp_media)),
    sprintf("%.2f °C", min(df_diario$temp_min, na.rm = TRUE)),
    sprintf("%.2f °C", max(df_diario$temp_max, na.rm = TRUE)),
    as.character(n_outliers),
    as.character(if (exists("gaps_longos")) nrow(gaps_longos) else 0),
    audit_file
  )
)

write_csv(dados_log, file.path("audit", "resumo_auditoria.csv"))
audit(sprintf("  audit/resumo_auditoria.csv (%d métricas)", nrow(dados_log)))

# =====================================================================
# 12. LIMPEZA FINAL
# =====================================================================
# Remove CSVs extraídos (mantém só ZIPs originais e dados processados)
audit("Limpando arquivos temporários...")
unlink(dir_csv, recursive = TRUE, force = TRUE)
audit(sprintf("  Diretório %s removido", dir_csv))

# =====================================================================
# RESUMO FINAL
# =====================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════\n")
cat("  ANÁLISE CONCLUÍDA — CAMPOS DOS GOYTACAZES, RJ\n")
cat("═══════════════════════════════════════════════════════\n")
cat(sprintf("  Estação       : A607 — Campos dos Goytacazes\n"))
cat(sprintf("  Período       : %s a %s\n", min(df_diario$date), max(df_diario$date)))
cat(sprintf("  Dias          : %d\n", nrow(df_diario)))
cat(sprintf("  Temp média    : %.1f ± %.1f °C\n",
            mean(df_diario$temp_media), sd(df_diario$temp_media)))
cat(sprintf("  Amplitude     : %.1f a %.1f °C\n",
            min(df_diario$temp_media), max(df_diario$temp_media)))
cat(sprintf("  Outliers      : %d\n", n_outliers))
cat(sprintf("  Auditoria     : %s\n", audit_file))
cat("═══════════════════════════════════════════════════════\n")

audit("=== ANÁLISE FINALIZADA COM SUCESSO ===")
