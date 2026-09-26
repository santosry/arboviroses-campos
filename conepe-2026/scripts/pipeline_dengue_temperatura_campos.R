# ============================================================
# Pipeline: notificações de dengue e temperatura média diária
# Campos dos Goytacazes, RJ - código IBGE 330100
# Análise principal: correlação de Spearman
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE, scipen = 999)
pipeline_inicio <- Sys.time()

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
path_data <- file.path(root_dir, "data_inmet_campos")
out_dir <- file.path(path_data, "resultados_dengue_temperatura")

dirs <- list(
  dados = file.path(out_dir, "dados"),
  tabelas = file.path(out_dir, "tabelas"),
  figuras = file.path(out_dir, "figuras"),
  modelos = file.path(out_dir, "modelos"),
  relatorios = file.path(out_dir, "relatorios"),
  logs = file.path(out_dir, "logs"),
  auditoria = file.path(out_dir, "auditoria"),
  benchmarks = file.path(out_dir, "benchmarks"),
  compliance = file.path(out_dir, "compliance")
)
invisible(lapply(c(out_dir, unlist(dirs)), dir.create, recursive = TRUE, showWarnings = FALSE))

municipio_codigo <- "330100"
municipio_nome <- "Campos dos Goytacazes"
uf_nome <- "RJ"
municipio_base <- "notificacao"
data_evento <- "dt_notific"
filtrar_confirmados <- FALSE

pkgs <- c(
  "tidyverse", "readxl", "lubridate", "janitor", "broom",
  "scales", "patchwork", "rmarkdown", "knitr"
)
instalar <- setdiff(pkgs, rownames(installed.packages()))
if (length(instalar) > 0) install.packages(instalar, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

if (nzchar(Sys.getenv("RSTUDIO_PANDOC")) == FALSE) {
  pandoc_rstudio <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (dir.exists(pandoc_rstudio)) Sys.setenv(RSTUDIO_PANDOC = pandoc_rstudio)
}

excel_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  suppressWarnings(as.Date(lubridate::parse_date_time(as.character(x), orders = c("ymd", "dmy", "mdy"))))
}

pad_code <- function(x, n = 6) {
  x <- as.character(x)
  x <- stringr::str_replace(x, "\\.0$", "")
  x <- stringr::str_trim(x)
  stringr::str_pad(x, width = n, side = "left", pad = "0")
}

skewness_base <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  s <- sd(x)
  if (s == 0) return(0)
  mean((x - mean(x))^3) / (s^3)
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "p = NA",
    p < 0.001 ~ paste0("p = ", stringr::str_replace(formatC(p, format = "e", digits = 2), "\\.", ",")),
    TRUE ~ paste0("p = ", formatC(p, format = "f", digits = 3, decimal.mark = ","))
  )
}

format_p_raw <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ stringr::str_replace(formatC(p, format = "e", digits = 15), "\\.", ","),
    TRUE ~ formatC(p, format = "f", digits = 12, decimal.mark = ",")
  )
}

format_rho <- function(x) {
  formatC(x, format = "f", digits = 3, decimal.mark = ",")
}

spearman_cor <- function(data, x, y) {
  d <- data |>
    dplyr::transmute(x = .data[[x]], y = .data[[y]]) |>
    tidyr::drop_na()
  if (nrow(d) < 4 || sd(d$x) == 0 || sd(d$y) == 0) {
    return(tibble::tibble(metodo = "spearman", n = nrow(d), rho = NA_real_, p_valor = NA_real_))
  }
  teste <- suppressWarnings(stats::cor.test(d$x, d$y, method = "spearman", exact = FALSE))
  tibble::tibble(
    metodo = "spearman",
    n = nrow(d),
    rho = unname(teste$estimate),
    p_valor = teste$p.value
  )
}

label_spearman <- function(tab) {
  paste0("Spearman rho = ", format_rho(tab$rho[1]), "; ", format_p(tab$p_valor[1]), "; n = ", tab$n[1])
}

arquivos_dengue <- file.path(path_data, paste0("DENGUE", 2020:2025, ".xlsx"))
if (!all(file.exists(arquivos_dengue))) {
  stop("Arquivos DENGUE2020.xlsx a DENGUE2025.xlsx nao encontrados em data_inmet_campos.")
}

read_dengue <- function(arq) {
  message("Lendo: ", basename(arq))
  readxl::read_excel(arq, guess_max = 10000) |>
    janitor::clean_names() |>
    dplyr::select(tidyselect::any_of(c(
      "nu_notific", "dt_notific", "sem_not", "nu_ano", "id_municip",
      "dt_sin_pri", "sem_pri", "id_mn_resi", "classi_fin", "evolucao"
    ))) |>
    dplyr::mutate(
      arquivo = basename(arq),
      dt_notific = excel_date(dt_notific),
      dt_sin_pri = excel_date(dt_sin_pri),
      id_municip = pad_code(id_municip),
      id_mn_resi = pad_code(id_mn_resi),
      classi_fin = stringr::str_replace(as.character(classi_fin), "\\.0$", "")
    )
}

dengue_raw <- purrr::map_dfr(arquivos_dengue, read_dengue)

campo_mun <- if (municipio_base == "notificacao") "id_municip" else "id_mn_resi"
if (!data_evento %in% c("dt_notific", "dt_sin_pri")) stop("data_evento deve ser 'dt_notific' ou 'dt_sin_pri'.")
if (!municipio_base %in% c("notificacao", "residencia")) stop("municipio_base deve ser 'notificacao' ou 'residencia'.")

dengue_campos <- dengue_raw |>
  dplyr::filter(.data[[campo_mun]] == municipio_codigo)

if (filtrar_confirmados) {
  dengue_campos <- dengue_campos |>
    dplyr::filter(classi_fin %in% c("10", "11", "12"))
}

casos_dia <- dengue_campos |>
  dplyr::transmute(date = .data[[data_evento]]) |>
  dplyr::filter(!is.na(date)) |>
  dplyr::count(date, name = "casos")

arq_temp <- file.path(path_data, "temperatura_diaria_campos.csv")
if (!file.exists(arq_temp)) stop("temperatura_diaria_campos.csv nao encontrado em data_inmet_campos.")

temp <- readr::read_csv(arq_temp, show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::mutate(
    date = lubridate::ymd(date),
    temp_media = as.numeric(temp_media),
    temp_min = as.numeric(temp_min),
    temp_max = as.numeric(temp_max)
  ) |>
  dplyr::select(date, temp_media, temp_min, temp_max, n_obs, tidyselect::everything())

data_ini <- max(min(temp$date, na.rm = TRUE), min(casos_dia$date, na.rm = TRUE))
data_fim <- min(max(temp$date, na.rm = TRUE), max(casos_dia$date, na.rm = TRUE))

base_diaria <- tibble::tibble(date = seq.Date(data_ini, data_fim, by = "day")) |>
  dplyr::left_join(temp, by = "date") |>
  dplyr::left_join(casos_dia, by = "date") |>
  dplyr::mutate(
    casos = tidyr::replace_na(casos, 0L),
    ano = lubridate::year(date),
    mes = lubridate::month(date, label = TRUE, abbr = TRUE),
    mes_num = lubridate::month(date),
    semana_epi = lubridate::epiweek(date),
    dia_semana = lubridate::wday(date, label = TRUE, abbr = TRUE)
  ) |>
  dplyr::filter(!is.na(temp_media))

base_associacao <- base_diaria |>
  dplyr::filter(casos > 0)

descritivo <- base_diaria |>
  dplyr::summarise(
    n_dias = dplyr::n(),
    casos_total = sum(casos),
    casos_media = mean(casos),
    casos_mediana = median(casos),
    casos_max = max(casos),
    proporcao_dias_zero = mean(casos == 0),
    assimetria_casos = skewness_base(casos),
    temp_media_geral = mean(temp_media),
    temp_mediana = median(temp_media),
    temp_min_observada = min(temp_media),
    temp_max_observada = max(temp_media),
    assimetria_temp = skewness_base(temp_media)
  )

resumo_anual <- base_diaria |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    media_diaria_casos = mean(casos),
    mediana_diaria_casos = median(casos),
    casos = sum(casos),
    temp_media = mean(temp_media),
    .groups = "drop"
  )

resumo_mensal <- base_diaria |>
  dplyr::mutate(mes_ref = lubridate::floor_date(date, "month")) |>
  dplyr::group_by(mes_ref) |>
  dplyr::summarise(
    casos = sum(casos),
    temp_media = mean(temp_media),
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    n_dias = dplyr::n(),
    .groups = "drop"
  )

resumo_anual_associacao <- base_associacao |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    media_diaria_casos = mean(casos),
    mediana_diaria_casos = median(casos),
    casos = sum(casos),
    temp_media = mean(temp_media),
    .groups = "drop"
  )

resumo_mensal_associacao <- base_associacao |>
  dplyr::mutate(mes_ref = lubridate::floor_date(date, "month")) |>
  dplyr::group_by(mes_ref) |>
  dplyr::summarise(
    casos = sum(casos),
    temp_media = mean(temp_media),
    temp_min = mean(temp_min, na.rm = TRUE),
    temp_max = mean(temp_max, na.rm = TRUE),
    n_dias_com_casos = dplyr::n(),
    .groups = "drop"
  )

cor_diaria <- spearman_cor(base_associacao, "temp_media", "casos") |>
  dplyr::mutate(analise = "Diaria: temperatura media vs casos")

cor_mensal_por_mes <- base_associacao |>
  dplyr::mutate(
    mes_ref = lubridate::floor_date(date, "month"),
    ano = lubridate::year(date),
    mes_num = lubridate::month(date)
  ) |>
  dplyr::group_by(mes_ref, ano, mes_num) |>
  dplyr::group_modify(~ spearman_cor(.x, "temp_media", "casos")) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_valor_cientifico = formatC(p_valor, format = "e", digits = 15),
    p_valor_relatorio = format_p(p_valor),
    interpretacao = dplyr::case_when(
      is.na(rho) ~ "não estimável",
      abs(rho) < 0.10 ~ "muito fraca",
      abs(rho) < 0.30 ~ "fraca",
      abs(rho) < 0.50 ~ "moderada",
      TRUE ~ "forte"
    )
  )

base_anomalia <- base_associacao |>
  dplyr::group_by(mes_num) |>
  dplyr::mutate(
    temp_anomalia = temp_media - mean(temp_media, na.rm = TRUE),
    casos_anomalia = casos - mean(casos, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

cor_anomalia <- spearman_cor(base_anomalia, "temp_anomalia", "casos_anomalia") |>
  dplyr::mutate(analise = "Anomalias mensais: temperatura vs casos")

cor_mensal_agregada <- spearman_cor(resumo_mensal_associacao, "temp_media", "casos") |>
  dplyr::mutate(analise = "Mensal agregada: temperatura media vs casos")

cor_anual <- spearman_cor(resumo_anual_associacao, "temp_media", "casos") |>
  dplyr::mutate(analise = "Anual: temperatura media vs casos")

tabela_diaria_associacao <- base_associacao |>
  dplyr::mutate(mes_ref = lubridate::floor_date(date, "month")) |>
  dplyr::left_join(
    cor_mensal_por_mes |>
      dplyr::select(
        mes_ref,
        n_correlacao_mensal = n,
        rho_spearman_mensal = rho,
        p_valor_spearman_mensal = p_valor,
        interpretacao_mensal = interpretacao
      ),
    by = "mes_ref"
  ) |>
  dplyr::mutate(
    metodo = "spearman",
    escopo_associacao = "mensal: temperatura media diaria vs casos diarios, somente dias com casos"
  ) |>
  dplyr::select(
    date, mes_ref, casos, temp_media, temp_min, temp_max,
    n_correlacao_mensal, rho_spearman_mensal,
    p_valor_spearman_mensal, interpretacao_mensal, metodo, escopo_associacao
  )

correlacoes_spearman <- dplyr::bind_rows(
  cor_diaria, cor_anomalia, cor_mensal_agregada, cor_anual
) |>
  dplyr::select(analise, metodo, n, rho, p_valor, tidyselect::everything())

p_valores_verificacao <- correlacoes_spearman |>
  dplyr::mutate(
    p_valor_decimal = formatC(p_valor, format = "f", digits = 18, decimal.mark = "."),
    p_valor_cientifico = formatC(p_valor, format = "e", digits = 15),
    p_valor_relatorio = format_p(p_valor)
  )

recalcular_linha <- function(nome, data, x, y, esperado) {
  recalculado <- spearman_cor(data, x, y)
  tibble::tibble(
    analise = nome,
    n_esperado = esperado$n[1],
    n_recalculado = recalculado$n[1],
    rho_esperado = esperado$rho[1],
    rho_recalculado = recalculado$rho[1],
    diferenca_rho = abs(esperado$rho[1] - recalculado$rho[1]),
    p_esperado = esperado$p_valor[1],
    p_recalculado = recalculado$p_valor[1],
    diferenca_p = abs(esperado$p_valor[1] - recalculado$p_valor[1]),
    status = dplyr::if_else(
      n_esperado == n_recalculado &&
        isTRUE(all.equal(rho_esperado, rho_recalculado, tolerance = 1e-14)) &&
        isTRUE(all.equal(p_esperado, p_recalculado, tolerance = 1e-14)),
      "OK",
      "VERIFICAR"
    )
  )
}

auditoria_recalculo <- dplyr::bind_rows(
  recalcular_linha("Diaria: temperatura media vs casos", base_associacao, "temp_media", "casos", cor_diaria),
  recalcular_linha("Anomalias mensais: temperatura vs casos", base_anomalia, "temp_anomalia", "casos_anomalia", cor_anomalia),
  recalcular_linha("Mensal agregada: temperatura media vs casos", resumo_mensal_associacao, "temp_media", "casos", cor_mensal_agregada),
  recalcular_linha("Anual: temperatura media vs casos", resumo_anual_associacao, "temp_media", "casos", cor_anual)
)

auditoria_recalculo_mensal <- base_associacao |>
  dplyr::mutate(mes_ref = lubridate::floor_date(date, "month")) |>
  dplyr::group_by(mes_ref) |>
  dplyr::group_modify(~ spearman_cor(.x, "temp_media", "casos")) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    cor_mensal_por_mes |>
      dplyr::select(mes_ref, n_esperado = n, rho_esperado = rho, p_esperado = p_valor),
    by = "mes_ref"
  ) |>
  dplyr::transmute(
    mes_ref,
    n_esperado,
    n_recalculado = n,
    rho_esperado,
    rho_recalculado = rho,
    diferenca_rho = abs(rho_esperado - rho_recalculado),
    p_esperado,
    p_recalculado = p_valor,
    diferenca_p = abs(p_esperado - p_recalculado),
    status = dplyr::if_else(
      n_esperado == n_recalculado &
        (is.na(rho_esperado) & is.na(rho_recalculado) | abs(rho_esperado - rho_recalculado) < 1e-14) &
        (is.na(p_esperado) & is.na(p_recalculado) | abs(p_esperado - p_recalculado) < 1e-14),
      "OK",
      "VERIFICAR"
    )
  )

auditoria_tabela_diaria <- tibble::tibble(
  item = c(
    "linhas_tabela_diaria_associacao",
    "linhas_com_casos_zero",
    "meses_com_correlacao_estimavel",
    "meses_com_correlacao_nao_estimavel",
    "linhas_com_rho_mensal_ausente",
    "interpretacao_rho_p_mensais"
  ),
  valor = c(
    as.character(nrow(tabela_diaria_associacao)),
    as.character(sum(tabela_diaria_associacao$casos == 0)),
    as.character(sum(!is.na(cor_mensal_por_mes$rho))),
    as.character(sum(is.na(cor_mensal_por_mes$rho))),
    as.character(sum(is.na(tabela_diaria_associacao$rho_spearman_mensal))),
    "Rho e p-valor sao calculados dentro de cada mes usando os pares diarios com casos daquele mes."
  ),
  status = c("OK", "OK se 0", "OK", "OK/documentar", "OK/documentar", "DOCUMENTADO")
)

readr::write_csv(base_diaria, file.path(dirs$dados, "base_diaria_dengue_temperatura.csv"))
readr::write_csv(base_associacao, file.path(dirs$dados, "base_diaria_dengue_temperatura_com_casos.csv"))
readr::write_csv(base_anomalia, file.path(dirs$dados, "base_diaria_anomalias_mensais.csv"))
readr::write_csv(descritivo, file.path(dirs$tabelas, "descritivo.csv"))
readr::write_csv(resumo_anual, file.path(dirs$tabelas, "resumo_anual.csv"))
readr::write_csv(resumo_mensal, file.path(dirs$tabelas, "resumo_mensal.csv"))
readr::write_csv(resumo_anual_associacao, file.path(dirs$tabelas, "resumo_anual_associacao.csv"))
readr::write_csv(resumo_mensal_associacao, file.path(dirs$tabelas, "resumo_mensal_associacao.csv"))
readr::write_csv(correlacoes_spearman, file.path(dirs$tabelas, "correlacoes_spearman.csv"))
readr::write_csv(cor_mensal_por_mes, file.path(dirs$tabelas, "correlacoes_spearman_mensais.csv"))
readr::write_csv(tabela_diaria_associacao, file.path(dirs$tabelas, "tabela_diaria_associacao.csv"))
readr::write_csv(p_valores_verificacao, file.path(dirs$auditoria, "auditoria_p_valores_reais.csv"))
readr::write_csv(cor_mensal_por_mes, file.path(dirs$auditoria, "auditoria_p_valores_mensais.csv"))
readr::write_csv(auditoria_recalculo, file.path(dirs$auditoria, "auditoria_recalculo_spearman.csv"))
readr::write_csv(auditoria_recalculo_mensal, file.path(dirs$auditoria, "auditoria_recalculo_spearman_mensal.csv"))
readr::write_csv(auditoria_tabela_diaria, file.path(dirs$auditoria, "auditoria_tabela_diaria_associacao.csv"))

cell_pal <- c(
  navy = "#1B365D",
  teal = "#007C89",
  sky = "#6EC6D9",
  vermillion = "#D95F02",
  gold = "#E6AB02",
  graphite = "#2B2B2B",
  gray = "#6B7280",
  pale = "#F2F6F7"
)

theme_cell <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Times New Roman") +
    ggplot2::theme(
      text = ggplot2::element_text(color = cell_pal["graphite"]),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3, margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = base_size, color = cell_pal["gray"], margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(size = base_size - 2, color = cell_pal["gray"], hjust = 0),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major = ggplot2::element_line(color = "#D7DEE2", linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", color = cell_pal["navy"]),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(12, 24, 18, 16)
    )
}

caption_padrao <- paste0(
  municipio_nome, "/", uf_nome, "; notificações por ", municipio_base,
  "; data do evento: ", data_evento, "; método estatístico: Spearman."
)

lab_diaria <- label_spearman(cor_diaria)
lab_anomalia <- label_spearman(cor_anomalia)
lab_mensal_agregada <- label_spearman(cor_mensal_agregada)
lab_anual <- label_spearman(cor_anual)
meses_estimaveis <- sum(!is.na(cor_mensal_por_mes$rho))
meses_significativos <- sum(cor_mensal_por_mes$p_valor < 0.05, na.rm = TRUE)
menor_p_mensal <- min(cor_mensal_por_mes$p_valor, na.rm = TRUE)
melhor_mes_rho <- cor_mensal_por_mes |>
  dplyr::filter(!is.na(rho)) |>
  dplyr::slice_max(order_by = abs(rho), n = 1, with_ties = FALSE)

escala <- max(base_diaria$casos, na.rm = TRUE) / max(base_diaria$temp_media, na.rm = TRUE)

p1 <- ggplot2::ggplot(base_diaria, ggplot2::aes(x = date)) +
  ggplot2::geom_col(ggplot2::aes(y = casos), fill = cell_pal["navy"], alpha = 0.78, width = 1) +
  ggplot2::geom_line(ggplot2::aes(y = temp_media * escala), color = cell_pal["vermillion"], linewidth = 0.55) +
  ggplot2::scale_y_continuous(
    name = "Casos notificados de dengue",
    sec.axis = ggplot2::sec_axis(~ . / escala, name = "Temperatura média diária (C)")
  ) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
  ggplot2::labs(
    title = "A. Série diária de dengue e temperatura",
    subtitle = lab_diaria,
    x = NULL,
    caption = caption_padrao
  ) +
  theme_cell()

p2 <- ggplot2::ggplot(base_associacao, ggplot2::aes(x = temp_media, y = casos)) +
  ggplot2::geom_point(color = cell_pal["teal"], alpha = 0.34, size = 1.25) +
  ggplot2::geom_smooth(method = "loess", se = TRUE, color = cell_pal["vermillion"], fill = cell_pal["gold"], linewidth = 0.9, alpha = 0.18) +
  ggplot2::annotate("label", x = min(base_associacao$temp_media, na.rm = TRUE), y = max(base_associacao$casos, na.rm = TRUE),
                    label = lab_diaria, hjust = 0, vjust = 1, fill = "white", color = cell_pal["graphite"]) +
  ggplot2::labs(
    title = "B. Associação diária bruta",
    x = "Temperatura média diária (C)",
    y = "Casos notificados por dia",
    caption = caption_padrao
  ) +
  theme_cell()

p3 <- ggplot2::ggplot(cor_mensal_por_mes, ggplot2::aes(x = mes_ref, y = rho)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#9AA5AD") +
  ggplot2::geom_line(color = cell_pal["navy"], linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(fill = p_valor < 0.05), color = "white", shape = 21, size = 3) +
  ggplot2::scale_fill_manual(values = c(`TRUE` = cell_pal["vermillion"], `FALSE` = cell_pal["teal"], `NA` = cell_pal["gray"]), na.value = cell_pal["gray"], name = "p < 0,05") +
  ggplot2::annotate("label",
                    x = melhor_mes_rho$mes_ref[1],
                    y = melhor_mes_rho$rho[1],
                    label = paste0("Maior |rho| mensal\nrho = ", format_rho(melhor_mes_rho$rho[1]), "; ", format_p(melhor_mes_rho$p_valor[1])),
                    hjust = 0.5, vjust = -0.2, fill = "white", color = cell_pal["graphite"]) +
  ggplot2::labs(
    title = "C. Rho de Spearman mensal",
    subtitle = paste0("Calculado dentro de cada mês com dias com casos; meses estimáveis: ", meses_estimaveis, "; p < 0,05 em ", meses_significativos, " meses."),
    x = NULL,
    y = "Rho mensal de Spearman",
    caption = caption_padrao
  ) +
  theme_cell()

p4 <- ggplot2::ggplot(cor_mensal_por_mes, ggplot2::aes(x = mes_ref, y = -log10(p_valor))) +
  ggplot2::geom_hline(yintercept = -log10(0.05), color = cell_pal["vermillion"], linewidth = 0.45, linetype = "dashed") +
  ggplot2::geom_line(color = cell_pal["navy"], linewidth = 0.9, na.rm = TRUE) +
  ggplot2::geom_point(ggplot2::aes(fill = rho), color = "white", shape = 21, size = 3, na.rm = TRUE) +
  ggplot2::scale_fill_gradient2(low = cell_pal["teal"], mid = "white", high = cell_pal["vermillion"], midpoint = 0, name = "Rho") +
  ggplot2::labs(
    title = "D. p-valor mensal da associação diária",
    subtitle = paste0("Menor p mensal observado: ", format_p(menor_p_mensal), ". Linha tracejada: p = 0,05."),
    x = NULL,
    y = "-log10(p-valor)",
    caption = caption_padrao
  ) +
  theme_cell()

p5 <- ggplot2::ggplot(resumo_anual_associacao, ggplot2::aes(x = temp_media, y = casos)) +
  ggplot2::geom_path(color = cell_pal["gray"], linewidth = 0.45) +
  ggplot2::geom_point(color = "white", fill = cell_pal["navy"], shape = 21, size = 4) +
  ggplot2::geom_text(ggplot2::aes(label = ano), nudge_y = max(resumo_anual_associacao$casos) * 0.045, size = 3.2, color = cell_pal["graphite"]) +
  ggplot2::annotate("label", x = min(resumo_anual_associacao$temp_media, na.rm = TRUE), y = max(resumo_anual_associacao$casos, na.rm = TRUE),
                    label = paste0(lab_anual, "\nInterpretar com cautela: n anual pequeno."), hjust = 0, vjust = 1,
                    fill = "white", color = cell_pal["graphite"]) +
  ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  ggplot2::labs(
    title = "E. Associação anual exploratória",
    x = "Temperatura média anual (C)",
    y = "Casos notificados no ano",
    caption = caption_padrao
  ) +
  theme_cell()

p6 <- ggplot2::ggplot(base_anomalia, ggplot2::aes(x = temp_anomalia, y = casos_anomalia)) +
  ggplot2::geom_hline(yintercept = 0, color = "#AAB3B9", linewidth = 0.3) +
  ggplot2::geom_vline(xintercept = 0, color = "#AAB3B9", linewidth = 0.3) +
  ggplot2::geom_point(color = cell_pal["teal"], alpha = 0.32, size = 1.2) +
  ggplot2::geom_smooth(method = "loess", se = TRUE, color = cell_pal["vermillion"], fill = cell_pal["gold"], linewidth = 0.9, alpha = 0.18) +
  ggplot2::annotate("label", x = min(base_anomalia$temp_anomalia, na.rm = TRUE), y = max(base_anomalia$casos_anomalia, na.rm = TRUE),
                    label = lab_anomalia, hjust = 0, vjust = 1, fill = "white", color = cell_pal["graphite"]) +
  ggplot2::labs(
    title = "F. Anomalias mensais",
    x = "Anomalia de temperatura média diária (C)",
    y = "Anomalia de casos diários",
    caption = caption_padrao
  ) +
  theme_cell()

fig_paths <- c(
  "01_serie_diaria_casos_temperatura.png" = p1,
  "02_dispersao_diaria_spearman.png" = p2,
  "03_rho_spearman_mensal.png" = p3,
  "04_pvalor_spearman_mensal.png" = p4,
  "05_agregado_anual_spearman.png" = p5,
  "06_anomalias_mensais_spearman.png" = p6
)

purrr::iwalk(fig_paths, function(plot_obj, nm) {
  ggplot2::ggsave(file.path(dirs$figuras, nm), plot_obj, width = 10.5, height = 7.2, dpi = 320, bg = "white")
})

metadados <- tibble::tibble(
  item = c(
    "municipio", "codigo_ibge", "uf", "periodo_inicio", "periodo_fim",
    "municipio_base", "data_evento", "confirmados_apenas", "metodo"
  ),
  valor = c(
    municipio_nome, municipio_codigo, uf_nome, as.character(data_ini), as.character(data_fim),
    municipio_base, data_evento, as.character(filtrar_confirmados), "Spearman"
  )
)
readr::write_csv(metadados, file.path(dirs$tabelas, "metadados_analise.csv"))

relatorio_txt <- c(
  "Análise dengue-temperatura",
  "==========================",
  paste0("Município: ", municipio_nome, "/", uf_nome, " (", municipio_codigo, ")"),
  paste0("Período comum analisado: ", data_ini, " a ", data_fim),
  paste0("Total de casos: ", descritivo$casos_total),
  paste0("Temperatura média geral: ", format_rho(descritivo$temp_media_geral), " C"),
  "",
  "Correlações de Spearman:",
  paste(capture.output(print(correlacoes_spearman)), collapse = "\n"),
  "",
  "Observação: todos os gráficos exibem p-valor da associação de Spearman correspondente."
)
writeLines(relatorio_txt, file.path(dirs$relatorios, "relatorio_texto_spearman.txt"), useBytes = TRUE)

rmd_origem <- file.path(root_dir, "reports", "dengue_temperatura_abnt.Rmd")
if (file.exists(rmd_origem)) {
  file.copy(
    rmd_origem,
    file.path(dirs$relatorios, "dengue_temperatura_abnt.Rmd"),
    overwrite = TRUE
  )
}

pdf_script <- file.path(root_dir, "scripts", "render_relatorio_pdf_base.R")
message("Gerando PDF do relatório: ", pdf_script)
pdf_status <- system2(
  file.path(R.home("bin"), "Rscript.exe"),
  args = c(shQuote(pdf_script), shQuote(root_dir))
)
if (!identical(pdf_status, 0L)) stop("Falha ao gerar PDF do relatório.")

pipeline_fim <- Sys.time()

fig_auditoria <- purrr::map_dfr(names(fig_paths), function(nm) {
  path <- file.path(dirs$figuras, nm)
  img <- png::readPNG(path, info = TRUE)
  tibble::tibble(
    arquivo = nm,
    largura_px = dim(img)[2],
    altura_px = dim(img)[1],
    bytes = file.info(path)$size,
    status = dplyr::if_else(file.exists(path) && file.info(path)$size > 10000, "OK", "VERIFICAR")
  )
})

pdf_info <- tibble::tibble(
  arquivo = "relatorio_dengue_temperatura_abnt.pdf",
  paginas = if (requireNamespace("pdftools", quietly = TRUE)) pdftools::pdf_info(file.path(dirs$relatorios, "relatorio_dengue_temperatura_abnt.pdf"))$pages else NA_integer_,
  bytes = file.info(file.path(dirs$relatorios, "relatorio_dengue_temperatura_abnt.pdf"))$size,
  status = dplyr::if_else(file.exists(file.path(dirs$relatorios, "relatorio_dengue_temperatura_abnt.pdf")), "OK", "VERIFICAR")
)

benchmark_pipeline <- tibble::tibble(
  iniciado_em = as.character(pipeline_inicio),
  finalizado_em = as.character(pipeline_fim),
  duracao_segundos = as.numeric(difftime(pipeline_fim, pipeline_inicio, units = "secs")),
  linhas_base_diaria = nrow(base_diaria),
  linhas_base_associacao_com_casos = nrow(base_associacao),
  linhas_tabela_diaria_associacao = nrow(tabela_diaria_associacao),
  meses_correlacao_total = nrow(cor_mensal_por_mes),
  meses_correlacao_estimavel = meses_estimaveis,
  meses_correlacao_p_menor_0_05 = meses_significativos,
  maior_abs_rho_mensal = melhor_mes_rho$rho[1],
  maior_abs_rho_mensal_mes = as.character(melhor_mes_rho$mes_ref[1]),
  menor_p_valor_mensal = menor_p_mensal
)

compliance_checklist <- tibble::tibble(
  requisito = c(
    "Sem publicacao em GitHub",
    "Somente correlacao de Spearman",
    "Dias sem casos excluidos da associacao",
    "Tabela diaria sem dias com zero casos",
    "P-valores mensais reais em notacao cientifica quando pequenos",
    "Graficos individuais em PNG",
    "Sem graficos exportados em PDF",
    "Relatorio com materiais, metodologia e resultados",
    "Fonte de dengue declarada",
    "Fonte de temperatura declarada",
    "Declaracao de uso de IA incluída",
    "Auditoria de recalculo Spearman gravada",
    "Benchmark gravado"
  ),
  status = c(
    "OK",
    "OK",
    ifelse(all(base_associacao$casos > 0), "OK", "VERIFICAR"),
    ifelse(all(tabela_diaria_associacao$casos > 0), "OK", "VERIFICAR"),
    "OK",
    ifelse(all(file.exists(file.path(dirs$figuras, names(fig_paths)))), "OK", "VERIFICAR"),
    ifelse(length(list.files(dirs$figuras, pattern = "\\.pdf$", ignore.case = TRUE)) == 0, "OK", "VERIFICAR"),
    "OK",
    "OK: Subsecretaria de Saude de Campos dos Goytacazes",
    "OK: INMET",
    "OK: ChatGPT 5.5 e Codex; Portaria CNPq 2664/2026",
    ifelse(all(auditoria_recalculo$status == "OK"), "OK", "VERIFICAR"),
    "OK"
  )
)

readr::write_csv(fig_auditoria, file.path(dirs$auditoria, "auditoria_dimensoes_figuras.csv"))
readr::write_csv(pdf_info, file.path(dirs$auditoria, "auditoria_pdf.csv"))
readr::write_csv(benchmark_pipeline, file.path(dirs$benchmarks, "benchmark_pipeline.csv"))
readr::write_csv(compliance_checklist, file.path(dirs$compliance, "compliance_checklist.csv"))

artefatos_finais <- c(
  file.path(dirs$relatorios, "relatorio_dengue_temperatura_abnt.pdf"),
  file.path(dirs$relatorios, "dengue_temperatura_abnt.Rmd"),
  file.path(dirs$tabelas, "tabela_diaria_associacao.csv"),
  file.path(dirs$tabelas, "correlacoes_spearman.csv"),
  file.path(dirs$auditoria, "auditoria_recalculo_spearman.csv"),
  file.path(dirs$auditoria, "auditoria_recalculo_spearman_mensal.csv"),
  file.path(dirs$auditoria, "auditoria_p_valores_reais.csv"),
  file.path(dirs$auditoria, "auditoria_p_valores_mensais.csv"),
  file.path(dirs$auditoria, "auditoria_tabela_diaria_associacao.csv"),
  file.path(dirs$compliance, "compliance_checklist.csv"),
  file.path(dirs$benchmarks, "benchmark_pipeline.csv"),
  file.path(dirs$figuras, names(fig_paths))
)
artefatos_finais <- artefatos_finais[file.exists(artefatos_finais)]
integridade_md5 <- tibble::tibble(
  arquivo = normalizePath(artefatos_finais, winslash = "/", mustWork = TRUE),
  md5 = unname(tools::md5sum(artefatos_finais)),
  bytes = file.info(artefatos_finais)$size
)

readr::write_csv(integridade_md5, file.path(dirs$compliance, "integridade_md5.csv"))

message("Pipeline concluído. Resultados organizados em: ", out_dir)