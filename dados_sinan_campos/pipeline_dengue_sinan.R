# =============================================================================
# PIPELINE ANALÍTICO — DENGUE/SINAN | CAMPOS DOS GOYTACAZES (2020–2025)
# Versão com pirâmide etária aprimorada, gráfico de linhas anual e cores vivas
# =============================================================================

# =============================================================================
# 1. PACOTES E CONFIGURAÇÃO GLOBAL
# =============================================================================

pkgs_required <- c(
  "tidyverse", "readxl", "janitor", "lubridate", "tsibble",
  "naniar", "skimr", "sf", "geobr", "stringi",
  "ggridges", "GGally", "patchwork", "scales", "viridis",
  "here", "glue", "DescTools", "RColorBrewer"
)

pkgs_missing <- pkgs_required[!sapply(pkgs_required, requireNamespace, quietly = TRUE)]
if (length(pkgs_missing) > 0) {
  message("Instalando pacotes ausentes: ", paste(pkgs_missing, collapse = ", "))
  install.packages(pkgs_missing, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(tsibble)
  library(sf)
  library(geobr)
  library(stringi)
  library(ggridges)
  library(GGally)
  library(patchwork)
  library(scales)
  library(viridis)
  library(here)
  library(glue)
  library(DescTools)
  library(RColorBrewer)
})

# =============================================================================
# 2. PARÂMETROS GLOBAIS
# =============================================================================

DIR_DADOS    <- "C:/Users/oorie/OneDrive/Documentos/TRABALHOS/ARBOVIROSES/dados_sinan_campos"
DIR_OUTPUT   <- file.path(DIR_DADOS, "outputs")
DIR_FIGURAS  <- file.path(DIR_OUTPUT, "figuras")
DIR_TABELAS  <- file.path(DIR_OUTPUT, "tabelas")
DIR_RELAT    <- file.path(DIR_OUTPUT, "relatorios")

purrr::walk(c(DIR_OUTPUT, DIR_FIGURAS, DIR_TABELAS, DIR_RELAT),
            dir.create, showWarnings = FALSE, recursive = TRUE)

ANO_INICIO     <- 2020L
ANO_FIM        <- 2025L
PADRAO_ARQUIVO <- "^DENGUE(\\d{4})\\.xlsx$"
set.seed(42)

# Tema padrão publicável com ajustes para melhor visualização
theme_sinan <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      strip.background = element_rect(fill = "gray95")
    )
}
theme_set(theme_sinan())

subtitulo_auto <- function(df) {
  glue("Campos dos Goytacazes, RJ \u2014 {ANO_INICIO}\u2013{ANO_FIM} | n = {scales::comma(nrow(df))}")
}

# =============================================================================
# 3. DICIONÁRIOS DE CODIFICAÇÃO
# =============================================================================

DICT_SEXO <- c("M" = "Masculino", "F" = "Feminino", "I" = "Ignorado")
DICT_RACA <- c("1"="Branca","2"="Preta","3"="Amarela","4"="Parda","5"="Indígena","9"="Ignorado")
DICT_CLASSI_FIN <- c("5"="Descartado","10"="Dengue","11"="Dengue c/ alarme","12"="Dengue grave","13"="Chikungunya", "8"="NA")
DICT_EVOLUCAO <- c("1"="Cura","2"="Óbito dengue","3"="Óbito outra","4"="Ignorado","9"="Ignorado")
DICT_HOSPITALIZ <- c("1"="Não","2"="Sim","9"="Ignorado")
DICT_CRITERIO <- c("1"="Laboratorial","2"="Clínico-epidemiológico","3"="Em investigação")
DICT_ZONA <- c("1"="Urbana","2"="Rural","3"="Periurbana","9"="Ignorado")
DICT_ESCOL <- c("0"="Analfabeto","1"="1ª-4ª inc","2"="4ª comp","3"="5ª-8ª inc","4"="Fund comp",
                "5"="Médio inc","6"="Médio comp","7"="Superior inc","8"="Superior comp","9"="Ignorado","10"="Não se aplica")
DICT_GESTANT <- c("1"="1º trim","2"="2º trim","3"="3º trim","4"="Idade g. ignorada","5"="Não","6"="Não se aplica","9"="Ignorado")
DICT_NS1 <- c("1"="Positivo","2"="Negativo","3"="Inconclusivo","4"="Não realizado")
DICT_FLXRET <- c("1"="Sim","2"="Não", "0" ="Não","9"="Ignorado")

VARS_SINTOMAS <- c("febre","mialgia","cefaleia","exantema","vomito","nausea","dor_costas","conjuntvit",
                   "artrite","artralgia","petequia_n","leucopenia","laco","dor_retro")
VARS_COMORB <- c("diabetes","hematolog","hepatopat","renal","hipertensa","acido_pept","auto_imune")

# =============================================================================
# 4. FUNÇÕES AUXILIARES
# =============================================================================

decode_idade_sinan <- function(x) {
  x <- as.character(x)
  codigo <- substr(x, 1, 1)
  mag <- as.numeric(substr(x, 2, nchar(x)))
  unidade <- case_when(codigo=="1"~"horas", codigo=="2"~"dias", codigo=="3"~"meses", codigo=="4"~"anos", TRUE~NA_character_)
  idade_anos <- case_when(codigo=="1"~mag/8760, codigo=="2"~mag/365, codigo=="3"~mag/12, codigo=="4"~mag, TRUE~NA_real_)
  tibble(idade_anos=idade_anos, unidade_idade=unidade)
}

bin_sinan <- function(x) {
  x <- as.character(x)
  case_when(x=="1"~TRUE, x=="2"~FALSE, TRUE~NA)
}

sanear_texto <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x %>% stri_trans_general("Latin-ASCII") %>% tolower() %>% str_trim() %>% str_squish()
}

ler_sinan_dengue <- function(caminho) {
  ano <- str_extract(basename(caminho), "\\d{4}") %>% as.integer()
  message("Lendo: ", basename(caminho))
  tryCatch({
    df <- read_excel(caminho, col_types = "text", na = c("", "NA", "N/A")) %>%
      clean_names() %>% mutate(ano_arquivo = ano, .before=1)
    return(df)
  }, error = function(e) { warning("Falha: ", e$message); NULL })
}

# =============================================================================
# 5. INGESTÃO E CONSOLIDAÇÃO
# =============================================================================

arquivos <- list.files(DIR_DADOS, pattern = PADRAO_ARQUIVO, full.names = TRUE)
if(length(arquivos)==0) stop("Nenhum arquivo encontrado.")

lista <- map(arquivos, ler_sinan_dengue) %>% compact()
dengue_raw <- bind_rows(lista) %>% arrange(ano_arquivo, dt_notific)
message("Consolidado: ", nrow(dengue_raw), " linhas, ", ncol(dengue_raw), " colunas.")

# =============================================================================
# 6. LIMPEZA E PADRONIZAÇÃO (DATAFRAME COMPLETO)
# =============================================================================

dengue_clean <- dengue_raw %>%
  mutate(across(c(dt_notific, dt_sin_pri, dt_nasc, dt_interna, dt_obito, dt_encerra, dt_ns1, dt_soro),
                ~ suppressWarnings(as.Date(case_when(
                  str_detect(., "^\\d{5}$") ~ as.character(as.Date(as.integer(.), origin="1899-12-30")),
                  str_detect(., "^\\d{4}-\\d{2}-\\d{2}") ~ .,
                  TRUE ~ NA_character_
                ))))) %>%
  { idade <- decode_idade_sinan(.$nu_idade_n); bind_cols(., idade) } %>%
  mutate(
    sexo_cat = recode(cs_sexo, !!!DICT_SEXO),
    raca_cat = recode(cs_raca, !!!DICT_RACA),
    classi_fin_txt = recode(classi_fin, !!!DICT_CLASSI_FIN),
    evolucao_txt = recode(evolucao, !!!DICT_EVOLUCAO),
    hospitaliz_txt = recode(hospitaliz, !!!DICT_HOSPITALIZ),
    criterio_txt = recode(criterio, !!!DICT_CRITERIO),
    zona_txt = recode(cs_zona, !!!DICT_ZONA),
    escol_txt = recode(cs_escol_n, !!!DICT_ESCOL),
    gestante_txt = recode(cs_gestant, !!!DICT_GESTANT),
    resul_ns1_txt = recode(resul_ns1, !!!DICT_NS1),
    cs_flxret_txt = recode(cs_flxret, !!!DICT_FLXRET),
    across(any_of(c(VARS_SINTOMAS, VARS_COMORB)), ~ bin_sinan(.)),
    hospitalizado = hospitaliz_txt == "Sim",
    obito_dengue = evolucao_txt == "Óbito dengue",
    caso_confirmado = classi_fin_txt %in% c("Dengue","Dengue c/ alarme","Dengue grave"),
    faixa_etaria = factor(case_when(
      idade_anos < 1 ~ "<1 ano", idade_anos < 5 ~ "1-4 anos", idade_anos < 10 ~ "5-9 anos",
      idade_anos < 20 ~ "10-19 anos", idade_anos < 30 ~ "20-29 anos", idade_anos < 40 ~ "30-39 anos",
      idade_anos < 50 ~ "40-49 anos", idade_anos < 60 ~ "50-59 anos", 
      idade_anos < 70 ~ "60-69 anos", idade_anos >=70 ~ "≥70 anos", TRUE ~ NA_character_
    ), levels = c("<1 ano","1-4 anos","5-9 anos","10-19 anos","20-29 anos",
                  "30-39 anos","40-49 anos","50-59 anos","60-69 anos","≥70 anos")),
    tempo_sin_not = as.numeric(dt_notific - dt_sin_pri),
    tempo_sin_not = if_else(tempo_sin_not<0 | tempo_sin_not>365, NA_real_, tempo_sin_not)
  )

df_analise <- dengue_clean
message("Dataset final para análises: ", nrow(df_analise), " registros.")

# =============================================================================
# 7. ANÁLISES SOLICITADAS
# =============================================================================

# 7.1 Tabela de missingness (opcional)
missing_table <- df_analise %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to="variavel", values_to="pct_missing") %>%
  filter(pct_missing > 0) %>%
  arrange(desc(pct_missing))
write_csv(missing_table, file.path(DIR_TABELAS, "missingness_tabela.csv"))

# 7.2 Gráficos de distribuição para variáveis categóricas (com cores vivas)
vars_categoricas <- c("sexo_cat", "raca_cat", "escol_txt", 
                      "zona_txt", "classi_fin_txt", "criterio_txt", "evolucao_txt", "cs_flxret_txt")
# Paleta de cores personalizada
minhas_cores <- c("#9b59b6", "#2ecc71", "#3498db", "#e74c3c", "#f1c40f", "#e67e22", "#1abc9c", "#e84393")

for (i in seq_along(vars_categoricas)) {
  v <- vars_categoricas[i]
  if(v %in% names(df_analise)) {
    p <- df_analise %>%
      filter(!is.na(!!sym(v))) %>%
      count(!!sym(v), name="n") %>%
      ggplot(aes(x = reorder(!!sym(v), n), y = n)) +
      geom_col(fill = minhas_cores[(i-1) %% length(minhas_cores) + 1]) + coord_flip() +
      labs(title = glue("Distribuição de {v}"), subtitle = subtitulo_auto(df_analise),
           x = v, y = "Frequência absoluta") +
      theme_sinan()
    ggsave(file.path(DIR_FIGURAS, glue("dist_{v}.png")), p, width=8, height=6, dpi=300)
  }
}

# =============================================================================
# 7.3 SEÇÃO ESPECÍFICA — GESTANTES: IDADE, ÓBITOS E DISTRIBUIÇÃO
# =============================================================================

categorias_gestantes <- c("1º trim", "2º trim", "3º trim", "Idade g. ignorada")
niveis_gestacao <- c("1º trim", "2º trim", "3º trim", "Idade g. ignorada", "Não", "Não se aplica", "Ignorado")

if(all(c("gestante_txt", "idade_anos", "evolucao_txt") %in% names(df_analise))) {
  theme_jama <- function(base_size = 11) {
    theme_classic(base_size = base_size, base_family = "Arial") +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray25"),
        axis.title = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 9, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.35),
        axis.ticks = element_line(color = "black", linewidth = 0.35),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.caption = element_text(size = 8, color = "gray35", hjust = 0),
        plot.margin = margin(8, 8, 8, 8)
      )
  }
  
  df_gestantes_notificadas <- df_analise %>%
    mutate(
      gestante_txt = factor(gestante_txt, levels = niveis_gestacao),
      gestante_notificada = gestante_txt %in% categorias_gestantes,
      obito_entre_gestantes = case_when(
        evolucao_txt %in% c("Óbito dengue", "Óbito outra") | !is.na(dt_obito) ~ "Óbito registrado",
        TRUE ~ "Sem óbito registrado"
      ),
      tipo_obito = case_when(
        evolucao_txt == "Óbito dengue" ~ "Óbito por dengue",
        evolucao_txt == "Óbito outra" ~ "Óbito por outra causa",
        !is.na(dt_obito) ~ "Óbito registrado sem causa final",
        TRUE ~ "Sem óbito registrado"
      )
    ) %>%
    filter(gestante_notificada)
  
  df_gestantes <- df_gestantes_notificadas %>%
    filter(caso_confirmado)
  
  if(nrow(df_gestantes) > 0) {
    tabela_gestantes_campo_bruto <- df_analise %>%
      filter(!is.na(gestante_txt)) %>%
      count(gestante_txt, name = "n") %>%
      mutate(
        pct = n / sum(n),
        gestante_txt = factor(gestante_txt, levels = niveis_gestacao)
      ) %>%
      arrange(gestante_txt)
    
    tabela_gestantes_auditoria_definicao <- df_gestantes_notificadas %>%
      count(cs_gestant, gestante_txt, classi_fin, classi_fin_txt, caso_confirmado, name = "n") %>%
      arrange(cs_gestant, desc(caso_confirmado), classi_fin)
    
    tabela_gestantes_cenarios_contagem <- tibble(
      cenario = c(
        "Notificadas com CS_GESTANT 1-4",
        "Confirmadas com CS_GESTANT 1-4",
        "Confirmadas com CS_GESTANT 1-4 e idade <= 70 anos",
        "Confirmadas com CS_GESTANT 1-4 e idade <= 60 anos",
        "Confirmadas com CS_GESTANT 1-3",
        "Confirmadas com CS_GESTANT 1-3 e idade <= 70 anos",
        "Confirmadas com CS_GESTANT 1-3 e idade <= 60 anos"
      ),
      n = c(
        nrow(df_gestantes_notificadas),
        nrow(df_gestantes),
        sum(!is.na(df_gestantes$idade_anos) & df_gestantes$idade_anos <= 70),
        sum(!is.na(df_gestantes$idade_anos) & df_gestantes$idade_anos <= 60),
        sum(df_gestantes$cs_gestant %in% c("1", "2", "3")),
        sum(df_gestantes$cs_gestant %in% c("1", "2", "3") & !is.na(df_gestantes$idade_anos) & df_gestantes$idade_anos <= 70),
        sum(df_gestantes$cs_gestant %in% c("1", "2", "3") & !is.na(df_gestantes$idade_anos) & df_gestantes$idade_anos <= 60)
      )
    )
    
    dados_gestantes_notificadas_auditoria <- df_gestantes_notificadas %>%
      transmute(
        ano_arquivo,
        nu_notific,
        dt_notific,
        cs_sexo,
        sexo_cat,
        cs_gestant_bruto = cs_gestant,
        gestante_txt = factor(gestante_txt, levels = categorias_gestantes),
        nu_idade_n,
        idade_anos,
        classi_fin_bruta = classi_fin,
        classi_fin_txt,
        caso_confirmado,
        evolucao_bruta = evolucao,
        evolucao_txt,
        dt_obito,
        obito_entre_gestantes,
        tipo_obito,
        hospitaliz_bruto = hospitaliz,
        hospitaliz_txt,
        nm_bairro
      ) %>%
      arrange(caso_confirmado, gestante_txt, ano_arquivo, nu_notific)
    
    dados_gestantes_grafico <- df_gestantes %>%
      transmute(
        ano_arquivo,
        nu_notific,
        dt_notific,
        cs_sexo,
        sexo_cat,
        cs_gestant_bruto = cs_gestant,
        gestante_txt = factor(gestante_txt, levels = categorias_gestantes),
        nu_idade_n,
        idade_anos,
        caso_confirmado,
        evolucao_bruta = evolucao,
        evolucao_txt,
        dt_obito,
        obito_entre_gestantes,
        tipo_obito,
        classi_fin_bruta = classi_fin,
        classi_fin_txt,
        hospitaliz_bruto = hospitaliz,
        hospitaliz_txt,
        nm_bairro
      ) %>%
      arrange(gestante_txt, desc(obito_entre_gestantes), idade_anos, ano_arquivo)
    
    tabela_gestantes_distribuicao <- dados_gestantes_grafico %>%
      count(gestante_txt, obito_entre_gestantes, name = "n") %>%
      group_by(gestante_txt) %>%
      mutate(n_categoria = sum(n)) %>%
      ungroup() %>%
      mutate(
        pct_total_gestantes = n / sum(n),
        pct_categoria = n / n_categoria
      ) %>%
      arrange(gestante_txt, obito_entre_gestantes)
    
    tabela_gestantes_obitos <- dados_gestantes_grafico %>%
      count(gestante_txt, obito_entre_gestantes, tipo_obito, evolucao_bruta, evolucao_txt, name = "n") %>%
      arrange(gestante_txt, desc(n))
    
    tabela_gestantes_idade <- dados_gestantes_grafico %>%
      filter(!is.na(idade_anos)) %>%
      group_by(gestante_txt, obito_entre_gestantes) %>%
      summarise(
        n = n(),
        idade_min = min(idade_anos, na.rm = TRUE),
        idade_mediana = median(idade_anos, na.rm = TRUE),
        idade_media = mean(idade_anos, na.rm = TRUE),
        idade_max = max(idade_anos, na.rm = TRUE),
        .groups = "drop"
      )
    
    tabela_gestantes_inconsistencias <- dados_gestantes_grafico %>%
      filter(is.na(idade_anos) | idade_anos < 10 | idade_anos > 55) %>%
      arrange(desc(idade_anos), gestante_txt) %>%
      select(
        ano_arquivo, nu_notific, dt_notific, cs_gestant_bruto, gestante_txt,
        nu_idade_n, idade_anos, evolucao_bruta, evolucao_txt, dt_obito,
        tipo_obito, classi_fin_bruta, classi_fin_txt, hospitaliz_bruto,
        hospitaliz_txt, nm_bairro
      )
    
    write_csv(tabela_gestantes_campo_bruto, file.path(DIR_TABELAS, "gestantes_auditoria_campo_bruto.csv"))
    write_csv(tabela_gestantes_auditoria_definicao, file.path(DIR_TABELAS, "gestantes_auditoria_notificadas_vs_confirmadas.csv"))
    write_csv(tabela_gestantes_cenarios_contagem, file.path(DIR_TABELAS, "gestantes_auditoria_cenarios_contagem.csv"))
    write_csv(dados_gestantes_notificadas_auditoria, file.path(DIR_TABELAS, "gestantes_auditoria_notificadas_linha_a_linha.csv"))
    write_csv(dados_gestantes_grafico, file.path(DIR_TABELAS, "gestantes_grafico_dados_linha_a_linha.csv"))
    write_csv(tabela_gestantes_distribuicao, file.path(DIR_TABELAS, "gestantes_grafico_distribuicao.csv"))
    write_csv(tabela_gestantes_obitos, file.path(DIR_TABELAS, "gestantes_idade_obitos_resumo.csv"))
    write_csv(tabela_gestantes_idade, file.path(DIR_TABELAS, "gestantes_grafico_idade_resumo.csv"))
    write_csv(tabela_gestantes_inconsistencias, file.path(DIR_TABELAS, "gestantes_auditoria_inconsistencias_idade.csv"))
    
    n_obitos_gestantes <- sum(df_gestantes$obito_entre_gestantes == "Óbito registrado", na.rm = TRUE)
    n_idades_inconsistentes <- nrow(tabela_gestantes_inconsistencias)
    n_gestantes_notificadas <- nrow(df_gestantes_notificadas)
    idade_max_plot <- dados_gestantes_grafico %>%
      filter(!is.na(idade_anos)) %>%
      summarise(max_idade = max(idade_anos, na.rm = TRUE)) %>%
      pull(max_idade) %>%
      ceiling()
    idade_max_plot <- max(60, idade_max_plot)
    paleta_obito_jama <- c("Sem óbito registrado" = "#4D4D4D", "Óbito registrado" = "#B2182B")
    
    p_gestantes_distribuicao <- tabela_gestantes_distribuicao %>%
      group_by(gestante_txt) %>%
      mutate(
        rotulo = if_else(obito_entre_gestantes == "Óbito registrado" | n_categoria <= 35,
                         as.character(n), NA_character_),
        gestante_txt = factor(gestante_txt, levels = rev(categorias_gestantes))
      ) %>%
      ungroup() %>%
      ggplot(aes(x = gestante_txt, y = n, fill = obito_entre_gestantes)) +
      geom_col(width = 0.7, color = "black", linewidth = 0.25) +
      geom_text(aes(label = rotulo), position = position_stack(vjust = 0.5),
                size = 3, color = "white", na.rm = TRUE) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = paleta_obito_jama, name = "Evolução") +
      labs(
        title = "Distribuição das gestantes com dengue confirmado",
        subtitle = glue("Confirmadas: {comma(nrow(df_gestantes))} de {comma(n_gestantes_notificadas)} notificadas | Óbitos registrados entre confirmadas: {comma(n_obitos_gestantes)} | Idades para auditar: {comma(n_idades_inconsistentes)}"),
        x = "Categoria gestacional",
        y = "Número de gestantes",
        caption = "Fonte: SINAN/SVS/MS. Gestantes definidas por CS_GESTANT = 1, 2, 3 ou 4; dengue confirmado por CLASSI_FIN = 10, 11 ou 12."
      ) +
      theme_jama()
    
    ggsave(file.path(DIR_FIGURAS, "gestantes_idade_obitos_distribuicao.png"),
           p_gestantes_distribuicao, width = 9, height = 5.5, dpi = 300)
    
    message("Seção de gestantes salva em: ", file.path(DIR_FIGURAS, "gestantes_idade_obitos_distribuicao.png"))
  } else {
    warning("Nenhuma gestante identificada nas categorias: ", paste(categorias_gestantes, collapse = ", "))
  }
}

# 7.4 PIRÂMIDE ETÁRIA (exatamente como na imagem: faixas no eixo Y, valores negativos para masculino)
if(all(c("faixa_etaria", "sexo_cat") %in% names(df_analise))) {
  # Garantir que a ordenação das faixas seja decrescente (mais velhos no topo)
  niveis_faixas <- c("≥70 anos", "60-69 anos", "50-59 anos", "40-49 anos", "30-39 anos",
                     "20-29 anos", "10-19 anos", "5-9 anos", "1-4 anos", "<1 ano")
  piramide <- df_analise %>%
    filter(!is.na(faixa_etaria), !is.na(sexo_cat), sexo_cat %in% c("Masculino","Feminino")) %>%
    mutate(faixa_etaria = factor(faixa_etaria, levels = niveis_faixas)) %>%
    count(faixa_etaria, sexo_cat) %>%
    group_by(sexo_cat) %>%
    mutate(n_plot = if_else(sexo_cat == "Masculino", -n, n)) %>%
    ggplot(aes(x = faixa_etaria, y = n_plot, fill = sexo_cat)) +
    geom_col(alpha = 0.85, width = 0.7) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    scale_y_continuous(labels = function(x) comma(abs(x)), name = "Número de casos") +
    scale_fill_manual(values = c("Feminino" = "#e91e63", "Masculino" = "#00bcd4"), name = "Sexo") +
    labs(title = "Distribuição dos casos notificados por sexo e faixa etária",
         subtitle = subtitulo_auto(df_analise),
         x = "Faixa etária", y = "Número de casos",
         caption = "Fonte: SINAN/SVS/MS") +
    theme_sinan() +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 10))
  ggsave(file.path(DIR_FIGURAS, "piramide_etaria.png"), piramide, width=10, height=8, dpi=300)
}

# 7.4 GRÁFICO DE LINHAS - NOTIFICAÇÕES POR ANO (todos os casos)
notificacoes_ano <- df_analise %>%
  filter(!is.na(ano_arquivo)) %>%
  group_by(ano_arquivo) %>%
  summarise(n_notificacoes = n(), .groups = "drop")

p_linha_ano <- notificacoes_ano %>%
  ggplot(aes(x = ano_arquivo, y = n_notificacoes)) +
  geom_line(color = "#181e77", linewidth = 1.5) +
  geom_point(size = 3, color = "#742f70") +
  geom_text(aes(label = comma(n_notificacoes)), vjust = -1, size = 3.5, color = "gray20") +
  scale_x_continuous(breaks = seq(ANO_INICIO, ANO_FIM, by = 1)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.1))) +
  labs(title = "Evolução anual das notificações de dengue",
       subtitle = subtitulo_auto(df_analise),
       x = "Ano", y = "Número de notificações",
       caption = "Fonte: SINAN/SVS/MS") +
  theme_sinan()
ggsave(file.path(DIR_FIGURAS, "linha_notificacoes_ano.png"), p_linha_ano, width=10, height=6, dpi=300)

# =============================================================================
# 7.5 GRÁFICO DE BARRAS - TOP 10 UNIDADES (CNES ou código bruto)
# =============================================================================

# Se já existir o nome traduzido, usa; senão mantém código original
if(!"hospital_nome" %in% names(df_analise)) {
  df_analise <- df_analise %>%
    mutate(hospital_nome = hospital)
}

p_hosp <- df_analise %>%
  filter(!is.na(hospital_nome)) %>%
  count(hospital_nome, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(hospital_nome, n), y = n)) +
  geom_col(fill = "#e84393") +
  coord_flip() +
  labs(
    title = "Top 10 unidades de saúde (CNES)",
    subtitle = subtitulo_auto(df_analise),
    x = "Unidade de saúde",
    y = "Frequência absoluta"
  ) +
  theme_sinan()

ggsave(
  file.path(DIR_FIGURAS, "barra_hospital_top10.png"),
  p_hosp,
  width = 10,
  height = 8,
  dpi = 300
)

# 7.6 Gráfico de barras conjunto para sintomas e comorbidades (números brutos)
sintomas_comorb <- c(VARS_SINTOMAS, VARS_COMORB)
sintomas_comorb <- sintomas_comorb[sintomas_comorb %in% names(df_analise)]
if(length(sintomas_comorb) > 0) {
  freq_sint_abs <- df_analise %>%
    select(all_of(sintomas_comorb)) %>%
    summarise(across(everything(), ~ sum(. == TRUE, na.rm=TRUE))) %>%
    pivot_longer(everything(), names_to="variavel", values_to="contagem") %>%
    arrange(desc(contagem))
  
  p_sint_abs <- freq_sint_abs %>%
    ggplot(aes(x = reorder(variavel, contagem), y = contagem)) +
    geom_col(fill = "#f39c12") + coord_flip() +
    labs(title = "Contagem absoluta de positividade para sintomas e comorbidades",
         subtitle = subtitulo_auto(df_analise), x = NULL, y = "Número de registros positivos") +
    theme_sinan()
  ggsave(file.path(DIR_FIGURAS, "barras_sintomas_comorb_abs.png"), p_sint_abs, width=10, height=8, dpi=300)
}

# 7.7 Matriz de correlação com p-valor (cores diferenciadas)
df_analise_num <- df_analise %>%
  mutate(
    n_sintomas = rowSums(across(any_of(VARS_SINTOMAS)), na.rm=TRUE),
    n_comorb = rowSums(across(any_of(VARS_COMORB)), na.rm=TRUE)
  )
vars_numericas <- c("idade_anos", "tempo_sin_not", "n_sintomas", "n_comorb")
vars_numericas <- vars_numericas[vars_numericas %in% names(df_analise_num)]
if(length(vars_numericas) >= 2) {
  cor_df <- df_analise_num %>% select(all_of(vars_numericas)) %>% na.omit()
  p_cor <- ggpairs(cor_df, 
                   upper = list(continuous = wrap("cor", size = 4, stars = TRUE)),
                   lower = list(continuous = wrap("smooth", alpha=0.3, color = "#e67e22")),
                   title = "Matriz de correlação com p-valor") +
    theme_sinan()
  ggsave(file.path(DIR_FIGURAS, "matriz_correlacao_pvalor.png"), p_cor, width=10, height=8, dpi=300)
}

# 7.9 Série temporal semanal (com LOESS e IC95%)
serie <- df_analise %>%
  filter(!is.na(dt_notific)) %>%
  mutate(se_index = yearweek(dt_notific)) %>%
  count(se_index, name = "n_casos")
p_serie <- serie %>%
  ggplot(aes(x = se_index, y = n_casos)) +
  geom_col(fill = "#1abc9c", alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.15, se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.2) +
  scale_x_yearweek(labels = function(x) paste0("SE ", str_extract(as.character(x), "\\d{2}$"), "/",
                                               str_extract(as.character(x), "^\\d{4}"))) +
  labs(title = "Casos notificados por semana epidemiológica",
       subtitle = glue("{subtitulo_auto(df_analise)} | Linha vermelha: LOESS com IC95%"),
       x = "Semana", y = "Número de casos") +
  theme_sinan()
ggsave(file.path(DIR_FIGURAS, "serie_temporal_semanal.png"), p_serie, width=12, height=5, dpi=300)

# 7.10 Teste de tendência para hospitalização (Cochran-Armitage)
if(requireNamespace("DescTools", quietly=TRUE)) {
  hosp_trend <- df_analise %>%
    filter(!is.na(ano_arquivo), !is.na(hospitalizado)) %>%
    group_by(ano_arquivo) %>%
    summarise(n_hosp = sum(hospitalizado), n_total = n(), .groups="drop") %>%
    arrange(ano_arquivo) %>%
    filter(n_total > 0)
  if(nrow(hosp_trend) >= 2) {
    mat <- rbind(hosp_trend$n_total - hosp_trend$n_hosp, hosp_trend$n_hosp)
    colnames(mat) <- hosp_trend$ano_arquivo
    ca_test <- DescTools::CochranArmitageTest(mat, alternative = "two.sided")
    res_ca <- tibble(
      Teste = "Cochran-Armitage (hospitalização)",
      Estatística = round(ca_test$statistic, 4),
      p_valor = round(ca_test$p.value, 6),
      Interpretação = ifelse(ca_test$p.value < 0.05, "Tendência significativa", "Sem tendência")
    )
    write_csv(res_ca, file.path(DIR_TABELAS, "teste_tendencia_hospitalizacao.csv"))
  }
}

# =============================================================================
# 10. GRÁFICO DE DISTRIBUIÇÃO DOS BAIRROS (TOP 20)
# =============================================================================

# Contagem de casos por bairro
bairros_count <- df_analise %>%
  filter(!is.na(nm_bairro)) %>%
  count(nm_bairro, name = "n") %>%
  arrange(desc(n))

# Selecionar top 20
top20_bairros <- bairros_count %>%
  slice_head(n = 20)

# Gráfico de barras horizontais
p_bairros_top20 <- top20_bairros %>%
  ggplot(aes(x = reorder(nm_bairro, n), y = n)) +
  geom_col(fill = "#6A9FB5", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = comma(n)), hjust = -0.2, size = 3.2) +
  labs(title = "Distribuição das notificações por bairro (top 20)",
       subtitle = subtitulo_auto(df_analise),
       x = "Bairro", y = "Número de notificações") +
  theme_sinan() +
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank())

# Salvar
ggsave(file.path(DIR_FIGURAS, "distribuicao_bairros_top20.png"),
       plot = p_bairros_top20, width = 10, height = 8, dpi = 300)

message("Gráfico de distribuição dos bairros salvo: ", file.path(DIR_FIGURAS, "distribuicao_bairros_top20.png"))

# =============================================================================
# 8. RELATÓRIO DE SESSÃO
# =============================================================================

sink(file.path(DIR_RELAT, "session_info.txt"))
cat("Data de execução:", Sys.time(), "\n")
cat("Diretório dados:", DIR_DADOS, "\n")
cat("Output:", DIR_OUTPUT, "\n")
cat("Período:", ANO_INICIO, "-", ANO_FIM, "\n")
cat("Registros totais (após limpeza):", nrow(df_analise), "\n\n")
print(sessionInfo())
sink()

message("\n=== PIPELINE CONCLUÍDO ===")
message("Figuras salvas em: ", DIR_FIGURAS)
message("Tabelas salvas em: ", DIR_TABELAS)
