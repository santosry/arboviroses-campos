# ============================================================================
# 07_RISCO.R — CLASSIFICACAO DE RISCO OFICIAL MS (DENGUE E CHIKUNGUNYA)
# ----------------------------------------------------------------------------
# Gera os caches de distribuicao de risco usados pelo dashboard, aplicando a
# classificacao oficial do Ministerio da Saude (Dengue 6a ed. e documentos
# equivalentes de chikungunya) EXCLUSIVAMENTE sobre sinais e sintomas e fatores
# de risco presentes nos registros individuais do SINAN.
#
# Referencias:
#   * Dengue: BRASIL. Ministerio da Saude. Dengue: diagnostico e manejo clinico
#     – adulto e crianca. 6. ed. Brasilia: Ministerio da Saude, 2024.
#   * Chikungunya: BRASIL. Ministerio da Saude. Chikungunya: manejo clinico.
#     Brasilia: Ministerio da Saude, 2017 (e documentos equivalentes).
#
# Uso:
#   Rscript scripts/07_risco.R
# ============================================================================

suppressMessages(library(dplyr))

source("R/classificacao_risco.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)

# ----------------------------------------------------------------------------
# Leitura dos microdados de dengue com colunas de sinais/sintomas
# ----------------------------------------------------------------------------
ler_dengue_microdados <- function() {
  candidatos <- c(
    root_path("data", "raw", "dengue_sinan_raw.rds"),
    root_path("data", "raw", "dengue_planilhas_locais_raw.rds")
  )
  for (raw_rds in candidatos) {
    if (file.exists(raw_rds)) {
      message("Lendo microdados de dengue de: ", raw_rds)
      return(readRDS(raw_rds))
    }
  }

  # Fallback: consolida as planilhas locais DENGUEYYYY.xlsx (SINAN-DENGUE)
  dir_dados <- root_path("dados_sinan_campos")
  arquivos <- list.files(dir_dados, pattern = "^DENGUE[0-9]{4}\\.xlsx$", full.names = TRUE)
  if (length(arquivos) == 0) {
    warning("Nenhum microdado de dengue encontrado; etapa de risco ignorada. Execute o pipeline de ingestao antes.")
    return(NULL)
  }

  message("Consolidando planilhas locais de dengue...")
  dplyr::bind_rows(lapply(arquivos, function(arq) {
    ano <- as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(basename(arq))))
    df <- suppressMessages(readxl::read_excel(arq, col_types = "text", na = c("", "NA", "N/A", "NULL")))
    df$ano_arquivo <- ano
    df
  }))
}

# ----------------------------------------------------------------------------
# Normalizacao de bairro (mesma regra usada em R/dados.R)
# ----------------------------------------------------------------------------
normalizar_bairro_risco <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  x <- toupper(x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- gsub("\\bPQ\\b", "PARQUE", x)
  x <- gsub("\\bJD\\b", "JARDIM", x)
  x <- gsub("\\bVL\\b", "VILA", x)
  x <- gsub("\\bALPHAVILE\\b", "ALPHAVILLE", x)
  x <- gsub("\\bALFAVILLE\\b", "ALPHAVILLE", x)
  x <- gsub("\\bALPHVILLE\\b", "ALPHAVILLE", x)
  x <- gsub("\\bNOVO JOCKEY\\b", "JOCKEY CLUB", x)
  x <- gsub("\\bJOQUEI\\b", "JOCKEY", x)
  x <- gsub("\\bJOKEY\\b", "JOCKEY", x)
  x <- gsub("\\bGOYTACASES\\b", "GOITACAZES", x)
  x <- gsub("\\bGOYTACAZES\\b", "GOITACAZES", x)
  x <- gsub("\\bMANHHAES\\b", "MANHAES", x)
  x <- gsub("\\bMANAHES\\b", "MANHAES", x)
  x <- gsub("\\bCARIOVA\\b", "CARIOCA", x)
  x <- gsub("\\bII\\b", "2", x)
  x <- gsub("\\bIII\\b", "3", x)
  tools::toTitleCase(tolower(x))
}

# ----------------------------------------------------------------------------
# Processamento principal — DENGUE
# ----------------------------------------------------------------------------
dengue_micro <- ler_dengue_microdados()
if (is.null(dengue_micro) || nrow(dengue_micro) == 0) {
  message("07_risco: sem microdados; etapa concluida sem gerar caches de risco.")
  quit(save = "no", status = 0)
}
message("Registros brutos de dengue: ", nrow(dengue_micro))

# Casos confirmados conforme CLASSI_FIN do layout local/SINAN:
# 10 = Dengue | 11 = Dengue com sinais de alarme | 12 = Dengue grave
confirmados <- dengue_micro %>%
  filter(as.character(CLASSI_FIN) %in% c("10", "11", "12"))
message("Casos confirmados de dengue: ", nrow(confirmados))

# Ano de notificacao
confirmados$Ano <- if ("NU_ANO" %in% names(confirmados)) {
  suppressWarnings(as.integer(confirmados$NU_ANO))
} else {
  suppressWarnings(as.integer(confirmados$ano_arquivo))
}
confirmados$Ano[is.na(confirmados$Ano)] <- suppressWarnings(as.integer(confirmados$ano_arquivo))[is.na(confirmados$Ano)]

# Bairro normalizado
confirmados$NM_BAIRRO <- if ("NM_BAIRRO" %in% names(confirmados)) {
  normalizar_bairro_risco(confirmados$NM_BAIRRO)
} else {
  ""
}

# Aplicacao da classificacao oficial do MS
classificados <- classificar_dengue_df(confirmados)

# Cache 1: distribuicao por grupo de risco (total e por ano)
risco_dengue <- classificados %>%
  filter(!is.na(Ano), !is.na(grupo_risco)) %>%
  count(Ano, Grupo = grupo_risco, name = "Casos") %>%
  arrange(Ano, Grupo)

# Cache 2: distribuicao por bairro (para tabela por area)
risco_dengue_bairros <- classificados %>%
  filter(!is.na(Ano), !is.na(grupo_risco)) %>%
  filter(nzchar(NM_BAIRRO), !grepl("ignorado|sem informa|nao informado", NM_BAIRRO, ignore.case = TRUE)) %>%
  count(Ano, NM_BAIRRO, Grupo = grupo_risco, name = "Casos") %>%
  arrange(Ano, NM_BAIRRO, Grupo)

dir.create(root_path("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(root_path("data", "app_cache"), recursive = TRUE, showWarnings = FALSE)

saveRDS(risco_dengue, root_path("data", "processed", "dengue_risco.rds"))
saveRDS(risco_dengue_bairros, root_path("data", "processed", "dengue_risco_bairros.rds"))
invisible(file.copy(root_path("data", "processed", "dengue_risco.rds"), root_path("data", "app_cache", "dengue_risco_campos_v1.rds"), overwrite = TRUE))
invisible(file.copy(root_path("data", "processed", "dengue_risco_bairros.rds"), root_path("data", "app_cache", "dengue_risco_bairros_campos_v1.rds"), overwrite = TRUE))

message("Distribuicao de risco (dengue) por grupo:")
print(risco_dengue %>% group_by(Grupo) %>% summarise(Casos = sum(Casos), .groups = "drop"))

message("07_risco concluido.")
