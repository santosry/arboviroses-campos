source("R/packages.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "interim"), recursive = TRUE, showWarnings = FALSE)

normalizar_texto <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  tolower(trimws(gsub("\\s+", " ", x)))
}

primeira_coluna <- function(df, candidatas) {
  cand <- candidatas[candidatas %in% names(df)]
  if (length(cand) == 0) NA_character_ else cand[1]
}

col_ou_na <- function(df, nome) {
  if (nome %in% names(df)) df[[nome]] else rep(NA_character_, nrow(df))
}

parse_data_sinan <- function(x) {
  x <- as.character(x)
  out <- tryCatch(
    suppressWarnings(as.Date(x)),
    error = function(e) rep(as.Date(NA), length(x))
  )
  faltantes <- is.na(out) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)
  out[faltantes] <- suppressWarnings(as.Date(x[faltantes], format = "%d/%m/%Y"))
  faltantes <- is.na(out) & grepl("^\\d{5}$", x)
  out[faltantes] <- suppressWarnings(as.Date(as.integer(x[faltantes]), origin = "1899-12-30"))
  out
}

decode_idade_sinan <- function(x) {
  x <- as.character(x)
  codigo <- substr(x, 1, 1)
  mag <- suppressWarnings(as.numeric(substr(x, 2, nchar(x))))
  dplyr::case_when(
    codigo == "1" ~ mag / 8760,
    codigo == "2" ~ mag / 365,
    codigo == "3" ~ mag / 12,
    codigo == "4" ~ mag,
    TRUE ~ NA_real_
  )
}

limpar_sinan <- function(df, agravo, municipio = Sys.getenv("ARBOVIROSES_MUNICIPIO", "330100")) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  names(df) <- toupper(names(df))
  col_municipio <- primeira_coluna(df, c("ID_MUNICIP", "ID_MN_RESI"))
  if (!is.na(col_municipio)) {
    df <- df[as.character(df[[col_municipio]]) %in% c(municipio, paste0(municipio, "9")), , drop = FALSE]
  }
  col_data <- primeira_coluna(df, c("DT_NOTIFIC", "DT_SIN_PRI"))
  data_notificacao <- if (!is.na(col_data)) parse_data_sinan(df[[col_data]]) else rep(as.Date(NA), nrow(df))
  ano <- suppressWarnings(as.integer(col_ou_na(df, "NU_ANO")))
  ano_fallback <- suppressWarnings(as.integer(col_ou_na(df, "ANO_ARQUIVO")))
  ano[is.na(ano)] <- ano_fallback[is.na(ano)]

  data.frame(
    Agravo = agravo,
    Ano = ano,
    Data_notificacao = data_notificacao,
    Sexo = normalizar_texto(col_ou_na(df, "CS_SEXO")),
    Gestacao = normalizar_texto(col_ou_na(df, "CS_GESTANT")),
    Raca_cor = normalizar_texto(col_ou_na(df, "CS_RACA")),
    Escolaridade = normalizar_texto(col_ou_na(df, "CS_ESCOL_N")),
    Idade = decode_idade_sinan(col_ou_na(df, "NU_IDADE_N")),
    Classificacao = normalizar_texto(col_ou_na(df, "CLASSI_FIN")),
    Evolucao = normalizar_texto(col_ou_na(df, "EVOLUCAO")),
    Bairro = if ("NM_BAIRRO" %in% names(df)) normalizar_texto(df$NM_BAIRRO) else "",
    stringsAsFactors = FALSE
  )
}

ler_raw <- function(nome) {
  path <- root_path("data", "raw", nome)
  if (file.exists(path)) readRDS(path) else data.frame()
}

dengue <- limpar_sinan(ler_raw("dengue_sinan_raw.rds"), "Dengue")
if (nrow(dengue) == 0) dengue <- limpar_sinan(ler_raw("dengue_planilhas_locais_raw.rds"), "Dengue")
zika <- limpar_sinan(ler_raw("zika_sinan_raw.rds"), "Zika")
chikungunya <- limpar_sinan(ler_raw("chikungunya_sinan_raw.rds"), "Chikungunya")

saveRDS(dengue, root_path("data", "interim", "dengue_limpo.rds"))
saveRDS(zika, root_path("data", "interim", "zika_limpo.rds"))
saveRDS(chikungunya, root_path("data", "interim", "chikungunya_limpo.rds"))

message("02_limpeza concluido.")
