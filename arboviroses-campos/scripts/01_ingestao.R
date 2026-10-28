source("R/packages.R", encoding = "UTF-8")

root_path <- function(...) {
  if (requireNamespace("here", quietly = TRUE)) {
    here::here(...)
  } else {
    file.path(getwd(), ...)
  }
}

dir.create(root_path("data", "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(root_path("data", "interim"), recursive = TRUE, showWarnings = FALSE)

anos_pipeline <- as.integer(strsplit(Sys.getenv("ARBOVIROSES_ANOS", "2020,2021,2022,2023,2024,2025"), ",")[[1]])
municipio_pipeline <- Sys.getenv("ARBOVIROSES_MUNICIPIO", "330100")
baixar_microdatasus <- identical(tolower(Sys.getenv("ARBOVIROSES_DOWNLOAD", "false")), "true")

fetch_sinan_microdatasus <- function(agravo, information_system, vars = NULL) {
  out <- root_path("data", "raw", paste0(tolower(agravo), "_sinan_raw.rds"))
  if (file.exists(out) && !baixar_microdatasus) {
    return(readRDS(out))
  }
  if (!baixar_microdatasus) {
    warning("Download desativado para ", agravo, ". Defina ARBOVIROSES_DOWNLOAD=true para baixar via microdatasus.")
    return(data.frame())
  }
  if (!requireNamespace("microdatasus", quietly = TRUE)) {
    stop("Pacote microdatasus ausente. Restaure o ambiente com renv::restore().", call. = FALSE)
  }

  message("Baixando ", agravo, " (", information_system, ") anos ", paste(anos_pipeline, collapse = ", "))
  lista <- lapply(anos_pipeline, function(ano) {
    message("  Ano ", ano, "...")
    bruto <- microdatasus::fetch_datasus(
      year_start = ano,
      year_end = ano,
      uf = "RJ",
      information_system = information_system,
      vars = vars,
      stop_on_error = FALSE,
      timeout = 600
    )
    if (is.null(bruto) || nrow(bruto) == 0) return(NULL)
    bruto$ano_arquivo <- ano
    bruto
  })
  dados <- dplyr::bind_rows(lista)
  if (nrow(dados) > 0) {
    saveRDS(dados, out)
    message("  Salvo: ", nrow(dados), " registros em ", out)
  }
  dados
}

message("Iniciando downloads via microdatasus...")

# Colunas necessarias para limpeza, agregacao (fase 2) e classificacao de risco.
# Reduzir o escopo evita alto consumo de memoria no runner do GitHub Actions.
variaveis_comuns <- c(
  "DT_NOTIFIC", "DT_SIN_PRI", "NU_ANO", "ID_MUNICIP", "ID_MN_RESI",
  "CS_SEXO", "CS_GESTANT", "CS_RACA", "CS_ESCOL_N", "NU_IDADE_N",
  "CLASSI_FIN", "EVOLUCAO"
)
variaveis_dengue <- c(
  variaveis_comuns, "SOROTIPO",
  "PETEQUIA_N", "LACO", "DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA",
  "ALRM_HIPOT", "ALRM_PLAQ", "ALRM_VOM", "ALRM_SANG", "ALRM_HEMAT",
  "ALRM_ABDOM", "ALRM_LETAR", "ALRM_HEPAT", "ALRM_LIQ",
  "GRAV_PULSO", "GRAV_CONV", "GRAV_ENCH", "GRAV_INSUF", "GRAV_TAQUI",
  "GRAV_EXTRE", "GRAV_HIPOT", "GRAV_HEMAT", "GRAV_MELEN", "GRAV_METRO",
  "GRAV_SANG", "GRAV_AST", "GRAV_MIOC", "GRAV_CONSC", "GRAV_ORGAO"
)
variaveis_chik <- c(variaveis_comuns, "SOROTIPO")
variaveis_zika <- variaveis_comuns

fetch_sinan_microdatasus("dengue", "SINAN-DENGUE", vars = variaveis_dengue)
fetch_sinan_microdatasus("zika", "SINAN-ZIKA", vars = variaveis_zika)
fetch_sinan_microdatasus("chikungunya", "SINAN-CHIKUNGUNYA", vars = variaveis_chik)

message("01_ingestao concluido.")
