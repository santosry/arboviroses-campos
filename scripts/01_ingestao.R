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

  lista <- lapply(anos_pipeline, function(ano) {
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
  saveRDS(dados, out)
  dados
}

ler_planilhas_dengue_locais <- function(dir_dados = root_path("dados_sinan_campos")) {
  arquivos <- list.files(dir_dados, pattern = "^DENGUE[0-9]{4}\\.xlsx$", full.names = TRUE)
  dados <- lapply(arquivos, function(path) {
    ano <- as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(basename(path))))
    df <- readxl::read_excel(path, col_types = "text", na = c("", "NA", "N/A", "NULL"))
    df$ano_arquivo <- ano
    df
  })
  dplyr::bind_rows(dados)
}

vars_padrao <- c(
  "NU_ANO", "ID_MUNICIP", "ID_MN_RESI", "CS_SEXO", "CS_GESTANT", "CS_RACA",
  "CS_ESCOL_N", "NU_IDADE_N", "CLASSI_FIN", "EVOLUCAO", "DT_NOTIFIC", "DT_SIN_PRI",
  "NM_BAIRRO", "SOROTIPO"
)

dengue_raw_local <- ler_planilhas_dengue_locais()
saveRDS(dengue_raw_local, root_path("data", "raw", "dengue_planilhas_locais_raw.rds"))

fetch_sinan_microdatasus("dengue", "SINAN-DENGUE", vars_padrao)
fetch_sinan_microdatasus("zika", "SINAN-ZIKA", setdiff(vars_padrao, "SOROTIPO"))
fetch_sinan_microdatasus("chikungunya", "SINAN-CHIKUNGUNYA", setdiff(vars_padrao, "SOROTIPO"))

message("01_ingestao concluido.")
