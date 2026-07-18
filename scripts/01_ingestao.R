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

fetch_sinan_microdatasus("dengue", "SINAN-DENGUE")
fetch_sinan_microdatasus("zika", "SINAN-ZIKA")
fetch_sinan_microdatasus("chikungunya", "SINAN-CHIKUNGUNYA")

message("01_ingestao concluido.")
