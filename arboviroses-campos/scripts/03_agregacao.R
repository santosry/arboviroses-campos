source("R/packages.R", encoding = "UTF-8")
source("R/utils.R", encoding = "UTF-8")
source("R/dados.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "processed"), recursive = TRUE, showWarnings = FALSE)

anos_pipeline <- suppressWarnings(as.integer(strsplit(
  Sys.getenv("ARBOVIROSES_ANOS", "2020,2021,2022,2023,2024,2025"), ","
)[[1]]))
anos_pipeline <- anos_pipeline[is.finite(anos_pipeline)]
if (length(anos_pipeline) == 0) anos_pipeline <- 2020:2025

TEMPORAL_VAZIO <- data.frame(
  Agravo = character(), Intervalo = character(), Periodo = character(),
  Data = as.Date(character()), Ano = integer(), Casos = integer(),
  stringsAsFactors = FALSE
)

agregar_limpo_ou_cache <- function(agravo, fallback) {
  path <- root_path("data", "interim", paste0(tolower(agravo), "_limpo.rds"))
  if (!file.exists(path)) {
    message("Dados limpos de ", agravo, " nao encontrados; usando fallback.")
    return(fallback)
  }
  limpo <- tryCatch(readRDS(path), error = function(e) {
    warning("Falha ao ler dados limpos de ", agravo, ": ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(limpo) || nrow(limpo) == 0) {
    message("Dados limpos de ", agravo, " vazios; usando fallback.")
    return(fallback)
  }
  # FASE 2: agrega os microdados limpos e substitui o fallback.
  agregado <- tryCatch(
    agregar_microdados_limpos(limpo, anos = anos_pipeline, agravo = agravo),
    error = function(e) {
      warning("Falha na agregacao de ", agravo, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(agregado) || nrow(agregado) == 0) {
    message("Agregacao de ", agravo, " vazia; usando fallback.")
    return(fallback)
  }
  message(
    "Microdados de ", agravo, " agregados via fase 2 (", nrow(limpo),
    " registros -> ", nrow(agregado), " linhas anuais)."
  )
  agregado
}

# Resolver variaveis temporais (podem nao existir se R/dados.R foi refatorado)
dengue_temporal <- if (exists("dengue_temporal", inherits = FALSE)) {
  dengue_temporal
} else {
  TEMPORAL_VAZIO
}
zika_temporal <- if (exists("zika_temporal", inherits = FALSE)) {
  zika_temporal
} else {
  TEMPORAL_VAZIO
}

dengue_bairros_agregado <- dengue_bairros
limpo_dengue <- tryCatch(
  {
    p <- root_path("data", "interim", "dengue_limpo.rds")
    if (file.exists(p)) readRDS(p) else NULL
  },
  error = function(e) NULL
)
if (!is.null(limpo_dengue) && nrow(limpo_dengue) > 0) {
  bairros_fase2 <- tryCatch(
    agregar_bairros_microdados_limpos(limpo_dengue, anos = anos_pipeline, agravo = "dengue"),
    error = function(e) {
      warning("Falha ao agregar bairros de dengue: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(bairros_fase2) && nrow(bairros_fase2) > 0) {
    dengue_bairros_agregado <- bairros_fase2
    message("Bairros de dengue agregados via fase 2: ", nrow(bairros_fase2), " linhas.")
  }
}

saveRDS(agregar_limpo_ou_cache("chikungunya", chikungunya), root_path("data", "processed", "chikungunya_agregado.rds"))
saveRDS(agregar_limpo_ou_cache("dengue", dengue), root_path("data", "processed", "dengue_agregado.rds"))
saveRDS(agregar_limpo_ou_cache("zika", zika), root_path("data", "processed", "zika_agregado.rds"))
saveRDS(dengue_temporal, root_path("data", "processed", "dengue_temporal.rds"))
saveRDS(zika_temporal, root_path("data", "processed", "zika_temporal.rds"))
saveRDS(populacao_campos, root_path("data", "processed", "populacao_campos.rds"))
saveRDS(dengue_bairros_agregado, root_path("data", "processed", "dengue_bairros.rds"))

message("03_agregacao concluido.")
