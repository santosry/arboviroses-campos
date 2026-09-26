source("R/packages.R", encoding = "UTF-8")
source("R/utils.R", encoding = "UTF-8")
source("R/dados.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "processed"), recursive = TRUE, showWarnings = FALSE)

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
  # FASE 2: Quando o pipeline de agregacao a partir de microdados estiver
  # completamente validado com ARBOVIROSES_DOWNLOAD=true, substituir o fallback
  # pela chamada a agregar_dengue_sinan(limpo, anos, agravo).
  # Por enquanto, preservamos o fallback validado para manter compatibilidade.
  message("Microdados de ", agravo, " disponiveis (", nrow(limpo), " registros). Usando fallback agregado pre-validado (fase 2 pendente).")
  fallback
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

saveRDS(agregar_limpo_ou_cache("chikungunya", chikungunya), root_path("data", "processed", "chikungunya_agregado.rds"))
saveRDS(agregar_limpo_ou_cache("dengue", dengue), root_path("data", "processed", "dengue_agregado.rds"))
saveRDS(agregar_limpo_ou_cache("zika", zika), root_path("data", "processed", "zika_agregado.rds"))
saveRDS(dengue_temporal, root_path("data", "processed", "dengue_temporal.rds"))
saveRDS(zika_temporal, root_path("data", "processed", "zika_temporal.rds"))
saveRDS(populacao_campos, root_path("data", "processed", "populacao_campos.rds"))
saveRDS(dengue_bairros, root_path("data", "processed", "dengue_bairros.rds"))

message("03_agregacao concluido.")
