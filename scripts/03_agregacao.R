source("R/packages.R", encoding = "UTF-8")
source("R/utils.R", encoding = "UTF-8")
source("R/dados.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "processed"), recursive = TRUE, showWarnings = FALSE)

agregar_limpo_ou_cache <- function(agravo, fallback) {
  path <- root_path("data", "interim", paste0(tolower(agravo), "_limpo.rds"))
  if (!file.exists(path) || nrow(readRDS(path)) == 0) return(fallback)
  # O app atual usa estrutura agregada consolidada; enquanto a limpeza ampla amadurece,
  # preservamos o fallback validado para manter compatibilidade.
  fallback
}

saveRDS(agregar_limpo_ou_cache("chikungunya", chikungunya), root_path("data", "processed", "chikungunya_agregado.rds"))
saveRDS(agregar_limpo_ou_cache("dengue", dengue), root_path("data", "processed", "dengue_agregado.rds"))
saveRDS(agregar_limpo_ou_cache("zika", zika), root_path("data", "processed", "zika_agregado.rds"))
saveRDS(dengue_temporal, root_path("data", "processed", "dengue_temporal.rds"))
saveRDS(zika_temporal, root_path("data", "processed", "zika_temporal.rds"))
saveRDS(populacao_campos, root_path("data", "processed", "populacao_campos.rds"))
saveRDS(dengue_bairros, root_path("data", "processed", "dengue_bairros.rds"))

message("03_agregacao concluido.")
