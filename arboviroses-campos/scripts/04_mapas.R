source("R/packages.R", encoding = "UTF-8")
source("R/utils.R", encoding = "UTF-8")
source("R/dados.R", encoding = "UTF-8")
source("R/mapas.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "processed"), recursive = TRUE, showWarnings = FALSE)

mapa <- tryCatch(
  preparar_mapa_bairros_geobr(dengue_bairros),
  error = function(e) {
    warning("Mapa nao preparado: ", conditionMessage(e))
    list(mapa = data.frame(), nao_mapeados = dengue_bairros)
  }
)

saveRDS(mapa$nao_mapeados, root_path("data", "processed", "bairros_nao_mapeados.rds"))
if (file.exists(root_path("data", "app_cache", "geobr_bairros_campos_2010.rds"))) {
  file.copy(
    root_path("data", "app_cache", "geobr_bairros_campos_2010.rds"),
    root_path("data", "processed", "geobr_bairros_campos_2010.rds"),
    overwrite = TRUE
  )
}

message("04_mapas concluido.")
