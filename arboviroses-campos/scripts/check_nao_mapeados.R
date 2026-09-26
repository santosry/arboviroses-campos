source("R/packages.R")
source("R/utils.R")
source("R/dados.R")
source("R/mapas.R")
mapa_data <- preparar_mapa_bairros_tidyterra(dengue_bairros)
nao <- mapa_data$nao_mapeados[order(-mapa_data$nao_mapeados$Casos), ]
cat("=== TOP 15 NAO MAPEADOS ===\n")
for (i in 1:min(15, nrow(nao))) {
  cat(sprintf("%2d. %-35s %d casos\n", i, nao$NM_BAIRRO_DADOS[i], nao$Casos[i]))
}
