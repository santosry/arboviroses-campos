source("R/packages.R")
source("R/utils.R")
source("R/dados.R")
source("R/mapas.R")
source("R/graficos.R")

message("Preparando mapa Leaflet com setores censitarios...")

# Forcar uso apenas do cache, sem nova geocodificacao
mapa_data <- preparar_mapa_bairros_tidyterra(dengue_bairros)

message("Bairros com coordenadas validadas: ", nrow(mapa_data$pontos))
message("Bairros NAO mapeados: ", nrow(mapa_data$nao_mapeados))

# Estatisticas
total_casos <- sum(dengue_bairros$Casos)
casos_mapeados <- sum(mapa_data$pontos$Casos)
message(sprintf("Cobertura: %.1f%% dos casos (%d de %d)",
        100 * casos_mapeados / total_casos, casos_mapeados, total_casos))

# Top nao mapeados
cat("\nTop 10 bairros NAO mapeados:\n")
nao <- mapa_data$nao_mapeados[order(-mapa_data$nao_mapeados$Casos), ]
print(head(nao[, c("NM_BAIRRO_DADOS", "Casos")], 10))

# Gerar Leaflet
m <- mapa_leaflet_setores_pontos(mapa_data, "Periodo: 2020-2025")

dir_preview <- file.path(getwd(), "previews_mapas")
if (!dir.exists(dir_preview)) dir.create(dir_preview, recursive = TRUE, showWarnings = FALSE)
html_path <- file.path(dir_preview, "mapa_dengue_leaflet_preview.html")
htmlwidgets::saveWidget(m, html_path, selfcontained = TRUE)
message("Mapa HTML salvo em: ", html_path)
message("Abra o arquivo no navegador para visualizar.")
