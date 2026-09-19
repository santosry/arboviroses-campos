source("R/packages.R")
source("R/utils.R")
source("R/dados.R")
source("R/mapas.R")

mapa_data <- preparar_mapa_bairros_tidyterra(dengue_bairros)

cat("=== ESTATISTICAS DO MAPA ===\n")
cat("Total bairros unicos:", length(unique(dengue_bairros$NM_BAIRRO)), "\n")
cat("Bairros com coordenadas:", nrow(mapa_data$pontos), "\n")
cat("Bairros SEM coordenadas:", nrow(mapa_data$nao_mapeados), "\n\n")

cat("Total de casos:", sum(dengue_bairros$Casos), "\n")
cat("Casos em bairros mapeados:", sum(mapa_data$pontos$Casos), "\n")
cat("Casos em bairros NAO mapeados:", sum(mapa_data$nao_mapeados$Casos), "\n")
cat("Cobertura:", round(100 * sum(mapa_data$pontos$Casos) / sum(dengue_bairros$Casos), 1), "%\n\n")

cat("=== TOP 10 BAIRROS NAO MAPEADOS ===\n")
nao <- mapa_data$nao_mapeados[order(-mapa_data$nao_mapeados$Casos), ]
print(head(nao[, c("NM_BAIRRO_DADOS", "Casos")], 10))

cat("\n=== TOP 10 BAIRROS MAPEADOS ===\n")
sim <- mapa_data$pontos[order(-mapa_data$pontos$Casos), ]
print(head(sim[, c("NM_BAIRRO_DADOS", "Casos")], 10))

cat("\n=== BAIRROS COM MAIS DE 100 CASOS NAO MAPEADOS ===\n")
graves <- nao[nao$Casos >= 100, ]
cat(nrow(graves), "bairros com >=100 casos sem coordenadas\n")
if (nrow(graves) > 0) {
  print(graves[, c("NM_BAIRRO_DADOS", "Casos")])
}
