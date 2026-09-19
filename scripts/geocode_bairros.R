source("R/packages.R")
source("R/utils.R")
source("R/dados.R")
source("R/mapas.R")

todos_bairros <- unique(dengue_bairros$NM_BAIRRO)
message("Total bairros unicos: ", length(todos_bairros))

centroides <- obter_centroides_bairros_geobr()
message("Com centroide geobr: ", nrow(centroides))

faltam <- setdiff(normalizar_chave_bairro(todos_bairros), centroides$bairro_key)
message("Faltam geocodificar: ", length(faltam))

df_faltam <- dengue_bairros %>%
  mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
  filter(bairro_key %in% faltam) %>%
  distinct(NM_BAIRRO)

if (nrow(df_faltam) > 0) {
  geocodificar_bairros_campos(df_faltam$NM_BAIRRO)
}

cache_final <- carregar_bairros_geocoded()
message("Cache final: ", nrow(cache_final), " bairros, ", sum(!is.na(cache_final$lat)), " com coordenadas")
