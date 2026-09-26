source("R/packages.R")
source("R/utils.R")
source("R/dados.R")
source("R/mapas.R")

# Bairros com mais casos que nao tem centroide geobr
cent <- obter_centroides_bairros_geobr()

top_faltantes <- dengue_bairros %>%
  mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
  filter(!bairro_key %in% cent$bairro_key) %>%
  group_by(bairro_key) %>%
  summarise(NM_BAIRRO = dplyr::first(NM_BAIRRO), Casos = sum(Casos), .groups = "drop") %>%
  arrange(desc(Casos)) %>%
  head(100)

message("Geocodificando top ", nrow(top_faltantes), " bairros prioritarios...")
geocodificar_bairros_campos(top_faltantes$NM_BAIRRO)
message("Concluido!")
