source("R/packages.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "app_cache"), recursive = TRUE, showWarnings = FALSE)

copiar <- function(from, to) {
  if (file.exists(from)) file.copy(from, to, overwrite = TRUE)
}

copiar(root_path("data", "processed", "chikungunya_agregado.rds"), root_path("data", "app_cache", "chikungunya_microdatasus_campos_v1.rds"))
copiar(root_path("data", "processed", "dengue_agregado.rds"), root_path("data", "app_cache", "dengue_microdatasus_campos_v2.rds"))
copiar(root_path("data", "processed", "zika_agregado.rds"), root_path("data", "app_cache", "zika_microdatasus_campos_v1.rds"))
copiar(root_path("data", "processed", "dengue_temporal.rds"), root_path("data", "app_cache", "dengue_microdatasus_temporal_campos_v1.rds"))
copiar(root_path("data", "processed", "zika_temporal.rds"), root_path("data", "app_cache", "zika_microdatasus_temporal_campos_v1.rds"))
copiar(root_path("data", "processed", "populacao_campos.rds"), root_path("data", "app_cache", "populacao_campos_sidra.rds"))
copiar(root_path("data", "processed", "dengue_bairros.rds"), root_path("data", "app_cache", "dengue_bairros_campos_v1.rds"))
copiar(root_path("data", "processed", "geobr_bairros_campos_2010.rds"), root_path("data", "app_cache", "geobr_bairros_campos_2010.rds"))

metadata <- list(
  atualizado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  fonte = c("SINAN/SVS via microdatasus quando ARBOVIROSES_DOWNLOAD=true", "Planilhas locais DENGUEYYYY.xlsx", "IBGE/SIDRA para populacao quando cacheado"),
  anos_incluidos = 2020:2025,
  municipio = "Campos dos Goytacazes, RJ",
  versao_pipeline = "0.1.0",
  arquivos_gerados = list.files(root_path("data", "app_cache"), full.names = FALSE),
  observacoes_metodologicas = c(
    "O app Shiny le apenas caches processados e nao baixa dados no startup.",
    "Dados brutos e dados_sinan_campos/outputs nao sao publicados.",
    "Chikungunya possui etapa de ingestao via SINAN-CHIKUNGUNYA no microdatasus."
  )
)

if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(metadata, root_path("data", "app_cache", "metadata.json"), pretty = TRUE, auto_unbox = TRUE)
}

message("06_export_app concluido.")
