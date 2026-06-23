etapas <- c(
  "scripts/01_ingestao.R",
  "scripts/02_limpeza.R",
  "scripts/03_agregacao.R",
  "scripts/04_mapas.R",
  "scripts/05_validacao.R",
  "scripts/06_export_app.R"
)

for (etapa in etapas) {
  message("Executando ", etapa)
  sys.source(etapa, envir = globalenv(), keep.source = FALSE)
}

message("Pipeline completo. Caches finais em data/app_cache/.")
