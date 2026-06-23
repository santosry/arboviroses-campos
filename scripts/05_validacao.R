source("R/packages.R", encoding = "UTF-8")

root_path <- function(...) if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(getwd(), ...)
dir.create(root_path("data", "audit"), recursive = TRUE, showWarnings = FALSE)

ler <- function(path) if (file.exists(path)) readRDS(path) else data.frame()

bases <- list(
  Chikungunya = ler(root_path("data", "processed", "chikungunya_agregado.rds")),
  Dengue = ler(root_path("data", "processed", "dengue_agregado.rds")),
  Zika = ler(root_path("data", "processed", "zika_agregado.rds"))
)

totais <- dplyr::bind_rows(lapply(names(bases), function(agravo) {
  df <- bases[[agravo]]
  if (nrow(df) == 0) return(data.frame())
  data.frame(
    Agravo = agravo,
    Ano = df$Ano,
    Confirmado_casos = df$Confirmado_casos,
    Obitos_Agr = df$Obitos_Agr,
    Ign_Branco_casos = df$Ign_Branco_casos,
    stringsAsFactors = FALSE
  )
}))

completude <- dplyr::bind_rows(lapply(names(bases), function(agravo) {
  df <- bases[[agravo]]
  if (nrow(df) == 0) return(data.frame())
  total <- pmax(df$Confirmado_casos + df$Descartado_casos + df$Inconclusivo_casos + df$Ign_Branco_casos, 1)
  data.frame(
    Agravo = agravo,
    Ano = df$Ano,
    Perc_classificacao_ignorada = round(100 * df$Ign_Branco_casos / total, 2),
    Perc_raca_ignorada = round(100 * df$Ign_Branco_etnia / pmax(rowSums(df[, intersect(names(df), c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Ign_Branco_etnia")), drop = FALSE]), 1), 2),
    stringsAsFactors = FALSE
  )
}))

bairros_nao_mapeados <- ler(root_path("data", "processed", "bairros_nao_mapeados.rds"))

write.csv(totais, root_path("data", "audit", "totais_por_ano_agravo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(completude, root_path("data", "audit", "completude_variaveis.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bairros_nao_mapeados, root_path("data", "audit", "bairros_nao_mapeados.csv"), row.names = FALSE, fileEncoding = "UTF-8")

html <- c(
  "<!doctype html><html lang='pt-BR'><head><meta charset='utf-8'><title>Auditoria Arboviroses</title>",
  "<style>body{font-family:Arial,sans-serif;max-width:980px;margin:32px auto;line-height:1.5}table{border-collapse:collapse;width:100%;margin:16px 0}td,th{border:1px solid #ddd;padding:6px}th{background:#f2f4f8}</style></head><body>",
  "<h1>Relatorio de auditoria - Arboviroses Campos dos Goytacazes</h1>",
  paste0("<p>Gerado em: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>"),
  "<h2>Totais por ano e agravo</h2>",
  paste(capture.output(print(totais)), collapse = "<br>"),
  "<h2>Completude</h2>",
  paste(capture.output(print(completude)), collapse = "<br>"),
  "<h2>Bairros nao pareados</h2>",
  paste(capture.output(print(utils::head(bairros_nao_mapeados, 50))), collapse = "<br>"),
  "<h2>Alertas metodologicos</h2><ul><li>Dados de vigilancia podem conter atraso, campos ignorados/brancos e revisoes posteriores.</li><li>Incidencia depende de denominador populacional confiavel.</li><li>Correspondencia espacial depende da grafia dos bairros e da malha disponivel.</li></ul>",
  "</body></html>"
)
writeLines(html, root_path("data", "audit", "relatorio_auditoria.html"), useBytes = TRUE)

message("05_validacao concluido.")
