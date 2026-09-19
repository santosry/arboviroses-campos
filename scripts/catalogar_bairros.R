library(readxl)
library(dplyr)

# Ler todos os anos e catalogar bairros
arquivos <- list.files("dados_sinan_campos", pattern = "^DENGUE[0-9]{4}\\.xlsx$", full.names = TRUE)

todos_bairros <- data.frame()
for (arq in arquivos) {
  ano <- as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(basename(arq))))
  df <- read_excel(arq, col_types = "text")
  if (!"NM_BAIRRO" %in% names(df)) next
  
  bairros <- df %>%
    filter(!is.na(NM_BAIRRO), trimws(NM_BAIRRO) != "") %>%
    count(Ano = ano, NM_BAIRRO = trimws(NM_BAIRRO))
  
  todos_bairros <- bind_rows(todos_bairros, bairros)
  cat(sprintf("  %s: %d linhas, %d bairros unicos\n", basename(arq), nrow(df), nrow(bairros)))
}

cat(sprintf("\nTotal: %d registros, %d bairros unicos\n", nrow(todos_bairros), length(unique(todos_bairros$NM_BAIRRO))))

# Salvar catalogo para analise
write.csv(todos_bairros, "data/app_cache/catalogo_bairros_bruto.csv", row.names = FALSE)

# Mostrar exemplos de variacoes
cat("\n=== EXEMPLOS DE VARIACOES DE GRAFIA ===\n")
tbl <- table(todos_bairros$NM_BAIRRO)
bairros_ord <- names(sort(tbl, decreasing = TRUE))

# Agrupar por similaridade (prefixo comum)
for (prefixo in c("ALPHA", "PARQUE", "PQ ", "JD ", "JARDIM", "VILA", "ALTO", "BAIRRO")) {
  matches <- grep(paste0("^", prefixo), bairros_ord, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) {
    cat(sprintf("\n--- '%s...' (%d variantes) ---\n", prefixo, length(matches)))
    for (m in matches[1:min(25, length(matches))]) {
      cat(sprintf("  %s (%d)\n", m, tbl[m]))
    }
  }
}
