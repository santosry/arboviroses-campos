files <- c(
  "app.R",
  "app_arboviroses.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("scripts", pattern = "\\.R$", full.names = TRUE),
  "README.md"
)
files <- unique(files[file.exists(files)])

replacements <- c(
  "á" = "á", "à" = "à", "â" = "â", "ã" = "ã", "ê" = "ê",
  "é" = "é", "è" = "è", "í" = "í", "ó" = "ó", "ô" = "ô",
  "õ" = "õ", "ú" = "ú", "ç" = "ç", "Á" = "Á", "À" = "À",
  "Â" = "Â", "Ã" = "Ã", "É" = "É", "Ê" = "Ê", "Í" = "Í",
  "Ó" = "Ó", "Ô" = "Ô", "Õ" = "Õ", "Ú" = "Ú", "Ç" = "Ç",
  "ª" = "ª", "º" = "º", "°" = "°", "–" = "–", "—" = "—",
  "≥" = "≥", "≤" = "≤", "'" = "'", "'" = "'", """ = "\"",
  """ = "\"", "ü" = "ü"
)

for (path in files) {
  x <- readLines(path, encoding = "UTF-8", warn = FALSE)
  for (from in names(replacements)) {
    x <- gsub(from, replacements[[from]], x, fixed = TRUE)
  }
  writeLines(x, path, useBytes = TRUE)
}

message("Encoding textual corrigido em ", length(files), " arquivos.")
