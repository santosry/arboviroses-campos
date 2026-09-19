library(readxl)

# Ver um arquivo
df2024 <- read_excel("dados_sinan_campos/DENGUE2024.xlsx", col_types = "text")
cat("Colunas:", paste(names(df2024), collapse = ", "), "\n")
cat("Linhas:", nrow(df2024), "\n")

# Procurar alta da boa vista
bairros <- df2024$NM_BAIRRO
idx <- grep("alta", bairros, ignore.case = TRUE)
cat("Bairros com 'alta':", length(idx), "\n")
if (length(idx) > 0) print(head(unique(bairros[idx]), 20))

idx2 <- grep("boa vista", bairros, ignore.case = TRUE)
cat("\nBairros com 'boa vista':", length(idx2), "\n")
if (length(idx2) > 0) print(unique(bairros[idx2]))

# Amostra geral
cat("\nTotal bairros unicos em 2024:", length(unique(bairros)), "\n")
cat("Amostra:\n")
print(sort(unique(bairros))[1:30])
