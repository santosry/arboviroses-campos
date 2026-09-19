library(readxl)
library(dplyr)
library(stringr)

message("=== REBUILDING BAIRROS DATA WITH NORMALIZATION ===")

# ============================================================
# FUNÇÃO DE NORMALIZAÇÃO ROBUSTA
# ============================================================
normalizar_bairro_v2 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  
  # Remover acentos para ASCII
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  x <- toupper(x)
  x <- str_squish(x)
  
  # Remover pontuação
  x <- gsub("[[:punct:]]", " ", x)
  x <- str_squish(x)
  
  # Expandir abreviações comuns
  x <- gsub("\\bPQ\\b", "PARQUE", x)
  x <- gsub("\\bJD\\b", "JARDIM", x)
  x <- gsub("\\bVL\\b", "VILA", x)
  x <- gsub("\\bSTO\\b", "SANTO", x)
  x <- gsub("\\bSTA\\b", "SANTA", x)
  x <- gsub("\\bSAO\\b", "SAO", x)  # manter padronizado
  
  # Normalizar numerais romanos
  x <- gsub("\\bII\\b", "2", x)
  x <- gsub("\\bIII\\b", "3", x)
  x <- gsub("\\bIV\\b", "4", x)
  
  # Corrigir variacoes comuns de grafia
  x <- gsub("\\bALPHAVILE\\b", "ALPHAVILLE", x)
  x <- gsub("\\bALFAVILLE\\b", "ALPHAVILLE", x)
  x <- gsub("\\bALPHVILLE\\b", "ALPHAVILLE", x)
  x <- gsub("\\bMANHAES\\b", "MANHAES", x)
  x <- gsub("\\bMANHHAES\\b", "MANHAES", x)
  x <- gsub("\\bMANAHES\\b", "MANHAES", x)
  x <- gsub("\\bMANAES\\b", "MANHAES", x)
  x <- gsub("\\bROMANO\\b", "ROMANA", x)
  x <- gsub("\\bCARIOVA\\b", "CARIOCA", x)
  x <- gsub("\\bMARILEA\\b", "MARILEA", x)
  x <- gsub("\\bMARICEA\\b", "MARILEA", x)
  x <- gsub("\\bDONANA\\b", "DONANA", x)
  x <- gsub("\\bGOYTACASES\\b", "GOITACAZES", x)
  x <- gsub("\\bGOYTACAZES\\b", "GOITACAZES", x)
  x <- gsub("\\bTRAVESSAO\\b", "TRAVESSAO", x)
  x <- gsub("\\bURURAI\\b", "URURAI", x)
  x <- gsub("\\bRODOVIARIO\\b", "RODOVIARIO", x)
  x <- gsub("\\bAEROPORTO\\b", "AEROPORTO", x)
  x <- gsub("\\bJOCKEY\\b", "JOCKEY", x)
  x <- gsub("\\bJOQUEI\\b", "JOCKEY", x)
  x <- gsub("\\bJOKEY\\b", "JOCKEY", x)
  x <- gsub("\\bJOCKEY CLUB\\b", "JOCKEY CLUB", x)
  x <- gsub("\\bTURF CLUB\\b", "TURF CLUB", x)
  x <- gsub("\\bTURFE\\b", "TURF", x)
  x <- gsub("\\bLEOPOLDINA\\b", "LEOPOLDINA", x)
  x <- gsub("\\bROSARIO\\b", "ROSARIO", x)
  x <- gsub("\\bPRAZERES\\b", "PRAZERES", x)
  x <- gsub("\\bELDORADO\\b", "ELDORADO", x)
  x <- gsub("\\bELDORADO\\b", "ELDORADO", x)
  x <- gsub("\\bCALIFORNIA\\b", "CALIFORNIA", x)
  x <- gsub("\\bFLAMBOYANT\\b", "FLAMBOYANT", x)
  x <- gsub("\\bPECUARIA\\b", "PECUARIA", x)
  x <- gsub("\\bALDEIA\\b", "ALDEIA", x)
  x <- gsub("\\bBARAO\\b", "BARAO", x)
  x <- gsub("\\bPRESID\\b", "PRESIDENTE", x)
  x <- gsub("\\bVARGAS\\b", "VARGAS", x)
  x <- gsub("\\bVICENTE\\b", "VICENTE", x)
  x <- gsub("\\bGONCALVES\\b", "GONCALVES", x)
  x <- gsub("\\bBRASILIA\\b", "BRASILIA", x)
  x <- gsub("\\bIMPERIAL\\b", "IMPERIAL", x)
  x <- gsub("\\bNOVO JOCKEY\\b", "JOCKEY CLUB", x)
  x <- gsub("\\bTOCOS\\b", "TOCOS", x)
  x <- gsub("\\bCUSTODOPOLIS\\b", "CUSTODOPOLIS", x)
  x <- gsub("\\bSATURNINO\\b", "SATURNINO", x)
  x <- gsub("\\bBRAGA\\b", "BRAGA", x)
  x <- gsub("\\bCONSELHEIRO\\b", "CONSELHEIRO", x)
  x <- gsub("\\bJOSINO\\b", "JOSINO", x)
  x <- gsub("\\bTRES VENDAS\\b", "TRES VENDAS", x)
  x <- gsub("\\bCODIM\\b", "CODIN", x)
  x <- gsub("\\bPOCO GORDO\\b", "POCO GORDO", x)
  x <- gsub("\\bLEBRET\\b", "LEBRET", x)
  x <- gsub("\\bPONTA DA LAMA\\b", "PONTA DA LAMA", x)
  x <- gsub("\\bPONTA GROSSA\\b", "PONTA GROSSA", x)
  
  # Remover numeros no final que nao sao parte do nome
  # (mantem "ALPHAVILLE 2", "PARQUE SÃO JOSÉ 2", etc)
  
  # Remover espacos extras
  x <- str_squish(x)
  
  # Title case
  x <- tools::toTitleCase(tolower(x))
  
  x
}

# ============================================================
# CARREGAR TODOS OS DADOS
# ============================================================
arquivos <- list.files("dados_sinan_campos", pattern = "^DENGUE[0-9]{4}\\.xlsx$", full.names = TRUE)

dados_brutos <- data.frame()
for (arq in arquivos) {
  ano <- as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(basename(arq))))
  df <- read_excel(arq, col_types = "text")
  if (!"NM_BAIRRO" %in% names(df)) next
  
  df <- df %>%
    filter(!is.na(NM_BAIRRO), trimws(NM_BAIRRO) != "") %>%
    filter(!grepl("ignorado|sem informa|nao informado|IGNORADO", NM_BAIRRO, ignore.case = TRUE)) %>%
    mutate(
      Ano = ano,
      NM_BAIRRO_ORIGINAL = trimws(NM_BAIRRO),
      NM_BAIRRO = normalizar_bairro_v2(NM_BAIRRO_ORIGINAL)
    ) %>%
    filter(NM_BAIRRO != "")
  
  dados_brutos <- bind_rows(dados_brutos, df)
  cat(sprintf("  %s: %d registros apos limpeza\n", basename(arq), nrow(df)))
}

cat(sprintf("\nTotal registros: %d\n", nrow(dados_brutos)))
cat(sprintf("Bairros unicos (original): %d\n", length(unique(dados_brutos$NM_BAIRRO_ORIGINAL))))
cat(sprintf("Bairros unicos (normalizado): %d\n", length(unique(dados_brutos$NM_BAIRRO))))

# ============================================================
# AGREGAR POR BAIRRO (NORMALIZADO) E ANO
# ============================================================
dengue_bairros <- dados_brutos %>%
  count(Ano, NM_BAIRRO, name = "Casos") %>%
  arrange(Ano, desc(Casos), NM_BAIRRO)

cat(sprintf("Registros agregados: %d\n", nrow(dengue_bairros)))
cat(sprintf("Total de casos: %d\n", sum(dengue_bairros$Casos)))

# Top bairros
cat("\n=== TOP 20 BAIRROS (total 2020-2025) ===\n")
dengue_bairros %>%
  group_by(NM_BAIRRO) %>%
  summarise(Casos = sum(Casos), .groups = "drop") %>%
  arrange(desc(Casos)) %>%
  head(20) %>%
  as.data.frame() %>%
  print()

# Verificar "ALTO DA BOA VISTA"
cat("\n=== BUSCA: ALTO DA BOA VISTA ===\n")
idx <- grep("alto.*boa.*vista|boa.*vista.*alto", unique(dados_brutos$NM_BAIRRO), ignore.case = TRUE, value = TRUE)
print(idx)

# ============================================================
# SALVAR CACHE
# ============================================================
saveRDS(dengue_bairros, "data/app_cache/dengue_bairros_campos_v1.rds")
cat("\nCache salvo: data/app_cache/dengue_bairros_campos_v1.rds\n")

# Salvar tabela de mapeamento (original -> normalizado)
mapeamento <- dados_brutos %>%
  distinct(NM_BAIRRO_ORIGINAL, NM_BAIRRO)
write.csv(mapeamento, "data/app_cache/mapeamento_bairros.csv", row.names = FALSE)

# Salvar totais por bairro para auditoria
totais_bairro <- dengue_bairros %>%
  group_by(NM_BAIRRO) %>%
  summarise(Casos = sum(Casos), Anos = paste(sort(unique(Ano)), collapse = ","), .groups = "drop") %>%
  arrange(desc(Casos))
write.csv(totais_bairro, "data/audit/totais_por_bairro.csv", row.names = FALSE)

cat("Done.\n")
