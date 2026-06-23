# Forçar encoding UTF-8
options(encoding = "UTF-8")
if (.Platform$OS.type == "unix") Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")

# SISTEMA DE AUDITORIA

# Em publicacao (shinyapps.io), o diretorio do app pode ser somente leitura.
# Tentamos primeiro gravar em logs/ local; se falhar, usamos tempdir().
AUDIT_DIR <- tryCatch({
  dir.create("logs", showWarnings = FALSE, recursive = TRUE)
  test_file <- file.path("logs", ".write_test")
  writeLines("test", test_file)
  file.remove(test_file)
  "logs"
}, error = function(e) {
  d <- file.path(tempdir(), "auditoria")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
})
if (!dir.exists(AUDIT_DIR)) dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

# Arquivos de log
LOG_ACESSOS <- file.path(AUDIT_DIR, "01_acessos.csv")
LOG_ERROS <- file.path(AUDIT_DIR, "02_erros.csv")
LOG_DADOS <- file.path(AUDIT_DIR, "03_dados_consistentes.csv")
LOG_INCONSISTENCIAS <- file.path(AUDIT_DIR, "04_inconsistencias.csv")
LOG_SESSAO <- file.path(AUDIT_DIR, "05_sessao.csv")
LOG_POPULACAO <- file.path(AUDIT_DIR, "06_populacao_sidra.csv")

# Gerar ID unico da sessao
session_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", 
                     paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""))

# Funcao para registrar logs
registrar_log <- function(arquivo, dados) {
  dados$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dados$session_id <- session_id
  df_novo <- as.data.frame(dados, stringsAsFactors = FALSE)
  
  if(!file.exists(arquivo)) {
    write.csv(df_novo, arquivo, row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    write.table(df_novo, arquivo, append = TRUE, sep = ",", 
                row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
  }
}

# Registrar inicio da sessao
registrar_log(LOG_SESSAO, data.frame(
  evento = "inicio_sessao",
  detalhes = "Sessao iniciada"
))

# Funcao para validar dados
validar_dados <- function(df, nome_doenca) {
  inconsistencias <- c()
  colunas_sexo <- c("Masculino", "Feminino", "Ignorado_sexo")
  colunas_criancas_detalhadas <- c("Menor_1_Ano", "Faixa_1_4", "Faixa_5_9", "Faixa_10_14", "Faixa_15_19")
  colunas_adultos_detalhadas <- c("Faixa_20_39", "Faixa_40_59")
  colunas_idosos_detalhadas <- c("Faixa_60_64_anos", "Faixa_65_69_anos", "Faixa_70_79_anos", "Faixa_80_mais")
  colunas_idosos_detalhadas_alt <- c("Faixa_60_64", "Faixa_65_69", "Faixa_70_79", "Faixa_80_mais")
  tem_criancas_detalhadas <- all(colunas_criancas_detalhadas %in% names(df))
  tem_criancas_padrao <- "Criancas_e_jovens" %in% names(df)
  tem_ignorado_idade <- "Ignorado_idade" %in% names(df)
  tem_adultos_detalhados <- all(colunas_adultos_detalhadas %in% names(df))
  tem_adultos_padrao <- "Adultos" %in% names(df)
  tem_idosos_detalhados_padrao <- all(colunas_idosos_detalhadas %in% names(df))
  tem_idosos_detalhados_alt <- all(colunas_idosos_detalhadas_alt %in% names(df))
  tem_idosos_detalhados <- tem_idosos_detalhados_padrao || tem_idosos_detalhados_alt
  tem_idosos_padrao <- "Idosos" %in% names(df)
  
  # Verificar se totais de sexo correspondem aos totais de idade
  if(all(colunas_sexo %in% names(df)) && tem_ignorado_idade && (tem_criancas_detalhadas || tem_criancas_padrao) && (tem_adultos_detalhados || tem_adultos_padrao) && (tem_idosos_detalhados || tem_idosos_padrao)) {
    if(tem_criancas_detalhadas) {
      colunas_idade_validacao <- colunas_criancas_detalhadas
    } else {
      colunas_idade_validacao <- c("Criancas_e_jovens")
    }
    if(tem_adultos_detalhados) {
      colunas_idade_validacao <- c(colunas_idade_validacao, colunas_adultos_detalhadas)
    } else {
      colunas_idade_validacao <- c(colunas_idade_validacao, "Adultos")
    }
    if(tem_idosos_detalhados_padrao) {
      colunas_idade_validacao <- c(colunas_idade_validacao, colunas_idosos_detalhadas)
    } else if(tem_idosos_detalhados_alt) {
      colunas_idade_validacao <- c(colunas_idade_validacao, colunas_idosos_detalhadas_alt)
    } else {
      colunas_idade_validacao <- c(colunas_idade_validacao, "Idosos")
    }
    colunas_idade_validacao <- c(colunas_idade_validacao, "Ignorado_idade")
    
    for(i in 1:nrow(df)) {
      total_sexo <- df$Masculino[i] + df$Feminino[i] + df$Ignorado_sexo[i]
      total_idade <- sum(as.numeric(df[i, colunas_idade_validacao]), na.rm = TRUE)
      
      if(total_sexo != total_idade) {
        inconsistencias <- c(inconsistencias, 
                             paste("Ano", df$Ano[i], ": Sexo (M+F+Ign) =", total_sexo, 
                                   "vs Idade (faixas+Ign) =", total_idade, "diferenca =", total_sexo - total_idade))
      }
    }
  }
  
# Verificar valores negativos
cols_numeric <- names(df)[sapply(df, is.numeric)]
  for(col in cols_numeric) {
    negativos <- which(df[[col]] < 0)
    if(length(negativos) > 0) {
      inconsistencias <- c(inconsistencias,
                           paste("Valor negativo em", col, "nos anos:", 
                                 paste(df$Ano[negativos], collapse = ", ")))
    }
  }

  # Registrar inconsistencias
  if(length(inconsistencias) > 0) {
    registrar_log(LOG_INCONSISTENCIAS, data.frame(
      doenca = nome_doenca,
      n_inconsistencias = length(inconsistencias),
      detalhes = paste(inconsistencias, collapse = " | ")
    ))
  }

  return(inconsistencias)
}

# DADOS
APP_DATA_ATUALIZACAO <- Sys.Date()
APP_PERIODO_PADRAO <- "2020-2025"
APP_MUNICIPIO <- "Campos dos Goytacazes (RJ)"
APP_UNIDADE_ANALISE <- "casos notificados residentes/notificados no municipio, agregados por ano epidemiologico"

# Leitura segura de .rds com validacao de integridade
ler_rds_seguro <- function(path, nome = basename(path), quiet = FALSE) {
  if (!file.exists(path)) {
    if (!quiet) warning("Arquivo ", nome, " nao encontrado em ", path)
    return(NULL)
  }
  if (file.info(path)$size == 0) {
    if (!quiet) warning("Arquivo ", nome, " esta vazio (0 bytes)")
    return(NULL)
  }
  obj <- tryCatch(readRDS(path), error = function(e) {
    warning("Falha ao ler ", nome, ": ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(obj)) {
    if (!quiet) warning("Retorno nulo ao ler ", nome)
  } else if (is.data.frame(obj) && nrow(obj) == 0) {
    if (!quiet) message("Data frame vazio ao ler ", nome, " (esperado se cache nao foi gerado com downloads)")
  }
  obj
}

primeira_coluna_existente <- function(df, candidatas) {
  candidatas[candidatas %in% names(df)][1]
}

criterio_confirmado_sinan <- function(classi_norm, agravo = "dengue") {
  descartado <- grepl("descartado", classi_norm)
  inconclusivo <- grepl("inconclusivo", classi_norm)
  if(identical(tolower(agravo), "zika")) {
    return((
      grepl("zika|confirm", classi_norm) |
        classi_norm %in% c("1", "2", "3", "laboratorio", "clinico epidemiologico")
    ) & !descartado & !inconclusivo)
  }
  grepl("dengue|febre hemorr", classi_norm) & !grepl("descartado|chikungunya", classi_norm)
}

normalizar_texto_sinan <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  tolower(trimws(x))
}

preparar_serie_temporal_sinan <- function(df, anos = 2020:2025, agravo = "dengue") {
  if(is.null(df) || nrow(df) == 0) {
    return(data.frame(Agravo = character(), Intervalo = character(), Periodo = character(), Data = as.Date(character()), Ano = integer(), Casos = integer()))
  }
  col_data <- primeira_coluna_existente(df, c("DT_NOTIFIC", "DT_SIN_PRI"))
  if(is.na(col_data) || !"CLASSI_FIN" %in% names(df)) {
    return(data.frame(Agravo = character(), Intervalo = character(), Periodo = character(), Data = as.Date(character()), Ano = integer(), Casos = integer()))
  }
  
  datas <- suppressWarnings(as.Date(df[[col_data]]))
  if(all(is.na(datas))) {
    datas <- suppressWarnings(as.Date(as.character(df[[col_data]]), format = "%d/%m/%Y"))
  }
  
  classi_norm <- normalizar_texto_sinan(df$CLASSI_FIN)
  confirmado <- criterio_confirmado_sinan(classi_norm, agravo)
  base <- df[confirmado & !is.na(datas), , drop = FALSE]
  base$Data_notificacao <- datas[confirmado & !is.na(datas)]
  if(nrow(base) == 0) {
    return(data.frame(Agravo = character(), Intervalo = character(), Periodo = character(), Data = as.Date(character()), Ano = integer(), Casos = integer()))
  }
  
  base$Ano <- as.integer(format(base$Data_notificacao, "%Y"))
  base <- base[base$Ano %in% anos, , drop = FALSE]
  if(nrow(base) == 0) {
    return(data.frame(Agravo = character(), Intervalo = character(), Periodo = character(), Data = as.Date(character()), Ano = integer(), Casos = integer()))
  }
  
  mensal <- base %>%
    mutate(
      Intervalo = "Mensal",
      Periodo = format(Data_notificacao, "%Y-%m"),
      Data = as.Date(paste0(format(Data_notificacao, "%Y-%m"), "-01"))
    ) %>%
    count(Intervalo, Periodo, Data, Ano, name = "Casos")
  
  semanal <- base %>%
    mutate(
      Intervalo = "Semanal",
      Periodo = paste0(format(Data_notificacao, "%Y"), "-S", format(Data_notificacao, "%U")),
      Data = as.Date(Data_notificacao - as.integer(format(Data_notificacao, "%w")))
    ) %>%
    count(Intervalo, Periodo, Data, Ano, name = "Casos")
  
  bind_rows(mensal, semanal) %>%
    mutate(Agravo = tools::toTitleCase(tolower(agravo))) %>%
    select(Agravo, Intervalo, Periodo, Data, Ano, Casos) %>%
    arrange(Intervalo, Data)
}
