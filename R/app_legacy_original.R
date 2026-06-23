#pacotes
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(microdatasus)
library(ggplot2)
library(readxl)
library(geobr)
library(sf)
library(leaflet)

# Forçar encoding UTF-8
options(encoding = "UTF-8")
if (.Platform$OS.type == "unix") Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")

# SISTEMA DE AUDITORIA

# Em publicacao, o diretorio do app pode ser somente leitura.
# Por isso os logs vao para um diretorio temporario e gravavel.
AUDIT_DIR <- file.path(tempdir(), "auditoria")
if(!dir.exists(AUDIT_DIR)) dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

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

# Chikungunya
chikungunya <- data.frame(
  Ano = c(2020, 2021, 2022, 2023, 2024, 2025),
  Masculino = c(348, 60, 49, 104, 559, 70),
  Feminino = c(664, 108, 85, 209, 1234, 162),
  Ignorado_sexo = c(0, 0, 0, 0, 2, 0),
  Cura_evolucao = c(997, 157, 117, 186, 1559, 222),
  Obitos_Agr = c(0, 0, 0, 1, 2, 2),
  Obitos_Out = c(1, 1, 3, 2, 3, 0),
  Menor_1_Ano = c(13, 4, 6, 6, 11, 2),
  Faixa_1_4 = c(14, 5, 5, 8, 19, 1),
  Faixa_5_9 = c(22, 5, 5, 12, 26, 8),
  Faixa_10_14 = c(26, 5, 6, 11, 36, 8),
  Faixa_15_19 = c(48, 1, 7, 18, 56, 9),
  Faixa_20_39 = c(312, 38, 35, 81, 384, 58),
  Faixa_40_59 = c(372, 68, 39, 110, 742, 73),
  Faixa_60_64_anos = c(68, 14, 16, 19, 155, 15),
  Faixa_65_69_anos = c(57, 15, 7, 21, 147, 19),
  Faixa_70_79_anos = c(65, 11, 5, 22, 167, 24),
  Faixa_80_mais = c(15, 2, 3, 5, 52, 15),
  Ignorado_idade = c(0, 0, 0, 0, 0, 0),
  Ign_Branco_escolaridade = c(309, 154, 98, 207, 936, 31),
  Analfabeto = c(7, 0, 0, 0, 0, 0),
  Primeira_a_quarta_serie_incompleta_EF = c(71, 0, 1, 3, 12, 0),
  Quarta_serie_completa_EF = c(39, 1, 2, 1, 5, 0),
  Quinta_a_oitava_serie_incompleta_EF = c(118, 0, 3, 9, 45, 7),
  Ensino_fundamental_completo = c(63, 0, 1, 3, 6, 2),
  Ensino_medio_incompleto = c(24, 0, 0, 2, 10, 0),
  Ensino_medio_completo = c(271, 1, 11, 63, 723, 176),
  Educacao_superior_incompleta = c(0, 0, 0, 0, 0, 0),
  Educacao_superior_completa = c(49, 1, 2, 1, 14, 7),
  Nao_se_aplica_escolaridade = c(40, 11, 15, 22, 43, 9),
  Primeiro_trimestre = c(1, 0, 1, 0, 2, 0),
  Segundo_trimestre = c(3, 0, 0, 2, 0, 0),
  Terceiro_trimestre = c(2, 1, 0, 1, 2, 0),
  Nao_gestacao = c(185, 59, 64, 162, 755, 130),
  Nao_se_aplica_gestacao = c(708, 86, 58, 127, 684, 96),
  Idade_gestacional = c(4, 0, 1, 0, 0, 0),
  Ign_Branco_gestacao = c(109, 22, 10, 21, 352, 6),
  Inconclusivo_casos = c(0, 0, 0, 0, 0, 0),
  Branca = c(256, 9, 21, 70, 738, 185),
  Preta = c(88, 2, 5, 6, 22, 5),
  Amarela = c(0, 0, 0, 2, 2, 3),
  Parda = c(308, 7, 8, 29, 273, 15),
  Indigena = c(0, 1, 0, 0, 4, 1),
  Ign_Branco_etnia = c(360, 149, 100, 206, 756, 23),
  Confirmado_casos = c(1001, 102, 6, 52, 1460, 98),
  Descartado_casos = c(2, 64, 127, 250, 287, 130),
  Ign_Branco_casos = c(9, 2, 1, 11, 48, 4)
)

# Dengue
dengue <- data.frame(
  Ano = 2020:2025,
  Masculino = c(4L, 31L, 85L, 1432L, 8307L, 122L),
  Feminino = c(3L, 51L, 139L, 2211L, 11468L, 178L),
  Ignorado_sexo = c(0L, 0L, 0L, 0L, 23L, 0L),
  Cura_evolucao = c(6L, 72L, 214L, 2763L, 18654L, 250L),
  Obitos_Agr = c(0L, 0L, 0L, 2L, 5L, 1L),
  Obitos_Out = c(0L, 1L, 1L, 2L, 1L, 0L),
  Menor_1_Ano = c(1L, 3L, 2L, 21L, 171L, 1L),
  Faixa_1_4 = c(1L, 4L, 5L, 54L, 488L, 5L),
  Faixa_5_9 = c(0L, 2L, 8L, 171L, 1016L, 11L),
  Faixa_10_14 = c(0L, 5L, 10L, 275L, 1440L, 12L),
  Faixa_15_19 = c(2L, 5L, 24L, 329L, 1809L, 15L),
  Faixa_20_39 = c(3L, 25L, 88L, 1334L, 7519L, 115L),
  Faixa_40_59 = c(0L, 24L, 67L, 1036L, 4988L, 94L),
  Faixa_60_64 = c(0L, 4L, 9L, 175L, 896L, 16L),
  Faixa_65_69 = c(0L, 5L, 6L, 116L, 647L, 16L),
  Faixa_70_79 = c(0L, 5L, 5L, 99L, 637L, 11L),
  Faixa_80_mais = c(0L, 0L, 0L, 33L, 187L, 4L),
  Ignorado_idade = c(0L, 0L, 0L, 0L, 0L, 0L),
  Ign_Branco_escolaridade = c(4L, 72L, 51L, 500L, 12205L, 72L),
  Analfabeto = c(0L, 0L, 0L, 6L, 6L, 1L),
  Primeira_a_quarta_serie_incompleta_EF = c(0L, 0L, 6L, 69L, 123L, 5L),
  Quarta_serie_completa_EF = c(0L, 1L, 23L, 152L, 195L, 10L),
  Quinta_a_oitava_serie_incompleta_EF = c(0L, 0L, 28L, 299L, 374L, 27L),
  Ensino_fundamental_completo = c(0L, 0L, 1L, 92L, 126L, 5L),
  Ensino_medio_incompleto = c(0L, 0L, 11L, 544L, 156L, 0L),
  Ensino_medio_completo = c(1L, 0L, 80L, 2210L, 5525L, 140L),
  Educacao_superior_incompleta = c(0L, 0L, 11L, 53L, 36L, 4L),
  Educacao_superior_completa = c(0L, 0L, 31L, 217L, 211L, 33L),
  Nao_se_aplica_escolaridade = c(2L, 9L, 10L, 167L, 1156L, 12L),
  Primeiro_trimestre = c(0L, 0L, 1L, 0L, 13L, 0L),
  Segundo_trimestre = c(0L, 0L, 0L, 3L, 3L, 0L),
  Terceiro_trimestre = c(0L, 0L, 0L, 5L, 9L, 0L),
  Nao_gestacao = c(1L, 35L, 114L, 1976L, 7080L, 56L),
  Nao_se_aplica_gestacao = c(4L, 39L, 96L, 1608L, 9893L, 210L),
  Idade_gestacional = c(0L, 0L, 0L, 3L, 8L, 0L),
  Ign_Branco_gestacao = c(2L, 8L, 13L, 48L, 2792L, 34L),
  Inconclusivo_casos = c(0L, 1L, 3L, 74L, 22L, 46L),
  Branca = c(1L, 6L, 111L, 2136L, 5841L, 168L),
  Preta = c(0L, 2L, 32L, 337L, 546L, 16L),
  Amarela = c(0L, 1L, 1L, 9L, 31L, 6L),
  Parda = c(1L, 2L, 42L, 761L, 3351L, 82L),
  Indigena = c(0L, 1L, 1L, 4L, 56L, 0L),
  Ign_Branco_etnia = c(5L, 70L, 37L, 396L, 9973L, 28L),
  Confirmado_casos = c(7L, 33L, 221L, 3565L, 19774L, 253L),
  Descartado_casos = c(0L, 48L, 0L, 0L, 0L, 0L),
  Ign_Branco_casos = c(0L, 0L, 0L, 4L, 2L, 1L),
  Ign_Branco_sorotipo = c(7L, 82L, 223L, 3625L, 19716L, 300L),
  DEN_1 = c(0L, 0L, 0L, 8L, 63L, 0L),
  DEN_2 = c(0L, 0L, 1L, 10L, 19L, 0L)
)

# Zika
zika <- data.frame(
  Ano = c(2021, 2022, 2023, 2024, 2025),
  Masculino = c(17, 23, 61, 37, 7),
  Feminino = c(16, 38, 97, 73, 20),
  Ignorado_sexo = c(0, 0, 0, 0, 0),
  Cura_evolucao = c(28, 25, 105, 82, 27),
  Obitos_Agr = c(0, 0, 0, 0, 0),
  Obitos_Out = c(0, 2, 1, 0, 0),
  Menor_1_Ano = c(0, 3, 3, 0, 0),
  Faixa_1_4 = c(2, 4, 5, 2, 0),
  Faixa_5_9 = c(2, 2, 7, 1, 2),
  Faixa_10_14 = c(3, 1, 4, 5, 0),
  Faixa_15_19 = c(0, 2, 9, 5, 0),
  Faixa_20_39 = c(14, 27, 61, 31, 12),
  Faixa_40_59 = c(11, 22, 53, 33, 7),
  Faixa_60_64 = c(1, 0, 5, 11, 1),
  Faixa_65_69 = c(0, 0, 7, 8, 3),
  Faixa_70_79 = c(0, 0, 3, 12, 1),
  Faixa_80_mais = c(0, 0, 1, 2, 1),
  Ignorado_idade = c(0, 0, 0, 0, 0),
  Ign_Branco_escolaridade = c(30, 51, 122, 37, 2),
  Analfabeto = c(0, 0, 0, 0, 0),
  Primeira_a_quarta_serie_incompleta_EF = c(0, 0, 2, 0, 0),
  Quarta_serie_completa_EF = c(0, 0, 1, 0, 0),
  Quinta_a_oitava_serie_incompleta_EF = c(0, 0, 0, 1, 0),
  Ensino_fundamental_completo = c(0, 0, 1, 1, 0),
  Ensino_medio_incompleto = c(0, 0, 0, 1, 0),
  Ensino_medio_completo = c(0, 0, 21, 67, 23),
  Educacao_superior_incompleta = c(0, 0, 0, 0, 0),
  Educacao_superior_completa = c(0, 1, 0, 1, 0),
  Nao_se_aplica_escolaridade = c(3, 9, 11, 2, 2),
  Primeiro_trimestre = c(0, 2, 2, 0, 1),
  Segundo_trimestre = c(0, 0, 1, 0, 0),
  Terceiro_trimestre = c(0, 3, 1, 0, 2),
  Nao_gestacao = c(11, 3, 64, 58, 16),
  Nao_se_aplica_gestacao = c(18, 30, 71, 52, 9),
  Idade_gestacional = c(0, 26, 0, 0, 0),
  Ign_Branco_gestacao = c(4, 1, 19, 69, 26),
  Inconclusivo_casos = c(0, 0, 0, 0, 0),
  Branca = c(3, 4, 33, 69, 26),
  Preta = c(0, 0, 0, 0, 0),
  Amarela = c(0, 0, 0, 0, 0),
  Parda = c(0, 3, 1, 4, 0),
  Indigena = c(0, 0, 1, 0, 0),
  Ign_Branco_etnia = c(30, 54, 123, 37, 1),
  Confirmado_casos = c(2, 1, 0, 0, 0),
  Descartado_casos = c(0, 0, 0, 0, 0),
  Ign_Branco_casos = c(0, 1, 0, 0, 0)
)

agregar_dengue_sinan <- function(df, anos = 2020:2025, agravo = "dengue") {
  contar <- function(x, valores) sum(as.character(x) %in% valores, na.rm = TRUE)
  contar_regex <- function(x, padrao) sum(grepl(padrao, as.character(x), ignore.case = TRUE), na.rm = TRUE)
  normalizar <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
    tolower(trimws(x))
  }
  idade_anos <- function(x) {
    x <- as.character(x)
    codigo <- substr(x, 1, 1)
    valor <- suppressWarnings(as.numeric(substr(x, 2, nchar(x))))
    dplyr::case_when(
      codigo == "1" ~ valor / 8760,
      codigo == "2" ~ valor / 365,
      codigo == "3" ~ valor / 12,
      codigo == "4" ~ valor,
      TRUE ~ NA_real_
    )
  }
  
  # SINAN-DENGUE usa CS_ESCOL_N, não INSTRU (INSTRU é convenção de SIH-RD).
  # Codificação esperada de CS_ESCOL_N: 00 analfabeto; 01-08 níveis escolares;
  # 09 ignorado; 10 não se aplica.
  df$IDADEanos <- idade_anos(df$NU_IDADE_N)
  
  linhas <- lapply(anos, function(ano) {
    d <- df[df$Ano == ano, , drop = FALSE]
    classi <- as.character(d$CLASSI_FIN)
    evolucao <- as.character(d$EVOLUCAO)
    escol <- as.character(d$CS_ESCOL_N)
    gest <- as.character(d$CS_GESTANT)
    raca <- as.character(d$CS_RACA)
    sexo <- as.character(d$CS_SEXO)
    soro <- if("SOROTIPO" %in% names(d)) as.character(d$SOROTIPO) else rep("", nrow(d))
    idade <- d$IDADEanos
    classi_norm <- normalizar(classi)
    evolucao_norm <- normalizar(evolucao)
    escol_norm <- normalizar(escol)
    gest_norm <- normalizar(gest)
    raca_norm <- normalizar(raca)
    sexo_norm <- normalizar(sexo)
    soro_norm <- normalizar(soro)
    
    descartado <- grepl("descartado", classi_norm)
    inconclusivo <- grepl("inconclusivo", classi_norm)
    ign_classi <- classi_norm == "" | grepl("ignorado|branco", classi_norm)
    if(identical(tolower(agravo), "zika")) {
      confirmado <- (
        grepl("zika|confirm", classi_norm) |
          classi_norm %in% c("1", "2", "3", "laboratorio", "clinico epidemiologico")
      ) & !descartado & !inconclusivo
    } else {
      confirmado <- grepl("dengue|febre hemorr", classi_norm) &
        !grepl("descartado|chikungunya", classi_norm)
    }
    
    data.frame(
      Ano = ano,
      Masculino = sum(sexo_norm %in% c("masculino", "m"), na.rm = TRUE),
      Feminino = sum(sexo_norm %in% c("feminino", "f"), na.rm = TRUE),
      Ignorado_sexo = sum(sexo_norm == "" | sexo_norm %in% c("ignorado", "i"), na.rm = TRUE),
      Cura_evolucao = sum(evolucao_norm %in% c("cura", "1"), na.rm = TRUE),
      Obitos_Agr = sum(grepl(paste0("obito.*", agravo, "|", agravo), evolucao_norm), na.rm = TRUE),
      Obitos_Out = sum(grepl("obito.*outr|outr", evolucao_norm), na.rm = TRUE),
      Menor_1_Ano = sum(!is.na(idade) & idade < 1),
      Faixa_1_4 = sum(!is.na(idade) & idade >= 1 & idade <= 4),
      Faixa_5_9 = sum(!is.na(idade) & idade >= 5 & idade <= 9),
      Faixa_10_14 = sum(!is.na(idade) & idade >= 10 & idade <= 14),
      Faixa_15_19 = sum(!is.na(idade) & idade >= 15 & idade <= 19),
      Faixa_20_39 = sum(!is.na(idade) & idade >= 20 & idade <= 39),
      Faixa_40_59 = sum(!is.na(idade) & idade >= 40 & idade <= 59),
      Faixa_60_64 = sum(!is.na(idade) & idade >= 60 & idade <= 64),
      Faixa_65_69 = sum(!is.na(idade) & idade >= 65 & idade <= 69),
      Faixa_70_79 = sum(!is.na(idade) & idade >= 70 & idade <= 79),
      Faixa_80_mais = sum(!is.na(idade) & idade >= 80),
      Ignorado_idade = sum(is.na(idade)),
      Ign_Branco_escolaridade = sum(escol_norm == "" | escol_norm %in% c("09", "9", "ignorado"), na.rm = TRUE),
      Analfabeto = sum(escol_norm %in% c("00", "0", "analfabeto") | grepl("analf", escol_norm), na.rm = TRUE),
      Primeira_a_quarta_serie_incompleta_EF = sum(escol_norm %in% c("01", "1") | grepl("1.*4|primeira.*quarta", escol_norm), na.rm = TRUE),
      Quarta_serie_completa_EF = sum(escol_norm %in% c("02", "2") | grepl("4.*comp|quarta.*compl", escol_norm), na.rm = TRUE),
      Quinta_a_oitava_serie_incompleta_EF = sum(escol_norm %in% c("03", "3") | grepl("5.*8|quinta.*oitava", escol_norm), na.rm = TRUE),
      Ensino_fundamental_completo = sum(escol_norm %in% c("04", "4") | grepl("fund.*compl", escol_norm), na.rm = TRUE),
      Ensino_medio_incompleto = sum(escol_norm %in% c("05", "5") | grepl("medio.*incomp", escol_norm), na.rm = TRUE),
      Ensino_medio_completo = sum(escol_norm %in% c("06", "6") | grepl("medio.*comp", escol_norm), na.rm = TRUE),
      Educacao_superior_incompleta = sum(escol_norm %in% c("07", "7") | grepl("superior.*incomp", escol_norm), na.rm = TRUE),
      Educacao_superior_completa = sum(escol_norm %in% c("08", "8") | grepl("superior.*comp", escol_norm), na.rm = TRUE),
      Nao_se_aplica_escolaridade = sum(escol_norm %in% c("10", "nao se aplica"), na.rm = TRUE),
      Primeiro_trimestre = sum(gest_norm %in% c("1", "1o trimestre", "1 trimestre") | grepl("primeiro|1.*trim", gest_norm), na.rm = TRUE),
      Segundo_trimestre = sum(gest_norm %in% c("2", "2o trimestre", "2 trimestre") | grepl("segundo|2.*trim", gest_norm), na.rm = TRUE),
      Terceiro_trimestre = sum(gest_norm %in% c("3", "3o trimestre", "3 trimestre") | grepl("terceiro|3.*trim", gest_norm), na.rm = TRUE),
      Nao_gestacao = sum(gest_norm %in% c("nao", "5"), na.rm = TRUE),
      Nao_se_aplica_gestacao = sum(gest_norm %in% c("nao se aplica", "6"), na.rm = TRUE),
      Idade_gestacional = sum(grepl("idade", gest_norm), na.rm = TRUE),
      Ign_Branco_gestacao = sum(gest_norm == "" | gest_norm %in% c("ignorado", "9"), na.rm = TRUE),
      Inconclusivo_casos = sum(inconclusivo, na.rm = TRUE),
      Branca = sum(raca_norm %in% c("branca", "1"), na.rm = TRUE),
      Preta = sum(raca_norm %in% c("preta", "2"), na.rm = TRUE),
      Amarela = sum(raca_norm %in% c("amarela", "3"), na.rm = TRUE),
      Parda = sum(raca_norm %in% c("parda", "4"), na.rm = TRUE),
      Indigena = sum(raca_norm %in% c("indigena", "5"), na.rm = TRUE),
      Ign_Branco_etnia = sum(raca_norm == "" | raca_norm %in% c("ignorado", "9"), na.rm = TRUE),
      Confirmado_casos = sum(confirmado, na.rm = TRUE),
      Descartado_casos = sum(descartado, na.rm = TRUE),
      Ign_Branco_casos = sum(ign_classi, na.rm = TRUE),
      Ign_Branco_sorotipo = sum(soro_norm == "" | soro_norm %in% c("ignorado", "9"), na.rm = TRUE),
      DEN_1 = sum(soro_norm %in% c("1", "den 1", "den-1", "denv 1", "denv-1"), na.rm = TRUE),
      DEN_2 = sum(soro_norm %in% c("2", "den 2", "den-2", "denv 2", "denv-2"), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(linhas)
}

carregar_zika_microdatasus <- function(
  anos = 2020:2025,
  municipio = "330100",
  cache_path = file.path("dados_sinan_campos", "outputs", "tabelas", "zika_microdatasus_campos_v1.rds"),
  temporal_cache_path = file.path("dados_sinan_campos", "outputs", "tabelas", "zika_microdatasus_temporal_campos_v1.rds"),
  atualizar_cache = identical(tolower(Sys.getenv("ZIKA_ATUALIZAR_CACHE", "false")), "true")
) {
  cache_valido <- file.exists(cache_path)
  
  if(cache_valido && !atualizar_cache) return(readRDS(cache_path))
  if(!cache_valido && !atualizar_cache) {
    warning("Cache SINAN-ZIKA nao encontrado; usando tabela agregada local para evitar download no startup.")
    return(zika)
  }
  
  vars_zika <- c(
    "NU_ANO", "ID_MUNICIP", "ID_MN_RESI", "CS_SEXO", "CS_GESTANT", "CS_RACA",
    "CS_ESCOL_N", "NU_IDADE_N", "CLASSI_FIN", "EVOLUCAO"
  )
  
  tryCatch({
    lista <- lapply(anos, function(ano) {
      bruto <- fetch_datasus(
        year_start = ano,
        year_end = ano,
        uf = "RJ",
        information_system = "SINAN-ZIKA",
        vars = vars_zika,
        stop_on_error = FALSE,
        timeout = 600
      )
      
      if(is.null(bruto) || nrow(bruto) == 0) return(NULL)
      
      col_municipio <- if("ID_MUNICIP" %in% names(bruto)) "ID_MUNICIP" else "ID_MN_RESI"
      bruto <- bruto[as.character(bruto[[col_municipio]]) == municipio, , drop = FALSE]
      if(nrow(bruto) == 0) return(NULL)
      
      proc <- process_sinan_zika(bruto, municipality_data = FALSE)
      proc$Ano <- ano
      proc
    })
    
    dados_microdatasus <- dplyr::bind_rows(lista)
    if(nrow(dados_microdatasus) == 0) stop("Nenhum registro de zika encontrado para o municipio informado.")
    
    zika_agregado <- agregar_dengue_sinan(dados_microdatasus, anos, agravo = "zika")
    zika_temporal <- preparar_serie_temporal_sinan(dados_microdatasus, anos, agravo = "zika")
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(zika_agregado, cache_path)
    saveRDS(zika_temporal, temporal_cache_path)
    zika_agregado
  }, error = function(e) {
    warning("Falha ao carregar SINAN-ZIKA pelo microdatasus: ", conditionMessage(e))
    if(file.exists(cache_path)) return(readRDS(cache_path))
    warning("Sem cache de SINAN-ZIKA; usando tabela agregada informada no script.")
    zika
  })
}

carregar_dengue_microdatasus <- function(
  anos = 2020:2025,
  municipio = "330100",
  cache_path = file.path("dados_sinan_campos", "outputs", "tabelas", "dengue_microdatasus_campos_v2.rds"),
  temporal_cache_path = file.path("dados_sinan_campos", "outputs", "tabelas", "dengue_microdatasus_temporal_campos_v1.rds"),
  atualizar_cache = identical(tolower(Sys.getenv("DENGUE_ATUALIZAR_CACHE", "false")), "true")
) {
  cache_valido <- file.exists(cache_path)
  
  if(cache_valido && !atualizar_cache) return(readRDS(cache_path))
  if(!cache_valido && !atualizar_cache) {
    warning("Cache SINAN-DENGUE nao encontrado; usando tabela agregada local para evitar download no startup.")
    return(dengue)
  }
  
  vars_dengue <- c(
    "NU_ANO", "ID_MUNICIP", "ID_MN_RESI", "CS_SEXO", "CS_GESTANT", "CS_RACA",
    "CS_ESCOL_N", "NU_IDADE_N", "CLASSI_FIN", "EVOLUCAO", "SOROTIPO"
  )
  
  tryCatch({
    lista <- lapply(anos, function(ano) {
      bruto <- fetch_datasus(
        year_start = ano,
        year_end = ano,
        uf = "RJ",
        information_system = "SINAN-DENGUE",
        vars = vars_dengue,
        stop_on_error = FALSE,
        timeout = 600
      )
      
      if(is.null(bruto) || nrow(bruto) == 0) return(NULL)
      
      col_municipio <- if("ID_MUNICIP" %in% names(bruto)) "ID_MUNICIP" else "ID_MN_RESI"
      bruto <- bruto[as.character(bruto[[col_municipio]]) == municipio, , drop = FALSE]
      if(nrow(bruto) == 0) return(NULL)
      
      proc <- process_sinan_dengue(bruto, municipality_data = FALSE)
      proc$Ano <- ano
      proc
    })
    
    dados_microdatasus <- dplyr::bind_rows(lista)
    if(nrow(dados_microdatasus) == 0) stop("Nenhum registro de dengue encontrado para o município informado.")
    
    dengue_agregado <- agregar_dengue_sinan(dados_microdatasus, anos)
    dengue_temporal <- preparar_serie_temporal_sinan(dados_microdatasus, anos, agravo = "dengue")
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(dengue_agregado, cache_path)
    saveRDS(dengue_temporal, temporal_cache_path)
    dengue_agregado
  }, error = function(e) {
    warning("Falha ao carregar SINAN-DENGUE pelo microdatasus: ", conditionMessage(e))
    if(file.exists(cache_path)) return(readRDS(cache_path))
    stop("Não foi possível carregar dados de dengue do SINAN-DENGUE/microdatasus e não existe cache baixado.", call. = FALSE)
  })
}

carregar_populacao_campos_sidra <- function(
  anos = 2020:2025,
  cache_path = file.path("dados_sinan_campos", "outputs", "tabelas", "populacao_campos_sidra.rds"),
  atualizar_cache = identical(tolower(Sys.getenv("POPULACAO_ATUALIZAR_CACHE", "false")), "true")
) {
  registrar_status <- function(status, detalhes, linhas = 0) {
    registrar_log(LOG_POPULACAO, data.frame(
      fonte = "IBGE/SIDRA tabela 6579, variável 9324, município 3301009",
      status = status,
      anos_solicitados = paste(anos, collapse = ", "),
      linhas = linhas,
      detalhes = detalhes
    ))
  }

  preparar_pop <- function(pop) {
    pop$Ano <- as.integer(pop$Ano)
    pop$Populacao <- as.numeric(pop$Populacao)
    pop <- pop[!is.na(pop$Ano) & !is.na(pop$Populacao), , drop = FALSE]
    pop <- pop[order(pop$Ano), , drop = FALSE]
    if(nrow(pop) == 0) {
      return(data.frame(
        Ano = anos,
        Populacao = NA_real_,
        Fonte_populacao = "população indisponível",
        stringsAsFactors = FALSE
      ))
    }

    anos_faltantes <- setdiff(anos, pop$Ano)
    if(length(anos_faltantes) > 0 && nrow(pop) > 0) {
      estimados <- bind_rows(lapply(anos_faltantes, function(ano_faltante) {
        candidatos <- pop[pop$Ano <= ano_faltante, , drop = FALSE]
        if(nrow(candidatos) == 0) candidatos <- pop[pop$Ano >= ano_faltante, , drop = FALSE]
        base <- candidatos[which.max(candidatos$Ano), , drop = FALSE]
        data.frame(
          Ano = ano_faltante,
          Populacao = base$Populacao,
          Fonte_populacao = paste0("SIDRA ", base$Ano, " replicado por ausência do ano no retorno"),
          stringsAsFactors = FALSE
        )
      }))
      pop <- bind_rows(pop, estimados)
    }

    pop %>%
      filter(Ano %in% anos) %>%
      arrange(Ano)
  }

  if(file.exists(cache_path)) {
    pop_cache <- readRDS(cache_path)
    pop_cache <- preparar_pop(pop_cache)
    if(nrow(pop_cache) > 0) {
      registrar_status("cache", "População carregada do cache local.", nrow(pop_cache))
      return(pop_cache)
    }
    registrar_status("cache_invalido", "Cache local de população estava vazio e será atualizado.", 0)
  }

  if(!atualizar_cache) {
    pop_fallback <- data.frame(
      Ano = c(2020, 2021, 2022, 2023, 2024, 2025),
      Populacao = c(511168, 514643, 514643, 514643, 519011, 519259),
      Fonte_populacao = c(
        "IBGE/SIDRA tabela 6579, variavel 9324",
        "IBGE/SIDRA tabela 6579, variavel 9324",
        "SIDRA 2021 replicado por ausencia do ano no retorno",
        "SIDRA 2021 replicado por ausencia do ano no retorno",
        "IBGE/SIDRA tabela 6579, variavel 9324",
        "IBGE/SIDRA tabela 6579, variavel 9324"
      ),
      stringsAsFactors = FALSE
    )
    pop_fallback <- preparar_pop(pop_fallback)
    registrar_status("fallback_embutido", "Populacao carregada do fallback embutido no app para evitar consulta SIDRA no startup.", nrow(pop_fallback))
    return(pop_fallback)
  }

  if(!requireNamespace("sidrar", quietly = TRUE)) {
    pop_fallback <- data.frame(
      Ano = anos,
      Populacao = NA_real_,
      Fonte_populacao = "sidrar indisponível no ambiente",
      stringsAsFactors = FALSE
    )
    registrar_status("erro", "Pacote sidrar não instalado; incidência por 100 mil será exibida como indisponível.", nrow(pop_fallback))
    return(pop_fallback)
  }

  pop <- tryCatch({
    bruto <- sidrar::get_sidra(
      api = paste0("/t/6579/n6/3301009/v/9324/p/", paste(anos, collapse = ","))
    )
    col_ano <- c("Ano", "D1C", "D2C")[c("Ano", "D1C", "D2C") %in% names(bruto)][1]
    col_valor <- c("Valor", "V")[c("Valor", "V") %in% names(bruto)][1]
    if(is.na(col_ano) || is.na(col_valor)) {
      stop("Retorno do SIDRA sem colunas de ano/valor esperadas.", call. = FALSE)
    }
    data.frame(
      Ano = suppressWarnings(as.integer(bruto[[col_ano]])),
      Populacao = suppressWarnings(as.numeric(gsub(",", ".", bruto[[col_valor]]))),
      Fonte_populacao = "IBGE/SIDRA tabela 6579, variável 9324",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    registrar_status("erro", paste("Falha ao consultar SIDRA:", conditionMessage(e)), 0)
    data.frame(
      Ano = anos,
      Populacao = NA_real_,
      Fonte_populacao = "falha na consulta SIDRA",
      stringsAsFactors = FALSE
    )
  })

  pop <- preparar_pop(pop)
  if(any(!is.na(pop$Populacao))) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(pop, cache_path)
    registrar_status("sidra", "População carregada via sidrar e salva em cache local.", nrow(pop))
  } else {
    registrar_status("indisponível", "População não foi carregada; incidência por 100 mil ficará indisponível até nova consulta SIDRA.", nrow(pop))
  }
  pop
}

dengue <- carregar_dengue_microdatasus()
zika <- carregar_zika_microdatasus()
populacao_campos <- carregar_populacao_campos_sidra()

carregar_temporal_cache <- function(cache_path) {
  if(file.exists(cache_path)) return(readRDS(cache_path))
  data.frame(Agravo = character(), Intervalo = character(), Periodo = character(), Data = as.Date(character()), Ano = integer(), Casos = integer())
}

dengue_temporal <- carregar_temporal_cache(file.path("dados_sinan_campos", "outputs", "tabelas", "dengue_microdatasus_temporal_campos_v1.rds"))
zika_temporal <- carregar_temporal_cache(file.path("dados_sinan_campos", "outputs", "tabelas", "zika_microdatasus_temporal_campos_v1.rds"))

normalizar_bairro <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tools::toTitleCase(tolower(x))
}

carregar_bairros_dengue_local <- function(
  dir_dados = "dados_sinan_campos"
) {
  arquivos <- list.files(dir_dados, pattern = "^DENGUE[0-9]{4}\\.xlsx$", full.names = TRUE)
  if(length(arquivos) == 0) {
    warning("Nenhuma planilha DENGUEYYYY.xlsx encontrada em: ", dir_dados)
    return(data.frame(Ano = integer(), NM_BAIRRO = character(), Casos = integer()))
  }
  
  dados_bairros <- lapply(arquivos, function(arquivo) {
    ano <- as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(basename(arquivo))))
    df <- readxl::read_excel(
      arquivo,
      col_types = "text",
      na = c("", "NA", "N/A", "NULL")
    )
    
    if(!"NM_BAIRRO" %in% names(df)) {
      return(data.frame(Ano = integer(), NM_BAIRRO = character(), Casos = integer()))
    }
    
    df %>%
      transmute(
        Ano = ano,
        NM_BAIRRO = normalizar_bairro(NM_BAIRRO)
      ) %>%
      filter(
        NM_BAIRRO != "",
        !grepl("ignorado|sem informa|nao informado|não informado", NM_BAIRRO, ignore.case = TRUE)
      ) %>%
      count(Ano, NM_BAIRRO, name = "Casos")
  })
  
  bind_rows(dados_bairros) %>%
    arrange(Ano, desc(Casos), NM_BAIRRO)
}

dengue_bairros <- carregar_bairros_dengue_local()

# Lista de dados
dados <- list(
  "Chikungunya" = chikungunya,
  "Dengue" = dengue,
  "Zika" = zika
)

# Validar todos os dados iniciais
for(nome in names(dados)) {
  inconsistencias <- validar_dados(dados[[nome]], nome)
  
  registrar_log(LOG_DADOS, data.frame(
    doenca = nome,
    linhas = nrow(dados[[nome]]),
    colunas = ncol(dados[[nome]]),
    anos = paste(unique(dados[[nome]]$Ano), collapse = ", "),
    total_confirmados = sum(dados[[nome]]$Confirmado_casos),
    total_inconclusivos = sum(dados[[nome]]$Inconclusivo_casos),
    total_gestantes = sum(dados[[nome]]$Primeiro_trimestre) + 
      sum(dados[[nome]]$Segundo_trimestre) + 
      sum(dados[[nome]]$Terceiro_trimestre),
    total_obitos_agr = sum(dados[[nome]]$Obitos_Agr),
    total_ignorado_sexo = sum(dados[[nome]]$Ignorado_sexo),
    total_ignorado_idade = sum(dados[[nome]]$Ignorado_idade),
    inconsistencias = length(inconsistencias)
  ))
}

# FUNCOES UTILITARIAS

filter_year <- function(df, ano) {
  if(ano == "Todos") return(df)
  ano_num <- as.numeric(ano)
  df[df$Ano == ano_num, , drop = FALSE]
}

format_number <- function(x) {
  if(length(x) == 0) return(character(0))
  x_num <- suppressWarnings(as.numeric(x))
  resultado <- format(x_num, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  resultado[is.na(x_num) | x_num == 0] <- "0"
  resultado
}

format_percent <- function(x) {
  if(length(x) == 0) return(character(0))
  x_num <- suppressWarnings(as.numeric(x))
  resultado <- format(round(x_num, 1), nsmall = 1, decimal.mark = ",", scientific = FALSE, trim = TRUE)
  resultado[is.na(x_num)] <- "0,0"
  paste0(resultado, "%")
}

format_rate <- function(x) {
  if(length(x) == 0) return(character(0))
  x_num <- suppressWarnings(as.numeric(x))
  resultado <- format(round(x_num, 1), nsmall = 1, decimal.mark = ",", big.mark = ".", scientific = FALSE, trim = TRUE)
  resultado[is.na(x_num) | !is.finite(x_num)] <- "Indisponível"
  resultado
}

adicionar_populacao <- function(df) {
  df %>%
    left_join(populacao_campos, by = "Ano") %>%
    mutate(
      Incidencia_100mil = ifelse(!is.na(Populacao) & Populacao > 0, Confirmado_casos / Populacao * 100000, NA_real_)
    )
}

incidencia_periodo <- function(df) {
  df_pop <- adicionar_populacao(df)
  casos <- sum(df_pop$Confirmado_casos, na.rm = TRUE)
  pop_media <- mean(df_pop$Populacao, na.rm = TRUE)
  if(!is.finite(pop_media) || pop_media <= 0) return(NA_real_)
  casos / pop_media * 100000
}

somar_colunas <- function(df, colunas) {
  colunas <- colunas[colunas %in% names(df)]
  if(length(colunas) == 0) return(0)
  sum(as.matrix(df[, colunas, drop = FALSE]), na.rm = TRUE)
}

qualidade_variavel <- function(df, variavel, colunas_total, colunas_ignoradas) {
  colunas <- colunas_total[colunas_total %in% names(df)]
  total <- somar_colunas(df, colunas)
  ignorado <- somar_colunas(df, colunas_ignoradas)
  ausentes <- sum(sapply(colunas, function(coluna) sum(is.na(df[[coluna]]))))
  problema <- ignorado + ausentes
  percentual <- if(total > 0) problema / total * 100 else NA_real_
  data.frame(
    Variavel = variavel,
    Total = total,
    Ignorado_Branco_Ausente = problema,
    Percentual = percentual,
    stringsAsFactors = FALSE
  )
}

qualidade_dados <- function(df) {
  colunas_idade <- c(
    "Menor_1_Ano", "Faixa_1_4", "Faixa_5_9", "Faixa_10_14", "Faixa_15_19",
    "Criancas_e_jovens", "Faixa_20_39", "Faixa_40_59", "Adultos",
    "Faixa_60_64_anos", "Faixa_65_69_anos", "Faixa_70_79_anos",
    "Faixa_60_64", "Faixa_65_69", "Faixa_70_79", "Faixa_80_mais",
    "Idosos", "Ignorado_idade"
  )
  bind_rows(
    qualidade_variavel(df, "Sexo", c("Masculino", "Feminino", "Ignorado_sexo"), c("Ignorado_sexo")),
    qualidade_variavel(df, "Idade", colunas_idade, c("Ignorado_idade")),
    qualidade_variavel(df, "Escolaridade", c(
      "Ign_Branco_escolaridade", "Analfabeto", "Primeira_a_quarta_serie_incompleta_EF",
      "Quarta_serie_completa_EF", "Quinta_a_oitava_serie_incompleta_EF",
      "Ensino_fundamental_completo", "Ensino_medio_incompleto", "Ensino_medio_completo",
      "Educacao_superior_incompleta", "Educacao_superior_completa", "Nao_se_aplica_escolaridade"
    ), c("Ign_Branco_escolaridade")),
    qualidade_variavel(df, "Raça/cor", c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Ign_Branco_etnia"), c("Ign_Branco_etnia")),
    qualidade_variavel(df, "Gestação", c(
      "Primeiro_trimestre", "Segundo_trimestre", "Terceiro_trimestre",
      "Nao_gestacao", "Nao_se_aplica_gestacao", "Idade_gestacional", "Ign_Branco_gestacao"
    ), c("Ign_Branco_gestacao", "Idade_gestacional"))
  )
}

qualidade_dados_base <- qualidade_dados
qualidade_dados <- function(df) {
  bind_rows(
    qualidade_dados_base(df),
    qualidade_variavel(df, "Classificacao final", c(
      "Confirmado_casos", "Descartado_casos", "Inconclusivo_casos", "Ign_Branco_casos"
    ), c("Ign_Branco_casos"))
  )
}

dashboard_qualidade_dados <- function(output_id) {
  div(class = "quality-panel",
    h4("Qualidade dos dados"),
    p("Percentual de registros ignorados, brancos ou ausentes nas principais variáveis do periodo selecionado."),
    uiOutput(output_id)
  )
}

criar_donut <- function(valores, rotulos, cores, exibir_rotulo_detalhado = FALSE) {
  manter <- valores > 0
  valores <- valores[manter]
  rotulos <- rotulos[manter]
  cores <- cores[manter]
  
  if(length(valores) == 0) {
    return(plot_ly() %>% layout(margin = list(l = 60, r = 60, t = 10, b = 80)))
  }
  
  df <- data.frame(
    rotulo = rotulos,
    valor = valores,
    cor = cores
  )
  df$percentual <- (df$valor / sum(df$valor)) * 100
  df$rotulo_hover <- paste0(
    df$rotulo,
    ": ",
    format_percent(df$percentual),
    " (",
    format_number(df$valor),
    ")"
  )
  texto_template <- if(
    exibir_rotulo_detalhado
  ) "%{label}<br>%{value} (%{percent})" else "%{label}"
  
  plot_ly(df, 
          labels = ~rotulo, 
          values = ~valor, 
          type = "pie",
          hole = 0.5,
          textposition = 'outside',
          texttemplate = texto_template,
          textinfo = 'none',
          hovertemplate = "<b>%{label}: %{value} (%{percent})</b><extra></extra>",
          marker = list(colors = cores),
          textfont = list(size = 10, color = "#1e293b"),
          insidetextorientation = 'auto',
      automargin = TRUE,
          pull = 0.04,
          showlegend = FALSE) %>%
        layout(margin = list(l = 60, r = 60, t = 10, b = 80))
}

criar_barras_verticais <- function(valores, rotulos, cores = NULL) {
  if(length(valores) == 0) {
    return(plot_ly() %>% layout(margin = list(l = 40, r = 20, t = 10, b = 80)))
  }
  
  if(is.null(cores)) {
    cores <- colorRampPalette(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B", "#7C6A0A"))(length(valores))
  }
  
  df_plot <- data.frame(
    categoria = factor(rotulos, levels = rotulos),
    casos = valores
  )
  df_plot$rotulo_texto <- ifelse(df_plot$casos > 0, format_number(df_plot$casos), "")
  
  # Calcular altura máxima para dar espaço aos números
  y_max <- max(valores) * 1.15
  
  plot_ly(df_plot) %>%
    add_bars(x = ~categoria, y = ~casos,
             marker = list(color = cores),
             text = ~rotulo_texto,
             textposition = "outside",
          cliponaxis = FALSE,
             textfont = list(size = 11, color = "#1e293b"),
             hovertemplate = "<b>%{x}</b><br>Casos: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickfont = list(size = 9), tickangle = -45, automargin = TRUE),
        yaxis = list(title = "Casos", tickfont = list(size = 10), range = c(0, y_max), automargin = TRUE),
           showlegend = FALSE,
        margin = list(l = 36, r = 12, t = 8, b = 72))
}

# Gráfico de série temporal (Confirmado_casos por ano)
criar_grafico_serie <- function(df, cor, nome) {
  df_plot <- df
  df_plot$Ano <- as.integer(df_plot$Ano)
  df_plot$rotulo_texto <- ifelse(df_plot$Confirmado_casos > 0, format_number(df_plot$Confirmado_casos), "")
  df_plot$posicao_texto <- ifelse(seq_len(nrow(df_plot)) == which.max(df_plot$Confirmado_casos), "top left", "top center")
  anos_unicos <- sort(unique(df_plot$Ano))
  y_max <- max(df_plot$Confirmado_casos, na.rm = TRUE) * 1.18
  # Gráfico principal
  p <- plot_ly(df_plot, x = ~Ano, y = ~Confirmado_casos, 
          type = "scatter", mode = "lines+markers+text",
          line = list(color = cor, width = 3),
      marker = list(color = cor, size = 9),
          text = ~rotulo_texto,
      textposition = ~posicao_texto,
      cliponaxis = FALSE,
      textfont = list(size = 15, color = "#1e293b"),
          name = "Casos confirmados",
          hovertemplate = "<b>Ano: %{x}</b><br>Casos: %{y}<extra></extra>")
  # Layout sem título, apenas legenda
  p %>% layout(xaxis = list(title = "", tickfont = list(size = 10),
                            tickmode = "array", tickvals = anos_unicos,
                            ticktext = as.character(anos_unicos)),
         yaxis = list(title = "", tickfont = list(size = 11), range = c(0, y_max)),
               hovermode = "x unified",
               showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, y = 1.5, xanchor = "center", font = list(size = 14)),
    margin = list(l = 45, r = 35, t = 30, b = 55))
}

criar_grafico_incidencia <- function(df, cor, nome) {
  df_plot <- adicionar_populacao(df)
  if(all(is.na(df_plot$Incidencia_100mil))) {
    return(plot_ly() %>% layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = "População indisponível para calcular incidência.",
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 13, color = "#475569")
      ))
    ))
  }
  df_plot$rotulo_texto <- ifelse(is.na(df_plot$Incidencia_100mil), "", format_rate(df_plot$Incidencia_100mil))
  y_max <- max(df_plot$Incidencia_100mil, na.rm = TRUE) * 1.18
  plot_ly(df_plot, x = ~Ano, y = ~Incidencia_100mil,
          type = "scatter", mode = "lines+markers+text",
          line = list(color = cor, width = 3),
          marker = list(color = cor, size = 9),
          text = ~rotulo_texto,
          textposition = "top center",
          cliponaxis = FALSE,
          textfont = list(size = 13, color = "#1e293b"),
          name = "Incidência/100 mil",
          hovertemplate = "<b>Ano: %{x}</b><br>Incidência: %{y:.1f}/100 mil<br>População: %{customdata}<extra></extra>",
          customdata = ~format_number(Populacao)) %>%
    layout(
      xaxis = list(title = "", tickfont = list(size = 10), tickmode = "array", tickvals = sort(unique(df_plot$Ano))),
      yaxis = list(title = "Casos por 100 mil habitantes", tickfont = list(size = 10), range = c(0, y_max), automargin = TRUE),
      showlegend = FALSE,
      margin = list(l = 55, r = 25, t = 20, b = 55)
    )
}

criar_grafico_sexo <- function(df, destacar_feminino_2024 = FALSE, organizar_rotulos = FALSE) {
  if(nrow(df) == 0) {
    return(plot_ly() %>% layout(margin = list(l = 40, r = 20, t = 10, b = 80)))
  }

  df_plot <- df %>%
    select(Ano, Masculino, Feminino, Ignorado_sexo) %>%
    mutate(Ano_num = as.numeric(Ano))
  anos_unicos <- sort(unique(df_plot$Ano_num))
  y_base_max <- max(c(df_plot$Masculino, df_plot$Feminino, df_plot$Ignorado_sexo), na.rm = TRUE)
  largura_barra <- 0.22
  deslocamento_masculino <- -0.26
  deslocamento_feminino <- 0
  deslocamento_ignorado <- 0.26
  offset_rotulo <- max(y_base_max * 0.04, 10)
  faixa_baixa_limite <- max(y_base_max * 0.10, 15)
  base_rotulos_baixos <- max(y_base_max * 0.08, 18)
  espacamento_rotulos_baixos <- max(y_base_max * 0.06, 14)
  distancia_minima_rotulos <- max(y_base_max * 0.045, 26)
  deslocamento_ano_alternado <- distancia_minima_rotulos * 0.55
  deslocamento_serie_rotulo <- c("Masculino" = 0, "Feminino" = 1.15, "Ign/Branco" = 2.35)
  df_feminino_2024 <- df_plot %>% filter(destacar_feminino_2024, Ano_num == 2024, Feminino > 0)
  df_rotulos <- bind_rows(
    data.frame(
      Ano_num = df_plot$Ano_num,
      x = df_plot$Ano_num + deslocamento_masculino,
      valor = df_plot$Masculino,
      rotulo = ifelse(df_plot$Masculino > 0, format_number(df_plot$Masculino), ""),
      serie = "Masculino",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Ano_num = df_plot$Ano_num,
      x = df_plot$Ano_num + deslocamento_feminino,
      valor = df_plot$Feminino,
      rotulo = ifelse(
        destacar_feminino_2024 & df_plot$Ano_num == 2024,
        "",
        ifelse(df_plot$Feminino > 0, format_number(df_plot$Feminino), "")
      ),
      serie = "Feminino",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Ano_num = df_plot$Ano_num,
      x = df_plot$Ano_num + deslocamento_ignorado,
      valor = df_plot$Ignorado_sexo,
      rotulo = ifelse(df_plot$Ignorado_sexo > 0, format_number(df_plot$Ignorado_sexo), ""),
      serie = "Ign/Branco",
      stringsAsFactors = FALSE
    )
  ) %>%
    filter(rotulo != "")

  if(nrow(df_rotulos) > 0) {
    if(organizar_rotulos) {
      df_rotulos <- df_rotulos %>%
        group_by(Ano_num) %>%
        mutate(
          ordem_serie = unname(deslocamento_serie_rotulo[serie]),
          rotulo_baixo = valor <= faixa_baixa_limite,
          deslocamento_base_ano = ifelse(first(Ano_num) %% 2 == 0, 0, deslocamento_ano_alternado),
          y_rotulo = ifelse(
            rotulo_baixo,
            base_rotulos_baixos + ordem_serie * (espacamento_rotulos_baixos * 1.8) + deslocamento_base_ano,
            valor + offset_rotulo + ordem_serie * distancia_minima_rotulos + deslocamento_base_ano
          )
        ) %>%
        ungroup()
    } else {
      df_rotulos <- df_rotulos %>% mutate(y_rotulo = valor + offset_rotulo)
    }

    df_rotulos <- bind_rows(lapply(split(df_rotulos, df_rotulos$Ano_num), function(df_ano) {
      df_ano <- df_ano[order(df_ano$y_rotulo, df_ano$valor), ]

      if(nrow(df_ano) > 1) {
        df_ano$y_rotulo <- df_ano$y_rotulo + seq(0, by = distancia_minima_rotulos * 0.45, length.out = nrow(df_ano))

        for(i in 2:nrow(df_ano)) {
          df_ano$y_rotulo[i] <- max(df_ano$y_rotulo[i], df_ano$y_rotulo[i - 1] + distancia_minima_rotulos)
        }
      }

      df_ano
    }))
  }

  y_max <- max(
    c(
      df_plot$Masculino,
      df_plot$Feminino,
      df_plot$Ignorado_sexo,
      if(nrow(df_rotulos) > 0) df_rotulos$y_rotulo,
      if(nrow(df_feminino_2024) > 0) df_feminino_2024$Feminino + (offset_rotulo * 1.25)
    ),
    na.rm = TRUE
  ) * 1.08

  p <- plot_ly() %>%
    add_bars(
      data = df_plot,
      x = ~Ano_num + deslocamento_masculino,
      y = ~Masculino,
      width = largura_barra,
      name = "Masculino",
      marker = list(color = "#2E86AB"),
      hovertext = ~paste0("Ano ", Ano_num, "<br>Masculino: ", format_number(Masculino)),
      hoverinfo = "text"
    ) %>%
    add_bars(
      data = df_plot,
      x = ~Ano_num + deslocamento_feminino,
      y = ~Feminino,
      width = largura_barra,
      name = "Feminino",
      marker = list(color = "#A23B72"),
      hovertext = ~paste0("Ano ", Ano_num, "<br>Feminino: ", format_number(Feminino)),
      hoverinfo = "text"
    ) %>%
    add_bars(
      data = df_plot,
      x = ~Ano_num + deslocamento_ignorado,
      y = ~Ignorado_sexo,
      width = largura_barra,
      name = "Ign/Branco",
      marker = list(color = "#ea8a0e"),
      hovertext = ~paste0("Ano ", Ano_num, "<br>Ign/Branco: ", format_number(Ignorado_sexo)),
      hoverinfo = "text"
    )

  if(nrow(df_rotulos) > 0) {
    p <- p %>%
      add_text(
        data = df_rotulos,
        x = ~x,
        y = ~y_rotulo,
        text = ~rotulo,
        textposition = "top center",
        textfont = list(size = 12, color = "#1e293b"),
        cliponaxis = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
  }

  if(nrow(df_feminino_2024) > 0) {
    p <- p %>%
      add_text(
        data = df_feminino_2024,
        x = ~Ano_num + deslocamento_feminino,
        y = ~Feminino + (offset_rotulo * 1.25),
        text = ~format_number(Feminino),
        textposition = "top center",
        textfont = list(size = 14, color = "#1e293b"),
        cliponaxis = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
  }

  p %>%
    layout(
      barmode = "overlay",
      xaxis = list(
        title = "Ano",
        tickfont = list(size = 10),
        tickmode = "array",
        tickvals = anos_unicos,
        ticktext = as.character(anos_unicos),
        range = c(min(anos_unicos) - 0.65, max(anos_unicos) + 0.65),
        automargin = TRUE
      ),
      yaxis = list(title = "", tickfont = list(size = 10), range = c(0, y_max), automargin = TRUE),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.24,
        xanchor = "center",
        font = list(size = 12)
      ),
      margin = list(l = 36, r = 12, t = 64, b = 52)
    )
}

criar_grafico_faixa_etaria <- function(df) {
  if(nrow(df) == 0) {
    return(plot_ly() %>% layout(margin = list(l = 40, r = 15, t = 10, b = 90)))
  }

  colunas_criancas_detalhadas <- c("Menor_1_Ano", "Faixa_1_4", "Faixa_5_9", "Faixa_10_14", "Faixa_15_19")
  colunas_adultos_detalhadas <- c("Faixa_20_39", "Faixa_40_59")
  colunas_idosos_detalhadas <- c("Faixa_60_64_anos", "Faixa_65_69_anos", "Faixa_70_79_anos", "Faixa_80_mais")
  colunas_idosos_detalhadas_alt <- c("Faixa_60_64", "Faixa_65_69", "Faixa_70_79", "Faixa_80_mais")
  if(all(colunas_criancas_detalhadas %in% names(df))) {
    colunas_faixa <- colunas_criancas_detalhadas
  } else {
    colunas_faixa <- c("Criancas_e_jovens")
  }
  if(all(colunas_adultos_detalhadas %in% names(df))) {
    colunas_faixa <- c(colunas_faixa, colunas_adultos_detalhadas)
  } else if("Adultos" %in% names(df)) {
    colunas_faixa <- c(colunas_faixa, "Adultos")
  }
  if(all(colunas_idosos_detalhadas %in% names(df))) {
    colunas_faixa <- c(colunas_faixa, colunas_idosos_detalhadas)
  } else if(all(colunas_idosos_detalhadas_alt %in% names(df))) {
    colunas_faixa <- c(colunas_faixa, colunas_idosos_detalhadas_alt)
  } else if("Idosos" %in% names(df)) {
    colunas_faixa <- c(colunas_faixa, "Idosos")
  }
  colunas_faixa <- c(colunas_faixa, "Ignorado_idade")

  mapa_categorias <- c(
    "Menor_1_Ano" = "<1",
    "Faixa_1_4" = "1-4",
    "Faixa_5_9" = "5-9",
    "Faixa_10_14" = "10-14",
    "Faixa_15_19" = "15-19",
    "Criancas_e_jovens" = "0-19",
    "Faixa_20_39" = "20-39",
    "Faixa_40_59" = "40-59",
    "Adultos" = "20-59",
    "Idosos" = "60+",
    "Faixa_60_64_anos" = "60-64",
    "Faixa_65_69_anos" = "65-69",
    "Faixa_70_79_anos" = "70-79",
    "Faixa_60_64" = "60-64",
    "Faixa_65_69" = "65-69",
    "Faixa_70_79" = "70-79",
    "Faixa_80_mais" = "80+",
    "Ignorado_idade" = "Ign/Branco"
  )
  rotulos <- unname(mapa_categorias[colunas_faixa])
  valores <- sapply(colunas_faixa, function(coluna) sum(df[[coluna]], na.rm = TRUE))

  if(all(valores <= 0)) {
    return(
      plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "Sem valores positivos para exibir.",
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 13, color = "#475569")
            )
          ),
          margin = list(l = 40, r = 20, t = 40, b = 40)
        )
    )
  }
  
  cores_categorias <- c(
    "<1" = "#F6C85F",
    "1-4" = "#FF8C42",
    "5-9" = "#E4572E",
    "10-14" = "#76B041",
    "15-19" = "#4B8F8C",
    "0-19" = "#F18F01",
    "20-39" = "#C73E1D",
    "40-59" = "#2E86AB",
    "20-59" = "#C73E1D",
    "60-64" = "#3B1F2B",
    "65-69" = "#1ABC9C",
    "70-79" = "#7C6A0A",
    "80+" = "#A23B72",
    "60+" = "#3B1F2B",
    "Ign/Branco" = "#95A5A6"
  )

  criar_barras_verticais(
    valores = valores,
    rotulos = rotulos,
    cores = unname(cores_categorias[rotulos])
  ) %>%
    layout(
      xaxis = list(title = "", tickfont = list(size = 9), tickangle = -45),
      yaxis = list(title = "", tickfont = list(size = 10)),
      margin = list(l = 45, r = 20, t = 10, b = 95)
    )
}

# Gráfico de etnia (rosca)
criar_grafico_etnia <- function(df) {
  valores <- c(sum(df$Branca), sum(df$Preta), sum(df$Amarela), sum(df$Parda), sum(df$Indigena), sum(df$Ign_Branco_etnia))
  rotulos <- c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ign/Branco")
  cores <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B", "#7C6A0A")
  criar_donut(valores, rotulos, cores, exibir_rotulo_detalhado = TRUE)
}

criar_grafico_escolaridade <- function(df) {
  rotulos <- c(
    "Ign/Branco esc.", "Analfabeto", "EF 1-4 inc.",
    "EF 4 comp.", "EF 5-8 inc.", "Fund. completo",
    "Médio incompleto", "Médio completo",
    "Sup. incompleta", "Sup. completa",
    "Não se aplica"
  )
  valores <- c(
    sum(df$Ign_Branco_escolaridade),
    sum(df$Analfabeto),
    sum(df$Primeira_a_quarta_serie_incompleta_EF),
    sum(df$Quarta_serie_completa_EF),
    sum(df$Quinta_a_oitava_serie_incompleta_EF),
    sum(df$Ensino_fundamental_completo),
    sum(df$Ensino_medio_incompleto),
    sum(df$Ensino_medio_completo),
    sum(df$Educacao_superior_incompleta),
    sum(df$Educacao_superior_completa),
    sum(df$Nao_se_aplica_escolaridade)
  )
  cores <- c("#7C6A0A", "#95A5A6", "#2E86AB", "#A23B72", "#F18F01", "#C73E1D", 
             "#3B1F2B", "#1ABC9C", "#9B59B6", "#E67E22", "#3498DB")
  
  criar_barras_horizontais <- function(valores, rotulos, cores) {
    df_plot <- data.frame(
      categoria = factor(rotulos, levels = rev(rotulos)),
      casos = valores
    )
    x_max <- max(df_plot$casos, na.rm = TRUE)
    if(!is.finite(x_max) || x_max <= 0) x_max <- 1
    df_plot$rotulo_texto <- ifelse(df_plot$casos > 0, format_number(df_plot$casos), "")
    
    plot_ly(df_plot) %>%
      add_bars(x = ~casos, y = ~categoria,
               orientation = 'h',
               marker = list(color = cores),
               text = ~rotulo_texto,
               textposition = "outside",
               cliponaxis = FALSE,
               textfont = list(size = 13, color = "#1e293b"),
               hovertemplate = "<b>%{y}</b><br>Casos: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickfont = list(size = 12),
              range = c(0, x_max * 1.3)),
         yaxis = list(title = "", tickfont = list(size = 11), automargin = TRUE),
         showlegend = FALSE,
         margin = list(l = 165, r = 70, t = 12, b = 34),
         bargap = 0.24)
  }
  
  criar_barras_horizontais(valores, rotulos, cores)
}

criar_grafico_gestacao <- function(df) {
  rotulos <- c(
    "Ign/Branco", "Idade gestacional", "Não se aplica",
    "Não gestante", "3º trimestre", "2º trimestre", "1º trimestre"
  )
  valores <- c(
    sum(df$Ign_Branco_gestacao),
    sum(df$Idade_gestacional),
    sum(df$Nao_se_aplica_gestacao),
    sum(df$Nao_gestacao),
    sum(df$Terceiro_trimestre),
    sum(df$Segundo_trimestre),
    sum(df$Primeiro_trimestre)
  )
  cores <- c("#7C6A0A", "#95A5A6", "#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B")
  
  criar_barras_verticais(valores, rotulos, cores)
}

opcoes_visualizacao <- c(
  "Série temporal" = "serie",
  "Incidência por 100 mil" = "incidencia",
  "Sexo" = "sexo",
  "Raça/cor" = "etnia",
  "Faixa etária" = "faixa",
  "Gestação" = "gestacao",
  "Escolaridade" = "escolaridade"
)

tabela_visualizacao <- function(df, visualizacao) {
  if(visualizacao == "serie") {
    return(df %>% select(Ano, Confirmado_casos, Descartado_casos, Inconclusivo_casos, Ign_Branco_casos))
  }
  if(visualizacao == "incidencia") {
    return(adicionar_populacao(df) %>% select(Ano, Confirmado_casos, Populacao, Incidencia_100mil, Fonte_populacao))
  }
  if(visualizacao == "sexo") {
    return(df %>% select(Ano, Masculino, Feminino, Ignorado_sexo))
  }
  if(visualizacao == "etnia") {
    valores <- c(sum(df$Branca), sum(df$Preta), sum(df$Amarela), sum(df$Parda), sum(df$Indigena), sum(df$Ign_Branco_etnia))
    return(data.frame(Categoria = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ign/Branco"), Casos = valores))
  }
  if(visualizacao == "faixa") {
    valores <- c(
      sum(df$Menor_1_Ano), sum(df$Faixa_1_4), sum(df$Faixa_5_9), sum(df$Faixa_10_14),
      sum(df$Faixa_15_19), sum(df$Faixa_20_39), sum(df$Faixa_40_59),
      sum(if("Faixa_60_64" %in% names(df)) df$Faixa_60_64 else df$Faixa_60_64_anos),
      sum(if("Faixa_65_69" %in% names(df)) df$Faixa_65_69 else df$Faixa_65_69_anos),
      sum(if("Faixa_70_79" %in% names(df)) df$Faixa_70_79 else df$Faixa_70_79_anos),
      sum(df$Faixa_80_mais), sum(df$Ignorado_idade)
    )
    return(data.frame(Categoria = c("<1", "1-4", "5-9", "10-14", "15-19", "20-39", "40-59", "60-64", "65-69", "70-79", "80+", "Ign/Branco"), Casos = valores))
  }
  if(visualizacao == "gestacao") {
    valores <- c(sum(df$Primeiro_trimestre), sum(df$Segundo_trimestre), sum(df$Terceiro_trimestre), sum(df$Nao_gestacao), sum(df$Nao_se_aplica_gestacao), sum(df$Idade_gestacional), sum(df$Ign_Branco_gestacao))
    return(data.frame(Categoria = c("1º trimestre", "2º trimestre", "3º trimestre", "Não gestante", "Não se aplica", "Idade gestacional ignorada", "Ign/Branco"), Casos = valores))
  }
  valores <- c(
    sum(df$Ign_Branco_escolaridade), sum(df$Analfabeto), sum(df$Primeira_a_quarta_serie_incompleta_EF),
    sum(df$Quarta_serie_completa_EF), sum(df$Quinta_a_oitava_serie_incompleta_EF),
    sum(df$Ensino_fundamental_completo), sum(df$Ensino_medio_incompleto),
    sum(df$Ensino_medio_completo), sum(df$Educacao_superior_incompleta),
    sum(df$Educacao_superior_completa), sum(df$Nao_se_aplica_escolaridade)
  )
  data.frame(
    Categoria = c("Ign/Branco", "Analfabeto", "EF 1-4 inc.", "EF 4 comp.", "EF 5-8 inc.", "Fund. completo", "Médio incompleto", "Médio completo", "Sup. incompleta", "Sup. completa", "Não se aplica"),
    Casos = valores
  )
}

tabela_analitica_doenca <- function(df, nome_doenca, periodo_label = NULL) {
  df_pop <- adicionar_populacao(df)
  qualidade <- qualidade_dados(df)
  qualidade_resumo <- qualidade %>%
    transmute(
      Indicador = paste0("Qualidade_", gsub("[^A-Za-z0-9]+", "_", Variavel), "_pct"),
      Valor = round(Percentual, 2)
    )
  
  resumo <- data.frame(
    Doenca = nome_doenca,
    Periodo = if(is.null(periodo_label)) paste(min(df$Ano), max(df$Ano), sep = "-") else periodo_label,
    Unidade_analise = APP_UNIDADE_ANALISE,
    Fonte = if(nome_doenca %in% c("Dengue", "Zika")) "SINAN/SVS via microdatasus" else "SINAN/SVS em tabela agregada",
    Casos_confirmados = sum(df$Confirmado_casos, na.rm = TRUE),
    Casos_descartados = sum(df$Descartado_casos, na.rm = TRUE),
    Casos_inconclusivos = sum(df$Inconclusivo_casos, na.rm = TRUE),
    Ignorado_branco_classificacao = sum(df$Ign_Branco_casos, na.rm = TRUE),
    Incidencia_periodo_100mil = incidencia_periodo(df),
    Populacao_media = mean(df_pop$Populacao, na.rm = TRUE),
    Data_atualizacao = as.character(APP_DATA_ATUALIZACAO),
    stringsAsFactors = FALSE
  )
  
  qualidade_wide <- as.data.frame(t(qualidade_resumo$Valor), stringsAsFactors = FALSE)
  names(qualidade_wide) <- qualidade_resumo$Indicador
  cbind(resumo, qualidade_wide)
}

nota_metodologica_doenca <- function(nome_doenca, fonte, criterio_confirmacao, limitacoes) {
  div(class = "method-note",
    h4("Nota metodologica"),
    tags$dl(
      tags$dt("Fonte"),
      tags$dd(fonte),
      tags$dt("Periodo"),
      tags$dd(APP_PERIODO_PADRAO),
      tags$dt("Unidade de analise"),
      tags$dd(APP_UNIDADE_ANALISE),
      tags$dt("Criterio de confirmacao"),
      tags$dd(criterio_confirmacao),
      tags$dt("Limitacoes"),
      tags$dd(limitacoes)
    )
  )
}

painel_temporal_bruto <- function(prefixo) {
  div(class = "download-panel",
    h4("Serie temporal a partir dos registros brutos"),
    p("Contagem de casos confirmados por intervalo mensal ou semanal, quando os registros brutos do microdatasus estao disponiveis em cache."),
    fluidRow(
      column(4, selectInput(paste0(prefixo, "_temporal_intervalo"), "Intervalo", choices = c("Mensal", "Semanal"), selected = "Mensal")),
      column(4, br(), downloadButton(paste0(prefixo, "_download_temporal"), "Baixar serie temporal CSV"))
    ),
    plotlyOutput(paste0(prefixo, "_temporal_plot"), height = "280px"),
    DTOutput(paste0(prefixo, "_temporal_tabela"))
  )
}

painel_correspondencia_bairros <- function() {
  div(class = "download-panel",
    h4("Correspondencia bairro-planilha vs bairro-geobr"),
    p("Tabela de auditoria da vinculacao espacial. Bairros sem correspondencia indicam divergencias de grafia, localidade sem poligono na malha de 2010 ou necessidade de ajuste manual."),
    downloadButton("dengue_download_correspondencia_bairros", "Baixar correspondencia CSV"),
    DTOutput("dengue_correspondencia_bairros")
  )
}

metadados_figura <- function(nome_doenca, visualizacao, periodo_label = "Período analisado") {
  nomes <- c(
    serie = "série temporal dos casos confirmados",
    incidencia = "incidência anual por 100 mil habitantes",
    sexo = "distribuicao por sexo e ano",
    etnia = "distribuicao por raça/cor",
    faixa = "distribuicao por faixa etaria",
    gestacao = "situação gestacional",
    escolaridade = "distribuicao por escolaridade"
  )
  o_que <- paste0("O que mostra: ", nome_doenca, " - ", nomes[[visualizacao]], ".")
  interpretar <- switch(
    visualizacao,
    serie = "Como interpretar: observe picos, quedas e concentracao temporal de casos confirmados.",
    incidencia = "Como interpretar: compare a carga anual padronizada pela população estimada do município.",
    sexo = "Como interpretar: compare a distribuicao entre masculino, feminino e ignorado/branco ao longo dos anos.",
    etnia = "Como interpretar: avalie a composicao dos registros segundo raça/cor e o peso de ignorado/branco.",
    faixa = "Como interpretar: identifique grupos etarios com maior carga registrada no periodo selecionado.",
    gestacao = "Como interpretar: observe registros relacionados à gestação e categorias não aplicáveis ou ignoradas.",
    escolaridade = "Como interpretar: avalie padrões de escolaridade sempre considerando incompletude de preenchimento."
  )
  atencao <- "Atenção metodológica: dados notificados não equivalem à incidência real; subnotificação e campos ignorados/brancos podem alterar a interpretação."
  fonte <- if(visualizacao == "incidencia") {
    "Fonte: SINAN/SVS; população IBGE/SIDRA, tabela 6579, variável 9324."
  } else {
    "Fonte: SINAN/SVS."
  }
  paste(o_que, interpretar, atencao, fonte, periodo_label, sep = "\n")
}

grafico_publicacao <- function(df, visualizacao, titulo, nome_doenca = titulo, periodo_label = "Período analisado") {
  tab <- tabela_visualizacao(df, visualizacao)
  caption <- metadados_figura(nome_doenca, visualizacao, periodo_label)
  tema_publicacao <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 17, color = "#1A2535"),
      plot.subtitle = element_text(size = 11, color = "#475569"),
      plot.caption = element_text(size = 9, color = "#475569", hjust = 0, lineheight = 1.15),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )

  if(visualizacao == "serie") {
    tab_long <- tab %>% tidyr::pivot_longer(-Ano, names_to = "Categoria", values_to = "Casos")
    return(
      ggplot(tab_long, aes(x = Ano, y = Casos, color = Categoria, group = Categoria)) +
        geom_line(linewidth = 0.9) + geom_point(size = 2.2) +
        labs(title = titulo, subtitle = periodo_label, x = "Ano", y = "Casos", caption = caption) +
        tema_publicacao
    )
  }
  if(visualizacao == "sexo") {
    tab_long <- tab %>% tidyr::pivot_longer(-Ano, names_to = "Categoria", values_to = "Casos")
    return(
      ggplot(tab_long, aes(x = factor(Ano), y = Casos, fill = Categoria)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.68) +
        geom_text(aes(label = ifelse(Casos > 0, format_number(Casos), "")), position = position_dodge(width = 0.75), vjust = -0.25, size = 3.1, color = "#1e293b") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
        labs(title = titulo, subtitle = periodo_label, x = "Ano", y = "Casos", caption = caption) +
        tema_publicacao
    )
  }
  if(visualizacao == "incidencia") {
    tab$rotulo <- format_rate(tab$Incidencia_100mil)
    return(
      ggplot(tab, aes(x = Ano, y = Incidencia_100mil)) +
        geom_line(linewidth = 1, color = "#1B3A6B") +
        geom_point(size = 2.5, color = "#1B3A6B") +
        geom_text(aes(label = rotulo), vjust = -0.7, size = 3.4, color = "#1e293b") +
        scale_x_continuous(breaks = tab$Ano) +
        labs(title = titulo, subtitle = periodo_label, x = "Ano", y = "Casos por 100 mil habitantes", caption = caption) +
        tema_publicacao
    )
  }
  ggplot(tab, aes(x = reorder(Categoria, Casos), y = Casos)) +
    geom_col(fill = "#1B3A6B") +
    geom_text(aes(label = format_number(Casos)), hjust = -0.08, size = 3.3, color = "#1e293b") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(title = titulo, subtitle = periodo_label, x = "", y = "Casos", caption = caption) +
    tema_publicacao
}

painel_downloads <- function(prefixo) {
  div(class = "download-panel",
      fluidRow(
        column(4, selectInput(paste0(prefixo, "_vis"), "Visualização para tabela/download", choices = opcoes_visualizacao)),
        column(3, selectInput(paste0(prefixo, "_fmt"), "Formato do plot", choices = c("PDF" = "pdf", "TIFF" = "tiff", "SVG" = "svg"))),
        column(2, br(), downloadButton(paste0(prefixo, "_download_plot"), "Baixar plot")),
        column(3, br(), downloadButton(paste0(prefixo, "_download_table"), "Baixar tabela CSV"))
      ),
      fluidRow(
        column(4, br(), downloadButton(paste0(prefixo, "_download_analitica"), "Baixar tabela analitica CSV"))
      ),
      DTOutput(paste0(prefixo, "_tabela"))
  )
}

botao_download_grafico <- function(output_id) {
  div(
    class = "plot-download",
    downloadButton(output_id, "Baixar em alta resolução")
  )
}

perfil_equipe <- function(nome, subtitulo, mini_curriculo, lattes, iniciais) {
  div(class = "team-card",
    div(class = "team-avatar", iniciais),
    h4(nome),
    div(class = "team-role", subtitulo),
    p(class = "team-bio", mini_curriculo),
    div(class = "team-links",
      tags$a(
        href = lattes,
        target = "_blank",
        class = "team-link-btn",
        tags$i(class = "fa fa-graduation-cap"),
        "Lattes"
      )
    )
  )
}

texto_contexto_dengue <- div(class = "context-box",
  tags$strong("Guia de interpretação: "),
  "use os gráficos de dengue para observar concentração etária, distribuição por sexo, completude de escolaridade/raça e mudanças temporais de confirmação. Campos ignorados/brancos devem ser interpretados como sinal de qualidade de preenchimento, não como categoria biológica."
)

normalizar_chave_bairro <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("\\bparq\\b|\\bpq\\b", "parque", x)
  x <- gsub("\\bjd\\b", "jardim", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

primeira_coluna_existente <- function(df, candidatas) {
  candidatas[candidatas %in% names(df)][1]
}

carregar_malha_bairros_campos <- function(
  cache_path = file.path("dados_sinan_campos", "outputs", "tabelas", "geobr_bairros_campos_2010.rds"),
  gpkg_path = file.path("dados_sinan_campos", "outputs", "tabelas", "neighborhoods_2010_simplified.gpkg")
) {
  preparar_malha <- function(malha) {
    col_bairro <- primeira_coluna_existente(malha, c("name_neighborhood", "name_neighbourhood", "NM_BAIRRO", "bairro", "NM_BAIR", "NM_BAIRRO_GEO"))
    if(is.na(col_bairro)) {
      stop("Não encontrei uma coluna de nome de bairro na malha do geobr.", call. = FALSE)
    }
    
    malha$NM_BAIRRO_GEO <- normalizar_bairro(malha[[col_bairro]])
    malha$bairro_key <- normalizar_chave_bairro(malha[[col_bairro]])
    sf::st_make_valid(malha)
  }
  
  if(file.exists(cache_path)) {
    malha_cache <- readRDS(cache_path)
    if(!all(c("NM_BAIRRO_GEO", "bairro_key") %in% names(malha_cache))) {
      malha_cache <- preparar_malha(malha_cache)
      saveRDS(malha_cache, cache_path)
    }
    return(malha_cache)
  }
  
  malha <- tryCatch(
    geobr::read_neighborhood(
      year = 2010,
      simplified = TRUE,
      showProgress = FALSE,
      cache = TRUE
    ),
    error = function(e) NULL
  )
  
  if(is.null(malha) || nrow(malha) == 0) {
    dir.create(dirname(gpkg_path), recursive = TRUE, showWarnings = FALSE)
    if(!file.exists(gpkg_path) || file.info(gpkg_path)$size == 0) {
      utils::download.file(
        url = "https://github.com/ipeaGIT/geobr/releases/download/v1.7.0/neighborhoods_2010_simplified.gpkg",
        destfile = gpkg_path,
        mode = "wb",
        quiet = TRUE
      )
    }
    malha <- sf::read_sf(gpkg_path)
  }
  
  if(is.null(malha) || nrow(malha) == 0) {
    stop("O geobr não retornou a malha de bairros.", call. = FALSE)
  }
  
  col_codigo <- primeira_coluna_existente(malha, c("code_muni", "code_muni_7", "CD_MUN", "CD_GEOCMU"))
  col_municipio <- primeira_coluna_existente(malha, c("name_muni", "NM_MUN", "municipio"))
  if(!is.na(col_codigo)) {
    malha <- malha[as.character(malha[[col_codigo]]) %in% c("3301009", "330100"), , drop = FALSE]
  } else if(!is.na(col_municipio)) {
    malha <- malha[grepl("Campos dos Goytacazes", as.character(malha[[col_municipio]]), ignore.case = TRUE), , drop = FALSE]
  } else {
    stop("Não encontrei coluna de município na malha do geobr para filtrar Campos dos Goytacazes.", call. = FALSE)
  }
  
  if(nrow(malha) == 0) {
    stop("A base de bairros do geobr não trouxe polígonos para Campos dos Goytacazes.", call. = FALSE)
  }
  
  malha <- preparar_malha(malha)
  
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(malha, cache_path)
  malha
}

preparar_mapa_bairros_geobr <- function(df_bairros) {
  malha <- carregar_malha_bairros_campos()
  dados_bairro <- df_bairros %>%
    mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
    group_by(bairro_key) %>%
    summarise(
      NM_BAIRRO_DADOS = first(NM_BAIRRO),
      Casos = sum(Casos, na.rm = TRUE),
      .groups = "drop"
    )
  
  mapa <- malha %>%
    left_join(dados_bairro, by = "bairro_key") %>%
    mutate(
      Casos = ifelse(is.na(Casos), 0, Casos),
      rotulo_bairro = dplyr::coalesce(NM_BAIRRO_DADOS, NM_BAIRRO_GEO)
    )
  
  nao_mapeados <- dados_bairro %>%
    filter(!bairro_key %in% mapa$bairro_key) %>%
    arrange(desc(Casos))

  list(mapa = mapa, nao_mapeados = nao_mapeados)
}

preparar_correspondencia_bairros <- function(df_bairros) {
  if(is.null(df_bairros) || nrow(df_bairros) == 0) {
    return(data.frame(
      NM_BAIRRO_PLANILHA = character(),
      bairro_key = character(),
      Casos = integer(),
      NM_BAIRRO_GEOBR = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  malha <- carregar_malha_bairros_campos() %>%
    sf::st_drop_geometry() %>%
    mutate(NM_BAIRRO_GEOBR = dplyr::coalesce(
      if("NM_BAIRRO_GEOBR" %in% names(.)) NM_BAIRRO_GEOBR else NA_character_,
      if("NM_BAIRRO_GEO" %in% names(.)) NM_BAIRRO_GEO else NA_character_
    )) %>%
    select(bairro_key, NM_BAIRRO_GEOBR) %>%
    distinct()
  
  df_bairros %>%
    mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
    group_by(NM_BAIRRO_PLANILHA = NM_BAIRRO, bairro_key) %>%
    summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
    left_join(malha, by = "bairro_key") %>%
    mutate(
      Status = ifelse(is.na(NM_BAIRRO_GEOBR), "Nao mapeado", "Mapeado"),
      NM_BAIRRO_GEOBR = ifelse(is.na(NM_BAIRRO_GEOBR), "", NM_BAIRRO_GEOBR)
    ) %>%
    arrange(Status, desc(Casos), NM_BAIRRO_PLANILHA)
}

grafico_mapa_bairros_geobr <- function(mapa_sf, periodo_label) {
  ggplot(mapa_sf) +
    geom_sf(aes(fill = Casos), color = "#ffffff", linewidth = 0.16) +
    scale_fill_gradientn(
      colours = c("#fff7bc", "#fec44f", "#fe9929", "#d95f0e", "#7f0000"),
      name = "Casos",
      labels = format_number
    ) +
    labs(
      title = "Dengue por bairro em Campos dos Goytacazes",
      subtitle = periodo_label,
      caption = "Fonte espacial: geobr/IBGE, bairros 2010. Fonte epidemiológica: planilhas locais SINAN-DENGUE."
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#1A2535"),
      plot.subtitle = element_text(size = 11, color = "#475569"),
      plot.caption = element_text(size = 9, color = "#64748b"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(34, "pt")
    )
}

mapa_leaflet_bairros_geobr <- function(mapa_sf, periodo_label) {
  mapa_sf <- sf::st_transform(mapa_sf, 4326)
  valores <- mapa_sf$Casos
  dominio <- valores[is.finite(valores)]
  if(length(dominio) == 0) dominio <- c(0, 1)
  
  pal <- leaflet::colorNumeric(
    palette = c("#fff7bc", "#fec44f", "#fe9929", "#d95f0e", "#7f0000"),
    domain = dominio,
    na.color = "#e2e8f0"
  )
  
  labels <- paste0(
    "<strong>", mapa_sf$rotulo_bairro, "</strong><br>",
    "Casos: ", format_number(mapa_sf$Casos), "<br>",
    periodo_label
  )
  
  leaflet::leaflet(mapa_sf, options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Base clara") %>%
    leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
    leaflet::addPolygons(
      group = "Dengue por bairro",
      fillColor = ~ifelse(Casos > 0, pal(Casos), "#e2e8f0"),
      fillOpacity = ~ifelse(Casos > 0, 0.82, 0.34),
      color = "#ffffff",
      opacity = 0.95,
      weight = 0.7,
      smoothFactor = 0.25,
      label = lapply(labels, htmltools::HTML),
      popup = lapply(labels, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#1B3A6B",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = dominio,
      title = "Casos de dengue",
      opacity = 0.85
    ) %>%
    leaflet::addScaleBar(
      position = "bottomleft",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Base clara", "OpenStreetMap"),
      overlayGroups = c("Dengue por bairro"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}

# UI

ui <- dashboardPage(skin = "black",
  dashboardHeader(
    title = "",
    titleWidth = 0,
    disable = FALSE
  ),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("INÍCIO", tabName = "inicio", icon = icon("home")),
      menuItem("TUTORIAL", tabName = "tutorial", icon = icon("book-open")),
      menuItem("MÉTODOS", tabName = "metodos", icon = icon("flask")),
      menuItem("EQUIPE", tabName = "equipe", icon = icon("users")),
      menuItem("CHIKUNGUNYA", tabName = "chik"),
      menuItem("DENGUE", tabName = "dengue"),
      menuItem("ZIKA", tabName = "zika")
    ),
    hr(),
    tags$div(class = "sidebar-fonte",
      tags$strong("Fonte de Dados"),
      "SINAN/SVS — 2020–2025"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,300;0,400;0,600;0,700;1,400&family=Source+Serif+4:wght@400;600;700&display=swap');
        
        * { font-family: 'Source Sans 3', 'Helvetica Neue', Arial, sans-serif; box-sizing: border-box; }
        
        /* ===== HEADER ===== */
        .skin-black .main-header .logo,
        .skin-black .main-header .logo:hover {
          background-color: #1B3A6B !important;
          font-family: 'Source Serif 4', Georgia, serif !important;
          font-size: 0 !important;
          font-weight: 700 !important;
          letter-spacing: 1.5px !important;
          text-transform: uppercase !important;
          color: transparent !important;
          border-bottom: 3px solid #122856 !important;
        }
        .skin-black .main-header .logo {
          width: 0 !important;
          padding: 0 !important;
        }
        .skin-black .main-header .navbar {
          background-color: #1B3A6B !important;
          border-bottom: 3px solid #122856 !important;
          margin-left: 0 !important;
        }
        .skin-black .main-header .navbar::after {
          content: none !important;
          display: none !important;
        }
        .fixed-app-title {
          position: fixed !important;
          top: 0 !important;
          left: 50% !important;
          transform: translateX(-50%);
          height: 50px;
          display: flex;
          align-items: center;
          justify-content: center;
          color: #ffffff;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 21px;
          font-weight: 700;
          letter-spacing: 0.3px;
          white-space: nowrap;
          pointer-events: none;
          z-index: 2000 !important;
        }
        .skin-black .main-header .sidebar-toggle {
          color: rgba(255,255,255,0.85) !important;
          width: 50px !important;
          text-align: left;
          padding-left: 9px !important;
          padding-right: 0 !important;
          position: fixed !important;
          left: 0 !important;
          top: 0 !important;
          z-index: 2100 !important;
        }
        .skin-black .main-header .sidebar-toggle:hover {
          background: rgba(0,0,0,0.18) !important;
          color: white !important;
        }
        
        /* ===== SIDEBAR ===== */
        .skin-black .main-sidebar,
        .skin-black .left-side {
          background-color: #1A2535 !important;
        }
        .skin-black .sidebar a { color: #B8C8DC !important; }
        .skin-black .sidebar-menu > li > a {
          color: #B8C8DC !important;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 1.4px;
          text-transform: uppercase;
          padding: 14px 18px;
          border-left: 3px solid transparent !important;
          border-bottom: 1px solid rgba(255,255,255,0.04) !important;
          transition: all 0.18s ease;
        }
        .skin-black .sidebar-menu > li:hover > a {
          color: #ffffff !important;
          background: rgba(27,58,107,0.28) !important;
          border-left-color: #1B3A6B !important;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li.active > a:hover {
          color: #ffffff !important;
          background: rgba(27,58,107,0.55) !important;
          border-left-color: #ffffff !important;
        }
        .skin-black .sidebar hr {
          border-color: rgba(255,255,255,0.1);
          margin: 8px 18px;
        }
        .sidebar .form-group { padding: 0 14px 10px 14px; }
        .sidebar .form-group label {
          color: #9E9E9E !important;
          font-size: 10px;
          font-weight: 700;
          letter-spacing: 1.5px;
          text-transform: uppercase;
        }
        .sidebar .selectize-input {
          background: rgba(255,255,255,0.07) !important;
          border: 1px solid rgba(255,255,255,0.18) !important;
          color: #ffffff !important;
          border-radius: 4px !important;
          font-size: 13px !important;
        }
        .sidebar .selectize-input .item { color: #fff !important; }
        .sidebar .selectize-dropdown {
          background: #1E3048 !important;
          border: 1px solid rgba(255,255,255,0.12) !important;
        }
        .sidebar .selectize-dropdown-content .option { color: #B8C8DC !important; }
        .sidebar .selectize-dropdown-content .option.active,
        .sidebar .selectize-dropdown-content .option:hover {
          background: rgba(27,58,107,0.55) !important;
          color: white !important;
        }
        /* Data source footnote inside sidebar */
        .sidebar-fonte {
          padding: 10px 18px 14px 18px;
          color: #4A6580;
          font-size: 10px;
          line-height: 1.5;
          border-top: 1px solid rgba(255,255,255,0.07);
          margin-top: 6px;
        }
        .sidebar-fonte strong {
          display: block;
          color: #9E9E9E;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 1.2px;
          text-transform: uppercase;
          margin-bottom: 3px;
        }
        
        /* ===== CONTENT AREA ===== */
        .content-wrapper, .right-side { background-color: #F4F4F4 !important; }
        .content { padding: 22px 24px 14px 24px; }
        
        /* ===== DISEASE TITLE ===== */
        .doenca-titulo {
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 34px;
          font-weight: 700;
          color: #1A2535;
          margin: 0 0 16px 0;
          padding-bottom: 10px;
          border-bottom: 3px solid #1B3A6B;
          display: flex;
          align-items: center;
          letter-spacing: -0.2px;
        }
        .mosquito-icon {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 40px;
          height: 40px;
          background: #1B3A6B;
          border-radius: 6px;
          margin-right: 14px;
          box-shadow: 0 2px 8px rgba(27,58,107,0.35);
        }
        .mosquito-emoji { font-size: 22px; line-height: 1; }
        
        /* ===== METRIC CARDS ===== */
        .custom-card {
          background: #ffffff;
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          border: none !important;
          border-top: 3px solid #1B3A6B !important;
          width: 100%;
          padding: 14px 10px 12px 10px;
          text-align: center;
          margin-bottom: 14px;
        }
        .card-value {
          font-size: 34px;
          font-weight: 700;
          color: #1A2535;
          line-height: 1.1;
          font-variant-numeric: tabular-nums;
        }
        .card-label {
          font-size: 11px;
          font-weight: 700;
          color: #1B3A6B;
          text-transform: uppercase;
          letter-spacing: 1.2px;
          margin-top: 5px;
        }
        
        /* ===== GRAPH BOXES ===== */
        .graph-box {
          background: #ffffff;
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          border: 1px solid #B8C8D8 !important;
          border-top: 3px solid #1B3A6B !important;
          padding: 14px 10px 10px 10px;
          margin-bottom: 14px;
        }
        .graph-box .plotly, .graph-box .js-plotly-plot,
        .graph-box .plot-container, .graph-box .svg-container {
          width: 100% !important;
        }
        .graph-title {
          font-size: 13px;
          font-weight: 600;
          color: #1A2535;
          margin-bottom: 8px;
          padding-bottom: 5px;
          border-bottom: 1px solid #EDEDED;
        }
        .plot-download {
          display: flex;
          justify-content: center;
          margin: 8px 0 10px 0;
        }
        .plot-download .btn {
          background: #1B3A6B !important;
          border: 1px solid #122856 !important;
          border-radius: 4px !important;
          color: #ffffff !important;
          font-size: 12px !important;
          font-weight: 600 !important;
          padding: 6px 12px !important;
          box-shadow: 0 1px 4px rgba(27,58,107,0.18);
        }
        .plot-download .btn:hover,
        .plot-download .btn:focus {
          background: #122856 !important;
          color: #ffffff !important;
        }
        .plot-download .fa {
          margin-right: 6px;
        }
        
        /* ===== FIGURE CAPTIONS ===== */
        .fig-caption {
          font-size: 16px;
          color: #777;
          font-style: italic;
          margin-top: 7px;
          line-height: 1.45;
          padding-left: 8px;
          border-left: 2px solid #B8C8D8;
        }
        .landing-section, .context-box, .download-panel, .team-profile {
          background: #ffffff;
          border: 1px solid #B8C8D8;
          border-top: 3px solid #1B3A6B;
          border-radius: 6px;
          padding: 16px;
          margin-bottom: 14px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        .landing-section h3, .landing-section h4, .team-profile h4 {
          margin-top: 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
        }
        .context-box {
          color: #334155;
          line-height: 1.45;
        }
        .download-panel .dt-container {
          font-size: 12px;
        }
        .home-hero {
          background: #ffffff;
          border: 1px solid #B8C8D8;
          border-top: 4px solid #1B3A6B;
          border-radius: 8px;
          padding: 22px;
          margin-bottom: 14px;
          box-shadow: 0 2px 10px rgba(15,23,42,0.06);
        }
        .home-hero h3 {
          margin: 0 0 8px 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 26px;
        }
        .home-hero p {
          color: #334155;
          font-size: 15px;
          line-height: 1.5;
          max-width: 980px;
        }
        .hero-metrics {
          display: grid;
          grid-template-columns: repeat(4, minmax(0, 1fr));
          gap: 10px;
          margin-top: 16px;
        }
        .hero-metric {
          background: #F8FAFC;
          border: 1px solid #D8E0EA;
          border-radius: 6px;
          padding: 12px;
        }
        .hero-metric strong {
          display: block;
          color: #1B3A6B;
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.8px;
          margin-bottom: 4px;
        }
        .hero-metric span {
          color: #1A2535;
          font-weight: 700;
          font-size: 15px;
        }
        .contact-button {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          background: #1B3A6B;
          color: #ffffff !important;
          border: 1px solid #122856;
          border-radius: 4px;
          padding: 9px 14px;
          font-size: 13px;
          font-weight: 700;
          text-decoration: none !important;
          margin-top: 8px;
          box-shadow: 0 1px 5px rgba(27,58,107,0.2);
        }
        .contact-button:hover,
        .contact-button:focus {
          background: #122856;
          color: #ffffff !important;
        }
        .quality-panel {
          background: #ffffff;
          border: 1px solid #B8C8D8;
          border-top: 3px solid #1B3A6B;
          border-radius: 6px;
          padding: 16px;
          margin-bottom: 14px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        .quality-panel h4 {
          margin-top: 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
        }
        .quality-grid {
          display: grid;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 10px;
          margin-top: 10px;
        }
        .quality-card {
          background: #F8FAFC;
          border: 1px solid #D8E0EA;
          border-radius: 6px;
          padding: 11px 10px;
          min-height: 92px;
        }
        .quality-label {
          color: #475569;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.8px;
          text-transform: uppercase;
        }
        .quality-value {
          color: #1A2535;
          font-size: 24px;
          font-weight: 700;
          line-height: 1.15;
          margin-top: 5px;
        }
        .quality-note {
          color: #64748B;
          font-size: 11px;
          margin-top: 3px;
        }
        .team-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px;
          margin-top: 14px;
        }
        .team-card {
          background: #ffffff;
          border: 1px solid #D8E0EA;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(15,23,42,0.06);
          padding: 18px 18px 16px 18px;
          text-align: center;
          min-height: 245px;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: flex-start;
        }
        .team-avatar {
          width: 96px;
          height: 96px;
          border-radius: 50%;
          background: #EEF3F8;
          border: 1px solid #D8E0EA;
          color: #1B3A6B;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 28px;
          font-weight: 700;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-bottom: 14px;
        }
        .team-card h4 {
          margin: 0 0 6px 0;
          color: #0F172A;
          font-family: 'Source Sans 3', 'Helvetica Neue', Arial, sans-serif;
          font-size: 17px;
          font-weight: 700;
        }
        .team-role {
          color: #64748B;
          font-size: 13px;
          margin-bottom: 10px;
        }
        .team-bio {
          color: #334155;
          font-size: 13px;
          line-height: 1.45;
          margin: 0 0 12px 0;
          max-width: 92%;
        }
        .team-links {
          margin-top: auto;
          display: flex;
          justify-content: center;
        }
        .team-link-btn {
          display: inline-flex;
          align-items: center;
          gap: 7px;
          border: 1px solid #D8E0EA;
          border-radius: 18px;
          padding: 7px 13px;
          color: #0F172A;
          background: #FFFFFF;
          font-size: 13px;
          font-weight: 600;
          text-decoration: none !important;
          box-shadow: 0 1px 4px rgba(15,23,42,0.04);
        }
        .team-link-btn:hover,
        .team-link-btn:focus {
          color: #ffffff;
          background: #1B3A6B;
          border-color: #1B3A6B;
        }
        @media (max-width: 900px) {
          .fixed-app-title {
            font-size: 15px;
            max-width: calc(100vw - 105px);
            overflow: hidden;
            text-overflow: ellipsis;
          }
          .content {
            padding: 14px 10px 10px 10px;
          }
          .doenca-titulo {
            font-size: 24px;
            align-items: flex-start;
          }
          .hero-metrics,
          .quality-grid {
            grid-template-columns: 1fr;
          }
          .graph-box {
            padding: 12px 8px 9px 8px;
          }
          .fig-caption {
            font-size: 13px;
          }
          .team-grid {
            grid-template-columns: 1fr;
          }
        }
        .mapa-bairros {
          width: 100%;
          min-height: 380px;
          border-radius: 6px;
          border: 1px solid #B8C8D8;
          overflow: hidden;
          background: #e5e7eb;
        }
        .mapa-legenda {
          background: rgba(255,255,255,0.94);
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.18);
          padding: 10px 12px;
          font-size: 12px;
          color: #1e293b;
          line-height: 1.4;
        }
        .mapa-legenda-item {
          display: flex;
          align-items: center;
          gap: 7px;
          margin-top: 4px;
        }
        .mapa-legenda-cor {
          width: 14px;
          height: 14px;
          border-radius: 3px;
          display: inline-block;
          border: 1px solid rgba(0,0,0,0.15);
        }
        
        /* ===== LAYOUT ===== */
        .col-sm-6, .col-sm-3, .col-sm-4, .col-sm-12 { padding-left: 6px; padding-right: 6px; }
        .row { margin-left: -6px; margin-right: -6px; margin-bottom: 4px; }
        .graph-row { margin-bottom: 0 !important; }
        .cards-destaque { margin-top: 0; }
        
        /* ===== SCROLLBAR ===== */
        ::-webkit-scrollbar { width: 6px; height: 6px; }
        ::-webkit-scrollbar-track { background: #f0f0f0; }
        ::-webkit-scrollbar-thumb { background: #1B3A6B; border-radius: 3px; }
        
        /* ===== TAB CONTENT PADDING ===== */
        .tab-content { padding-top: 2px; }
        
        /* ===== CARD OBITOS DESTAQUE ===== */
        .custom-card.card-obitos {
          border-top-color: #D97706 !important;
        }
        .custom-card.card-obitos .card-label {
          color: #B45309 !important;
        }
        .custom-card.card-obitos .card-value {
          color: #92400E;
        }
        
        /* ===== PERIODO BADGE ===== */
        .periodo-badge {
          display: inline-flex;
          align-items: center;
          gap: 5px;
          background: #EBF0F8;
          color: #1B3A6B;
          font-size: 11px;
          font-weight: 600;
          letter-spacing: 0.8px;
          text-transform: uppercase;
          padding: 4px 12px;
          border-radius: 20px;
          border: 1px solid #B8C8D8;
          white-space: nowrap;
          margin-bottom: 14px;
        }
      "))
    ),
    tags$div(class = "fixed-app-title", "Painel de Inteligência Epidemiológica - Campos dos Goytacazes"),
    tabItems(
      tabItem(
        tabName = "inicio",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "A")),
          span("Painel de Inteligência Epidemiológica para Chikungunya, Dengue e Zika em Campos dos Goytacazes (2020–2025)")
        ),
        div(class = "home-hero",
          h3("Painel de Inteligência Epidemiológica para Arboviroses em Campos dos Goytacazes"),
          p("Leitura integrada de Chikungunya, Dengue e Zika em Campos dos Goytacazes, com foco em vigilância epidemiológica, qualidade da informação, comparação temporal, perfil sociodemográfico e apoio à produção de relatórios e apresentações."),
          div(class = "hero-metrics",
            div(class = "hero-metric", tags$strong("Doenças"), span("Chikungunya, Dengue e Zika")),
            div(class = "hero-metric", tags$strong("Período"), span("2020-2025")),
            div(class = "hero-metric", tags$strong("Fonte"), span("SINAN/SVS, microdatasus, SIDRA e dados locais")),
            div(class = "hero-metric", tags$strong("Uso"), span("Ensino, pesquisa e vigilância"))
          )
        ),
        div(class = "landing-section",
          h3("Projeto"),
          p("Este aplicativo Shiny organiza dados de arboviroses em uma experiência interativa para leitura epidemiológica, sociodemográfica e biológica. As abas de dengue e zika utilizam dados baixados pelo pacote microdatasus, filtrados e agregados para Campos dos Goytacazes."),
          p("A plataforma integra um projeto de iniciação científica do Instituto Federal de Educação, Ciência e Tecnologia Fluminense, Campus Campos Guarus, financiado pelo CNPq e vinculado ao curso de Bacharelado em Enfermagem do IFF Guarus. O projeto é coordenado pela Profa. Dra. Karla Rangel Ribeiro e articula vigilância em saúde, ciência de dados, formação em enfermagem e comunicação científica aplicada ao território."),
          p("O site foi desenvolvido com o intuito de funcionar como um painel de apoio à decisão estratégica e baseada em dados para a gestão de saúde do município, oferecendo uma leitura organizada dos registros de arboviroses e servindo também como exemplo metodológico para outras iniciativas de vigilância, ensino, extensão e pesquisa aplicada.")
        ),
        fluidRow(
          column(4, div(class = "landing-section",
            h4("Objetivos"),
            tags$ul(
              tags$li("Explorar padrões temporais, etários, sexuais, gestacionais e sociodemográficos."),
              tags$li("Avaliar qualidade de preenchimento por campos ignorados ou brancos."),
              tags$li("Gerar tabelas e figuras exportáveis para relatório, apresentação e publicação.")
            )
          )),
          column(4, div(class = "landing-section",
            h4("Significado biológico"),
            p("Diferenças por idade, sexo, gestação e raça/cor podem sugerir padrões de exposição, vulnerabilidade, acesso ao cuidado, busca por diagnóstico e gravidade potencial. Essas associações não provam causalidade, mas ajudam a gerar hipóteses.")
          )),
          column(4, div(class = "landing-section",
            h4("O que explorar"),
            p("Use os filtros de ano, gráficos e tabelas para comparar incidência observada, perfil dos casos, completude das notificações e mudanças no tempo. Em seguida, baixe as figuras e tabelas da visualização selecionada.")
          ))
        ),
        div(class = "landing-section",
          h4("Estado atual e perspectivas"),
          p("No estágio atual, o painel apresenta principalmente indicadores sociodemográficos e epidemiológicos descritivos para cada arbovirose, como distribuição por sexo, idade, escolaridade, raça/cor, situação gestacional, evolução, classificação final e série temporal. Esses recortes permitem reconhecer grupos mais registrados, lacunas de preenchimento, possíveis desigualdades de exposição e padrões que merecem investigação complementar."),
          p("As perspectivas futuras incluem a integração de modelos de machine learning interpretável para aprofundar a análise dos dados, identificar combinações de fatores associadas a maior risco, apoiar a detecção de padrões temporais e espaciais e produzir explicações compreensíveis para profissionais de saúde, gestores e pesquisadores. A proposta é que o painel evolua de um ambiente descritivo para uma ferramenta de inteligência epidemiológica, mantendo transparência metodológica, rastreabilidade dos dados e utilidade prática para a tomada de decisão.")
        ),
        div(class = "landing-section",
          h4("Sugestões"),
          p("Estamos abertos a sugestões, críticas, correções e ideias para aprimorar este painel. Contribuições sobre novas visualizações, melhorias de interpretação, ajustes metodológicos ou possibilidades de uso em ensino, pesquisa e vigilância em saúde são muito bem-vindas."),
          p(
            "Para entrar em contato, envie uma mensagem para ",
            tags$a("ryan.paulo@gsuite.iff.edu.br", href = "mailto:ryan.paulo@gsuite.iff.edu.br"),
            "."
          ),
          tags$a(
            href = "mailto:ryan.paulo@gsuite.iff.edu.br?subject=Sugest%C3%A3o%20para%20o%20Painel%20de%20Arboviroses",
            onclick = "window.location.href='mailto:ryan.paulo@gsuite.iff.edu.br?subject=Sugest%C3%A3o%20para%20o%20Painel%20de%20Arboviroses'; return false;",
            target = "_blank",
            class = "contact-button",
            tags$i(class = "fa fa-envelope"),
            "Enviar sugestão"
          )
        )
      ),
      # CHIKUNGUNYA
      tabItem(
        tabName = "chik",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "🦟")),
          span("CHIKUNGUNYA")
        ),
        fluidRow(
          column(3, selectInput("chik_ano", "PERÍODO", choices = c("Todos", sort(unique(dados$Chikungunya$Ano))), selected = "Todos")),
          column(9, uiOutput("chik_periodo"))
        ),
        fluidRow(
          column(3, uiOutput("chik_card_cura")),
          column(3, uiOutput("chik_card_obitos")),
          column(3, uiOutput("chik_card_descartados")),
          column(3, uiOutput("chik_card_ignorados"))
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("chik_card_confirmados")),
          column(6, uiOutput("chik_card_inconclusivos"))
        ),
        div(class = "context-box",
          tags$strong("Leitura guiada: "),
          "observe a evolução temporal e os perfis por sexo, idade, gestação, escolaridade e raça/cor. Campos ignorados/brancos indicam limites de completude e devem moderar inferências causais."
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("chik_card_incidencia")),
          column(6, uiOutput("chik_card_populacao"))
        ),
        nota_metodologica_doenca(
          "Chikungunya",
          "SINAN/SVS em tabela agregada do projeto.",
          "Casos classificados como confirmados na base agregada disponivel no painel.",
          "Dados anuais agregados; nao ha granularidade mensal/semanal nesta aba. Notificacoes podem sofrer subregistro, atraso de digitacao e incompletude de campos."
        ),
        dashboard_qualidade_dados("chik_qualidade"),
        div(class = "graph-box",
          div(class = "graph-title", "Incidência anual por 100 mil habitantes"),
          plotlyOutput("chik_incidencia", height = "280px"),
          botao_download_grafico("chik_download_incidencia"),
          div(class="fig-caption","O que mostra: casos confirmados de chikungunya padronizados pela população estimada de Campos dos Goytacazes. Como interpretar: compare anos com maior carga relativa. Atenção metodológica: a taxa usa casos notificados e população SIDRA/IBGE.")
        ),
        painel_downloads("chik"),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Raça/Cor"),
                 plotlyOutput("chik_etnia", height = "280px"),
                 botao_download_grafico("chik_download_etnia")
              ,div(class="fig-caption","Figura 1. Distribuição proporcional dos casos notificados de chikungunya segundo raça/cor autodeclarada. Os segmentos refletem a proporção relativa de cada categoria em relação ao total de casos com raça informada. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Sexo e Ano"),
                 plotlyOutput("chik_sexo", height = "280px"),
                 botao_download_grafico("chik_download_sexo")
              ,div(class="fig-caption","Figura 2. Número absoluto de casos notificados de chikungunya por sexo (masculino, feminino e ignorado/branco) e ano de notificação. Permite identificar a predominância do sexo feminino ao longo da série. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
               div(class = "graph-title", "Evolução Temporal dos Casos"),
               plotlyOutput("chik_serie", height = "280px"),
               botao_download_grafico("chik_download_serie")
              ,div(class="fig-caption","Figura 3. Série histórica anual de casos confirmados de chikungunya. O pico de 2024 (n=1.460) reflete o maior surto do período analisado. Período: 2020–2025. Fonte: SINAN/SVS.")
          )
        ),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
               div(class = "graph-title", "Situação Gestacional"),
               plotlyOutput("chik_gestacao", height = "320px"),
               botao_download_grafico("chik_download_gestacao")
              ,div(class="fig-caption","Figura 4. Casos de chikungunya segundo situação gestacional: 1º, 2º e 3º trimestres, não gestante, não se aplica (sexo masculino/crianças), idade gestacional ignorada e campo ignorado/branco. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
            div(class = "graph-title", "Distribuição por Faixa Etária"),
            plotlyOutput("chik_faixa_etaria", height = "320px"),
            botao_download_grafico("chik_download_faixa")
              ,div(class="fig-caption","Figura 5. Casos confirmados de chikungunya distribuídos por faixa etária (em anos: <1, 1–4, 5–9, 10–14, 15–19, 20–39, 40–59, 60–64, 65–69, 70–79, 80+). Adultos de 40–59 anos concentram a maior carga. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
              div(class = "graph-title", "Distribuição por Escolaridade"),
              plotlyOutput("chik_escolaridade", height = "320px"),
              botao_download_grafico("chik_download_escolaridade")
              ,div(class="fig-caption","Figura 6. Casos de chikungunya segundo nível de escolaridade do paciente. A elevada frequência de ignorado/branco reflete sub-registro e limita interpretações causais. Período: 2020–2025. Fonte: SINAN/SVS.")
          )
        )
      ),
      
      # DENGUE
      tabItem(
        tabName = "dengue",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "🦟")),
          span("DENGUE")
        ),
        fluidRow(
          column(3, selectInput("dengue_ano", "PERÍODO", choices = c("Todos", sort(unique(dados$Dengue$Ano))), selected = "Todos")),
          column(9, uiOutput("dengue_periodo"))
        ),
        fluidRow(
          column(3, uiOutput("dengue_card_cura")),
          column(3, uiOutput("dengue_card_obitos")),
          column(3, uiOutput("dengue_card_descartados")),
          column(3, uiOutput("dengue_card_ignorados"))
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("dengue_card_confirmados")),
          column(6, uiOutput("dengue_card_inconclusivos"))
        ),
        texto_contexto_dengue,
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("dengue_card_incidencia")),
          column(6, uiOutput("dengue_card_populacao"))
        ),
        nota_metodologica_doenca(
          "Dengue",
          "SINAN-DENGUE/SVS baixado pelo pacote microdatasus; populacao IBGE/SIDRA para incidencia.",
          "Classificacao final compativel com dengue ou febre hemorragica, excluindo descartados e registros classificados como chikungunya.",
          "Dados notificados nao equivalem a todos os casos ocorridos; ha risco de subnotificacao, atraso de encerramento, mudancas de criterio e divergencias de nomes de bairros."
        ),
        dashboard_qualidade_dados("dengue_qualidade"),
        div(class = "graph-box",
          div(class = "graph-title", "Incidência anual por 100 mil habitantes"),
          plotlyOutput("dengue_incidencia", height = "280px"),
          botao_download_grafico("dengue_download_incidencia"),
          div(class="fig-caption","O que mostra: casos confirmados de dengue padronizados pela população estimada de Campos dos Goytacazes. Como interpretar: compare anos com maior carga relativa. Atenção metodológica: a taxa usa casos notificados e população SIDRA/IBGE.")
        ),
        painel_downloads("dengue"),
        painel_temporal_bruto("dengue"),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Raça/Cor"),
                 plotlyOutput("dengue_etnia", height = "280px"),
                 botao_download_grafico("dengue_download_etnia")
              ,div(class="fig-caption","Figura 1. Distribuição proporcional dos casos notificados de dengue segundo raça/cor autodeclarada, agregada automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Sexo e Ano"),
                 plotlyOutput("dengue_sexo", height = "280px"),
                 botao_download_grafico("dengue_download_sexo")
              ,div(class="fig-caption","Figura 2. Número absoluto de casos notificados de dengue por sexo e ano. Os valores são calculados a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
               div(class = "graph-title", "Evolução Temporal dos Casos"),
               plotlyOutput("dengue_serie", height = "280px"),
               botao_download_grafico("dengue_download_serie")
              ,div(class="fig-caption","Figura 3. Série histórica anual de casos confirmados de dengue, calculada automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020–2025. Fonte: SINAN/SVS.")
          )
        ),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
               div(class = "graph-title", "Situação Gestacional"),
               plotlyOutput("dengue_gestacao", height = "320px"),
               botao_download_grafico("dengue_download_gestacao")
              ,div(class="fig-caption","Figura 4. Casos de dengue segundo situação gestacional (1º, 2º e 3º trimestres, não gestante, não se aplica e dados ignorados/brancos). Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
            div(class = "graph-title", "Distribuição por Faixa Etária"),
            plotlyOutput("dengue_faixa_etaria", height = "320px"),
            botao_download_grafico("dengue_download_faixa")
              ,div(class="fig-caption","Figura 5. Casos confirmados de dengue por faixa etária, agregados automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
              div(class = "graph-title", "Distribuição por Escolaridade"),
              plotlyOutput("dengue_escolaridade", height = "320px"),
              botao_download_grafico("dengue_download_escolaridade")
              ,div(class="fig-caption","Figura 6. Casos de dengue por nível de escolaridade, agregados automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020–2025. Fonte: SINAN/SVS.")
          )
        ),
        div(class = "landing-section",
          h4("Mapa de dengue por bairro"),
          p("Distribuição espacial dos registros de dengue por bairro no período selecionado, calculada a partir das planilhas locais e unida à malha de bairros do geobr/IBGE. A escala de cores representa o número de notificações."),
          uiOutput("dengue_mapa_bairros"),
          painel_correspondencia_bairros()
        )
      ),
      
      # ZIKA
      tabItem(
        tabName = "zika",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "🦟")),
          span("ZIKA")
        ),
        fluidRow(
          column(3, selectInput("zika_ano", "PERÍODO", choices = c("Todos", sort(unique(dados$Zika$Ano))), selected = "Todos")),
          column(9, uiOutput("zika_periodo"))
        ),
        fluidRow(
          column(3, uiOutput("zika_card_cura")),
          column(3, uiOutput("zika_card_obitos")),
          column(3, uiOutput("zika_card_descartados")),
          column(3, uiOutput("zika_card_ignorados"))
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("zika_card_confirmados")),
          column(6, uiOutput("zika_card_inconclusivos"))
        ),
        div(class = "context-box",
          tags$strong("Leitura guiada: "),
          "interprete a distribuição de Zika considerando vigilância em gestantes, investigação clínica e alta sensibilidade a sub-registro. As visualizações ajudam a localizar padrões e lacunas de preenchimento."
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("zika_card_incidencia")),
          column(6, uiOutput("zika_card_populacao"))
        ),
        nota_metodologica_doenca(
          "Zika",
          "SINAN-ZIKA/SVS baixado pelo pacote microdatasus; populacao IBGE/SIDRA para incidencia.",
          "Classificacao final compativel com confirmacao de Zika apos processamento do SINAN-ZIKA, excluindo descartados e inconclusivos.",
          "Zika e sensivel a subregistro, investigacao em gestantes, mudancas de vigilancia e baixa confirmacao laboratorial; interpretar perfis sociais junto da incompletude dos campos."
        ),
        dashboard_qualidade_dados("zika_qualidade"),
        div(class = "graph-box",
          div(class = "graph-title", "Incidência anual por 100 mil habitantes"),
          plotlyOutput("zika_incidencia", height = "280px"),
          botao_download_grafico("zika_download_incidencia"),
          div(class="fig-caption","O que mostra: casos confirmados de Zika vírus padronizados pela população estimada de Campos dos Goytacazes. Como interpretar: compare anos com maior carga relativa. Atenção metodológica: a taxa usa casos notificados e população SIDRA/IBGE.")
        ),
        painel_downloads("zika"),
        painel_temporal_bruto("zika"),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Raça/Cor"),
                 plotlyOutput("zika_etnia", height = "280px"),
                 botao_download_grafico("zika_download_etnia")
              ,div(class="fig-caption","Figura 1. Distribuição proporcional dos casos notificados de Zika vírus segundo raça/cor autodeclarada. O alto percentual de ignorado/branco compromete a representatividade dos demais grupos. Período: 2021–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Sexo e Ano"),
                 plotlyOutput("zika_sexo", height = "280px"),
                 botao_download_grafico("zika_download_sexo")
              ,div(class="fig-caption","Figura 2. Número absoluto de casos notificados de Zika vírus por sexo e ano. O predomínio feminino é consistente em todo o período, possivelmente relacionado à maior busca por assistência durante a gestação. Período: 2021–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
               div(class = "graph-title", "Evolução Temporal dos Casos"),
               plotlyOutput("zika_serie", height = "280px"),
               botao_download_grafico("zika_download_serie")
              ,div(class="fig-caption","Figura 3. Série histórica anual de casos confirmados de Zika vírus. O período mostra tendência de queda após o pico epidêmico; os anos de 2023 e 2024 registraram leve elevação. Período: 2021–2025. Fonte: SINAN/SVS.")
          )
        ),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
               div(class = "graph-title", "Situação Gestacional"),
               plotlyOutput("zika_gestacao", height = "320px"),
               botao_download_grafico("zika_download_gestacao")
              ,div(class="fig-caption","Figura 4. Casos de Zika vírus segundo situação gestacional. Destaque para o volume de idade gestacional não informada em 2022 (n = 26), sugerindo possível sub-registro do trimestre. Período: 2021–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
            div(class = "graph-title", "Distribuição por Faixa Etária"),
            plotlyOutput("zika_faixa_etaria", height = "320px"),
            botao_download_grafico("zika_download_faixa")
              ,div(class="fig-caption","Figura 5. Casos confirmados de Zika vírus por faixa etária. Adultos de 20–59 anos concentram a maior parte dos casos; a presença em crianças (≤1 ano) pode indicar transmissão vertical. Período: 2021–2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
              div(class = "graph-title", "Distribuição por Escolaridade"),
              plotlyOutput("zika_escolaridade", height = "320px"),
              botao_download_grafico("zika_download_escolaridade")
              ,div(class="fig-caption","Figura 6. Casos de Zika vírus por nível de escolaridade. O ensino médio completo e o grupo ignorado/branco predominam; interpretações sobre vulnerabilidade socioeconômica devem considerar o elevado sub-registro. Período: 2021–2025. Fonte: SINAN/SVS.")
          )
        )
      )
      ,
      tabItem(
        tabName = "tutorial",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "?")),
          span("TUTORIAL E INTERPRETAÇÃO")
        ),
        div(class = "landing-section",
          h3("Como utilizar o app"),
          p("Este painel foi criado para apoiar a leitura epidemiológica das arboviroses em Campos dos Goytacazes. Ele reúne indicadores descritivos de Chikungunya, Dengue e Zika, permitindo observar a magnitude dos registros, o perfil dos casos, a completude das notificações e a distribuição espacial da dengue por bairro."),
          tags$ol(
            tags$li("Escolha a doença no menu lateral: Chikungunya, Dengue ou Zika."),
            tags$li("Use o filtro de período para analisar todos os anos juntos ou focar em um ano específico."),
            tags$li("Comece pelos cartões-resumo para entender rapidamente curas, óbitos, casos confirmados, descartados, inconclusivos e campos ignorados/brancos."),
            tags$li("Compare os gráficos de série temporal, sexo, faixa etária, gestação, raça/cor e escolaridade para reconhecer padrões e diferenças entre os grupos."),
            tags$li("Na aba de Dengue, explore também o mapa por bairros e o ranking dos bairros com maior número de registros."),
            tags$li("Use os botões abaixo de cada gráfico para baixar figuras em alta resolução e use o painel de downloads para exportar tabelas e plots em formatos de relatório.")
          )
        ),
        fluidRow(
          column(6, div(class = "landing-section",
            h4("Como interpretar os gráficos"),
            p("A série temporal mostra como os casos confirmados variam ao longo dos anos e ajuda a identificar picos, quedas e possíveis períodos de maior transmissão. Os gráficos por sexo e faixa etária ajudam a observar quais grupos aparecem com maior frequência nos registros."),
            p("Os recortes por raça/cor, escolaridade e situação gestacional devem ser lidos com cuidado: eles podem sugerir desigualdades, padrões de exposição, acesso ao serviço de saúde e diferenças de preenchimento, mas não provam causalidade isoladamente."),
            p("Campos ignorados/brancos são indicadores importantes de qualidade da informação. Quando aparecem em grande volume, eles reduzem a confiança de interpretações sobre o perfil social ou biológico dos casos.")
          )),
          column(6, div(class = "landing-section",
            h4("Para que o site serve"),
            p("O aplicativo serve como ferramenta de apoio para ensino, pesquisa, vigilância epidemiológica e comunicação científica. Ele organiza dados brutos e agregados em visualizações que facilitam a comparação entre doenças, anos e perfis populacionais."),
            p("Na prática, o painel pode ajudar a construir relatórios, selecionar figuras para apresentações, levantar hipóteses de investigação, discutir qualidade de preenchimento das fichas e orientar perguntas para análises futuras mais profundas.")
          ))
        ),
        fluidRow(
          column(6, div(class = "landing-section",
            h4("Exemplos de inferência epidemiológica"),
            tags$ul(
              tags$li("Aumento em crianças e adolescentes pode sugerir mudanças de exposição domiciliar, escolar ou comunitária."),
              tags$li("Diferenças por sexo podem refletir exposição, comportamento de busca por cuidado ou vigilância em gestantes."),
              tags$li("Maior proporção de idosos pode levantar hipóteses sobre gravidade e risco de evolução desfavorável."),
              tags$li("Sub-registro de escolaridade ou raça/cor limita inferências sociais e deve ser relatado.")
            )
          )),
          column(6, div(class = "landing-section",
            h4("Boas práticas de uso"),
            tags$ul(
              tags$li("Compare categorias sempre considerando o período selecionado no filtro."),
              tags$li("Evite concluir que um grupo tem maior risco apenas porque aparece mais no gráfico; diferenças de população, acesso e registro também influenciam."),
              tags$li("Ao usar uma figura em relatório, cite o período, a doença, a fonte SINAN/SVS e o recorte analisado."),
              tags$li("Quando houver muitos registros ignorados/brancos, destaque essa limitação junto da interpretação.")
            )
          ))
        ),
        div(class = "landing-section",
          h4("Metodologia"),
          tags$ul(
            tags$li("Chikungunya: dados provenientes do SINAN, organizados em tabela agregada para o periodo analisado."),
            tags$li("Dengue e Zika: dados baixados e processados com o pacote microdatasus, filtrados para Campos dos Goytacazes e agregados por ano e variáveis epidemiológicas."),
            tags$li("Mapa de dengue: utiliza planilha fornecida pela Subsecretaria de Vigilância Epidemiológica de Campos e malha de bairros do geobr/IBGE."),
            tags$li("População: estimativas municipais consultadas via pacote sidrar na tabela 6579 do IBGE/SIDRA, variável 9324, com cache local e registro de auditoria."),
            tags$li("Indicadores: casos absolutos, incidência por 100 mil habitantes e percentuais de campos ignorados, brancos ou ausentes.")
          )
        ),
        div(class = "landing-section",
          h4("Limitações metodológicas"),
          tags$ul(
            tags$li("Os dados representam notificações registradas, não necessariamente todos os casos reais ocorridos no município."),
            tags$li("Subnotificação, atraso de digitação, mudanças de definição de caso e diferenças de acesso aos serviços podem alterar a leitura temporal e territorial."),
            tags$li("Campos ignorados, brancos ou ausentes reduzem a precisão das análises por sexo, idade, raça/cor, escolaridade e gestação."),
            tags$li("Associações observadas nos gráficos são descritivas e não estabelecem causalidade."),
            tags$li("A incidência por 100 mil usa população estimada; por isso deve ser lida como padronização aproximada para comparação entre anos.")
          )
        )
      ),
      tabItem(
        tabName = "metodos",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "i")),
          span("METODOS")
        ),
        div(class = "landing-section",
          h3("Metodos, fontes e reprodutibilidade"),
          p("Esta aba documenta as fontes, os filtros aplicados, os criterios de agregacao e as limitacoes usadas no painel."),
          fluidRow(
            column(6, div(class = "method-note",
              h4("Recorte analitico"),
              tags$dl(
                tags$dt("Municipio"),
                tags$dd(APP_MUNICIPIO),
                tags$dt("Periodo"),
                tags$dd(APP_PERIODO_PADRAO),
                tags$dt("Unidade de analise"),
                tags$dd(APP_UNIDADE_ANALISE),
                tags$dt("Filtros aplicados"),
                tags$dd("Dengue e Zika: UF RJ, municipio 330100, anos 2020-2025, registros processados pelo microdatasus. Chikungunya: tabela agregada do projeto.")
              )
            )),
            column(6, div(class = "method-note",
              h4("Pacotes e atualizacao"),
              tags$dl(
                tags$dt("R"),
                tags$dd(R.version.string),
                tags$dt("Pacotes principais"),
                tags$dd(paste(
                  "shiny", as.character(packageVersion("shiny")),
                  "| shinydashboard", as.character(packageVersion("shinydashboard")),
                  "| microdatasus", as.character(packageVersion("microdatasus")),
                  "| plotly", as.character(packageVersion("plotly")),
                  "| sf", as.character(packageVersion("sf")),
                  "| geobr", as.character(packageVersion("geobr"))
                )),
                tags$dt("Data de atualizacao"),
                tags$dd(as.character(APP_DATA_ATUALIZACAO))
              )
            ))
          )
        ),
        div(class = "landing-section",
          h4("Criterios de confirmacao e agregacao"),
          tags$ul(
            tags$li("Dengue: registros do SINAN-DENGUE classificados como dengue ou febre hemorragica, excluindo descartados e registros classificados como chikungunya."),
            tags$li("Zika: registros do SINAN-ZIKA com classificacao compativel com confirmacao apos processamento pelo microdatasus, excluindo descartados e inconclusivos."),
            tags$li("Chikungunya: casos confirmados conforme tabela agregada do projeto."),
            tags$li("Incidencia: casos confirmados divididos pela populacao estimada do IBGE/SIDRA e multiplicados por 100 mil."),
            tags$li("Qualidade dos dados: percentual de ignorado, branco ou ausente por sexo, idade, raca/cor, escolaridade, gestacao e classificacao final.")
          )
        ),
        div(class = "landing-section",
          h4("Series temporais e mapa"),
          tags$ul(
            tags$li("Series mensal e semanal: calculadas a partir da data de notificacao dos registros brutos de Dengue e Zika, quando disponiveis em cache."),
            tags$li("Mapa de bairros: une registros locais de dengue por NM_BAIRRO com a malha geobr/IBGE de bairros 2010 por chave textual normalizada."),
            tags$li("Correspondencia de bairros: bairros nao mapeados devem ser revisados manualmente, pois podem representar grafias alternativas, localidades sem poligono ou nomes nao presentes na malha.")
          )
        ),
        div(class = "landing-section",
          h4("Limitacoes"),
          tags$ul(
            tags$li("Os dados sao notificacoes registradas e podem subestimar a ocorrencia real."),
            tags$li("Atrasos de digitacao, encerramento e investigacao podem alterar anos recentes, especialmente 2025."),
            tags$li("Campos ignorados/brancos reduzem a interpretabilidade dos perfis sociodemograficos."),
            tags$li("As visualizacoes sao descritivas e nao estabelecem causalidade."),
            tags$li("A malha de bairros de 2010 pode nao refletir todos os limites ou nomes territoriais usados nos registros atuais.")
          )
        )
      ),
      tabItem(
        tabName = "equipe",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", span(class = "mosquito-emoji", "i")),
          span("EQUIPE E DESENVOLVEDORES")
        ),
        div(class = "landing-section",
          h4("Equipe do projeto"),
          p("Esta empreitada reúne bacharelandos em Enfermagem pelo Instituto Federal Fluminense, Campus Campos Guarus, sob orientação da Profa. Dra. Karla Rangel Ribeiro. A proposta integra formação em saúde, vigilância epidemiológica, ciência de dados e comunicação científica aplicada às arboviroses."),
          div(class = "team-grid",
            perfil_equipe(
              "Ryan de Paulo Santos",
              "Bacharelando em Enfermagem",
              "Bacharelando em Enfermagem pelo IFF Guarus, integrante da equipe de desenvolvimento, organização dos dados e análise das visualizações do painel.",
              "http://lattes.cnpq.br/7503796642571978",
              "RS"
            ),
            perfil_equipe(
              "Brenda Velasco Moreira",
              "Bacharelanda em Enfermagem",
              "Bacharelanda em Enfermagem pelo IFF Guarus, integrante da equipe do projeto e colaboradora na leitura epidemiológica e organização científica dos dados.",
              "http://lattes.cnpq.br/2823380252102590",
              "BM"
            ),
            perfil_equipe(
              "Mirella Guimarães Lourenço de Souza",
              "Bacharelanda em Enfermagem",
              "Bacharelanda em Enfermagem pelo IFF Guarus, integrante da equipe do projeto e colaboradora na interpretação dos indicadores de arboviroses.",
              "http://lattes.cnpq.br/1000563712788651",
              "MS"
            ),
            perfil_equipe(
              "Marcelly Rangel de Mello",
              "Bacharelanda em Enfermagem",
              "Bacharelanda em Enfermagem pelo IFF Guarus, integrante da equipe do projeto e colaboradora na análise, revisão e comunicação dos resultados.",
              "http://lattes.cnpq.br/9803145019193571",
              "MM"
            ),
            perfil_equipe(
              "Karla Rangel Ribeiro",
              "Orientadora",
              "Docente do IFF Guarus e orientadora desta empreitada, articulando vigilância em saúde, pesquisa aplicada, formação em enfermagem e análise de dados.",
              "http://lattes.cnpq.br/6725528158895476",
              "KR"
            )
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  observeEvent(input$sidebarItemExpanded, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = input$sidebarItemExpanded,
      acao = "navegacao",
      detalhes = paste("Navegou para aba:", input$sidebarItemExpanded)
    ))
  })
  
  ano_selecionado <- function(input_id) {
    valor <- input[[input_id]]
    if(is.null(valor)) "Todos" else valor
  }
  
  observeEvent(input$chik_ano, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "chikungunya",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", input$chik_ano)
    ))
  })
  observeEvent(input$dengue_ano, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "dengue",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", input$dengue_ano)
    ))
  })
  observeEvent(input$zika_ano, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "zika",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", input$zika_ano)
    ))
  })
  
  chik_filtrado <- reactive({ filter_year(dados$Chikungunya, ano_selecionado("chik_ano")) })
  dengue_filtrado <- reactive({ filter_year(dados$Dengue, ano_selecionado("dengue_ano")) })
  zika_filtrado <- reactive({ filter_year(dados$Zika, ano_selecionado("zika_ano")) })
  dengue_bairros_filtrado <- reactive({
    df <- dengue_bairros
    if(nrow(df) == 0) return(df)
    ano <- ano_selecionado("dengue_ano")
    if(ano != "Todos") df <- df[df$Ano == as.numeric(ano), , drop = FALSE]
    df %>%
      group_by(NM_BAIRRO) %>%
      summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Casos), NM_BAIRRO)
  })

  registrar_downloads <- function(prefixo, df_reativo, nome_doenca) {
    output[[paste0(prefixo, "_tabela")]] <- renderDT({
      visualizacao <- input[[paste0(prefixo, "_vis")]]
      if(is.null(visualizacao)) visualizacao <- "serie"
      datatable(
        tabela_visualizacao(df_reativo(), visualizacao),
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })
    
    output[[paste0(prefixo, "_download_table")]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", input[[paste0(prefixo, "_vis")]], "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        visualizacao <- input[[paste0(prefixo, "_vis")]]
        if(is.null(visualizacao)) visualizacao <- "serie"
        write.csv(tabela_visualizacao(df_reativo(), visualizacao), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    output[[paste0(prefixo, "_download_analitica")]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_tabela_analitica_", Sys.Date(), ".csv")
      },
      content = function(file) {
        ano <- ano_selecionado(paste0(prefixo, "_ano"))
        periodo_label <- if(ano == "Todos") "Todos" else ano
        write.csv(
          tabela_analitica_doenca(df_reativo(), tools::toTitleCase(nome_doenca), periodo_label),
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
      }
    )
    output[[paste0(prefixo, "_download_plot")]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", input[[paste0(prefixo, "_vis")]], "_", Sys.Date(), ".", input[[paste0(prefixo, "_fmt")]])
      },
      content = function(file) {
        visualizacao <- input[[paste0(prefixo, "_vis")]]
        formato <- input[[paste0(prefixo, "_fmt")]]
        if(is.null(visualizacao)) visualizacao <- "serie"
        if(is.null(formato)) formato <- "pdf"
        df_plot <- df_reativo()
        periodo_label <- if(length(unique(df_plot$Ano)) == 1) paste("Ano:", unique(df_plot$Ano)) else paste0("Período: ", min(df_plot$Ano), "-", max(df_plot$Ano))
        p <- grafico_publicacao(df_plot, visualizacao, paste(nome_doenca, "-", names(opcoes_visualizacao)[opcoes_visualizacao == visualizacao]), nome_doenca, periodo_label)
        ggsave(file, plot = p, device = formato, width = 9, height = 6, dpi = 600, units = "in")
      }
    )
  }
  
  registrar_downloads("chik", chik_filtrado, "chikungunya")
  registrar_downloads("dengue", dengue_filtrado, "dengue")
  registrar_downloads("zika", zika_filtrado, "zika")

  registrar_temporal_bruto <- function(prefixo, temporal_df, nome_doenca) {
    temporal_filtrado <- reactive({
      df <- temporal_df
      if(is.null(df) || nrow(df) == 0) return(df)
      intervalo <- input[[paste0(prefixo, "_temporal_intervalo")]]
      if(is.null(intervalo)) intervalo <- "Mensal"
      ano <- ano_selecionado(paste0(prefixo, "_ano"))
      df <- df[df$Intervalo == intervalo, , drop = FALSE]
      if(ano != "Todos") df <- df[df$Ano == as.numeric(ano), , drop = FALSE]
      df %>% arrange(Data)
    })
    
    output[[paste0(prefixo, "_temporal_plot")]] <- renderPlotly({
      df <- temporal_filtrado()
      if(is.null(df) || nrow(df) == 0) {
        return(plot_ly() %>% layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "Serie temporal bruta indisponivel em cache.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE
          ))
        ))
      }
      plot_ly(
        df,
        x = ~Data,
        y = ~Casos,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = if(nome_doenca == "Dengue") "#C73E1D" else "#A23B72", width = 2),
        marker = list(size = 6),
        text = ~paste0(Periodo, "<br>Casos confirmados: ", Casos),
        hoverinfo = "text"
      ) %>%
        layout(
          title = paste(nome_doenca, "- casos confirmados por intervalo"),
          xaxis = list(title = ""),
          yaxis = list(title = "Casos confirmados"),
          margin = list(l = 55, r = 25, t = 55, b = 45)
        )
    })
    
    output[[paste0(prefixo, "_temporal_tabela")]] <- renderDT({
      datatable(temporal_filtrado(), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })
    
    output[[paste0(prefixo, "_download_temporal")]] <- downloadHandler(
      filename = function() {
        paste0(tolower(nome_doenca), "_serie_temporal_bruta_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(temporal_filtrado(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  }

  registrar_temporal_bruto("dengue", dengue_temporal, "Dengue")
  registrar_temporal_bruto("zika", zika_temporal, "Zika")

  salvar_png_alta_resolucao <- function(plot, file, width = 9, height = 6) {
    ggsave(file, plot = plot, device = "png", width = width, height = height, dpi = 600, units = "in", bg = "white")
  }

  registrar_download_grafico <- function(output_id, df_func, nome_doenca, visualizacao, titulo, width = 9, height = 6) {
    output[[output_id]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", visualizacao, "_alta_resolucao_", Sys.Date(), ".png")
      },
      content = function(file) {
        df_plot <- df_func()
        periodo_label <- if(length(unique(df_plot$Ano)) == 1) paste("Ano:", unique(df_plot$Ano)) else paste0("Período: ", min(df_plot$Ano), "-", max(df_plot$Ano))
        p <- grafico_publicacao(df_plot, visualizacao, titulo, nome_doenca, periodo_label)
        salvar_png_alta_resolucao(p, file, width = width, height = height)
      }
    )
  }

  registrar_downloads_graficos_doenca <- function(prefixo, df_filtrado, df_serie, nome_doenca) {
    registrar_download_grafico(
      paste0(prefixo, "_download_etnia"),
      df_filtrado,
      nome_doenca,
      "etnia",
      paste(nome_doenca, "- Raça/cor")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_sexo"),
      df_filtrado,
      nome_doenca,
      "sexo",
      paste(nome_doenca, "- Sexo por ano")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_serie"),
      df_serie,
      nome_doenca,
      "serie",
      paste(nome_doenca, "- Série temporal")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_incidencia"),
      df_filtrado,
      nome_doenca,
      "incidencia",
      paste(nome_doenca, "- Incidência por 100 mil")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_gestacao"),
      df_filtrado,
      nome_doenca,
      "gestacao",
      paste(nome_doenca, "- Situação gestacional")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_faixa"),
      df_filtrado,
      nome_doenca,
      "faixa",
      paste(nome_doenca, "- Faixa etaria")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_escolaridade"),
      df_filtrado,
      nome_doenca,
      "escolaridade",
      paste(nome_doenca, "- Escolaridade")
    )
  }

  registrar_downloads_graficos_doenca("chik", chik_filtrado, function() dados$Chikungunya, "chikungunya")
  registrar_downloads_graficos_doenca("dengue", dengue_filtrado, function() dados$Dengue, "dengue")
  registrar_downloads_graficos_doenca("zika", zika_filtrado, function() dados$Zika, "zika")

  output$dengue_download_mapa_bairros <- downloadHandler(
    filename = function() {
      paste0("dengue_mapa_bairros_alta_resolucao_", Sys.Date(), ".png")
    },
    content = function(file) {
      ano <- ano_selecionado("dengue_ano")
      periodo_label <- if(ano == "Todos") "Período: 2020-2025" else paste("Ano:", ano)
      p <- grafico_mapa_bairros_geobr(dengue_mapa_geobr()$mapa, periodo_label)
      salvar_png_alta_resolucao(p, file, width = 10, height = 8)
    }
  )

  output$dengue_download_bairros_barra <- downloadHandler(
    filename = function() {
      paste0("dengue_bairros_barra_alta_resolucao_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- dengue_bairros_filtrado() %>%
        slice_max(Casos, n = 25, with_ties = FALSE) %>%
        arrange(Casos)

      p <- ggplot(df, aes(x = Casos, y = reorder(NM_BAIRRO, Casos))) +
        geom_col(fill = "#C73E1D") +
        geom_text(aes(label = format_number(Casos)), hjust = -0.08, size = 3.4, color = "#1e293b") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
        labs(title = "Top 25 bairros com mais registros de dengue", x = "Casos", y = "") +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#1A2535"),
          axis.text.y = element_text(size = 10, color = "#334155"),
          panel.grid.major.y = element_blank()
        )
      salvar_png_alta_resolucao(p, file, width = 10, height = 8)
    }
  )

  output$dengue_mapa_bairros <- renderUI({
    ano <- ano_selecionado("dengue_ano")
    periodo_label <- if(ano == "Todos") "Período: 2020-2025" else paste("Ano:", ano)
    
    if(nrow(dengue_bairros_filtrado()) == 0) {
      return(div(class = "context-box", "Não há registros com bairro informado nas planilhas locais para o período selecionado."))
    }
    
    tagList(
      leafletOutput("dengue_mapa_bairros_plot", height = "620px"),
      botao_download_grafico("dengue_download_mapa_bairros"),
      div(class = "context-box",
        tags$strong("Nota metodológica: "),
        "o mapa usa a malha de bairros de 2010 disponibilizada pelo geobr/IBGE. A vinculacao com as planilhas e feita por nome normalizado do bairro; divergencias de grafia podem deixar alguns bairros sem correspondencia espacial."
      ),
      plotlyOutput("dengue_bairros_barra", height = "430px"),
      botao_download_grafico("dengue_download_bairros_barra")
    )
  })

  dengue_mapa_geobr <- reactive({
    preparar_mapa_bairros_geobr(dengue_bairros_filtrado())
  })

  dengue_correspondencia_bairros_df <- reactive({
    tryCatch(
      preparar_correspondencia_bairros(dengue_bairros_filtrado()),
      error = function(e) data.frame(
        NM_BAIRRO_PLANILHA = character(),
        bairro_key = character(),
        Casos = integer(),
        NM_BAIRRO_GEOBR = character(),
        Status = character(),
        stringsAsFactors = FALSE
      )
    )
  })

  output$dengue_correspondencia_bairros <- renderDT({
    datatable(
      dengue_correspondencia_bairros_df(),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$dengue_download_correspondencia_bairros <- downloadHandler(
    filename = function() {
      paste0("dengue_correspondencia_bairros_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dengue_correspondencia_bairros_df(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dengue_mapa_bairros_plot <- renderLeaflet({
    ano <- ano_selecionado("dengue_ano")
    periodo_label <- if(ano == "Todos") "Período: 2020-2025" else paste("Ano:", ano)
    tryCatch({
      mapa_leaflet_bairros_geobr(dengue_mapa_geobr()$mapa, periodo_label)
    }, error = function(e) {
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addControl(
          html = paste(
            "<strong>Não foi possível carregar a malha de bairros.</strong><br>",
            conditionMessage(e)
          ),
          position = "topright"
        )
    })
  })

  output$dengue_bairros_barra <- renderPlotly({
    df <- dengue_bairros_filtrado() %>%
      slice_max(Casos, n = 25, with_ties = FALSE) %>%
      arrange(Casos)
    
    if(nrow(df) == 0) {
      return(plot_ly() %>% layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "Sem bairros informados para exibir.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
      ))
    }
    
    paleta <- colorRampPalette(c("#facc15", "#f97316", "#ef4444", "#b91c1c", "#7f1d1d"))(nrow(df))
    plot_ly(df) %>%
      add_bars(
        x = ~Casos,
        y = ~factor(NM_BAIRRO, levels = NM_BAIRRO),
        orientation = "h",
        marker = list(color = paleta),
        text = ~format_number(Casos),
        textposition = "outside",
        cliponaxis = FALSE,
        hovertemplate = "<b>%{y}</b><br>Casos: %{x}<extra></extra>"
      ) %>%
      layout(
        title = list(text = "Top 25 bairros com mais registros de dengue"),
        xaxis = list(title = "Casos"),
        yaxis = list(title = "", automargin = TRUE),
        showlegend = FALSE,
        margin = list(l = 175, r = 70, t = 50, b = 45)
      )
  })
  
  criar_card <- function(df_reativo, coluna, rotulo, extra_class = NULL) {
    renderUI({
      valor <- sum(df_reativo()[[coluna]], na.rm = TRUE)
      card_cls <- paste(c("custom-card", extra_class), collapse = " ")
      div(class = card_cls,
          div(class = "card-value", format_number(valor)),
          div(class = "card-label", rotulo)
      )
    })
  }

  criar_card_incidencia <- function(df_reativo) {
    renderUI({
      taxa <- incidencia_periodo(df_reativo())
      div(class = "custom-card",
          div(class = "card-value", if(is.finite(taxa)) format_number(round(taxa)) else "Indisponível"),
          div(class = "card-label", "INCIDÊNCIA/100 MIL")
      )
    })
  }

  criar_card_populacao <- function(df_reativo) {
    renderUI({
      df_pop <- adicionar_populacao(df_reativo())
      pop_media <- mean(df_pop$Populacao, na.rm = TRUE)
      div(class = "custom-card",
          div(class = "card-value", if(is.finite(pop_media)) format_number(round(pop_media)) else "Indisponível"),
          div(class = "card-label", "POPULAÇÃO MÉDIA SIDRA")
      )
    })
  }

  render_qualidade <- function(df_reativo) {
    renderUI({
      q <- qualidade_dados(df_reativo())
      div(class = "quality-grid",
        lapply(seq_len(nrow(q)), function(i) {
          div(class = "quality-card",
            div(class = "quality-label", q$Variavel[i]),
            div(class = "quality-value", format_percent(q$Percentual[i])),
            div(class = "quality-note", paste0(format_number(q$Ignorado_Branco_Ausente[i]), " de ", format_number(q$Total[i]), " registros"))
          )
        })
      )
    })
  }
  
  # Chikungunya cards (sem valor forçado)
  output$chik_card_cura <- criar_card(chik_filtrado, "Cura_evolucao", "CURAS")
  output$chik_card_obitos <- criar_card(chik_filtrado, "Obitos_Agr", "\u00d3BITOS", extra_class = "card-obitos")
  output$chik_card_descartados <- criar_card(chik_filtrado, "Descartado_casos", "DESCARTADOS")
  output$chik_card_ignorados <- criar_card(chik_filtrado, "Ign_Branco_casos", "IGN/BRANCO")
  output$chik_card_confirmados <- criar_card(chik_filtrado, "Confirmado_casos", "CONFIRMADOS")
  output$chik_card_inconclusivos <- criar_card(chik_filtrado, "Inconclusivo_casos", "INCONCLUSIVOS")
  output$chik_card_incidencia <- criar_card_incidencia(chik_filtrado)
  output$chik_card_populacao <- criar_card_populacao(chik_filtrado)
  output$chik_qualidade <- render_qualidade(chik_filtrado)
  
  # Dengue cards
  output$dengue_card_cura <- criar_card(dengue_filtrado, "Cura_evolucao", "CURAS")
  output$dengue_card_obitos <- criar_card(dengue_filtrado, "Obitos_Agr", "\u00d3BITOS", extra_class = "card-obitos")
  output$dengue_card_descartados <- criar_card(dengue_filtrado, "Descartado_casos", "DESCARTADOS")
  output$dengue_card_ignorados <- criar_card(dengue_filtrado, "Ign_Branco_casos", "IGN/BRANCO")
  output$dengue_card_confirmados <- criar_card(dengue_filtrado, "Confirmado_casos", "CONFIRMADOS")
  output$dengue_card_inconclusivos <- criar_card(dengue_filtrado, "Inconclusivo_casos", "INCONCLUSIVOS")
  output$dengue_card_incidencia <- criar_card_incidencia(dengue_filtrado)
  output$dengue_card_populacao <- criar_card_populacao(dengue_filtrado)
  output$dengue_qualidade <- render_qualidade(dengue_filtrado)
  
  # Zika cards
  output$zika_card_cura <- criar_card(zika_filtrado, "Cura_evolucao", "CURAS")
  output$zika_card_obitos <- criar_card(zika_filtrado, "Obitos_Agr", "\u00d3BITOS", extra_class = "card-obitos")
  output$zika_card_descartados <- criar_card(zika_filtrado, "Descartado_casos", "DESCARTADOS")
  output$zika_card_ignorados <- criar_card(zika_filtrado, "Ign_Branco_casos", "IGN/BRANCO")
  output$zika_card_confirmados <- criar_card(zika_filtrado, "Confirmado_casos", "CONFIRMADOS")
  output$zika_card_inconclusivos <- criar_card(zika_filtrado, "Inconclusivo_casos", "INCONCLUSIVOS")
  output$zika_card_incidencia <- criar_card_incidencia(zika_filtrado)
  output$zika_card_populacao <- criar_card_populacao(zika_filtrado)
  output$zika_qualidade <- render_qualidade(zika_filtrado)
  
  # Gráficos
  output$chik_serie <- renderPlotly({ criar_grafico_serie(dados$Chikungunya, "#2E86AB", "Chikungunya") })
  output$chik_incidencia <- renderPlotly({ criar_grafico_incidencia(chik_filtrado(), "#2E86AB", "Chikungunya") })
  output$chik_etnia <- renderPlotly({ criar_grafico_etnia(chik_filtrado()) })
  output$chik_sexo <- renderPlotly({ criar_grafico_sexo(chik_filtrado(), destacar_feminino_2024 = TRUE) })
  output$chik_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(chik_filtrado()) })
  output$chik_gestacao <- renderPlotly({ criar_grafico_gestacao(chik_filtrado()) })
  output$chik_escolaridade <- renderPlotly({ criar_grafico_escolaridade(chik_filtrado()) })
  
  output$dengue_serie <- renderPlotly({ criar_grafico_serie(dados$Dengue, "#C73E1D", "Dengue") })
  output$dengue_incidencia <- renderPlotly({ criar_grafico_incidencia(dengue_filtrado(), "#C73E1D", "Dengue") })
  output$dengue_etnia <- renderPlotly({ criar_grafico_etnia(dengue_filtrado()) })
  output$dengue_sexo <- renderPlotly({ criar_grafico_sexo(dengue_filtrado(), organizar_rotulos = TRUE) })
  output$dengue_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(dengue_filtrado()) })
  output$dengue_gestacao <- renderPlotly({ criar_grafico_gestacao(dengue_filtrado()) })
  output$dengue_escolaridade <- renderPlotly({ criar_grafico_escolaridade(dengue_filtrado()) })
  
  output$zika_serie <- renderPlotly({ criar_grafico_serie(dados$Zika, "#A23B72", "Zika") })
  output$zika_incidencia <- renderPlotly({ criar_grafico_incidencia(zika_filtrado(), "#A23B72", "Zika") })
  output$zika_etnia <- renderPlotly({ criar_grafico_etnia(zika_filtrado()) })
  output$zika_sexo <- renderPlotly({ criar_grafico_sexo(zika_filtrado()) })
  output$zika_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(zika_filtrado()) })
  output$zika_gestacao <- renderPlotly({ criar_grafico_gestacao(zika_filtrado()) })
  output$zika_escolaridade <- renderPlotly({ criar_grafico_escolaridade(zika_filtrado()) })
  
  
  
  # ---- Badge de periodo ativo ----
  render_periodo <- function(df_full, input_id) {
    renderUI({
      ano <- ano_selecionado(input_id)
      if (ano == "Todos") {
        anos <- df_full$Ano
        lbl  <- paste0("Todos os anos (", min(anos), "–", max(anos), ")")
      } else {
        lbl <- paste0("Ano selecionado: ", ano)
      }
      
      tags$span(class = "periodo-badge",
        tags$i(class = "fa fa-calendar-o"), " ", lbl
      )
    })
  }
  output$chik_periodo   <- render_periodo(dados$Chikungunya, "chik_ano")
  output$dengue_periodo <- render_periodo(dados$Dengue, "dengue_ano")
  output$zika_periodo   <- render_periodo(dados$Zika, "zika_ano")
  
  
  session$onSessionEnded(function() {
    registrar_log(LOG_SESSAO, data.frame(
      evento = "fim_sessao",
      detalhes = "Sessao encerrada"
    ))
  })
}

# EXECUTAR

shinyApp(ui, server)
