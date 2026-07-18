# ============================================================
# DADOS: Leitura de caches gerados pelo pipeline via microdatasus
# Nenhum dado hardcoded. Tudo vem de data/app_cache/*.rds
# ============================================================

# Funcao local de leitura segura (redundante com utils.R para robustez no deploy)
ler_rds_seguro_local <- function(path, nome = basename(path), quiet = FALSE) {
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
  }
  obj
}

# Chikungunya
carregar_chikungunya_microdatasus <- function(
  cache_path = file.path("data", "app_cache", "chikungunya_microdatasus_campos_v1.rds")
) {
  cache <- ler_rds_seguro_local(cache_path, "SINAN-CHIKUNGUNYA")
  if (is.null(cache) || nrow(cache) == 0) {
    stop(
      "Dados de chikungunya nao encontrados. ",
      "Execute o pipeline com ARBOVIROSES_DOWNLOAD=true:\n",
      "  Sys.setenv(ARBOVIROSES_DOWNLOAD = 'true')\n",
      "  source('scripts/update_data.R')",
      call. = FALSE
    )
  }
  cache
}

chikungunya <- tryCatch(
  carregar_chikungunya_microdatasus(),
  error = function(e) {
    warning("Falha ao carregar chikungunya: ", conditionMessage(e))
    data.frame()
  }
)

# Dengue
carregar_dengue_microdatasus <- function(
  cache_path = file.path("data", "app_cache", "dengue_microdatasus_campos_v2.rds")
) {
  cache <- ler_rds_seguro_local(cache_path, "SINAN-DENGUE")
  if (is.null(cache) || nrow(cache) == 0) {
    stop(
      "Dados de dengue nao encontrados. ",
      "Execute o pipeline com ARBOVIROSES_DOWNLOAD=true:\n",
      "  Sys.setenv(ARBOVIROSES_DOWNLOAD = 'true')\n",
      "  source('scripts/update_data.R')",
      call. = FALSE
    )
  }
  cache
}

dengue <- tryCatch(
  carregar_dengue_microdatasus(),
  error = function(e) {
    warning("Falha ao carregar dengue: ", conditionMessage(e))
    data.frame()
  }
)

# Zika
carregar_zika_microdatasus <- function(
  cache_path = file.path("data", "app_cache", "zika_microdatasus_campos_v1.rds")
) {
  cache <- ler_rds_seguro_local(cache_path, "SINAN-ZIKA")
  if (is.null(cache) || nrow(cache) == 0) {
    stop(
      "Dados de zika nao encontrados. ",
      "Execute o pipeline com ARBOVIROSES_DOWNLOAD=true:\n",
      "  Sys.setenv(ARBOVIROSES_DOWNLOAD = 'true')\n",
      "  source('scripts/update_data.R')",
      call. = FALSE
    )
  }
  cache
}

zika <- tryCatch(
  carregar_zika_microdatasus(),
  error = function(e) {
    warning("Falha ao carregar zika: ", conditionMessage(e))
    data.frame()
  }
)

# Populacao
carregar_populacao_campos_sidra <- function(
  cache_path = file.path("data", "app_cache", "populacao_campos_sidra.rds")
) {
  pop <- ler_rds_seguro_local(cache_path, "populacao")
  if (is.null(pop) || nrow(pop) == 0 || !"Populacao" %in% names(pop)) {
    warning("Cache de populacao indisponivel; incidencia por 100 mil ficara indisponivel.")
    return(data.frame(
      Ano = 2020:2025,
      Populacao = NA_real_,
      Fonte_populacao = "indisponivel",
      stringsAsFactors = FALSE
    ))
  }
  pop[order(pop$Ano), , drop = FALSE]
}

populacao_campos <- tryCatch(
  carregar_populacao_campos_sidra(),
  error = function(e) {
    warning("Falha ao carregar populacao: ", conditionMessage(e))
    data.frame(Ano = 2020:2025, Populacao = NA_real_, Fonte_populacao = "indisponivel", stringsAsFactors = FALSE)
  }
)

# Normalizacao de bairro
normalizar_bairro <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tools::toTitleCase(tolower(x))
}

# Bairros de dengue (planilhas locais, somente se disponiveis)
carregar_bairros_dengue_local <- function(
  dir_dados = "dados_sinan_campos",
  cache_path = file.path("data", "app_cache", "dengue_bairros_campos_v1.rds")
) {
  cache <- ler_rds_seguro_local(cache_path)
  if (!is.null(cache) && nrow(cache) > 0) return(cache)

  arquivos <- list.files(dir_dados, pattern = "^DENGUE[0-9]{4}\\.xlsx$", full.names = TRUE)
  if (length(arquivos) == 0) {
    warning("Nenhuma planilha DENGUEYYYY.xlsx encontrada em: ", dir_dados)
    return(data.frame(Ano = integer(), NM_BAIRRO = character(), Casos = integer()))
  }

  dados_bairros <- lapply(arquivos, function(arquivo) {
    ano <- as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(basename(arquivo))))
    df <- readxl::read_excel(arquivo, col_types = "text", na = c("", "NA", "N/A", "NULL"))

    if (!"NM_BAIRRO" %in% names(df)) {
      return(data.frame(Ano = integer(), NM_BAIRRO = character(), Casos = integer()))
    }

    df %>%
      transmute(Ano = ano, NM_BAIRRO = normalizar_bairro(NM_BAIRRO)) %>%
      filter(
        NM_BAIRRO != "",
        !grepl("ignorado|sem informa|nao informado|não informado", NM_BAIRRO, ignore.case = TRUE)
      ) %>%
      count(Ano, NM_BAIRRO, name = "Casos")
  })

  bind_rows(dados_bairros) %>% arrange(Ano, desc(Casos), NM_BAIRRO)
}

dengue_bairros <- tryCatch(
  carregar_bairros_dengue_local(),
  error = function(e) {
    warning("Falha ao carregar bairros: ", conditionMessage(e))
    data.frame(Ano = integer(), NM_BAIRRO = character(), Casos = integer())
  }
)

# ============================================================
# Funcoes de agregacao (usadas pelo pipeline)
# NOTA: normalizar_texto_sinan, primeira_coluna_existente,
# criterio_confirmado_sinan e preparar_serie_temporal_sinan
# estao definidas em utils.R (nao duplicar aqui)
# ============================================================

agregar_dengue_sinan <- function(df, anos = 2020:2025, agravo = "dengue") {
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

  df$IDADEanos <- idade_anos(df$NU_IDADE_N)

  linhas <- lapply(anos, function(ano) {
    d <- df[df$Ano == ano, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)

    classi <- as.character(d$CLASSI_FIN)
    evolucao <- as.character(d$EVOLUCAO)
    escol <- as.character(d$CS_ESCOL_N)
    gest <- as.character(d$CS_GESTANT)
    raca <- as.character(d$CS_RACA)
    sexo <- as.character(d$CS_SEXO)
    soro <- if ("SOROTIPO" %in% names(d)) as.character(d$SOROTIPO) else rep("", nrow(d))
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

    if (identical(tolower(agravo), "zika")) {
      confirmado <- (grepl("zika|confirm", classi_norm) |
        classi_norm %in% c("1", "2", "3", "laboratorio", "clinico epidemiologico")) &
        !descartado & !inconclusivo
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

# Lista de dados
dados <- list(
  "Chikungunya" = chikungunya,
  "Dengue" = dengue,
  "Zika" = zika
)

# Validar todos os dados iniciais
tryCatch({
  for (nome in names(dados)) {
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
}, error = function(e) warning("Falha na validacao inicial: ", conditionMessage(e)))
