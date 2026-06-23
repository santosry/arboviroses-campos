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

chikungunya_cache_path <- file.path("data", "app_cache", "chikungunya_microdatasus_campos_v1.rds")
if(file.exists(chikungunya_cache_path)) {
  chikungunya <- readRDS(chikungunya_cache_path)
}

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
  cache_path = file.path("data", "app_cache", "zika_microdatasus_campos_v1.rds"),
  temporal_cache_path = file.path("data", "app_cache", "zika_microdatasus_temporal_campos_v1.rds"),
  atualizar_cache = identical(tolower(Sys.getenv("ZIKA_ATUALIZAR_CACHE", "false")), "true")
) {
  cache_valido <- file.exists(cache_path)
  
  if(cache_valido) return(readRDS(cache_path))
  warning("Cache SINAN-ZIKA nao encontrado; usando tabela agregada local. Atualize os dados com scripts/update_data.R.")
  return(zika)
  
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
  cache_path = file.path("data", "app_cache", "dengue_microdatasus_campos_v2.rds"),
  temporal_cache_path = file.path("data", "app_cache", "dengue_microdatasus_temporal_campos_v1.rds"),
  atualizar_cache = identical(tolower(Sys.getenv("DENGUE_ATUALIZAR_CACHE", "false")), "true")
) {
  cache_valido <- file.exists(cache_path)
  
  if(cache_valido) return(readRDS(cache_path))
  warning("Cache SINAN-DENGUE nao encontrado; usando tabela agregada local. Atualize os dados com scripts/update_data.R.")
  return(dengue)
  
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
  cache_path = file.path("data", "app_cache", "populacao_campos_sidra.rds"),
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
  registrar_status("fallback_embutido", "Populacao carregada do fallback embutido no app; atualize o cache com scripts/update_data.R.", nrow(pop_fallback))
  return(pop_fallback)

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

dengue_temporal <- carregar_temporal_cache(file.path("data", "app_cache", "dengue_microdatasus_temporal_campos_v1.rds"))
zika_temporal <- carregar_temporal_cache(file.path("data", "app_cache", "zika_microdatasus_temporal_campos_v1.rds"))

normalizar_bairro <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tools::toTitleCase(tolower(x))
}

carregar_bairros_dengue_local <- function(
  dir_dados = "dados_sinan_campos",
  cache_path = file.path("data", "app_cache", "dengue_bairros_campos_v1.rds")
) {
  if(file.exists(cache_path)) return(readRDS(cache_path))

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
