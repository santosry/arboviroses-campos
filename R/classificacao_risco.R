# ============================================================================
# CLASSIFICACAO DE RISCO / GRAVIDADE — DENGUE E CHIKUNGUNYA
# ----------------------------------------------------------------------------
# Logica 100% baseada em sinais e sintomas + fatores de risco demograficos e
# comorbidades, seguindo fielmente a classificacao oficial do Ministerio da
# Saude do Brasil (MS). Nao ha scores numericos arbitrarios.
#
# Referencias oficiais:
#   * DENGUE:
#     - BRASIL. Ministerio da Saude. Secretaria de Vigilancia em Saude e
#       Ambiente. Dengue: diagnostico e manejo clinico – adulto e crianca.
#       6. ed. Brasilia: Ministerio da Saude, 2024.
#       -> Classificacao de risco: Grupo A (azul), Grupo B (verde),
#          Grupo C (amarelo), Grupo D (vermelho).
#   * CHIKUNGUNYA:
#     - BRASIL. Ministerio da Saude. Secretaria de Vigilancia em Saude.
#       Chikungunya: manejo clinico. Brasilia: Ministerio da Saude, 2017.
#     - BRASIL. Ministerio da Saude. Guia de Vigilancia em Saude (edicoes
#       vigentes) e documentos equivalentes para manejo de chikungunya.
#
# IMPORTANTE CLINICO: estas funcoes sao ferramenta de apoio a vigilancia e ao
# ensino. A decisao assistencial e sempre do profissional de saude.
# ============================================================================

# Cores oficiais de classificacao de risco do MS
COR_GRUPO_A <- "#1D9BF0"  # azul
COR_GRUPO_B <- "#2E8B57"  # verde
COR_GRUPO_C <- "#F1C40F"  # amarelo
COR_GRUPO_D <- "#DC2626"  # vermelho

COR_CHIK_SEM_GRAVIDADE <- "#2E86AB"          # azul (acompanhamento ambulatorial)
COR_CHIK_EXTRA_ARTICULAR <- "#F18F01"        # laranja (atencao)
COR_CHIK_GRAVE <- "#DC2626"                  # vermelho (internacao / alto risco)

# ============================================================================
# DENGUE — sinais oficiais
# ============================================================================

# Sinais de alarme oficiais (qualquer um classifica como Grupo C)
SINAIS_ALARME_DENGUE <- c(
  "dor_abdominal_intensa",   # Dor abdominal intensa (referida ou a palpacao) e continua
  "vomitos_persistentes",    # Vomitos persistentes
  "acumulo_liquidos",        # Acumulo de liquidos (ascite, derrame pleural ou pericardico)
  "hipotensao_postural",     # Hipotensao postural e/ou lipotimia
  "hepatomegalia",           # Hepatomegalia > 2 cm abaixo do rebordo costal
  "sangramento_mucosas",     # Sangramento de mucosas
  "letargia_irritabilidade", # Letargia e/ou irritabilidade
  "aumento_hematocrito"      # Aumento progressivo do hematocrito
)

# Criterios de Dengue Grave (qualquer um classifica como Grupo D)
SINAIS_GRAVIDADE_DENGUE <- c(
  "choque",                    # Choque por extravasamento plasmatico grave
  "sangramento_grave",         # Sangramento grave (hematemese, melena, metrorragia, SNC)
  "comprometimento_orgaos"     # Comprometimento grave de orgaos (figado, SNC, coracao, rins)
)

# Comorbidades / condicoes especiais consideradas no Grupo B
COMORBIDADES_DENGUE <- c(
  "hipertensao",
  "diabetes",
  "doenca_hematologica",
  "hepatopatia",
  "doenca_renal",
  "doenca_acido_peptica",
  "doenca_autoimune"
)

# ============================================================================
# DENGUE — funcao de classificacao (caso a caso)
# ============================================================================

#' Classifica um caso de dengue segundo os grupos oficiais do MS.
#'
#' @param idade_anos Idade em anos (numeric). Usada para condicoes especiais:
#'   < 2 anos ou > 65 anos elevam para Grupo B.
#' @param gestante Logical. Gestante eleva para Grupo B.
#' @param comorbidades Vetor de caracteres com comorbidades presentes. Valores
#'   validos: ver COMORBIDADES_DENGUE. Tambem aceita um unico logical.
#' @param risco_social Logical. Risco social eleva para Grupo B.
#' @param sangramento_pele Logical. Sangramento espontaneo de pele (petequias)
#'   eleva para Grupo B.
#' @param prova_laco Logical. Prova do laco positiva eleva para Grupo B.
#' @param sinais_alarme Vetor de caracteres com sinais de alarme presentes.
#'   Valores validos: ver SINAIS_ALARME_DENGUE.
#' @param sinais_gravidade Vetor de caracteres com criterios de dengue grave.
#'   Valores validos: ver SINAIS_GRAVIDADE_DENGUE.
#'
#' @return Lista com: grupo (A/B/C/D), cor, rotulo, descricao, conduta e
#'   prioridade (1..4).
#' @export
classificar_dengue <- function(
    idade_anos = NA_real_,
    gestante = FALSE,
    comorbidades = character(0),
    risco_social = FALSE,
    sangramento_pele = FALSE,
    prova_laco = FALSE,
    sinais_alarme = character(0),
    sinais_gravidade = character(0)
) {
  # Normaliza entradas para logical coerente
  gestante <- isTRUE(gestante)
  risco_social <- isTRUE(risco_social)
  sangramento_pele <- isTRUE(sangramento_pele)
  prova_laco <- isTRUE(prova_laco)

  tem_comorbidade <- if (is.logical(comorbidades)) {
    any(comorbidades, na.rm = TRUE)
  } else {
    length(intersect(tolower(as.character(comorbidades)), COMORBIDADES_DENGUE)) > 0
  }

  idade_anos <- suppressWarnings(as.numeric(idade_anos))
  idade_especial <- !is.na(idade_anos) && (idade_anos < 2 || idade_anos > 65)

  tem_alarme <- length(intersect(tolower(as.character(sinais_alarme)), SINAIS_ALARME_DENGUE)) > 0
  tem_gravidade <- length(intersect(tolower(as.character(sinais_gravidade)), SINAIS_GRAVIDADE_DENGUE)) > 0

  # Ordem de decisao segue a hierarquia do MS: primeiro dengue grave,
  # depois sinais de alarme, depois condicoes especiais/comorbidades/sangramento.
  if (tem_gravidade) {
    return(list(
      grupo = "D",
      cor = COR_GRUPO_D,
      rotulo = "Grupo D - Dengue grave",
      descricao = "Presenca de choque por extravasamento plasmatico grave, sangramento grave ou comprometimento grave de orgaos.",
      conduta = "Emergencia: internacao em leito de terapia intensiva.",
      prioridade = 4L
    ))
  }

  if (tem_alarme) {
    return(list(
      grupo = "C",
      cor = COR_GRUPO_C,
      rotulo = "Grupo C - Dengue com sinais de alarme",
      descricao = "Presenca de qualquer sinal de alarme, mesmo sem sinais de gravidade.",
      conduta = "Urgencia: internacao em leito de observacao.",
      prioridade = 3L
    ))
  }

  condicao_especial <- gestante || idade_especial || tem_comorbidade || risco_social
  if (condicao_especial || sangramento_pele || prova_laco) {
    return(list(
      grupo = "B",
      cor = COR_GRUPO_B,
      rotulo = "Grupo B - Dengue sem sinais de alarme (risco intermediario)",
      descricao = "Dengue sem sinais de alarme, mas com condicao especial, risco social, comorbidade, sangramento espontaneo de pele ou prova do laco positiva.",
      conduta = "Acompanhamento ambulatorial com observacao e hidratacao oral; reavaliacao em 48h.",
      prioridade = 2L
    ))
  }

  list(
    grupo = "A",
    cor = COR_GRUPO_A,
    rotulo = "Grupo A - Dengue sem sinais de alarme",
    descricao = "Dengue sem sinais de alarme, sem condicoes especiais, sem risco social e sem comorbidades.",
    conduta = "Acompanhamento ambulatorial com hidratacao oral e orientacoes de retorno.",
    prioridade = 1L
  )
}

# ============================================================================
# DENGUE — mapeamento SINAN-DENGUE -> classificacao (vetorizada)
# ============================================================================

# Converte codigos SINAN "1" = sim / "2" = nao / "9" = ignorado em logical.
# "ignorado" e NA sao tratados como NA (nao conta como sinal presente).
sinan_flag <- function(x) {
  x <- as.character(x)
  ifelse(x %in% "1", TRUE, ifelse(x %in% "2", FALSE, NA))
}

# Decodifica NU_IDADE_N do SINAN para idade em anos
sinan_idade_anos <- function(x) {
  x <- as.character(x)
  codigo <- substr(x, 1, 1)
  valor <- suppressWarnings(as.numeric(substr(x, 2, nchar(x))))
  dplyr::case_when(
    codigo == "1" ~ valor / 8760,  # horas
    codigo == "2" ~ valor / 365,   # dias
    codigo == "3" ~ valor / 12,    # meses
    codigo == "4" ~ valor,         # anos
    TRUE ~ NA_real_
  )
}

# Coluna opcional (retorna NA logico quando ausente)
col_flag <- function(df, coluna) {
  if (coluna %in% names(df)) sinan_flag(df[[coluna]]) else rep(NA, nrow(df))
}

#' Classifica um data frame de registros SINAN-DENGUE (microdados) nos grupos
#' oficiais A/B/C/D do MS, baseando-se exclusivamente nos sinais e sintomas
#' preenchidos e nos fatores de risco.
#'
#' Mapeamento usado (layout SINAN-DENGUE):
#'   Sinais de alarme: ALRM_ABDOM, ALRM_VOM, ALRM_LIQ, ALRM_HIPOT, ALRM_HEPAT,
#'     ALRM_SANG, ALRM_LETAR, ALRM_HEMAT.
#'   Dengue grave: choque (GRAV_PULSO/GRAV_TAQUI/GRAV_EXTRE/GRAV_ENCH/GRAV_HIPOT),
#'     sangramento grave (GRAV_HEMAT/GRAV_MELEN/GRAV_METRO/GRAV_SANG),
#'     comprometimento de orgaos (GRAV_AST/GRAV_MIOC/GRAV_CONSC/GRAV_CONV/
#'     GRAV_INSUF/GRAV_ORGAO).
#'   Grupo B: idade (<2 ou >65), gestante (CS_GESTANT 1-4), comorbidades
#'     (DIABETES/HEMATOLOG/HEPATOPAT/RENAL/HIPERTENSA/ACIDO_PEPT/AUTO_IMUNE),
#'     sangramento de pele (PETEQUIA_N) e prova do laco (LACO).
#'   NOTA: ALRM_PLAQ (plaquetas) existe no SINAN, mas nao e um dos oito sinais
#'     de alarme da classificacao vigente do MS (6a edicao) e, portanto, nao e
#'     usado para definir Grupo C.
#'
#' @param df Data frame com colunas do layout SINAN-DENGUE.
#' @return Data frame original acrescido de: grupo_risco, cor_risco,
#'   rotulo_risco, conduta_risco, prioridade_risco, n_sinais_alarme e
#'   n_sinais_gravidade.
#' @export
classificar_dengue_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  idade_anos <- if ("NU_IDADE_N" %in% names(df)) sinan_idade_anos(df$NU_IDADE_N) else rep(NA_real_, nrow(df))
  gestante <- if ("CS_GESTANT" %in% names(df)) as.character(df$CS_GESTANT) %in% c("1", "2", "3", "4") else rep(FALSE, nrow(df))

  comorb <- c("DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA", "ACIDO_PEPT", "AUTO_IMUNE")
  comorb_flags <- lapply(comorb, function(coluna) col_flag(df, coluna) %in% TRUE)
  tem_comorbidade <- Reduce(`|`, Filter(function(x) length(x) == nrow(df), comorb_flags))
  if (is.null(tem_comorbidade) || length(tem_comorbidade) == 0) {
    tem_comorbidade <- rep(FALSE, nrow(df))
  }

  sangramento_pele <- col_flag(df, "PETEQUIA_N") %in% TRUE
  prova_laco <- col_flag(df, "LACO") %in% TRUE

  alarme_cols <- c("ALRM_ABDOM", "ALRM_VOM", "ALRM_LIQ", "ALRM_HIPOT", "ALRM_HEPAT", "ALRM_SANG", "ALRM_LETAR", "ALRM_HEMAT")
  alarme_flags <- lapply(alarme_cols, function(coluna) col_flag(df, coluna) %in% TRUE)
  n_alarme <- Reduce(`+`, lapply(alarme_flags, function(x) ifelse(is.na(x), 0L, as.integer(x))))
  tem_alarme <- n_alarme > 0

  choque <- Reduce(`|`, lapply(c("GRAV_PULSO", "GRAV_TAQUI", "GRAV_EXTRE", "GRAV_ENCH", "GRAV_HIPOT"), function(coluna) col_flag(df, coluna) %in% TRUE))
  sangramento_grave <- Reduce(`|`, lapply(c("GRAV_HEMAT", "GRAV_MELEN", "GRAV_METRO", "GRAV_SANG"), function(coluna) col_flag(df, coluna) %in% TRUE))
  orgao_grave <- Reduce(`|`, lapply(c("GRAV_AST", "GRAV_MIOC", "GRAV_CONSC", "GRAV_CONV", "GRAV_INSUF", "GRAV_ORGAO"), function(coluna) col_flag(df, coluna) %in% TRUE))

  grav_flags <- list(choque, sangramento_grave, orgao_grave)
  n_gravidade <- Reduce(`+`, lapply(grav_flags, function(x) ifelse(is.na(x), 0L, as.integer(x))))
  tem_gravidade <- n_gravidade > 0

  condicao_especial <- gestante | (idade_anos < 2 | idade_anos > 65) | tem_comorbidade
  condicao_especial[is.na(condicao_especial)] <- FALSE
  grupo_b <- condicao_especial | (sangramento_pele %in% TRUE) | (prova_laco %in% TRUE)

  grupo <- dplyr::case_when(
    tem_gravidade ~ "D",
    tem_alarme ~ "C",
    grupo_b ~ "B",
    TRUE ~ "A"
  )

  cores <- c(A = COR_GRUPO_A, B = COR_GRUPO_B, C = COR_GRUPO_C, D = COR_GRUPO_D)
  rotulos <- c(
    A = "Grupo A - Dengue sem sinais de alarme",
    B = "Grupo B - Dengue sem sinais de alarme (risco intermediario)",
    C = "Grupo C - Dengue com sinais de alarme",
    D = "Grupo D - Dengue grave"
  )
  condutas <- c(
    A = "Acompanhamento ambulatorial",
    B = "Acompanhamento ambulatorial com observacao",
    C = "Urgencia: internacao em leito de observacao",
    D = "Emergencia: internacao em leito de terapia intensiva"
  )
  prioridades <- c(A = 1L, B = 2L, C = 3L, D = 4L)

  df$grupo_risco <- grupo
  df$cor_risco <- unname(cores[grupo])
  df$rotulo_risco <- unname(rotulos[grupo])
  df$conduta_risco <- unname(condutas[grupo])
  df$prioridade_risco <- unname(prioridades[grupo])
  df$n_sinais_alarme <- n_alarme
  df$n_sinais_gravidade <- n_gravidade

  df
}

# ============================================================================
# CHIKUNGUNYA — sinais oficiais
# ============================================================================

# Sinais de gravidade / criterios de internacao (qualquer um = grave)
SINAIS_GRAVIDADE_CHIK <- c(
  "acometimento_neurologico",    # Acometimento neurologico
  "choque",                      # Sinais de choque (extremidades frias, cianose, tontura, hipotensao, enchimento capilar lento, instabilidade hemodinamica)
  "dispneia",                    # Dispneia
  "dor_toracica",                # Dor toracica
  "vomitos_persistentes",        # Vomitos persistentes
  "sangramento_mucosas",         # Sangramentos de mucosas
  "descompensacao_doenca_base",  # Descompensacao de doenca de base
  "neonato",                     # Neonatos
  "insuficiencia_orgao"          # Outras manifestacoes com risco de morte ou necessidade de internacao
)

# Grupos de risco (aumentam prioridade mesmo sem sinais de gravidade)
GRUPOS_RISCO_CHIK <- c(
  "gestante",
  "idade_65_mais",
  "idade_menor_2",
  "comorbidades"
)

# Manifestacoes extra-articulares (nao configuram gravidade por si so)
MANIFESTACOES_EXTRA_ARTICULARES <- c(
  "exantema",
  "cefaleia",
  "conjuntivite",
  "nausea",
  "vomito",
  "mialgia",
  "dor_retro_ocular"
)

# ============================================================================
# CHIKUNGUNYA — funcao de classificacao (caso a caso)
# ============================================================================

#' Classifica um caso de chikungunya segundo os criterios oficiais do MS.
#'
#' @param idade_anos Idade em anos (numeric).
#' @param gestante Logical.
#' @param comorbidades Vetor de caracteres ou logical (qualquer comorbidade).
#' @param sinais_gravidade Vetor de caracteres com sinais de gravidade
#'   presentes. Valores validos: ver SINAIS_GRAVIDADE_CHIK.
#' @param manifestacoes_extra Vetor de caracteres com manifestacoes
#'   extra-articulares presentes. Valores validos: ver
#'   MANIFESTACOES_EXTRA_ARTICULARES.
#'
#' @return Lista com: categoria, cor, rotulo, descricao, conduta, prioridade
#'   e grupo_risco (logical indicando se ha grupo de risco que aumenta a
#'   prioridade de acompanhamento).
#' @export
classificar_chikungunya <- function(
    idade_anos = NA_real_,
    gestante = FALSE,
    comorbidades = character(0),
    sinais_gravidade = character(0),
    manifestacoes_extra = character(0)
) {
  gestante <- isTRUE(gestante)
  idade_anos <- suppressWarnings(as.numeric(idade_anos))

  tem_comorbidade <- if (is.logical(comorbidades)) {
    any(comorbidades, na.rm = TRUE)
  } else {
    length(as.character(comorbidades)) > 0 && !all(is.na(comorbidades)) && any(nzchar(comorbidades))
  }

  tem_gravidade <- length(intersect(tolower(as.character(sinais_gravidade)), SINAIS_GRAVIDADE_CHIK)) > 0

  tem_extra <- length(intersect(tolower(as.character(manifestacoes_extra)), MANIFESTACOES_EXTRA_ARTICULARES)) > 0

  grupo_risco <- gestante ||
    (!is.na(idade_anos) && (idade_anos < 2 || idade_anos >= 65)) ||
    tem_comorbidade

  if (tem_gravidade) {
    return(list(
      categoria = "grave",
      cor = COR_CHIK_GRAVE,
      rotulo = "Chikungunya grave",
      descricao = "Presenca de sinais de gravidade ou criterio de internacao (acometimento neurologico, choque, dispneia/dor toracica, vomitos persistentes, sangramentos de mucosas, descompensacao de doenca de base, neonatos ou insuficiencia de orgao/sistema).",
      conduta = "Internacao hospitalar e manejo de suporte em unidade de maior complexidade.",
      prioridade = 3L,
      grupo_risco = grupo_risco
    ))
  }

  if (tem_extra) {
    return(list(
      categoria = "extra_articular",
      cor = COR_CHIK_EXTRA_ARTICULAR,
      rotulo = "Chikungunya com manifestacoes extra-articulares",
      descricao = "Presenca de manifestacoes extra-articulares (exantema, cefaleia, conjuntivite, nauseas/vomitos, mialgia, dor retro-ocular) sem sinais de gravidade.",
      conduta = "Acompanhamento ambulatorial com atencao a evolucao clinica.",
      prioridade = 2L,
      grupo_risco = grupo_risco
    ))
  }

  list(
    categoria = "sem_gravidade",
    cor = COR_CHIK_SEM_GRAVIDADE,
    rotulo = "Chikungunya (sem gravidade)",
    descricao = "Chikungunya sem sinais de gravidade e sem manifestacoes extra-articulares relevantes.",
    conduta = "Acompanhamento ambulatorial com hidratacao e controle de dor/artralgia.",
    prioridade = 1L,
    grupo_risco = grupo_risco
  )
}

# ============================================================================
# CHIKUNGUNYA — mapeamento SINAN-CHIKUNGUNYA -> classificacao (vetorizada)
# ============================================================================
# OBSERVACAO: o pipeline atual ainda nao publica microdados individuais de
# chikungunya no cache do app. Esta funcao esta pronta para uso assim que os
# registros individuais do SINAN-CHIKUNGUNYA forem baixados e limpos com as
# colunas de sinais de gravidade. Os nomes de colunas seguem o layout do
# SINAN-CHIKUNGUNYA quando disponivel; adapte o mapeamento se necessario.

#' Classifica um data frame de registros de chikungunya nas 3 categorias do MS.
#'
#' @param df Data frame com colunas de sinais/sintomas de chikungunya.
#' @return Data frame original acrescido de: chik_categoria, chik_cor,
#'   chik_rotulo, chik_conduta, chik_prioridade e chik_grupo_risco.
#' @export
classificar_chikungunya_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  idade_anos <- if ("Idade" %in% names(df)) {
    suppressWarnings(as.numeric(df$Idade))
  } else if ("NU_IDADE_N" %in% names(df)) {
    sinan_idade_anos(df$NU_IDADE_N)
  } else {
    rep(NA_real_, nrow(df))
  }

  gestante <- if ("Gestacao" %in% names(df)) {
    df$Gestacao %in% c("1", "2", "3", "4")
  } else if ("CS_GESTANT" %in% names(df)) {
    as.character(df$CS_GESTANT) %in% c("1", "2", "3", "4")
  } else {
    rep(FALSE, nrow(df))
  }

  # Mapeamento simples de colunas de gravidade (se existirem no layout usado)
  col_grav <- c(
    "GRAV_NEURO", "CHOQUE", "DISPNEIA", "DOR_TORACICA", "VOMITO_PERSISTENTE",
    "SANGRAMENTO_MUCOSA", "DESCOMPENSACAO", "NEONATO", "INSUF_ORGAO"
  )
  grav_presentes <- col_grav[col_grav %in% names(df)]
  if (length(grav_presentes) > 0) {
    tem_gravidade <- Reduce(`|`, lapply(grav_presentes, function(coluna) sinan_flag(df[[coluna]]) %in% TRUE))
  } else {
    tem_gravidade <- rep(FALSE, nrow(df))
  }

  # Manifestacoes extra-articulares comuns no SINAN-CHIKUNGUNYA
  col_extra <- c("EXANTEMA", "CEFALEIA", "CONJUNTVIT", "NAUSEA", "VOMITO", "MIALGIA", "DOR_RETRO")
  extra_presentes <- col_extra[col_extra %in% names(df)]
  if (length(extra_presentes) > 0) {
    tem_extra <- Reduce(`|`, lapply(extra_presentes, function(coluna) sinan_flag(df[[coluna]]) %in% TRUE))
  } else {
    tem_extra <- rep(FALSE, nrow(df))
  }

  comorb <- c("DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA", "ACIDO_PEPT", "AUTO_IMUNE")
  comorb_presentes <- comorb[comorb %in% names(df)]
  tem_comorbidade <- if (length(comorb_presentes) > 0) {
    Reduce(`|`, lapply(comorb_presentes, function(coluna) sinan_flag(df[[coluna]]) %in% TRUE))
  } else {
    rep(FALSE, nrow(df))
  }

  grupo_risco <- gestante | (idade_anos < 2 | idade_anos >= 65) | tem_comorbidade
  grupo_risco[is.na(grupo_risco)] <- FALSE

  categoria <- dplyr::case_when(
    tem_gravidade ~ "grave",
    tem_extra ~ "extra_articular",
    TRUE ~ "sem_gravidade"
  )

  cores <- c(sem_gravidade = COR_CHIK_SEM_GRAVIDADE, extra_articular = COR_CHIK_EXTRA_ARTICULAR, grave = COR_CHIK_GRAVE)
  rotulos <- c(
    sem_gravidade = "Chikungunya (sem gravidade)",
    extra_articular = "Chikungunya com manifestacoes extra-articulares",
    grave = "Chikungunya grave"
  )
  condutas <- c(
    sem_gravidade = "Acompanhamento ambulatorial",
    extra_articular = "Acompanhamento ambulatorial com atencao a evolucao clinica",
    grave = "Internacao hospitalar"
  )
  prioridades <- c(sem_gravidade = 1L, extra_articular = 2L, grave = 3L)

  df$chik_categoria <- categoria
  df$chik_cor <- unname(cores[categoria])
  df$chik_rotulo <- unname(rotulos[categoria])
  df$chik_conduta <- unname(condutas[categoria])
  df$chik_prioridade <- unname(prioridades[categoria])
  df$chik_grupo_risco <- grupo_risco

  df
}

# ============================================================================
# UTILITARIOS DE AGREGACAO PARA O DASHBOARD
# ============================================================================

#' Agrega a distribuicao de casos por grupo de risco (dengue).
#'
#' @param df Data frame ja classificado por classificar_dengue_df().
#' @param coluna_grupo Nome da coluna com o grupo (default: grupo_risco).
#' @return Data frame com Grupo, Cor, Rotulo, Casos e Prioridade.
#' @export
resumir_risco_dengue <- function(df, coluna_grupo = "grupo_risco") {
  if (is.null(df) || nrow(df) == 0 || !coluna_grupo %in% names(df)) {
    return(data.frame(
      Grupo = character(), Cor = character(), Rotulo = character(),
      Casos = integer(), Prioridade = integer(), stringsAsFactors = FALSE
    ))
  }

  ordem <- c(A = 1L, B = 2L, C = 3L, D = 4L)
  rotulos <- c(
    A = "Grupo A - sem sinais de alarme",
    B = "Grupo B - risco intermediario",
    C = "Grupo C - sinais de alarme",
    D = "Grupo D - dengue grave"
  )
  cores <- c(A = COR_GRUPO_A, B = COR_GRUPO_B, C = COR_GRUPO_C, D = COR_GRUPO_D)

  df %>%
    dplyr::group_by(grupo = .data[[coluna_grupo]]) %>%
    dplyr::summarise(Casos = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(!is.na(grupo)) %>%
    dplyr::mutate(
      Cor = unname(cores[grupo]),
      Rotulo = unname(rotulos[grupo]),
      Prioridade = unname(ordem[grupo])
    ) %>%
    dplyr::arrange(Prioridade) %>%
    dplyr::select(Grupo = grupo, Cor, Rotulo, Casos, Prioridade)
}

#' Agrega a distribuicao de casos por categoria de risco (chikungunya).
#'
#' @param df Data frame ja classificado por classificar_chikungunya_df().
#' @param coluna_categoria Nome da coluna com a categoria (default: chik_categoria).
#' @return Data frame com Categoria, Cor, Rotulo, Casos e Prioridade.
#' @export
resumir_risco_chikungunya <- function(df, coluna_categoria = "chik_categoria") {
  if (is.null(df) || nrow(df) == 0 || !coluna_categoria %in% names(df)) {
    return(data.frame(
      Categoria = character(), Cor = character(), Rotulo = character(),
      Casos = integer(), Prioridade = integer(), stringsAsFactors = FALSE
    ))
  }

  ordem <- c(sem_gravidade = 1L, extra_articular = 2L, grave = 3L)
  rotulos <- c(
    sem_gravidade = "Chikungunya (sem gravidade)",
    extra_articular = "Com manifestacoes extra-articulares",
    grave = "Chikungunya grave"
  )
  cores <- c(sem_gravidade = COR_CHIK_SEM_GRAVIDADE, extra_articular = COR_CHIK_EXTRA_ARTICULAR, grave = COR_CHIK_GRAVE)

  df %>%
    dplyr::group_by(categoria = .data[[coluna_categoria]]) %>%
    dplyr::summarise(Casos = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(!is.na(categoria)) %>%
    dplyr::mutate(
      Cor = unname(cores[categoria]),
      Rotulo = unname(rotulos[categoria]),
      Prioridade = unname(ordem[categoria])
    ) %>%
    dplyr::arrange(Prioridade) %>%
    dplyr::select(Categoria = categoria, Cor, Rotulo, Casos, Prioridade)
}
