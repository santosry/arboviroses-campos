project_root <- if (requireNamespace("here", quietly = TRUE)) {
  here::here()
} else {
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
}
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

source(file.path(project_root, "R", "packages.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "classificacao_risco.R"), encoding = "UTF-8")

test_that("dengue classifica Grupo A sem sinais e sem fatores de risco", {
  r <- classificar_dengue(idade_anos = 30)
  expect_equal(r$grupo, "A")
  expect_equal(r$cor, COR_GRUPO_A)
  expect_equal(r$prioridade, 1L)
})

test_that("dengue classifica Grupo B por condicoes especiais e fatores de risco", {
  expect_equal(classificar_dengue(idade_anos = 70)$grupo, "B")
  expect_equal(classificar_dengue(idade_anos = 1)$grupo, "B")
  expect_equal(classificar_dengue(idade_anos = 28, gestante = TRUE)$grupo, "B")
  expect_equal(classificar_dengue(idade_anos = 30, comorbidades = "diabetes")$grupo, "B")
  expect_equal(classificar_dengue(idade_anos = 30, prova_laco = TRUE)$grupo, "B")
  expect_equal(classificar_dengue(idade_anos = 30, sangramento_pele = TRUE)$grupo, "B")
  expect_equal(classificar_dengue(idade_anos = 30, risco_social = TRUE)$grupo, "B")
})

test_that("dengue classifica Grupo C quando ha qualquer sinal de alarme", {
  for (sinal in SINAIS_ALARME_DENGUE) {
    expect_equal(classificar_dengue(idade_anos = 30, sinais_alarme = sinal)$grupo, "C")
  }
})

test_that("dengue classifica Grupo D quando ha qualquer sinal de gravidade", {
  for (sinal in SINAIS_GRAVIDADE_DENGUE) {
    expect_equal(classificar_dengue(idade_anos = 30, sinais_gravidade = sinal)$grupo, "D")
  }
})

test_that("dengue grave prevalece sobre sinais de alarme", {
  r <- classificar_dengue(
    idade_anos = 30,
    sinais_alarme = "vomitos_persistentes",
    sinais_gravidade = "sangramento_grave"
  )
  expect_equal(r$grupo, "D")
})

test_that("chikungunya classifica as tres categorias oficiais", {
  expect_equal(classificar_chikungunya(idade_anos = 30)$categoria, "sem_gravidade")
  expect_equal(
    classificar_chikungunya(idade_anos = 30, manifestacoes_extra = "exantema")$categoria,
    "extra_articular"
  )
  expect_equal(
    classificar_chikungunya(idade_anos = 30, sinais_gravidade = "acometimento_neurologico")$categoria,
    "grave"
  )
})

test_that("chikungunya identifica grupos de risco que aumentam prioridade", {
  expect_true(classificar_chikungunya(idade_anos = 0.1)$grupo_risco)
  expect_true(classificar_chikungunya(idade_anos = 70)$grupo_risco)
  expect_true(classificar_chikungunya(idade_anos = 30, gestante = TRUE)$grupo_risco)
  expect_true(classificar_chikungunya(idade_anos = 30, comorbidades = TRUE)$grupo_risco)
  expect_false(classificar_chikungunya(idade_anos = 30)$grupo_risco)
})

test_that("classificacao vetorizada de dengue produz grupos A/B/C/D", {
  df <- data.frame(
    NU_IDADE_N = c("4030", "4070", "4030", "4030", "4030"),
    CS_GESTANT = c("6", "6", "6", "6", "6"),
    PETEQUIA_N = c("2", "2", "2", "2", "2"),
    LACO = c("2", "2", "2", "2", "2"),
    DIABETES = c("2", "2", "2", "2", "2"),
    HEMATOLOG = c("2", "2", "2", "2", "2"),
    HEPATOPAT = c("2", "2", "2", "2", "2"),
    RENAL = c("2", "2", "2", "2", "2"),
    HIPERTENSA = c("2", "2", "2", "2", "2"),
    ACIDO_PEPT = c("2", "2", "2", "2", "2"),
    AUTO_IMUNE = c("2", "2", "2", "2", "2"),
    ALRM_ABDOM = c("2", "2", "1", "2", "2"),
    ALRM_VOM = c("2", "2", "2", "2", "2"),
    ALRM_LIQ = c("2", "2", "2", "2", "2"),
    ALRM_HIPOT = c("2", "2", "2", "2", "2"),
    ALRM_HEPAT = c("2", "2", "2", "2", "2"),
    ALRM_SANG = c("2", "2", "2", "2", "2"),
    ALRM_LETAR = c("2", "2", "2", "2", "2"),
    ALRM_HEMAT = c("2", "2", "2", "2", "2"),
    GRAV_PULSO = c("2", "2", "2", "1", "2"),
    GRAV_TAQUI = c("2", "2", "2", "2", "2"),
    GRAV_EXTRE = c("2", "2", "2", "2", "2"),
    GRAV_ENCH = c("2", "2", "2", "2", "2"),
    GRAV_HIPOT = c("2", "2", "2", "2", "2"),
    GRAV_HEMAT = c("2", "2", "2", "2", "2"),
    GRAV_MELEN = c("2", "2", "2", "2", "2"),
    GRAV_METRO = c("2", "2", "2", "2", "2"),
    GRAV_SANG = c("2", "2", "2", "2", "2"),
    GRAV_AST = c("2", "2", "2", "2", "2"),
    GRAV_MIOC = c("2", "2", "2", "2", "2"),
    GRAV_CONSC = c("2", "2", "2", "2", "2"),
    GRAV_CONV = c("2", "2", "2", "2", "2"),
    GRAV_INSUF = c("2", "2", "2", "2", "2"),
    GRAV_ORGAO = c("2", "2", "2", "2", "2"),
    stringsAsFactors = FALSE
  )
  out <- classificar_dengue_df(df)
  expect_equal(out$grupo_risco, c("A", "B", "C", "D", "A"))
  expect_true(all(c("cor_risco", "rotulo_risco", "conduta_risco", "n_sinais_alarme", "n_sinais_gravidade") %in% names(out)))
})

test_that("resumir_risco_dengue agrega com cores e ordem oficiais", {
  df <- data.frame(
    grupo_risco = c("A", "A", "B", "C", "D", "C"),
    stringsAsFactors = FALSE
  )
  res <- resumir_risco_dengue(df)
  expect_equal(res$Grupo, c("A", "B", "C", "D"))
  expect_equal(res$Casos, c(2L, 1L, 2L, 1L))
  expect_equal(res$Cor, c(COR_GRUPO_A, COR_GRUPO_B, COR_GRUPO_C, COR_GRUPO_D))
})
