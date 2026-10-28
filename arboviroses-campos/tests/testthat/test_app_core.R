project_root <- if (requireNamespace("here", quietly = TRUE)) {
  here::here()
} else {
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
}
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

source(file.path(project_root, "R", "packages.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "utils.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "dados.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "graficos.R"), encoding = "UTF-8")
source(file.path(project_root, "R", "mapas.R"), encoding = "UTF-8")

test_that("caches finais do app existem e carregam", {
  arquivos <- c(
    "data/app_cache/chikungunya_microdatasus_campos_v1.rds",
    "data/app_cache/dengue_microdatasus_campos_v2.rds",
    "data/app_cache/zika_microdatasus_campos_v1.rds",
    "data/app_cache/dengue_bairros_campos_v1.rds"
  )
  expect_true(all(file.exists(arquivos)))
  expect_gt(nrow(readRDS(arquivos[1])), 0)
  expect_gt(nrow(readRDS(arquivos[2])), 0)
  expect_gt(nrow(readRDS(arquivos[3])), 0)
})

test_that("filtro anual preserva Todos e filtra ano especifico", {
  expect_identical(filter_year(dengue, "Todos"), dengue)
  filtrado <- filter_year(dengue, "2024")
  expect_true(all(filtrado$Ano == 2024))
})

test_that("calculo de incidencia retorna numero finito quando ha populacao", {
  taxa <- incidencia_periodo(dengue)
  expect_type(taxa, "double")
  expect_true(is.finite(taxa))
})

test_that("correspondencia de bairros produz tabela auditavel", {
  tab <- preparar_correspondencia_bairros(utils::head(dengue_bairros, 10))
  expect_true(all(c("NM_BAIRRO_PLANILHA", "Status") %in% names(tab)))
  expect_gt(nrow(tab), 0)
})

test_that("grafico principal de serie e construido", {
  p <- criar_grafico_serie(dengue, CORES_AGRAVOS[["Dengue"]], "Dengue")
  expect_s3_class(p, "plotly")
})

test_that("benchmarks externos de RJ e Brasil estao disponiveis e sao finitos", {
  b <- obter_benchmarks("Dengue")
  expect_gt(nrow(b), 0)
  expect_true(all(c("RJ", "Brasil") %in% unique(b$Local)))
  expect_true(any(is.finite(b$Incidencia_100mil)))
})

test_that("grafico de incidencia aceita benchmarks sem quebrar", {
  p <- criar_grafico_incidencia(
    dengue, CORES_AGRAVOS[["Dengue"]], "Dengue",
    benchmarks = obter_benchmarks("Dengue")
  )
  expect_s3_class(p, "plotly")
})

test_that("figura de publicacao com benchmark e ggplot valido", {
  p <- grafico_publicacao(
    dengue, "incidencia", "Dengue - incidencia", "Dengue", "2020-2025",
    benchmarks = obter_benchmarks("Dengue")
  )
  expect_s3_class(p, "ggplot")
})

test_that("paleta viridis tem cores distintas e seguras", {
  cores <- unname(CORES_AGRAVOS)
  expect_length(unique(cores), length(cores))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cores)))
})

test_that("fase 2 agrega microdados limpos de dengue", {
  limpo <- data.frame(
    Agravo = "Dengue", Ano = c(2024, 2024, 2024, NA),
    Data_notificacao = as.Date(c("2024-01-10", "2024-02-10", "2024-03-10", "2024-04-10")),
    Sexo = c("masculino", "feminino", "", "feminino"),
    Gestacao = c("nao se aplica", "1", "ignorado", "5"),
    Raca_cor = c("parda", "branca", "ignorado", "preta"),
    Escolaridade = c("6", "08", "", "3"),
    Idade = c(30, 70, NA, 5),
    Classificacao = c("10", "11", "12", "5"),
    Evolucao = c("1", "2", "3", "1"),
    Sorotipo = c("1", "2", "ignorado", "1"),
    Bairro = c("Centro", "Centro", "Ignorado", "Parque Aurora"),
    stringsAsFactors = FALSE
  )
  agg <- agregar_microdados_limpos(limpo, anos = 2024, agravo = "dengue")
  expect_equal(nrow(agg), 1)
  expect_equal(agg$Ano, 2024L)
  expect_equal(agg$Masculino, 1)
  expect_equal(agg$Feminino, 2)
  expect_equal(agg$Ignorado_sexo, 1)
  expect_equal(agg$Confirmado_casos, 3)
  expect_equal(agg$Descartado_casos, 1)
  expect_equal(agg$Obitos_Agr, 1)
  expect_equal(agg$Obitos_Out, 1)
  expect_equal(agg$Faixa_5_9, 1)
  expect_equal(agg$Faixa_70_79, 1)
  expect_equal(agg$DEN_1, 2)
  expect_equal(agg$DEN_2, 1)
})

test_that("fase 2 agrega bairros apenas de casos confirmados", {
  limpo <- data.frame(
    Agravo = "Dengue", Ano = c(2024, 2024, 2024),
    Data_notificacao = as.Date(c("2024-01-10", "2024-02-10", "2024-03-10")),
    Sexo = "feminino", Gestacao = "5", Raca_cor = "parda",
    Escolaridade = "6", Idade = 30,
    Classificacao = c("10", "5", "11"),
    Evolucao = "1", Sorotipo = "1",
    Bairro = c("Centro", "Centro", "Parque Aurora"),
    stringsAsFactors = FALSE
  )
  b <- agregar_bairros_microdados_limpos(limpo, anos = 2024, agravo = "dengue")
  expect_equal(sum(b$Casos), 2)
  expect_true(all(c("Ano", "NM_BAIRRO", "Casos") %in% names(b)))
  expect_equal(sum(b$Casos[b$NM_BAIRRO == "Centro"]), 1)
})
