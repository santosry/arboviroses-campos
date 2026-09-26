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
  p <- criar_grafico_serie(dengue, "#C73E1D", "Dengue")
  expect_s3_class(p, "plotly")
})
