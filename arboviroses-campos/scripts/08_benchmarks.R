# ============================================================================
# 08_benchmarks.R
# ----------------------------------------------------------------------------
# Gera as referências externas anuais (Estado do RJ e Brasil) usadas nas
# linhas tracejadas dos gráficos de incidência.
#
# Fonte dos casos: SINAN/SVS via microdatasus (uf = "all"; os arquivos do
# SINAN-DENGUE, SINAN-CHIKUNGUNYA e SINAN-ZIKA são publicados nacionalmente).
# Fonte dos denominadores: IBGE/SIDRA (Censo 2022 e estimativas populacionais).
#
# Uso:
#   Sys.setenv(ARBOVIROSES_DOWNLOAD = "true")
#   source("scripts/08_benchmarks.R")
#
# Saída:
#   data/app_cache/benchmarks_uf_br.csv
#   data/audit/benchmarks_uf_br_auditoria.csv
#
# O download nacional é pesado (milhões de registros por ano). O workflow
# .github/workflows/atualizacao-semestral.yml executa esta etapa apenas duas
# vezes por ano. Para testes rápidos, use ARBOVIROSES_BENCHMARK_AGRAVOS.
# ============================================================================

options(encoding = "UTF-8")
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.1252"), silent = TRUE))
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8"), silent = TRUE))

if (!requireNamespace("microdatasus", quietly = TRUE)) {
  stop("Pacote microdatasus ausente. Restaure o ambiente com renv::restore().", call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Pacote dplyr ausente. Restaure o ambiente com renv::restore().", call. = FALSE)
}

root_path <- function(...) {
  file.path(getwd(), ...)
}

baixar <- identical(tolower(Sys.getenv("ARBOVIROSES_DOWNLOAD", "false")), "true")
if (!baixar) {
  message(
    "Atualização de benchmarks desativada. Defina ARBOVIROSES_DOWNLOAD=true ",
    "para baixar os microdados via microdatasus."
  )
} else {

anos <- suppressWarnings(as.integer(strsplit(
  Sys.getenv("ARBOVIROSES_BENCHMARK_ANOS", "2020,2021,2022,2023,2024,2025"),
  ","
)[[1]]))
anos <- anos[is.finite(anos)]

agravos <- list(
  Dengue = list(
    information_system = "SINAN-DENGUE",
    # 10 = dengue; 11 = dengue com sinais de alarme; 12 = dengue grave.
    confirmadas = c("10", "11", "12")
  ),
  Chikungunya = list(
    information_system = "SINAN-CHIKUNGUNYA",
    # 13 = chikungunya confirmado; 5 = descartado.
    confirmadas = c("13")
  ),
  Zika = list(
    information_system = "SINAN-ZIKA",
    # 1 = confirmado; 2 = descartado; 8 = inconclusivo.
    confirmadas = c("1")
  )
)

filtro_agravos <- tolower(trimws(strsplit(
  Sys.getenv("ARBOVIROSES_BENCHMARK_AGRAVOS", ""), ","
)[[1]]))
if (length(filtro_agravos) > 0 && any(nzchar(filtro_agravos))) {
  agravos <- agravos[tolower(names(agravos)) %in% filtro_agravos]
  if (length(agravos) == 0) {
    stop("ARBOVIROSES_BENCHMARK_AGRAVOS não corresponde a nenhum agravo disponível.", call. = FALSE)
  }
}

# População residente (IBGE/SIDRA).
# 2020, 2021, 2024 e 2025: estimativas (tabela 6579, variável 9324).
# 2022 e 2023: Censo 2022 (tabela 4714, variável 93), usado como denominador
# conservador para ambos os anos.
populacao_ref <- data.frame(
  Ano = rep(anos, each = 2),
  Local = rep(c("RJ", "Brasil"), times = length(anos)),
  Populacao = NA_real_,
  stringsAsFactors = FALSE
)

populacao_base <- rbind(
  data.frame(Ano = 2020, RJ = 17366189, Brasil = 211755692),
  data.frame(Ano = 2021, RJ = 17463349, Brasil = 213317639),
  data.frame(Ano = 2022, RJ = 16055174, Brasil = 203080756),
  data.frame(Ano = 2023, RJ = 16055174, Brasil = 203080756),
  data.frame(Ano = 2024, RJ = 17219679, Brasil = 212583750),
  data.frame(Ano = 2025, RJ = 17223547, Brasil = 213421037)
)
for (i in seq_len(nrow(populacao_ref))) {
  linha <- populacao_base[populacao_base$Ano == populacao_ref$Ano[i], , drop = FALSE]
  if (nrow(linha) == 1) populacao_ref$Populacao[i] <- as.numeric(linha[[populacao_ref$Local[i]]])
}

extrair_ano_notificacao <- function(x) {
  x_chr <- suppressWarnings(iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = ""))
  suppressWarnings(as.integer(substr(x_chr, 1, 4)))
}

contar_casos <- function(dados, ano, codigos_confirmados) {
  if (is.null(dados) || nrow(dados) == 0) {
    return(c(RJ = 0L, Brasil = 0L))
  }
  classe <- suppressWarnings(iconv(as.character(dados$CLASSI_FIN), from = "", to = "ASCII//TRANSLIT", sub = ""))
  confirmado <- !is.na(classe) & classe %in% codigos_confirmados
  ano_notif <- extrair_ano_notificacao(dados$DT_NOTIFIC)
  municipio <- suppressWarnings(as.numeric(iconv(
    as.character(dados$ID_MN_RESI), from = "", to = "ASCII//TRANSLIT", sub = ""
  )))
  uf_residencia <- municipio %/% 10000
  no_ano <- !is.na(ano_notif) & ano_notif == ano
  c(
    RJ = sum(confirmado & no_ano & !is.na(uf_residencia) & uf_residencia == 33, na.rm = TRUE),
    Brasil = sum(confirmado & no_ano, na.rm = TRUE)
  )
}

resultados <- list()
auditoria <- list()

for (nome_agravo in names(agravos)) {
  config <- agravos[[nome_agravo]]
  for (ano in anos) {
    message("Baixando ", nome_agravo, " ", ano, " via microdatasus...")
    dados <- tryCatch(
      suppressWarnings(
        microdatasus::fetch_datasus(
          year_start = ano,
          year_end = ano,
          uf = "all",
          information_system = config$information_system,
          vars = c("DT_NOTIFIC", "CLASSI_FIN", "ID_MN_RESI"),
          stop_on_error = FALSE,
          timeout = 1200
        )
      ),
      error = function(e) {
        warning("Falha no download de ", nome_agravo, " ", ano, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(dados) || nrow(dados) == 0) {
      warning("Sem dados para ", nome_agravo, " em ", ano, "; ano ignorado.")
      next
    }

    casos <- contar_casos(dados, ano, config$confirmadas)
    message(
      "  ", nome_agravo, " ", ano,
      ": RJ = ", format(casos[["RJ"]], big.mark = ".", decimal.mark = ","),
      " | Brasil = ", format(casos[["Brasil"]], big.mark = ".", decimal.mark = ",")
    )

    for (local in c("RJ", "Brasil")) {
      pop <- populacao_ref$Populacao[
        populacao_ref$Ano == ano & populacao_ref$Local == local
      ]
      pop <- if (length(pop) == 1) pop else NA_real_
      inc <- if (!is.na(pop) && pop > 0) casos[[local]] / pop * 100000 else NA_real_
      resultados[[length(resultados) + 1]] <- data.frame(
        Agravo = nome_agravo,
        Ano = ano,
        Local = local,
        Casos_confirmados = as.integer(casos[[local]]),
        Populacao = as.numeric(pop),
        Incidencia_100mil = round(inc, 2),
        Fonte = paste0(
          "SINAN/SVS via microdatasus (", config$information_system,
          "); população IBGE/SIDRA"
        ),
        Atualizado_em = as.character(Sys.Date()),
        stringsAsFactors = FALSE
      )
    }

    auditoria[[length(auditoria) + 1]] <- data.frame(
      Agravo = nome_agravo,
      Ano = ano,
      Registros_baixados = nrow(dados),
      Confirmados_Brasil = as.integer(casos[["Brasil"]]),
      Confirmados_RJ = as.integer(casos[["RJ"]]),
      Codigos_confirmados = paste(config$confirmadas, collapse = "|"),
      Data_processamento = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )

    rm(dados)
    invisible(gc())
  }
}

if (length(resultados) == 0) {
  stop("Nenhum benchmark foi gerado. Verifique a conexão e os parâmetros do microdatasus.", call. = FALSE)
}

benchmarks <- dplyr::bind_rows(resultados)
benchmarks <- benchmarks[order(benchmarks$Agravo, benchmarks$Local, benchmarks$Ano), , drop = FALSE]

dir.create(root_path("data", "app_cache"), recursive = TRUE, showWarnings = FALSE)
dir.create(root_path("data", "audit"), recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  benchmarks,
  root_path("data", "app_cache", "benchmarks_uf_br.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
utils::write.csv(
  dplyr::bind_rows(auditoria),
  root_path("data", "audit", "benchmarks_uf_br_auditoria.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message(
  "Benchmarks concluídos: ",
  nrow(benchmarks),
  " linhas em data/app_cache/benchmarks_uf_br.csv"
)
}
