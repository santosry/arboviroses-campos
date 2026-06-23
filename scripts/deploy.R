#!/usr/bin/env Rscript

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Pacote rsconnect ausente. Restaure com renv::restore().", call. = FALSE)
}

account <- Sys.getenv("SHINYAPPS_ACCOUNT", "arbovirosescamposiff")
server <- Sys.getenv("SHINYAPPS_SERVER", "shinyapps.io")
app_name <- Sys.getenv("SHINYAPPS_APP_NAME", "arboviroses")
old_accounts <- c("ryansantospaulo")

accounts <- rsconnect::accounts()
print(accounts)

if (!account %in% accounts$name || !server %in% accounts$server[accounts$name == account]) {
  stop(
    "Conta shinyapps.io nao encontrada: ", account, " / ", server, "\n",
    "Configure localmente com rsconnect::setAccountInfo(name, token, secret). ",
    "Nao grave tokens no repositorio.",
    call. = FALSE
  )
}

if (account %in% old_accounts) {
  stop("Abortado: a conta selecionada parece ser a conta antiga: ", account, call. = FALSE)
}

if (dir.exists("dados_sinan_campos/outputs")) {
  message("Aviso: dados_sinan_campos/outputs existe localmente, mas nao sera enviado no deploy.")
}

required_files <- c(
  "app.R",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("www", full.names = TRUE),
  list.files("data/app_cache", full.names = TRUE)
)
required_files <- required_files[file.exists(required_files)]

if (!"data/app_cache/metadata.json" %in% required_files) {
  warning("metadata.json nao encontrado em data/app_cache. Rode scripts/update_data.R antes do deploy.")
}

for (old in old_accounts) {
  try(
    rsconnect::forgetDeployment(
      appPath = ".",
      name = app_name,
      account = old,
      server = server
    ),
    silent = TRUE
  )
}

deploy <- rsconnect::deployApp(
  appDir = ".",
  appFiles = required_files,
  appName = app_name,
  account = account,
  server = server,
  forceUpdate = TRUE,
  launch.browser = FALSE
)

url <- paste0("https://", account, ".shinyapps.io/", app_name, "/")

status_ok <- FALSE
status_msg <- "verificacao HTTP nao executada"
if (requireNamespace("curl", quietly = TRUE)) {
  h <- curl::new_handle(nobody = TRUE, followlocation = TRUE, timeout = 60)
  res <- try(curl::curl_fetch_memory(url, handle = h), silent = TRUE)
  if (!inherits(res, "try-error")) {
    status_ok <- identical(res$status_code, 200L)
    status_msg <- paste("status", res$status_code)
  } else {
    status_msg <- conditionMessage(attr(res, "condition"))
  }
} else {
  status_msg <- "pacote curl ausente; pulando verificacao HTTP"
}

if (!status_ok) {
  warning("Deploy enviado, mas a verificacao da URL nao confirmou 200 OK: ", status_msg)
} else {
  message("Deploy publicado e verificado com 200 OK.")
}

message("URL final: ", url)
invisible(deploy)
