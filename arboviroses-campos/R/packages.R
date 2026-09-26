options(encoding = "UTF-8")
if (.Platform$OS.type == "unix") {
  try(Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8"), silent = TRUE)
}

required_packages <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "plotly",
  "DT",
  "ggplot2",
  "readxl",
  "sf",
  "leaflet"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Pacotes ausentes: ",
    paste(missing_packages, collapse = ", "),
    ". Restaure o ambiente com renv::restore().",
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))
