args <- commandArgs(trailingOnly = TRUE)
root_dir <- if (length(args) >= 1) args[[1]] else Sys.getenv("TEMPERATURA_ROOT")
if (!nzchar(root_dir)) root_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
if (length(args) >= 2) {
  Sys.setenv(TMP = args[[2]], TEMP = args[[2]], TMPDIR = args[[2]])
}

if (nzchar(Sys.getenv("RSTUDIO_PANDOC")) == FALSE) {
  pandoc_rstudio <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (dir.exists(pandoc_rstudio)) Sys.setenv(RSTUDIO_PANDOC = pandoc_rstudio)
}

rmd_path <- file.path(root_dir, "reports", "dengue_temperatura_abnt.Rmd")
out_dir <- file.path(root_dir, "data_inmet_campos", "resultados_dengue_temperatura", "relatorios")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rmarkdown::render(
  input = rmd_path,
  output_format = "pdf_document",
  output_file = "relatorio_dengue_temperatura_abnt.pdf",
  output_dir = out_dir,
  quiet = FALSE,
  envir = new.env(parent = globalenv())
)
