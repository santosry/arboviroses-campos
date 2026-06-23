pk <- c(
  "shiny", "shinydashboard", "dplyr", "tidyr", "plotly", "DT", "ggplot2",
  "readxl", "sf", "leaflet", "microdatasus", "geobr", "sidrar", "here",
  "jsonlite", "rmarkdown", "testthat", "rsconnect", "renv", "htmltools"
)

ip <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
ip <- ip[ip$Package %in% pk, , drop = FALSE]

packages <- setNames(
  lapply(seq_len(nrow(ip)), function(i) {
    list(
      Package = ip$Package[i],
      Version = ip$Version[i],
      Source = "Repository",
      Repository = "CRAN"
    )
  }),
  ip$Package
)

lock <- list(
  R = list(
    Version = paste(R.version$major, R.version$minor, sep = "."),
    Repositories = list(CRAN = "https://cloud.r-project.org")
  ),
  Packages = packages
)

jsonlite::write_json(lock, "renv.lock", pretty = TRUE, auto_unbox = TRUE)
message("renv.lock escrito com ", length(packages), " pacotes diretos.")
