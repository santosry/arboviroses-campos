# Entrada modular do app Shiny de arboviroses.
# O arquivo legado app_arboviroses.R foi preservado como referência funcional.

source("R/packages.R", encoding = "UTF-8")
source("R/utils.R", encoding = "UTF-8")
source("R/dados.R", encoding = "UTF-8")
source("R/graficos.R", encoding = "UTF-8")
source("R/mapas.R", encoding = "UTF-8")
source("R/ui.R", encoding = "UTF-8")
source("R/server.R", encoding = "UTF-8")

shinyApp(ui, server)
