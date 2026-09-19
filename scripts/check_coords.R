source("R/packages.R")
source("R/utils.R")
source("R/dados.R")
source("R/mapas.R")

setores <- carregar_setores_censitarios_campos()
bb <- sf::st_bbox(setores)
cat(sprintf("BBox: xmin=%.3f ymin=%.3f xmax=%.3f ymax=%.3f\n", bb$xmin, bb$ymin, bb$xmax, bb$ymax))
cat(sprintf("Centro: X=%.2f Y=%.2f\n", mean(c(bb$xmin,bb$xmax)), mean(c(bb$ymin,bb$ymax))))
cat("Google Maps Campos: lon=-41.3, lat=-21.75\n")

# Verificar se x parece longitude (-41) e y latitude (-21)
if (abs(mean(c(bb$xmin,bb$xmax)) + 41) < 5) {
  cat("X = longitude (OK), Y = latitude (OK)\n")
} else {
  cat("ATENCAO: coordenadas parecem trocadas!\n")
}
