# ============================================================
# Regenera APENAS as duas figuras do banner (01 e 02)
# com legendas e rótulos maiores, sem rodar o pipeline inteiro.
# Lê os dados/resultados já gravados pelo pipeline.
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(stringr)
  library(scales)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root_dir, "data_inmet_campos", "resultados_dengue_temperatura")

cell_pal <- c(
  navy = "#1B365D",
  teal = "#007C89",
  sky = "#6EC6D9",
  vermillion = "#D95F02",
  gold = "#E6AB02",
  graphite = "#2B2B2B",
  gray = "#6B7280",
  pale = "#F2F6F7"
)

# ------------------------------------------------------------
# Leitura dos artefatos já gerados pelo pipeline
# ------------------------------------------------------------
base_diaria <- readr::read_csv(
  file.path(out_dir, "dados", "base_diaria_dengue_temperatura.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(date = as.Date(date))

base_associacao <- readr::read_csv(
  file.path(out_dir, "dados", "base_diaria_dengue_temperatura_com_casos.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(date = as.Date(date))

cor_diaria <- readr::read_csv(
  file.path(out_dir, "tabelas", "correlacoes_spearman.csv"),
  show_col_types = FALSE
) |>
  dplyr::filter(analise == "Diaria: temperatura media vs casos")

metadados <- readr::read_csv(
  file.path(out_dir, "tabelas", "metadados_analise.csv"),
  show_col_types = FALSE
)

municipio_nome <- metadados$valor[metadados$item == "municipio"]
uf_nome <- metadados$valor[metadados$item == "uf"]
municipio_base <- metadados$valor[metadados$item == "municipio_base"]
data_evento <- metadados$valor[metadados$item == "data_evento"]

# ------------------------------------------------------------
# Helpers (iguais ao pipeline)
# ------------------------------------------------------------
format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "p = NA",
    p < 0.001 ~ paste0("p = ", stringr::str_replace(formatC(p, format = "e", digits = 2), "\\.", ",")),
    TRUE ~ paste0("p = ", formatC(p, format = "f", digits = 3, decimal.mark = ","))
  )
}

format_rho <- function(x) formatC(x, format = "f", digits = 3, decimal.mark = ",")

lab_diaria <- paste0(
  "Spearman rho = ", format_rho(cor_diaria$rho[1]), "; ",
  format_p(cor_diaria$p_valor[1]), "; n = ", cor_diaria$n[1]
)

caption_padrao <- paste0(
  municipio_nome, "/", uf_nome, "; notificações por ", municipio_base,
  "; data do evento: ", data_evento, "; método estatístico: Spearman."
)

# ------------------------------------------------------------
# Tema com textos ampliados para banner
# ------------------------------------------------------------
tamanho_banner <- 18  # fonte base (em pontos)

theme_banner <- ggplot2::theme_minimal(
  base_size = tamanho_banner,
  base_family = "Times New Roman"
) +
  ggplot2::theme(
    text = ggplot2::element_text(color = cell_pal["graphite"]),
    plot.title = ggplot2::element_text(face = "bold", size = tamanho_banner + 6, margin = ggplot2::margin(b = 6)),
    plot.subtitle = ggplot2::element_text(size = tamanho_banner + 1, color = cell_pal["gray"], margin = ggplot2::margin(b = 10)),
    plot.caption = ggplot2::element_text(size = tamanho_banner - 3, color = cell_pal["gray"], hjust = 0),
    axis.title = ggplot2::element_text(face = "bold", size = tamanho_banner + 2),
    axis.text = ggplot2::element_text(size = tamanho_banner),
    legend.title = ggplot2::element_text(face = "bold", size = tamanho_banner + 1),
    legend.text = ggplot2::element_text(size = tamanho_banner),
    panel.grid.major = ggplot2::element_line(color = "#D7DEE2", linewidth = 0.25),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    strip.text = ggplot2::element_text(face = "bold", color = cell_pal["navy"]),
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin = ggplot2::margin(12, 24, 18, 16)
  )

# ------------------------------------------------------------
# Figura 01 - série diária
# ------------------------------------------------------------
escala <- max(base_diaria$casos, na.rm = TRUE) / max(base_diaria$temp_media, na.rm = TRUE)

p1 <- ggplot2::ggplot(base_diaria, ggplot2::aes(x = date)) +
  ggplot2::geom_col(ggplot2::aes(y = casos), fill = cell_pal["navy"], alpha = 0.78, width = 1) +
  ggplot2::geom_line(ggplot2::aes(y = temp_media * escala), color = cell_pal["vermillion"], linewidth = 0.7) +
  ggplot2::scale_y_continuous(
    name = "Casos notificados de dengue",
    sec.axis = ggplot2::sec_axis(~ . / escala, name = "Temperatura média diária (C)")
  ) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::labs(
    title = "A. Série diária de dengue e temperatura",
    subtitle = lab_diaria,
    x = NULL,
    caption = caption_padrao
  ) +
  theme_banner

# ------------------------------------------------------------
# Figura 02 - dispersão diária (Spearman)
# ------------------------------------------------------------
p2 <- ggplot2::ggplot(base_associacao, ggplot2::aes(x = temp_media, y = casos)) +
  ggplot2::geom_point(color = cell_pal["teal"], alpha = 0.34, size = 2) +
  ggplot2::geom_smooth(method = "loess", se = TRUE, color = cell_pal["vermillion"], fill = cell_pal["gold"], linewidth = 1.1, alpha = 0.18) +
  ggplot2::annotate(
    "label",
    x = min(base_associacao$temp_media, na.rm = TRUE),
    y = max(base_associacao$casos, na.rm = TRUE),
    label = lab_diaria,
    hjust = 0, vjust = 1,
    fill = "white", color = cell_pal["graphite"],
    size = 6.5
  ) +
  ggplot2::labs(
    title = "B. Associação diária bruta",
    x = "Temperatura média diária (C)",
    y = "Casos notificados por dia",
    caption = caption_padrao
  ) +
  theme_banner

# ------------------------------------------------------------
# Gravação (sobrescreve os dois PNGs do banner)
# ------------------------------------------------------------
fig_dir <- file.path(out_dir, "figuras")

ggplot2::ggsave(
  file.path(fig_dir, "01_serie_diaria_casos_temperatura.png"),
  p1, width = 12, height = 8.5, dpi = 320, bg = "white"
)

ggplot2::ggsave(
  file.path(fig_dir, "02_dispersao_diaria_spearman.png"),
  p2, width = 12, height = 8.5, dpi = 320, bg = "white"
)

message("Figuras do banner regeneradas com textos ampliados em: ", fig_dir)
