args <- commandArgs(trailingOnly = TRUE)
root_dir <- if (length(args) >= 1) args[[1]] else Sys.getenv("TEMPERATURA_ROOT")
if (!nzchar(root_dir)) root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

out_dir <- file.path(root_dir, "data_inmet_campos", "resultados_dengue_temperatura")
dirs <- list(
  tabelas = file.path(out_dir, "tabelas"),
  figuras = file.path(out_dir, "figuras"),
  relatorios = file.path(out_dir, "relatorios"),
  auditoria = file.path(out_dir, "auditoria")
)

descritivo <- readr::read_csv(file.path(dirs$tabelas, "descritivo.csv"), show_col_types = FALSE)
correlacoes <- readr::read_csv(file.path(dirs$tabelas, "correlacoes_spearman.csv"), show_col_types = FALSE)
cor_mensal <- readr::read_csv(file.path(dirs$tabelas, "correlacoes_spearman_mensais.csv"), show_col_types = FALSE)
tabela_diaria <- readr::read_csv(file.path(dirs$tabelas, "tabela_diaria_associacao.csv"), show_col_types = FALSE)
metadados <- readr::read_csv(file.path(dirs$tabelas, "metadados_analise.csv"), show_col_types = FALSE)
auditoria_recalculo <- readr::read_csv(file.path(dirs$auditoria, "auditoria_recalculo_spearman.csv"), show_col_types = FALSE)
auditoria_recalculo_mensal <- readr::read_csv(file.path(dirs$auditoria, "auditoria_recalculo_spearman_mensal.csv"), show_col_types = FALSE)
auditoria_tabela <- readr::read_csv(file.path(dirs$auditoria, "auditoria_tabela_diaria_associacao.csv"), show_col_types = FALSE)

fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits, decimal.mark = ",", big.mark = ".")
fmt_int <- function(x) format(as.integer(round(x)), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
fmt_p <- function(p) ifelse(
  is.na(p),
  "NA",
  ifelse(p < 0.001, gsub("\\.", ",", formatC(p, format = "e", digits = 2)), fmt_num(p, 3))
)
fmt_rho <- function(x) fmt_num(x, 3)
get_meta <- function(key) metadados$valor[match(key, metadados$item)]

daily <- correlacoes[correlacoes$analise == "Diaria: temperatura media vs casos", ][1, ]
maior_abs_rho_mensal <- cor_mensal[which.max(abs(cor_mensal$rho)), ]

pdf_path <- file.path(dirs$relatorios, "relatorio_dengue_temperatura_abnt.pdf")
dir.create(dirs$relatorios, recursive = TRUE, showWarnings = FALSE)

W <- 8.27
H <- 11.69
left <- 1.18
right <- 0.79
top <- 1.18
content_w <- W - left - right

txt_col <- "black"
navy <- "black"
gray <- "black"

new_page <- function() {
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
}

draw_text <- function(label, x, y, size = 10, font = 1, col = txt_col, just = c("left", "top"), lineheight = 1.15) {
  grid::grid.text(
    label,
    x = grid::unit(x, "in"),
    y = grid::unit(y, "in"),
    just = just,
    gp = grid::gpar(fontsize = size, fontface = font, col = col, lineheight = lineheight)
  )
}

draw_footer <- function(page) {
  draw_text(as.character(page), W / 2, 0.42, size = 12, col = gray, just = c("center", "bottom"))
}

wrap_lines <- function(text, width = 92) unlist(strwrap(text, width = width, simplify = FALSE))

string_width_in <- function(label, size = 12, font = 1) {
  grid::convertWidth(
    grid::grobWidth(grid::textGrob(label, gp = grid::gpar(fontsize = size, fontface = font))),
    "in",
    valueOnly = TRUE
  )
}

draw_justified_line <- function(line, x, y, size = 12, font = 1, target_w = content_w) {
  words <- strsplit(line, "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (length(words) <= 1) {
    draw_text(line, x, y, size = size, font = font)
    return(invisible(NULL))
  }
  word_widths <- vapply(words, string_width_in, numeric(1), size = size, font = font)
  gap <- max((target_w - sum(word_widths)) / (length(words) - 1), 0.04)
  xpos <- x
  for (i in seq_along(words)) {
    draw_text(words[i], xpos, y, size = size, font = font)
    xpos <- xpos + word_widths[i] + gap
  }
  invisible(NULL)
}

rich_words <- function(line) {
  parts <- strsplit(line, "(\\*)", perl = TRUE)[[1]]
  italic <- FALSE
  out <- list()
  for (part in parts) {
    if (identical(part, "*")) {
      italic <- !italic
      next
    }
    words <- strsplit(part, "\\s+")[[1]]
    words <- words[nzchar(words)]
    if (length(words) == 0) next
    out[[length(out) + 1]] <- data.frame(
      word = words,
      font = if (italic) 3L else 1L,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) return(data.frame(word = character(), font = integer()))
  do.call(rbind, out)
}

draw_justified_rich_line <- function(line, x, y, size = 12, target_w = content_w) {
  words <- rich_words(line)
  if (nrow(words) <= 1) {
    if (nrow(words) == 1) draw_text(words$word[1], x, y, size = size, font = words$font[1])
    return(invisible(NULL))
  }
  word_widths <- mapply(string_width_in, words$word, size = size, font = words$font)
  gap <- max((target_w - sum(word_widths)) / (nrow(words) - 1), 0.04)
  xpos <- x
  for (i in seq_len(nrow(words))) {
    draw_text(words$word[i], xpos, y, size = size, font = words$font[i])
    xpos <- xpos + word_widths[i] + gap
  }
  invisible(NULL)
}

draw_rich_line <- function(line, x, y, size = 12) {
  words <- rich_words(line)
  if (nrow(words) == 0) return(invisible(NULL))
  xpos <- x
  space_w <- string_width_in(" ", size = size, font = 1)
  for (i in seq_len(nrow(words))) {
    draw_text(words$word[i], xpos, y, size = size, font = words$font[i])
    xpos <- xpos + string_width_in(words$word[i], size = size, font = words$font[i]) + space_w
  }
  invisible(NULL)
}

draw_rich_paragraph <- function(text, x, y, size = 12, width = 78, leading = 0.28, first_indent = TRUE) {
  lines <- wrap_lines(text, width)
  if (length(lines) == 0) return(y)
  for (i in seq_along(lines)) {
    xi <- if (i == 1 && first_indent) x + 0.45 else x
    if (i < length(lines)) {
      draw_justified_rich_line(lines[i], xi, y, size = size, target_w = content_w - (xi - x))
    } else {
      draw_rich_line(lines[i], xi, y, size = size)
    }
    y <- y - leading
  }
  y - 0.09
}

draw_paragraph <- function(text, x, y, size = 12, width = 78, leading = 0.28, first_indent = TRUE) {
  lines <- wrap_lines(text, width)
  if (length(lines) == 0) return(y)
  for (i in seq_along(lines)) {
    xi <- if (i == 1 && first_indent) x + 0.45 else x
    if (i < length(lines)) {
      draw_justified_line(lines[i], xi, y, size = size, target_w = content_w - (xi - x))
    } else {
      draw_text(lines[i], xi, y, size = size)
    }
    y <- y - leading
  }
  y - 0.09
}

draw_heading <- function(text, y) {
  draw_text(text, left, y, size = 12, font = 2, col = navy)
  y - 0.36
}

draw_table <- function(df, y, title, size = 8.1, col_widths = NULL, row_h = 0.26) {
  draw_text(title, left, y, size = 12, font = 2, col = navy)
  y <- y - 0.24
  df <- as.data.frame(df)
  headers <- names(df)
  ncol_df <- ncol(df)
  if (is.null(col_widths)) col_widths <- rep(1 / ncol_df, ncol_df)
  col_widths <- col_widths / sum(col_widths) * content_w
  grid::grid.rect(
    x = grid::unit(left + content_w / 2, "in"),
    y = grid::unit(y - row_h / 2, "in"),
    width = grid::unit(content_w, "in"),
    height = grid::unit(row_h, "in"),
    gp = grid::gpar(fill = "white", col = "black")
  )
  for (j in seq_len(ncol_df)) {
    draw_text(headers[j], left + sum(col_widths[seq_len(j - 1)]) + 0.03, y - 0.06, size = size, font = 2)
  }
  y <- y - row_h
  for (i in seq_len(nrow(df))) {
    fill <- if (i %% 2 == 0) "white" else "#FBFCFC"
    grid::grid.rect(
      x = grid::unit(left + content_w / 2, "in"),
      y = grid::unit(y - row_h / 2, "in"),
      width = grid::unit(content_w, "in"),
      height = grid::unit(row_h, "in"),
      gp = grid::gpar(fill = "white", col = "black", lwd = 0.35)
    )
    for (j in seq_len(ncol_df)) {
      draw_text(as.character(df[i, j]), left + sum(col_widths[seq_len(j - 1)]) + 0.03, y - 0.06, size = size)
    }
    y <- y - row_h
  }
  y - 0.18
}

draw_png <- function(path, y, title, max_h = 8.9) {
  draw_text(title, left, y, size = 12, font = 2, col = navy)
  y <- y - 0.28
  img <- png::readPNG(path)
  h <- dim(img)[1]
  w <- dim(img)[2]
  img_h <- min(max_h, content_w * h / w)
  img_w <- img_h * w / h
  if (img_w > content_w) {
    img_w <- content_w
    img_h <- img_w * h / w
  }
  grid::grid.raster(
    img,
    x = grid::unit(left + content_w / 2, "in"),
    y = grid::unit(y - img_h / 2, "in"),
    width = grid::unit(img_w, "in"),
    height = grid::unit(img_h, "in"),
    interpolate = TRUE
  )
}

if (capabilities("cairo")) {
  grDevices::cairo_pdf(pdf_path, width = W, height = H, family = "Times New Roman", onefile = TRUE)
} else {
  grDevices::pdf(pdf_path, width = W, height = H, family = "serif", onefile = TRUE)
}

page <- 1
new_page()
y <- H - top
y <- draw_heading("1 MATERIAIS", y)
y <- draw_rich_paragraph(
  paste0(
    "Foram utilizadas tabelas anuais de dengue em formato *Excel* (DENGUE2020.xlsx a DENGUE2025.xlsx), fornecidas pela Subsecretaria de Saúde de Campos dos Goytacazes, e a base temperatura_diaria_campos.csv, derivada de dados meteorológicos do Instituto Nacional de Meteorologia (INMET). A unidade geográfica analisada foi ",
    get_meta("municipio"), "/", get_meta("uf"), ", código IBGE ", get_meta("codigo_ibge"), "."
  ), left, y
)
y <- draw_paragraph(
  paste0(
    "A variável de desfecho foi o número diário de notificações de dengue, agregado pela data ",
    get_meta("data_evento"), ". A variável de exposição foi a temperatura média diária em graus Celsius. O período comum analisado foi de ",
    get_meta("periodo_inicio"), " a ", get_meta("periodo_fim"), "."
  ), left, y
)
y <- draw_heading("2 METODOLOGIA", y)
y <- draw_rich_paragraph(
  "As bases anuais de dengue foram importadas de planilhas do tipo *Excel* e padronizadas quanto aos nomes das colunas, sendo filtradas para Campos dos Goytacazes pelo código municipal de notificação. As datas de notificação foram convertidas para formato calendário e os registros foram agregados por dia, resultando na contagem diária de notificações.", left, y
)
y <- draw_paragraph(
  "A base meteorológica do INMET foi processada como série diária, com temperatura média, mínima e máxima. A série final manteve apenas o período comum entre as notificações de dengue e a temperatura. Para estatísticas descritivas, a série temporal completa foi mantida; para a tabela de associação e para os cálculos de correlação, foram incluídos somente os dias com casos de dengue.", left, y
)
y <- draw_rich_paragraph(
  "Foi utilizada exclusivamente correlação de Spearman, adequada para contagens assimétricas e possíveis relações monotônicas não lineares. A associação principal foi calculada mês a mês a partir dos pares diários com casos de dengue: em cada mês, os dias com casos formaram o conjunto usado para estimar rho e p-valor entre temperatura média diária e número diário de casos. Meses com menos de quatro pares válidos ou sem variação suficiente foram classificados como não estimáveis.", left, y
)
draw_footer(page)

page <- page + 1
new_page()
y <- H - top
y <- draw_heading("Declaração de uso de inteligência artificial", y)
y <- draw_rich_paragraph(
  "Declaração de uso de inteligência artificial: houve uso de *ChatGPT 5.5* e *Codex* como apoio técnico para organização de *scripts* e estruturação do relatório, revisão de código e geração automatizada dos artefatos locais, em observância à declaração de uso de IA solicitada conforme a Portaria CNPq 2664/2026, disponível em http://memoria2.cnpq.br/web/guest/view/-/journal_content/56_INSTANCE_0oED/10157/23142775. A curadoria dos dados, a interpretação epidemiológica e a responsabilidade final permanecem humanas.", left, y
)
draw_footer(page)

page <- page + 1
new_page()
y <- H - top
y <- draw_heading("3 RESULTADOS", y)
desc_table <- data.frame(
  Indicador = c("Dias analisados", "Casos totais", "Média diária de casos", "Mediana diária de casos", "Máximo diário", "Dias com zero casos (%)", "Temperatura média geral"),
  Valor = c(fmt_int(descritivo$n_dias), fmt_int(descritivo$casos_total), fmt_num(descritivo$casos_media, 2), fmt_num(descritivo$casos_mediana, 2), fmt_int(descritivo$casos_max), fmt_num(100 * descritivo$proporcao_dias_zero, 1), fmt_num(descritivo$temp_media_geral, 2))
)
y <- draw_table(desc_table, y, "Tabela 1 - Resumo descritivo da base diária", size = 8.8)
cor_table <- correlacoes[, intersect(c("analise", "n", "rho", "p_valor"), names(correlacoes))]
cor_table$analise <- c("Diária geral", "Anomalias mensais", "Mensal agregada", "Anual")
cor_table$rho <- fmt_rho(cor_table$rho)
cor_table$p_valor <- fmt_p(cor_table$p_valor)
names(cor_table) <- c("Analise", "n", "Rho", "p-valor")
y <- draw_table(cor_table, y, "Tabela 2 - Correlações gerais auxiliares", size = 7.4, col_widths = c(2.1, 0.6, 0.75, 1.15))
y <- draw_rich_paragraph(
  paste0(
    "A tabela principal de associação foi aglutinada por mês. O maior valor absoluto de rho mensal ocorreu em ",
    maior_abs_rho_mensal$mes_ref[1], " (rho = ", fmt_rho(maior_abs_rho_mensal$rho[1]), ", p = ",
    fmt_p(maior_abs_rho_mensal$p_valor[1]), ")."
  ), left, y, width = 88
)
y <- draw_paragraph(
  "Na tabela diária, rho mensal e p mensal correspondem ao mês daquela data. Eles são iguais dentro de um mesmo mês porque a correlação é calculada sobre o conjunto de dias do mês, não sobre uma única observação isolada.", left, y, width = 88
)
aud_table <- data.frame(
  Item = c("Recálculos gerais", "Recálculos mensais", "Tabela sem dias zero", "Rho/p mensais documentados"),
  Resultado = c(
    paste0(sum(auditoria_recalculo$status == "OK"), "/", nrow(auditoria_recalculo), " OK"),
    paste0(sum(auditoria_recalculo_mensal$status == "OK"), "/", nrow(auditoria_recalculo_mensal), " OK"),
    auditoria_tabela$valor[auditoria_tabela$item == "linhas_com_casos_zero"],
    "Sim"
  )
)
y <- draw_table(aud_table, y, "Tabela 3 - Auditoria interna dos cálculos", size = 7.6, col_widths = c(1.8, 1.4), row_h = 0.24)
draw_footer(page)

monthly_full <- data.frame(
  "Mês" = cor_mensal$mes_ref,
  "n dias" = cor_mensal$n,
  "Rho mensal" = ifelse(is.na(cor_mensal$rho), "NA", fmt_rho(cor_mensal$rho)),
  "p mensal" = fmt_p(cor_mensal$p_valor),
  "Interpretação" = cor_mensal$interpretacao,
  check.names = FALSE
)
rows_per_month_page <- 31
for (start in seq(1, nrow(monthly_full), by = rows_per_month_page)) {
  page <- page + 1
  new_page()
  y <- H - top
  end <- min(start + rows_per_month_page - 1, nrow(monthly_full))
  title <- paste0("Tabela 4 - Correlações mensais de Spearman (linhas ", start, " a ", end, " de ", nrow(monthly_full), ")")
  y <- draw_table(monthly_full[start:end, , drop = FALSE], y, title, size = 6.9, col_widths = c(0.9, 0.55, 0.75, 1.0, 1.0), row_h = 0.22)
  draw_footer(page)
}

rows_per_page <- 36
daily_full <- data.frame(
  "Data" = tabela_diaria$date,
  "Mês" = tabela_diaria$mes_ref,
  "Casos" = tabela_diaria$casos,
  "Temp." = fmt_num(tabela_diaria$temp_media, 2),
  "n mês" = tabela_diaria$n_correlacao_mensal,
  "Rho mensal" = ifelse(is.na(tabela_diaria$rho_spearman_mensal), "NA", fmt_rho(tabela_diaria$rho_spearman_mensal)),
  "p mensal" = fmt_p(tabela_diaria$p_valor_spearman_mensal),
  check.names = FALSE
)
for (start in seq(1, nrow(daily_full), by = rows_per_page)) {
  page <- page + 1
  new_page()
  y <- H - top
  end <- min(start + rows_per_page - 1, nrow(daily_full))
  daily_table <- daily_full[start:end, , drop = FALSE]
  title <- paste0("Tabela 5 - Tabela diária com associação mensal (linhas ", start, " a ", end, " de ", nrow(daily_full), ")")
  y <- draw_table(daily_table, y, title, size = 5.9, col_widths = c(0.95, 0.85, 0.45, 0.55, 0.45, 0.75, 0.95), row_h = 0.205)
  draw_footer(page)
}

figs <- c(
  "01_serie_diaria_casos_temperatura.png",
  "02_dispersao_diaria_spearman.png",
  "03_rho_spearman_mensal.png",
  "04_pvalor_spearman_mensal.png",
  "05_agregado_anual_spearman.png",
  "06_anomalias_mensais_spearman.png"
)
titles <- c(
  "Figura 1 - Série diária de dengue e temperatura",
  "Figura 2 - Associação diária bruta",
  "Figura 3 - Rho de Spearman mensal",
  "Figura 4 - p-valor mensal",
  "Figura 5 - Associação anual exploratória",
  "Figura 6 - Anomalias mensais"
)
for (i in seq_along(figs)) {
  page <- page + 1
  new_page()
  draw_png(file.path(dirs$figuras, figs[i]), H - top, titles[i], max_h = 6.9)
  draw_footer(page)
}

grDevices::dev.off()
message("PDF gerado em: ", pdf_path)
