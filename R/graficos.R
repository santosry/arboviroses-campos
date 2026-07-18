# FUNCOES UTILITARIAS

filter_year <- function(df, ano) {
  if(ano == "Todos") return(df)
  ano_num <- as.numeric(ano)
  df[df$Ano == ano_num, , drop = FALSE]
}

format_number <- function(x) {
  if(length(x) == 0) return(character(0))
  x_num <- suppressWarnings(as.numeric(x))
  resultado <- format(x_num, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  resultado[is.na(x_num) | x_num == 0] <- "0"
  resultado
}

format_percent <- function(x) {
  if(length(x) == 0) return(character(0))
  x_num <- suppressWarnings(as.numeric(x))
  resultado <- format(round(x_num, 1), nsmall = 1, decimal.mark = ",", scientific = FALSE, trim = TRUE)
  resultado[is.na(x_num)] <- "0,0"
  paste0(resultado, "%")
}

format_rate <- function(x) {
  if(length(x) == 0) return(character(0))
  x_num <- suppressWarnings(as.numeric(x))
  resultado <- format(round(x_num, 1), nsmall = 1, decimal.mark = ",", big.mark = ".", scientific = FALSE, trim = TRUE)
  resultado[is.na(x_num) | !is.finite(x_num)] <- "Indisponível"
  resultado
}

adicionar_populacao <- function(df) {
  df %>%
    left_join(populacao_campos, by = "Ano") %>%
    mutate(
      Incidencia_100mil = ifelse(!is.na(Populacao) & Populacao > 0, Confirmado_casos / Populacao * 100000, NA_real_)
    )
}

incidencia_periodo <- function(df) {
  df_pop <- adicionar_populacao(df)
  casos <- sum(df_pop$Confirmado_casos, na.rm = TRUE)
  pop_media <- mean(df_pop$Populacao, na.rm = TRUE)
  if(!is.finite(pop_media) || pop_media <= 0) return(NA_real_)
  casos / pop_media * 100000
}

somar_colunas <- function(df, colunas) {
  colunas <- colunas[colunas %in% names(df)]
  if(length(colunas) == 0) return(0)
  sum(as.matrix(df[, colunas, drop = FALSE]), na.rm = TRUE)
}

qualidade_variavel <- function(df, variavel, colunas_total, colunas_ignoradas) {
  colunas <- colunas_total[colunas_total %in% names(df)]
  total <- somar_colunas(df, colunas)
  ignorado <- somar_colunas(df, colunas_ignoradas)
  ausentes <- sum(sapply(colunas, function(coluna) sum(is.na(df[[coluna]]))))
  problema <- ignorado + ausentes
  percentual <- if(total > 0) problema / total * 100 else NA_real_
  data.frame(
    Variavel = variavel,
    Total = total,
    Ignorado_Branco_Ausente = problema,
    Percentual = percentual,
    stringsAsFactors = FALSE
  )
}

qualidade_dados <- function(df) {
  colunas_idade <- c(
    "Menor_1_Ano", "Faixa_1_4", "Faixa_5_9", "Faixa_10_14", "Faixa_15_19",
    "Criancas_e_jovens", "Faixa_20_39", "Faixa_40_59", "Adultos",
    "Faixa_60_64_anos", "Faixa_65_69_anos", "Faixa_70_79_anos",
    "Faixa_60_64", "Faixa_65_69", "Faixa_70_79", "Faixa_80_mais",
    "Idosos", "Ignorado_idade"
  )
  bind_rows(
    qualidade_variavel(df, "Sexo", c("Masculino", "Feminino", "Ignorado_sexo"), c("Ignorado_sexo")),
    qualidade_variavel(df, "Idade", colunas_idade, c("Ignorado_idade")),
    qualidade_variavel(df, "Escolaridade", c(
      "Ign_Branco_escolaridade", "Analfabeto", "Primeira_a_quarta_serie_incompleta_EF",
      "Quarta_serie_completa_EF", "Quinta_a_oitava_serie_incompleta_EF",
      "Ensino_fundamental_completo", "Ensino_medio_incompleto", "Ensino_medio_completo",
      "Educacao_superior_incompleta", "Educacao_superior_completa", "Nao_se_aplica_escolaridade"
    ), c("Ign_Branco_escolaridade")),
    qualidade_variavel(df, "Raça/cor", c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Ign_Branco_etnia"), c("Ign_Branco_etnia")),
    qualidade_variavel(df, "Gestação", c(
      "Primeiro_trimestre", "Segundo_trimestre", "Terceiro_trimestre",
      "Nao_gestacao", "Nao_se_aplica_gestacao", "Idade_gestacional", "Ign_Branco_gestacao"
    ), c("Ign_Branco_gestacao", "Idade_gestacional"))
  )
}

qualidade_dados_base <- qualidade_dados
qualidade_dados <- function(df) {
  bind_rows(
    qualidade_dados_base(df),
    qualidade_variavel(df, "Classificação final", c(
      "Confirmado_casos", "Descartado_casos", "Inconclusivo_casos", "Ign_Branco_casos"
    ), c("Ign_Branco_casos"))
  )
}

dashboard_qualidade_dados <- function(output_id) {
  div(class = "quality-panel",
    h4("Qualidade dos dados"),
    p("Percentual de registros ignorados, brancos ou ausentes nas principais variáveis do período selecionado."),
    uiOutput(output_id)
  )
}

criar_donut <- function(valores, rotulos, cores, exibir_rotulo_detalhado = FALSE) {
  manter <- valores > 0
  valores <- valores[manter]
  rotulos <- rotulos[manter]
  cores <- cores[manter]
  
  if(length(valores) == 0 || all(valores == 0)) {
    return(plot_ly() %>%
      layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "Sem dados disponíveis para este recorte.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 13, color = "#475569")
        )),
        margin = list(l = 40, r = 40, t = 40, b = 40)
      )
    )
  }
  
  df <- data.frame(
    rotulo = rotulos,
    valor = valores,
    cor = cores
  )
  df$percentual <- (df$valor / sum(df$valor)) * 100
  df$rotulo_hover <- paste0(
    df$rotulo,
    ": ",
    format_percent(df$percentual),
    " (",
    format_number(df$valor),
    ")"
  )
  texto_template <- if(
    exibir_rotulo_detalhado
  ) "%{label}<br>%{value} (%{percent})" else "%{label}"
  
  plot_ly(df, 
          labels = ~rotulo, 
          values = ~valor, 
          type = "pie",
          hole = 0.5,
          textposition = 'outside',
          texttemplate = texto_template,
          textinfo = 'none',
          hovertemplate = "<b>%{label}: %{value} (%{percent})</b><extra></extra>",
          marker = list(colors = cores),
          textfont = list(size = 10, color = "#1e293b"),
          insidetextorientation = 'auto',
      automargin = TRUE,
          pull = 0.04,
          showlegend = FALSE) %>%
        layout(margin = list(l = 60, r = 60, t = 10, b = 80))
}

criar_barras_verticais <- function(valores, rotulos, cores = NULL) {
  if(length(valores) == 0 || all(valores == 0)) {
    return(plot_ly() %>%
      layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "Sem dados disponíveis para este recorte.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 13, color = "#475569")
        )),
        margin = list(l = 40, r = 40, t = 40, b = 40)
      )
    )
  }
  
  if(is.null(cores)) {
    cores <- colorRampPalette(c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B", "#7C6A0A"))(length(valores))
  }
  
  df_plot <- data.frame(
    categoria = factor(rotulos, levels = rotulos),
    casos = valores
  )
  df_plot$rotulo_texto <- ifelse(df_plot$casos > 0, format_number(df_plot$casos), "")
  
  # Calcular altura máxima para dar espaço aos números
  y_max <- max(valores) * 1.15
  
  plot_ly(df_plot) %>%
    add_bars(x = ~categoria, y = ~casos,
             marker = list(color = cores),
             text = ~rotulo_texto,
             textposition = "outside",
          cliponaxis = FALSE,
             textfont = list(size = 11, color = "#1e293b"),
             hovertemplate = "<b>%{x}</b><br>Casos: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickfont = list(size = 9), tickangle = -45, automargin = TRUE),
        yaxis = list(title = "Casos", tickfont = list(size = 10), range = c(0, y_max), automargin = TRUE),
           showlegend = FALSE,
        margin = list(l = 36, r = 12, t = 8, b = 72))
}

# Gráfico de série temporal (Confirmado_casos por ano)
criar_grafico_serie <- function(df, cor, nome) {
  if (nrow(df) == 0 || all(df$Confirmado_casos == 0)) {
    return(plot_ly() %>%
      layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(
          text = paste("Sem dados de", nome, "para o período selecionado."),
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 13, color = "#475569")
        )),
        margin = list(l = 40, r = 40, t = 40, b = 40)
      )
    )
  }
  df_plot <- df
  df_plot$Ano <- as.integer(df_plot$Ano)
  df_plot$rotulo_texto <- ifelse(df_plot$Confirmado_casos > 0, format_number(df_plot$Confirmado_casos), "")
  df_plot$posicao_texto <- ifelse(seq_len(nrow(df_plot)) == which.max(df_plot$Confirmado_casos), "top left", "top center")
  anos_unicos <- sort(unique(df_plot$Ano))
  y_max <- max(df_plot$Confirmado_casos, na.rm = TRUE) * 1.18
  # Gráfico principal
  p <- plot_ly(df_plot, x = ~Ano, y = ~Confirmado_casos, 
          type = "scatter", mode = "lines+markers+text",
          line = list(color = cor, width = 3),
      marker = list(color = cor, size = 9),
          text = ~rotulo_texto,
      textposition = ~posicao_texto,
      cliponaxis = FALSE,
      textfont = list(size = 15, color = "#1e293b"),
          name = "Casos confirmados",
          hovertemplate = "<b>Ano: %{x}</b><br>Casos: %{y}<extra></extra>")
  # Layout sem título, apenas legenda
  p %>% layout(xaxis = list(title = "", tickfont = list(size = 10),
                            tickmode = "array", tickvals = anos_unicos,
                            ticktext = as.character(anos_unicos)),
         yaxis = list(title = "", tickfont = list(size = 11), range = c(0, y_max)),
               hovermode = "x unified",
               showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, y = 1.5, xanchor = "center", font = list(size = 14)),
    margin = list(l = 45, r = 35, t = 30, b = 55))
}

criar_grafico_incidencia <- function(df, cor, nome) {
  df_plot <- adicionar_populacao(df)
  if(all(is.na(df_plot$Incidencia_100mil))) {
    return(plot_ly() %>% layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(list(
        text = "População indisponível para calcular incidência.",
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 13, color = "#475569")
      ))
    ))
  }
  df_plot$rotulo_texto <- ifelse(is.na(df_plot$Incidencia_100mil), "", format_rate(df_plot$Incidencia_100mil))
  y_max <- max(df_plot$Incidencia_100mil, na.rm = TRUE) * 1.18
  plot_ly(df_plot, x = ~Ano, y = ~Incidencia_100mil,
          type = "scatter", mode = "lines+markers+text",
          line = list(color = cor, width = 3),
          marker = list(color = cor, size = 9),
          text = ~rotulo_texto,
          textposition = "top center",
          cliponaxis = FALSE,
          textfont = list(size = 13, color = "#1e293b"),
          name = "Incidência/100 mil",
          hovertemplate = "<b>Ano: %{x}</b><br>Incidência: %{y:.1f}/100 mil<br>População: %{customdata}<extra></extra>",
          customdata = ~format_number(Populacao)) %>%
    layout(
      xaxis = list(title = "", tickfont = list(size = 10), tickmode = "array", tickvals = sort(unique(df_plot$Ano))),
      yaxis = list(title = "Casos por 100 mil habitantes", tickfont = list(size = 10), range = c(0, y_max), automargin = TRUE),
      showlegend = FALSE,
      margin = list(l = 55, r = 25, t = 20, b = 55)
    )
}

criar_grafico_sexo <- function(df, destacar_feminino_2024 = FALSE, organizar_rotulos = FALSE) {
  if(nrow(df) == 0) {
    return(plot_ly() %>% layout(margin = list(l = 40, r = 20, t = 10, b = 80)))
  }

  df_plot <- df %>%
    select(Ano, Masculino, Feminino, Ignorado_sexo) %>%
    mutate(Ano_num = as.numeric(Ano))
  anos_unicos <- sort(unique(df_plot$Ano_num))
  y_base_max <- max(c(df_plot$Masculino, df_plot$Feminino, df_plot$Ignorado_sexo), na.rm = TRUE)
  largura_barra <- 0.22
  deslocamento_masculino <- -0.26
  deslocamento_feminino <- 0
  deslocamento_ignorado <- 0.26
  offset_rotulo <- max(y_base_max * 0.04, 10)
  faixa_baixa_limite <- max(y_base_max * 0.10, 15)
  base_rotulos_baixos <- max(y_base_max * 0.08, 18)
  espacamento_rotulos_baixos <- max(y_base_max * 0.06, 14)
  distancia_minima_rotulos <- max(y_base_max * 0.045, 26)
  deslocamento_ano_alternado <- distancia_minima_rotulos * 0.55
  deslocamento_serie_rotulo <- c("Masculino" = 0, "Feminino" = 1.15, "Ign/Branco" = 2.35)
  df_feminino_2024 <- df_plot %>% filter(destacar_feminino_2024, Ano_num == 2024, Feminino > 0)
  df_rotulos <- bind_rows(
    data.frame(
      Ano_num = df_plot$Ano_num,
      x = df_plot$Ano_num + deslocamento_masculino,
      valor = df_plot$Masculino,
      rotulo = ifelse(df_plot$Masculino > 0, format_number(df_plot$Masculino), ""),
      serie = "Masculino",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Ano_num = df_plot$Ano_num,
      x = df_plot$Ano_num + deslocamento_feminino,
      valor = df_plot$Feminino,
      rotulo = ifelse(
        destacar_feminino_2024 & df_plot$Ano_num == 2024,
        "",
        ifelse(df_plot$Feminino > 0, format_number(df_plot$Feminino), "")
      ),
      serie = "Feminino",
      stringsAsFactors = FALSE
    ),
    data.frame(
      Ano_num = df_plot$Ano_num,
      x = df_plot$Ano_num + deslocamento_ignorado,
      valor = df_plot$Ignorado_sexo,
      rotulo = ifelse(df_plot$Ignorado_sexo > 0, format_number(df_plot$Ignorado_sexo), ""),
      serie = "Ign/Branco",
      stringsAsFactors = FALSE
    )
  ) %>%
    filter(rotulo != "")

  if(nrow(df_rotulos) > 0) {
    if(organizar_rotulos) {
      df_rotulos <- df_rotulos %>%
        group_by(Ano_num) %>%
        mutate(
          ordem_serie = unname(deslocamento_serie_rotulo[serie]),
          rotulo_baixo = valor <= faixa_baixa_limite,
          deslocamento_base_ano = ifelse(first(Ano_num) %% 2 == 0, 0, deslocamento_ano_alternado),
          y_rotulo = ifelse(
            rotulo_baixo,
            base_rotulos_baixos + ordem_serie * (espacamento_rotulos_baixos * 1.8) + deslocamento_base_ano,
            valor + offset_rotulo + ordem_serie * distancia_minima_rotulos + deslocamento_base_ano
          )
        ) %>%
        ungroup()
    } else {
      df_rotulos <- df_rotulos %>% mutate(y_rotulo = valor + offset_rotulo)
    }

    df_rotulos <- bind_rows(lapply(split(df_rotulos, df_rotulos$Ano_num), function(df_ano) {
      df_ano <- df_ano[order(df_ano$y_rotulo, df_ano$valor), ]

      if(nrow(df_ano) > 1) {
        df_ano$y_rotulo <- df_ano$y_rotulo + seq(0, by = distancia_minima_rotulos * 0.45, length.out = nrow(df_ano))

        for(i in 2:nrow(df_ano)) {
          df_ano$y_rotulo[i] <- max(df_ano$y_rotulo[i], df_ano$y_rotulo[i - 1] + distancia_minima_rotulos)
        }
      }

      df_ano
    }))
  }

  y_max <- max(
    c(
      df_plot$Masculino,
      df_plot$Feminino,
      df_plot$Ignorado_sexo,
      if(nrow(df_rotulos) > 0) df_rotulos$y_rotulo,
      if(nrow(df_feminino_2024) > 0) df_feminino_2024$Feminino + (offset_rotulo * 1.25)
    ),
    na.rm = TRUE
  ) * 1.08

  p <- plot_ly() %>%
    add_bars(
      data = df_plot,
      x = ~Ano_num + deslocamento_masculino,
      y = ~Masculino,
      width = largura_barra,
      name = "Masculino",
      marker = list(color = "#2E86AB"),
      hovertext = ~paste0("Ano ", Ano_num, "<br>Masculino: ", format_number(Masculino)),
      hoverinfo = "text"
    ) %>%
    add_bars(
      data = df_plot,
      x = ~Ano_num + deslocamento_feminino,
      y = ~Feminino,
      width = largura_barra,
      name = "Feminino",
      marker = list(color = "#A23B72"),
      hovertext = ~paste0("Ano ", Ano_num, "<br>Feminino: ", format_number(Feminino)),
      hoverinfo = "text"
    ) %>%
    add_bars(
      data = df_plot,
      x = ~Ano_num + deslocamento_ignorado,
      y = ~Ignorado_sexo,
      width = largura_barra,
      name = "Ign/Branco",
      marker = list(color = "#ea8a0e"),
      hovertext = ~paste0("Ano ", Ano_num, "<br>Ign/Branco: ", format_number(Ignorado_sexo)),
      hoverinfo = "text"
    )

  if(nrow(df_rotulos) > 0) {
    p <- p %>%
      add_text(
        data = df_rotulos,
        x = ~x,
        y = ~y_rotulo,
        text = ~rotulo,
        textposition = "top center",
        textfont = list(size = 12, color = "#1e293b"),
        cliponaxis = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
  }

  if(nrow(df_feminino_2024) > 0) {
    p <- p %>%
      add_text(
        data = df_feminino_2024,
        x = ~Ano_num + deslocamento_feminino,
        y = ~Feminino + (offset_rotulo * 1.25),
        text = ~format_number(Feminino),
        textposition = "top center",
        textfont = list(size = 14, color = "#1e293b"),
        cliponaxis = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip",
        inherit = FALSE
      )
  }

  p %>%
    layout(
      barmode = "overlay",
      xaxis = list(
        title = "Ano",
        tickfont = list(size = 10),
        tickmode = "array",
        tickvals = anos_unicos,
        ticktext = as.character(anos_unicos),
        range = c(min(anos_unicos) - 0.65, max(anos_unicos) + 0.65),
        automargin = TRUE
      ),
      yaxis = list(title = "", tickfont = list(size = 10), range = c(0, y_max), automargin = TRUE),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.24,
        xanchor = "center",
        font = list(size = 12)
      ),
      margin = list(l = 36, r = 12, t = 64, b = 52)
    )
}

criar_grafico_faixa_etaria <- function(df) {
  if(nrow(df) == 0) {
    return(plot_ly() %>% layout(margin = list(l = 40, r = 15, t = 10, b = 90)))
  }

  colunas_criancas_detalhadas <- c("Menor_1_Ano", "Faixa_1_4", "Faixa_5_9", "Faixa_10_14", "Faixa_15_19")
  colunas_adultos_detalhadas <- c("Faixa_20_39", "Faixa_40_59")
  colunas_idosos_detalhadas <- c("Faixa_60_64_anos", "Faixa_65_69_anos", "Faixa_70_79_anos", "Faixa_80_mais")
  colunas_idosos_detalhadas_alt <- c("Faixa_60_64", "Faixa_65_69", "Faixa_70_79", "Faixa_80_mais")
  if(all(colunas_criancas_detalhadas %in% names(df))) {
    colunas_faixa <- colunas_criancas_detalhadas
  } else {
    colunas_faixa <- c("Criancas_e_jovens")
  }
  if(all(colunas_adultos_detalhadas %in% names(df))) {
    colunas_faixa <- c(colunas_faixa, colunas_adultos_detalhadas)
  } else if("Adultos" %in% names(df)) {
    colunas_faixa <- c(colunas_faixa, "Adultos")
  }
  if(all(colunas_idosos_detalhadas %in% names(df))) {
    colunas_faixa <- c(colunas_faixa, colunas_idosos_detalhadas)
  } else if(all(colunas_idosos_detalhadas_alt %in% names(df))) {
    colunas_faixa <- c(colunas_faixa, colunas_idosos_detalhadas_alt)
  } else if("Idosos" %in% names(df)) {
    colunas_faixa <- c(colunas_faixa, "Idosos")
  }
  colunas_faixa <- c(colunas_faixa, "Ignorado_idade")

  mapa_categorias <- c(
    "Menor_1_Ano" = "<1",
    "Faixa_1_4" = "1-4",
    "Faixa_5_9" = "5-9",
    "Faixa_10_14" = "10-14",
    "Faixa_15_19" = "15-19",
    "Criancas_e_jovens" = "0-19",
    "Faixa_20_39" = "20-39",
    "Faixa_40_59" = "40-59",
    "Adultos" = "20-59",
    "Idosos" = "60+",
    "Faixa_60_64_anos" = "60-64",
    "Faixa_65_69_anos" = "65-69",
    "Faixa_70_79_anos" = "70-79",
    "Faixa_60_64" = "60-64",
    "Faixa_65_69" = "65-69",
    "Faixa_70_79" = "70-79",
    "Faixa_80_mais" = "80+",
    "Ignorado_idade" = "Ign/Branco"
  )
  rotulos <- unname(mapa_categorias[colunas_faixa])
  valores <- sapply(colunas_faixa, function(coluna) sum(df[[coluna]], na.rm = TRUE))

  if(all(valores <= 0)) {
    return(
      plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "Sem valores positivos para exibir.",
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 13, color = "#475569")
            )
          ),
          margin = list(l = 40, r = 20, t = 40, b = 40)
        )
    )
  }
  
  cores_categorias <- c(
    "<1" = "#F6C85F",
    "1-4" = "#FF8C42",
    "5-9" = "#E4572E",
    "10-14" = "#76B041",
    "15-19" = "#4B8F8C",
    "0-19" = "#F18F01",
    "20-39" = "#C73E1D",
    "40-59" = "#2E86AB",
    "20-59" = "#C73E1D",
    "60-64" = "#3B1F2B",
    "65-69" = "#1ABC9C",
    "70-79" = "#7C6A0A",
    "80+" = "#A23B72",
    "60+" = "#3B1F2B",
    "Ign/Branco" = "#95A5A6"
  )

  criar_barras_verticais(
    valores = valores,
    rotulos = rotulos,
    cores = unname(cores_categorias[rotulos])
  ) %>%
    layout(
      xaxis = list(title = "", tickfont = list(size = 9), tickangle = -45),
      yaxis = list(title = "", tickfont = list(size = 10)),
      margin = list(l = 45, r = 20, t = 10, b = 95)
    )
}

# Gráfico de etnia (rosca)
criar_grafico_etnia <- function(df) {
  valores <- c(sum(df$Branca), sum(df$Preta), sum(df$Amarela), sum(df$Parda), sum(df$Indigena), sum(df$Ign_Branco_etnia))
  rotulos <- c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ign/Branco")
  cores <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B", "#7C6A0A")
  criar_donut(valores, rotulos, cores, exibir_rotulo_detalhado = TRUE)
}

criar_grafico_escolaridade <- function(df) {
  rotulos <- c(
    "Ign/Branco esc.", "Analfabeto", "EF 1-4 inc.",
    "EF 4 comp.", "EF 5-8 inc.", "Fund. completo",
    "Médio incomp.", "Médio completo",
    "Sup. incompleta", "Sup. completa",
    "Não se aplica"
  )
  valores <- c(
    sum(df$Ign_Branco_escolaridade),
    sum(df$Analfabeto),
    sum(df$Primeira_a_quarta_serie_incompleta_EF),
    sum(df$Quarta_serie_completa_EF),
    sum(df$Quinta_a_oitava_serie_incompleta_EF),
    sum(df$Ensino_fundamental_completo),
    sum(df$Ensino_medio_incompleto),
    sum(df$Ensino_medio_completo),
    sum(df$Educacao_superior_incompleta),
    sum(df$Educacao_superior_completa),
    sum(df$Nao_se_aplica_escolaridade)
  )
  cores <- c("#7C6A0A", "#95A5A6", "#2E86AB", "#A23B72", "#F18F01", "#C73E1D", 
             "#3B1F2B", "#1ABC9C", "#9B59B6", "#E67E22", "#3498DB")
  
  criar_barras_horizontais <- function(valores, rotulos, cores) {
    df_plot <- data.frame(
      categoria = factor(rotulos, levels = rev(rotulos)),
      casos = valores
    )
    x_max <- max(df_plot$casos, na.rm = TRUE)
    if(!is.finite(x_max) || x_max <= 0) x_max <- 1
    df_plot$rotulo_texto <- ifelse(df_plot$casos > 0, format_number(df_plot$casos), "")
    
    plot_ly(df_plot) %>%
      add_bars(x = ~casos, y = ~categoria,
               orientation = 'h',
               marker = list(color = cores),
               text = ~rotulo_texto,
               textposition = "outside",
               cliponaxis = FALSE,
               textfont = list(size = 13, color = "#1e293b"),
               hovertemplate = "<b>%{y}</b><br>Casos: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickfont = list(size = 12),
              range = c(0, x_max * 1.3)),
         yaxis = list(title = "", tickfont = list(size = 11), automargin = TRUE),
         showlegend = FALSE,
         margin = list(l = 165, r = 70, t = 12, b = 34),
         bargap = 0.24)
  }
  
  criar_barras_horizontais(valores, rotulos, cores)
}

criar_grafico_gestacao <- function(df) {
  rotulos <- c(
    "Ign/Branco", "Idade gestacional", "Não se aplica",
    "Não gestante", "3º trimestre", "2º trimestre", "1º trimestre"
  )
  valores <- c(
    sum(df$Ign_Branco_gestacao),
    sum(df$Idade_gestacional),
    sum(df$Nao_se_aplica_gestacao),
    sum(df$Nao_gestacao),
    sum(df$Terceiro_trimestre),
    sum(df$Segundo_trimestre),
    sum(df$Primeiro_trimestre)
  )
  cores <- c("#7C6A0A", "#95A5A6", "#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B")
  
  criar_barras_verticais(valores, rotulos, cores)
}

opcoes_visualizacao <- c(
  "Série temporal" = "serie",
  "Incidência por 100 mil" = "incidencia",
  "Sexo" = "sexo",
  "Raça/cor" = "etnia",
  "Faixa etária" = "faixa",
  "Gestação" = "gestacao",
  "Escolaridade" = "escolaridade"
)

tabela_visualizacao <- function(df, visualizacao) {
  if(visualizacao == "serie") {
    return(df %>% select(Ano, Confirmado_casos, Descartado_casos, Inconclusivo_casos, Ign_Branco_casos))
  }
  if(visualizacao == "incidencia") {
    return(adicionar_populacao(df) %>% select(Ano, Confirmado_casos, Populacao, Incidencia_100mil, Fonte_populacao))
  }
  if(visualizacao == "sexo") {
    return(df %>% select(Ano, Masculino, Feminino, Ignorado_sexo))
  }
  if(visualizacao == "etnia") {
    valores <- c(sum(df$Branca), sum(df$Preta), sum(df$Amarela), sum(df$Parda), sum(df$Indigena), sum(df$Ign_Branco_etnia))
    return(data.frame(Categoria = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ign/Branco"), Casos = valores))
  }
  if(visualizacao == "faixa") {
    valores <- c(
      sum(df$Menor_1_Ano), sum(df$Faixa_1_4), sum(df$Faixa_5_9), sum(df$Faixa_10_14),
      sum(df$Faixa_15_19), sum(df$Faixa_20_39), sum(df$Faixa_40_59),
      sum(if("Faixa_60_64" %in% names(df)) df$Faixa_60_64 else df$Faixa_60_64_anos),
      sum(if("Faixa_65_69" %in% names(df)) df$Faixa_65_69 else df$Faixa_65_69_anos),
      sum(if("Faixa_70_79" %in% names(df)) df$Faixa_70_79 else df$Faixa_70_79_anos),
      sum(df$Faixa_80_mais), sum(df$Ignorado_idade)
    )
    return(data.frame(Categoria = c("<1", "1-4", "5-9", "10-14", "15-19", "20-39", "40-59", "60-64", "65-69", "70-79", "80+", "Ign/Branco"), Casos = valores))
  }
  if(visualizacao == "gestacao") {
    valores <- c(sum(df$Primeiro_trimestre), sum(df$Segundo_trimestre), sum(df$Terceiro_trimestre), sum(df$Nao_gestacao), sum(df$Nao_se_aplica_gestacao), sum(df$Idade_gestacional), sum(df$Ign_Branco_gestacao))
    return(data.frame(Categoria = c("1º trimestre", "2º trimestre", "3º trimestre", "Não gestante", "Não se aplica", "Idade gest. ignorada", "Ign/Branco"), Casos = valores))
  }
  valores <- c(
    sum(df$Ign_Branco_escolaridade), sum(df$Analfabeto), sum(df$Primeira_a_quarta_serie_incompleta_EF),
    sum(df$Quarta_serie_completa_EF), sum(df$Quinta_a_oitava_serie_incompleta_EF),
    sum(df$Ensino_fundamental_completo), sum(df$Ensino_medio_incompleto),
    sum(df$Ensino_medio_completo), sum(df$Educacao_superior_incompleta),
    sum(df$Educacao_superior_completa), sum(df$Nao_se_aplica_escolaridade)
  )
  data.frame(
    Categoria = c("Ign/Branco", "Analfabeto", "EF 1-4 inc.", "EF 4 comp.", "EF 5-8 inc.", "Fund. completo", "Médio incomp.", "Médio completo", "Sup. incompleta", "Sup. completa", "Não se aplica"),
    Casos = valores
  )
}

tabela_analitica_doenca <- function(df, nome_doenca, periodo_label = NULL) {
  df_pop <- adicionar_populacao(df)
  qualidade <- qualidade_dados(df)
  qualidade_resumo <- qualidade %>%
    transmute(
      Indicador = paste0("Qualidade_", gsub("[^A-Za-z0-9]+", "_", Variavel), "_pct"),
      Valor = round(Percentual, 2)
    )
  
  resumo <- data.frame(
    Doenca = nome_doenca,
    Periodo = if(is.null(periodo_label)) paste(min(df$Ano), max(df$Ano), sep = "-") else periodo_label,
    Unidade_analise = APP_UNIDADE_ANALISE,
    Fonte = if(nome_doenca %in% c("Dengue", "Zika")) "SINAN/SVS via microdatasus" else "SINAN/SVS em tabela agregada",
    Casos_confirmados = sum(df$Confirmado_casos, na.rm = TRUE),
    Casos_descartados = sum(df$Descartado_casos, na.rm = TRUE),
    Casos_inconclusivos = sum(df$Inconclusivo_casos, na.rm = TRUE),
    Ignorado_branco_classificacao = sum(df$Ign_Branco_casos, na.rm = TRUE),
    Incidencia_periodo_100mil = incidencia_periodo(df),
    Populacao_media = mean(df_pop$Populacao, na.rm = TRUE),
    Data_atualizacao = as.character(APP_DATA_ATUALIZACAO),
    stringsAsFactors = FALSE
  )
  
  qualidade_wide <- as.data.frame(t(qualidade_resumo$Valor), stringsAsFactors = FALSE)
  names(qualidade_wide) <- qualidade_resumo$Indicador
  cbind(resumo, qualidade_wide)
}

nota_metodologica_doenca <- function(nome_doenca, fonte, criterio_confirmacao, limitacoes) {
  div(class = "method-note",
    h4("Nota metodológica"),
    tags$dl(
      tags$dt("Fonte"),
      tags$dd(fonte),
      tags$dt("Período"),
      tags$dd(APP_PERIODO_PADRAO),
      tags$dt("Unidade de análise"),
      tags$dd(APP_UNIDADE_ANALISE),
      tags$dt("Critério de confirmação"),
      tags$dd(criterio_confirmacao),
      tags$dt("Limitações"),
      tags$dd(limitacoes)
    )
  )
}

painel_correspondencia_bairros <- function() {
  div(class = "download-panel",
    h4("Correspondência bairro-planilha vs bairro-geobr"),
    p("Tabela de auditoria da vinculação espacial. Bairros sem correspondência indicam divergências de grafia, localidade sem polígono na malha de 2010 ou necessidade de ajuste manual."),
    downloadButton("dengue_download_correspondencia_bairros", "Baixar correspondência CSV"),
    DTOutput("dengue_correspondencia_bairros")
  )
}

metadados_figura <- function(nome_doenca, visualizacao, periodo_label = "Período analisado") {
  nomes <- c(
    serie = "série temporal dos casos confirmados",
    incidencia = "incidência anual por 100 mil habitantes",
    sexo = "distribuição por sexo e ano",
    etnia = "distribuição por raça/cor",
    faixa = "distribuição por faixa etária",
    gestacao = "situação gestacional",
    escolaridade = "distribuição por escolaridade"
  )
  o_que <- paste0("O que mostra: ", nome_doenca, " - ", nomes[[visualizacao]], ".")
  interpretar <- switch(
    visualizacao,
    serie = "Como interpretar: observe picos, quedas e concentração temporal de casos confirmados.",
    incidencia = "Como interpretar: compare a carga anual padronizada pela população estimada do município.",
    sexo = "Como interpretar: compare a distribuição entre masculino, feminino e ignorado/branco ao longo dos anos.",
    etnia = "Como interpretar: avalie a composição dos registros segundo raça/cor e o peso de ignorado/branco.",
    faixa = "Como interpretar: identifique grupos etários com maior carga registrada no período selecionado.",
    gestacao = "Como interpretar: observe registros relacionados à gestação e categorias não aplicáveis ou ignoradas.",
    escolaridade = "Como interpretar: avalie padrões de escolaridade sempre considerando incompletude de preenchimento."
  )
  atencao <- "Atenção metodológica: dados notificados não equivalem à incidência real; subnotificação e campos ignorados/brancos podem alterar a interpretação."
  fonte <- if(visualizacao == "incidencia") {
    "Fonte: SINAN/SVS; população IBGE/SIDRA, tabela 6579, variável 9324."
  } else {
    "Fonte: SINAN/SVS."
  }
  paste(o_que, interpretar, atencao, fonte, periodo_label, sep = "\n")
}

grafico_publicacao <- function(df, visualizacao, titulo, nome_doenca = titulo, periodo_label = "Período analisado") {
  tab <- tabela_visualizacao(df, visualizacao)
  caption <- metadados_figura(nome_doenca, visualizacao, periodo_label)
  tema_publicacao <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 17, color = "#1A2535"),
      plot.subtitle = element_text(size = 11, color = "#475569"),
      plot.caption = element_text(size = 9, color = "#475569", hjust = 0, lineheight = 1.15),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )

  if(visualizacao == "serie") {
    tab_long <- tab %>% tidyr::pivot_longer(-Ano, names_to = "Categoria", values_to = "Casos")
    return(
      ggplot(tab_long, aes(x = Ano, y = Casos, color = Categoria, group = Categoria)) +
        geom_line(linewidth = 0.9) + geom_point(size = 2.2) +
        labs(title = titulo, subtitle = periodo_label, x = "Ano", y = "Casos", caption = caption) +
        tema_publicacao
    )
  }
  if(visualizacao == "sexo") {
    tab_long <- tab %>% tidyr::pivot_longer(-Ano, names_to = "Categoria", values_to = "Casos")
    return(
      ggplot(tab_long, aes(x = factor(Ano), y = Casos, fill = Categoria)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.68) +
        geom_text(aes(label = ifelse(Casos > 0, format_number(Casos), "")), position = position_dodge(width = 0.75), vjust = -0.25, size = 3.1, color = "#1e293b") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
        labs(title = titulo, subtitle = periodo_label, x = "Ano", y = "Casos", caption = caption) +
        tema_publicacao
    )
  }
  if(visualizacao == "incidencia") {
    tab$rotulo <- format_rate(tab$Incidencia_100mil)
    return(
      ggplot(tab, aes(x = Ano, y = Incidencia_100mil)) +
        geom_line(linewidth = 1, color = "#1B3A6B") +
        geom_point(size = 2.5, color = "#1B3A6B") +
        geom_text(aes(label = rotulo), vjust = -0.7, size = 3.4, color = "#1e293b") +
        scale_x_continuous(breaks = tab$Ano) +
        labs(title = titulo, subtitle = periodo_label, x = "Ano", y = "Casos por 100 mil habitantes", caption = caption) +
        tema_publicacao
    )
  }
  ggplot(tab, aes(x = reorder(Categoria, Casos), y = Casos)) +
    geom_col(fill = "#1B3A6B") +
    geom_text(aes(label = format_number(Casos)), hjust = -0.08, size = 3.3, color = "#1e293b") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
    labs(title = titulo, subtitle = periodo_label, x = "", y = "Casos", caption = caption) +
    tema_publicacao
}

painel_downloads <- function(prefixo) {
  div(class = "download-panel",
      fluidRow(
        column(4, selectInput(paste0(prefixo, "_vis"), "Visualização para tabela/download", choices = opcoes_visualizacao)),
        column(3, selectInput(paste0(prefixo, "_fmt"), "Formato do plot", choices = c("PDF" = "pdf", "TIFF" = "tiff", "SVG" = "svg"))),
        column(2, br(), downloadButton(paste0(prefixo, "_download_plot"), "Baixar plot")),
        column(3, br(), downloadButton(paste0(prefixo, "_download_table"), "Baixar tabela CSV"))
      ),
      fluidRow(
        column(4, br(), downloadButton(paste0(prefixo, "_download_analitica"), "Baixar tabela analitica CSV"))
      ),
      DTOutput(paste0(prefixo, "_tabela"))
  )
}

botao_download_grafico <- function(output_id) {
  div(
    class = "plot-download",
    downloadButton(output_id, "Baixar em alta resolução")
  )
}

perfil_equipe <- function(nome, subtitulo, mini_curriculo, lattes, iniciais) {
  div(class = "team-card",
    div(class = "team-avatar", iniciais),
    h4(nome),
    div(class = "team-role", subtitulo),
    p(class = "team-bio", mini_curriculo),
    div(class = "team-links",
      tags$a(
        href = lattes,
        target = "_blank",
        class = "team-link-btn",
        tags$i(class = "fa fa-graduation-cap"),
        "Lattes"
      )
    )
  )
}

criar_grafico_qualidade_temporal <- function(df_list, agravo = NULL) {
  # Produz serie temporal de completude por variavel
  # df_list: lista nomeada de dataframes (ex: dados$Dengue, dados$Chikungunya)
  if (!is.null(agravo) && agravo != "Todos") {
    df_list <- df_list[names(df_list) == agravo]
  }
  if (length(df_list) == 0) {
    return(plot_ly() %>% layout(
      annotations = list(list(
        text = "Selecione um agravo para visualizar.",
        x = 0.5, y = 0.5, showarrow = FALSE
      ))
    ))
  }

  comp <- dplyr::bind_rows(lapply(names(df_list), function(nome) {
    df <- df_list[[nome]]
    if (nrow(df) == 0) return(data.frame())
    q <- qualidade_dados(df)
    q$Agravo <- nome
    q$Ano <- if (nrow(df) == 1) as.character(df$Ano[1]) else paste(min(df$Ano), max(df$Ano), sep = "-")
    q
  }))

  if (nrow(comp) == 0) {
    return(plot_ly() %>% layout(
      annotations = list(list(
        text = "Dados insuficientes para gerar painel de qualidade.",
        x = 0.5, y = 0.5, showarrow = FALSE
      ))
    ))
  }

  cores <- c(
    "Sexo" = "#2E86AB",
    "Idade" = "#C73E1D",
    "Escolaridade" = "#A23B72",
    "Raça/cor" = "#F18F01",
    "Gestação" = "#1ABC9C",
    "Classificacao final" = "#3B1F2B"
  )

  comp$Variavel_label <- comp$Variavel
  comp$hover_text <- paste0(
    comp$Variavel_label, "<br>",
    comp$Agravo, "<br>",
    "Ignorado/Branco: ", format_percent(comp$Percentual)
  )

  plot_ly(comp,
    x = ~Variavel_label, y = ~Percentual, color = ~Agravo,
    type = "bar",
    text = ~paste0(format_percent(Percentual)),
    textposition = "outside",
    cliponaxis = FALSE,
    textfont = list(size = 12),
    hovertemplate = "%{hovertext}<extra></extra>",
    hovertext = ~hover_text,
    marker = list(line = list(width = 0.5, color = "#ffffff"))
  ) %>%
    layout(
      barmode = "group",
      xaxis = list(title = "", tickfont = list(size = 11)),
      yaxis = list(title = "% Ignorado/Branco", range = c(0, max(c(comp$Percentual, 1), na.rm = TRUE) * 1.25), ticksuffix = "%"),
      legend = list(orientation = "h", y = 1.2, x = 0.5, xanchor = "center"),
      margin = list(l = 50, r = 30, t = 10, b = 80)
    )
}

texto_contexto_dengue <- div(class = "context-box",
  tags$strong("Guia de interpretação: "),
  "use os gráficos de dengue para observar concentração etária, distribuição por sexo, completude de escolaridade/raça e mudanças temporais de confirmação. Campos ignorados/brancos devem ser interpretados como sinal de qualidade de preenchimento, não como categoria biológica."
)

