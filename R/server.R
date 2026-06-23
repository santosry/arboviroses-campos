# ============================================================
# SERVER
# ============================================================

# Thresholds centralizados de qualidade (usados em alertas e auditoria)
QUALIDADE_THRESHOLD_CRITICO  <- 60   # % ignorado/branco acima disso = critico
QUALIDADE_THRESHOLD_ALERTA   <- 40   # % ignorado/branco acima disso = alerta
QUALIDADE_THRESHOLD_ATENCAO  <- 30   # % ignorado/branco acima disso = atencao

server <- function(input, output, session) {
  
  observeEvent(input$sidebarItemExpanded, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = input$sidebarItemExpanded,
      acao = "navegacao",
      detalhes = paste("Navegou para aba:", input$sidebarItemExpanded)
    ))
  })

  # Filtro global sincronizado com filtros por doenca
  observeEvent(input$global_ano, {
    updateSelectInput(session, "chik_ano", selected = input$global_ano)
    updateSelectInput(session, "dengue_ano", selected = input$global_ano)
    updateSelectInput(session, "zika_ano", selected = input$global_ano)
  })
  
  ano_selecionado <- function(input_id) {
    # Usa filtro global como preferencia; se per-tab diferir, usa per-tab
    valor <- input[[input_id]]
    if(is.null(valor)) input$global_ano else valor
  }

  dados_longos_home <- reactive({
    dplyr::bind_rows(lapply(names(dados), function(agravo) {
      df <- dados[[agravo]]
      df$Agravo <- agravo
      df
    }))
  })

  resumo_home <- reactive({
    df <- dados_longos_home()
    por_agravo <- df %>%
      group_by(Agravo) %>%
      summarise(
        Casos = sum(Confirmado_casos, na.rm = TRUE),
        Obitos = sum(Obitos_Agr, na.rm = TRUE),
        .groups = "drop"
      )
    por_ano <- df %>%
      group_by(Ano) %>%
      summarise(Casos = sum(Confirmado_casos, na.rm = TRUE), .groups = "drop")
    list(
      total = sum(por_agravo$Casos, na.rm = TRUE),
      obitos = sum(por_agravo$Obitos, na.rm = TRUE),
      pico = por_ano$Ano[which.max(por_ano$Casos)],
      predominante = por_agravo$Agravo[which.max(por_agravo$Casos)],
      incidencia = incidencia_periodo(df),
      atualizado = if (file.exists(file.path("data", "app_cache", "metadata.json")) && requireNamespace("jsonlite", quietly = TRUE)) {
        meta <- jsonlite::read_json(file.path("data", "app_cache", "metadata.json"))
        substr(meta$atualizado_em, 1, 10)
      } else {
        as.character(APP_DATA_ATUALIZACAO)
      }
    )
  })

  criar_home_card <- function(campo, rotulo, formatador = identity) {
    renderUI({
      valor <- resumo_home()[[campo]]
      div(class = "custom-card",
          div(class = "card-value", formatador(valor)),
          div(class = "card-label", rotulo)
      )
    })
  }

  output$home_card_total <- criar_home_card("total", "TOTAL DE CASOS", format_number)
  output$home_card_incidencia <- criar_home_card("incidencia", "INCIDENCIA/100 MIL", function(x) if(is.finite(x)) format_number(round(x)) else "Indisponivel")
  output$home_card_obitos <- criar_home_card("obitos", "OBITOS", format_number)
  output$home_card_pico <- criar_home_card("pico", "ANO DE PICO", as.character)
  output$home_card_predominante <- criar_home_card("predominante", "AGRAVO PREDOMINANTE", as.character)
  output$home_card_atualizacao <- criar_home_card("atualizado", "ULTIMA ATUALIZACAO", as.character)

  tabela_comparador_home <- reactive({
    variavel <- input$home_comparador_variavel
    if(is.null(variavel)) variavel <- "ano"
    df <- dados_longos_home()

    if(variavel == "ano") {
      return(df %>% group_by(Agravo, Categoria = as.character(Ano)) %>% summarise(Casos = sum(Confirmado_casos, na.rm = TRUE), .groups = "drop"))
    }
    if(variavel == "sexo") {
      return(df %>% transmute(Agravo, Masculino, Feminino, Ignorado = Ignorado_sexo) %>% tidyr::pivot_longer(-Agravo, names_to = "Categoria", values_to = "Casos") %>% group_by(Agravo, Categoria) %>% summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop"))
    }
    if(variavel == "raca") {
      return(df %>% transmute(Agravo, Branca, Preta, Amarela, Parda, Indigena, Ignorado = Ign_Branco_etnia) %>% tidyr::pivot_longer(-Agravo, names_to = "Categoria", values_to = "Casos") %>% group_by(Agravo, Categoria) %>% summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop"))
    }
    if(variavel == "gestacao") {
      cols <- intersect(names(df), c("Primeiro_trimestre", "Segundo_trimestre", "Terceiro_trimestre", "Nao_gestacao", "Nao_se_aplica_gestacao", "Ign_Branco_gestacao"))
      return(df %>% select(Agravo, all_of(cols)) %>% tidyr::pivot_longer(-Agravo, names_to = "Categoria", values_to = "Casos") %>% group_by(Agravo, Categoria) %>% summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop"))
    }
    cols <- intersect(names(df), c("Menor_1_Ano", "Faixa_1_4", "Faixa_5_9", "Faixa_10_14", "Faixa_15_19", "Faixa_20_39", "Faixa_40_59", "Faixa_60_64", "Faixa_65_69", "Faixa_70_79", "Faixa_80_mais", "Ignorado_idade"))
    df %>% select(Agravo, all_of(cols)) %>% tidyr::pivot_longer(-Agravo, names_to = "Categoria", values_to = "Casos") %>% group_by(Agravo, Categoria) %>% summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop")
  })

  output$home_comparador_plot <- renderPlotly({
    tab <- tabela_comparador_home()
    plot_ly(tab, x = ~Categoria, y = ~Casos, color = ~Agravo, type = "bar") %>%
      layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Casos confirmados"))
  })

  output$home_comparador_tabela <- renderDT({
    datatable(tabela_comparador_home(), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  # Painel de qualidade na pagina inicial
  dados_filtrados_global <- reactive({
    ano <- input$global_ano
    if (is.null(ano) || ano == "Todos") {
      return(dados)
    }
    lapply(dados, function(df) df[df$Ano == as.numeric(ano), , drop = FALSE])
  })

  output$home_qualidade_plot <- renderPlotly({
    agravo <- input$home_qualidade_agravo
    if (is.null(agravo)) agravo <- "Todos"
    criar_grafico_qualidade_temporal(dados_filtrados_global(), agravo)
  })

  output$home_alertas_qualidade <- renderUI({
    agravo <- input$home_qualidade_agravo
    df_list <- dados_filtrados_global()
    if (!is.null(agravo) && agravo != "Todos") {
      df_list <- df_list[names(df_list) == agravo]
    }

    alertas <- lapply(names(df_list), function(nome) {
      df <- df_list[[nome]]
      if (nrow(df) == 0) return(NULL)
      q <- qualidade_dados(df)
      criticos <- q[q$Percentual > QUALIDADE_THRESHOLD_ATENCAO | is.na(q$Percentual), ]
      if (nrow(criticos) == 0) return(NULL)

      lapply(seq_len(nrow(criticos)), function(i) {
        v <- criticos$Variavel[i]
        p <- criticos$Percentual[i]
        nivel <- if (is.na(p) || p > QUALIDADE_THRESHOLD_CRITICO) "critico" else if (p > QUALIDADE_THRESHOLD_ALERTA) "alerta" else "atencao"
        icone <- if (nivel == "critico") "\u26A0\uFE0F" else if (nivel == "alerta") "\u2757" else "\u2139\uFE0F"
        cores <- c(critico = "#DC2626", alerta = "#D97706", atencao = "#2563EB")
        div(class = "quality-card", style = paste0("border-left: 3px solid ", cores[nivel]),
          div(class = "quality-label", paste(icone, nome, "—", v)),
          div(class = "quality-value", if (is.na(p)) "?" else paste0(format_percent(p))),
          div(class = "quality-note", if (nivel == "critico") "Campo critico: inferencias nao confiaveis" else if (nivel == "alerta") "Atencao: alta incompletude" else "Verificar preenchimento")
        )
      })
    })

    alertas <- unlist(alertas, recursive = FALSE)
    if (length(alertas) == 0) {
      return(div(class = "quality-card", style = "grid-column: 1 / -1; text-align: center;",
        div(class = "quality-label", paste0("\u2705 Nenhum campo com incompletude superior a ", QUALIDADE_THRESHOLD_ATENCAO, "% no periodo selecionado."))
      ))
    }
    alertas
  })
  
  observeEvent(input$chik_ano, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "chikungunya",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", input$chik_ano)
    ))
  })
  observeEvent(input$dengue_ano, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "dengue",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", input$dengue_ano)
    ))
  })
  observeEvent(input$zika_ano, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "zika",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", input$zika_ano)
    ))
  })
  
  chik_filtrado <- reactive({ filter_year(dados$Chikungunya, ano_selecionado("chik_ano")) })
  dengue_filtrado <- reactive({ filter_year(dados$Dengue, ano_selecionado("dengue_ano")) })
  zika_filtrado <- reactive({ filter_year(dados$Zika, ano_selecionado("zika_ano")) })
  dengue_bairros_filtrado <- reactive({
    df <- dengue_bairros
    if(nrow(df) == 0) return(df)
    ano <- ano_selecionado("dengue_ano")
    if(ano != "Todos") df <- df[df$Ano == as.numeric(ano), , drop = FALSE]
    df %>%
      group_by(NM_BAIRRO) %>%
      summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Casos), NM_BAIRRO)
  })

  registrar_downloads <- function(prefixo, df_reativo, nome_doenca) {
    output[[paste0(prefixo, "_tabela")]] <- renderDT({
      visualizacao <- input[[paste0(prefixo, "_vis")]]
      if(is.null(visualizacao)) visualizacao <- "serie"
      datatable(
        tabela_visualizacao(df_reativo(), visualizacao),
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })
    
    output[[paste0(prefixo, "_download_table")]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", input[[paste0(prefixo, "_vis")]], "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        visualizacao <- input[[paste0(prefixo, "_vis")]]
        if(is.null(visualizacao)) visualizacao <- "serie"
        write.csv(tabela_visualizacao(df_reativo(), visualizacao), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    output[[paste0(prefixo, "_download_analitica")]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_tabela_analitica_", Sys.Date(), ".csv")
      },
      content = function(file) {
        ano <- ano_selecionado(paste0(prefixo, "_ano"))
        periodo_label <- if(ano == "Todos") "Todos" else ano
        write.csv(
          tabela_analitica_doenca(df_reativo(), tools::toTitleCase(nome_doenca), periodo_label),
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
      }
    )
    output[[paste0(prefixo, "_download_plot")]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", input[[paste0(prefixo, "_vis")]], "_", Sys.Date(), ".", input[[paste0(prefixo, "_fmt")]])
      },
      content = function(file) {
        visualizacao <- input[[paste0(prefixo, "_vis")]]
        formato <- input[[paste0(prefixo, "_fmt")]]
        if(is.null(visualizacao)) visualizacao <- "serie"
        if(is.null(formato)) formato <- "pdf"
        df_plot <- df_reativo()
        periodo_label <- if(length(unique(df_plot$Ano)) == 1) paste("Ano:", unique(df_plot$Ano)) else paste0("Período: ", min(df_plot$Ano), "-", max(df_plot$Ano))
        p <- grafico_publicacao(df_plot, visualizacao, paste(nome_doenca, "-", names(opcoes_visualizacao)[opcoes_visualizacao == visualizacao]), nome_doenca, periodo_label)
        ggsave(file, plot = p, device = formato, width = 9, height = 6, dpi = 600, units = "in")
      }
    )
  }
  
  registrar_downloads("chik", chik_filtrado, "chikungunya")
  registrar_downloads("dengue", dengue_filtrado, "dengue")
  registrar_downloads("zika", zika_filtrado, "zika")

  registrar_temporal_bruto <- function(prefixo, temporal_df, nome_doenca) {
    temporal_filtrado <- reactive({
      df <- temporal_df
      if(is.null(df) || nrow(df) == 0) return(df)
      intervalo <- input[[paste0(prefixo, "_temporal_intervalo")]]
      if(is.null(intervalo)) intervalo <- "Mensal"
      ano <- ano_selecionado(paste0(prefixo, "_ano"))
      df <- df[df$Intervalo == intervalo, , drop = FALSE]
      if(ano != "Todos") df <- df[df$Ano == as.numeric(ano), , drop = FALSE]
      df %>% arrange(Data)
    })
    
    output[[paste0(prefixo, "_temporal_plot")]] <- renderPlotly({
      df <- temporal_filtrado()
      if(is.null(df) || nrow(df) == 0) {
        return(plot_ly() %>% layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "Serie temporal bruta indisponivel em cache.",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            showarrow = FALSE
          ))
        ))
      }
      plot_ly(
        df,
        x = ~Data,
        y = ~Casos,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = if(nome_doenca == "Dengue") "#C73E1D" else "#A23B72", width = 2),
        marker = list(size = 6),
        text = ~paste0(Periodo, "<br>Casos confirmados: ", Casos),
        hoverinfo = "text"
      ) %>%
        layout(
          title = paste(nome_doenca, "- casos confirmados por intervalo"),
          xaxis = list(title = ""),
          yaxis = list(title = "Casos confirmados"),
          margin = list(l = 55, r = 25, t = 55, b = 45)
        )
    })
    
    output[[paste0(prefixo, "_temporal_tabela")]] <- renderDT({
      datatable(temporal_filtrado(), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })
    
    output[[paste0(prefixo, "_download_temporal")]] <- downloadHandler(
      filename = function() {
        paste0(tolower(nome_doenca), "_serie_temporal_bruta_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(temporal_filtrado(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  }

  registrar_temporal_bruto("dengue", dengue_temporal, "Dengue")
  registrar_temporal_bruto("zika", zika_temporal, "Zika")

  salvar_png_alta_resolucao <- function(plot, file, width = 9, height = 6) {
    ggsave(file, plot = plot, device = "png", width = width, height = height, dpi = 600, units = "in", bg = "white")
  }

  registrar_download_grafico <- function(output_id, df_func, nome_doenca, visualizacao, titulo, width = 9, height = 6) {
    output[[output_id]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", visualizacao, "_alta_resolucao_", Sys.Date(), ".png")
      },
      content = function(file) {
        df_plot <- df_func()
        periodo_label <- if(length(unique(df_plot$Ano)) == 1) paste("Ano:", unique(df_plot$Ano)) else paste0("Período: ", min(df_plot$Ano), "-", max(df_plot$Ano))
        p <- grafico_publicacao(df_plot, visualizacao, titulo, nome_doenca, periodo_label)
        salvar_png_alta_resolucao(p, file, width = width, height = height)
      }
    )
  }

  registrar_downloads_graficos_doenca <- function(prefixo, df_filtrado, df_serie, nome_doenca) {
    registrar_download_grafico(
      paste0(prefixo, "_download_etnia"),
      df_filtrado,
      nome_doenca,
      "etnia",
      paste(nome_doenca, "- Raça/cor")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_sexo"),
      df_filtrado,
      nome_doenca,
      "sexo",
      paste(nome_doenca, "- Sexo por ano")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_serie"),
      df_serie,
      nome_doenca,
      "serie",
      paste(nome_doenca, "- Série temporal")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_incidencia"),
      df_filtrado,
      nome_doenca,
      "incidencia",
      paste(nome_doenca, "- Incidência por 100 mil")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_gestacao"),
      df_filtrado,
      nome_doenca,
      "gestacao",
      paste(nome_doenca, "- Situação gestacional")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_faixa"),
      df_filtrado,
      nome_doenca,
      "faixa",
      paste(nome_doenca, "- Faixa etaria")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_escolaridade"),
      df_filtrado,
      nome_doenca,
      "escolaridade",
      paste(nome_doenca, "- Escolaridade")
    )
  }

  registrar_downloads_graficos_doenca("chik", chik_filtrado, function() dados$Chikungunya, "chikungunya")
  registrar_downloads_graficos_doenca("dengue", dengue_filtrado, function() dados$Dengue, "dengue")
  registrar_downloads_graficos_doenca("zika", zika_filtrado, function() dados$Zika, "zika")

  output$dengue_download_mapa_bairros <- downloadHandler(
    filename = function() {
      paste0("dengue_mapa_bairros_alta_resolucao_", Sys.Date(), ".png")
    },
    content = function(file) {
      ano <- ano_selecionado("dengue_ano")
      periodo_label <- if(ano == "Todos") "Período: 2020-2025" else paste("Ano:", ano)
      p <- grafico_mapa_bairros_geobr(dengue_mapa_geobr()$mapa, periodo_label)
      salvar_png_alta_resolucao(p, file, width = 10, height = 8)
    }
  )

  output$dengue_download_bairros_barra <- downloadHandler(
    filename = function() {
      paste0("dengue_bairros_barra_alta_resolucao_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- dengue_bairros_filtrado() %>%
        slice_max(Casos, n = 25, with_ties = FALSE) %>%
        arrange(Casos)

      p <- ggplot(df, aes(x = Casos, y = reorder(NM_BAIRRO, Casos))) +
        geom_col(fill = "#C73E1D") +
        geom_text(aes(label = format_number(Casos)), hjust = -0.08, size = 3.4, color = "#1e293b") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
        labs(title = "Top 25 bairros com mais registros de dengue", x = "Casos", y = "") +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "#1A2535"),
          axis.text.y = element_text(size = 10, color = "#334155"),
          panel.grid.major.y = element_blank()
        )
      salvar_png_alta_resolucao(p, file, width = 10, height = 8)
    }
  )

  output$dengue_mapa_bairros <- renderUI({
    ano <- ano_selecionado("dengue_ano")
    periodo_label <- if(ano == "Todos") "Período: 2020-2025" else paste("Ano:", ano)
    
    if(nrow(dengue_bairros_filtrado()) == 0) {
      return(div(class = "context-box", "Não há registros com bairro informado nas planilhas locais para o período selecionado."))
    }
    
    tagList(
      leafletOutput("dengue_mapa_bairros_plot", height = "620px"),
      botao_download_grafico("dengue_download_mapa_bairros"),
      div(class = "context-box",
        tags$strong("Nota metodológica: "),
        "o mapa usa a malha de bairros de 2010 disponibilizada pelo geobr/IBGE. A vinculacao com as planilhas e feita por nome normalizado do bairro; divergencias de grafia podem deixar alguns bairros sem correspondencia espacial."
      ),
      plotlyOutput("dengue_bairros_barra", height = "430px"),
      botao_download_grafico("dengue_download_bairros_barra")
    )
  })

  dengue_mapa_geobr <- reactive({
    preparar_mapa_bairros_geobr(dengue_bairros_filtrado())
  })

  dengue_correspondencia_bairros_df <- reactive({
    tryCatch(
      preparar_correspondencia_bairros(dengue_bairros_filtrado()),
      error = function(e) data.frame(
        NM_BAIRRO_PLANILHA = character(),
        bairro_key = character(),
        Casos = integer(),
        NM_BAIRRO_GEOBR = character(),
        Status = character(),
        stringsAsFactors = FALSE
      )
    )
  })

  output$dengue_correspondencia_bairros <- renderDT({
    datatable(
      dengue_correspondencia_bairros_df(),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$dengue_download_correspondencia_bairros <- downloadHandler(
    filename = function() {
      paste0("dengue_correspondencia_bairros_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dengue_correspondencia_bairros_df(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dengue_mapa_bairros_plot <- renderLeaflet({
    ano <- ano_selecionado("dengue_ano")
    periodo_label <- if(ano == "Todos") "Período: 2020-2025" else paste("Ano:", ano)
    tryCatch({
      mapa_leaflet_bairros_geobr(dengue_mapa_geobr()$mapa, periodo_label)
    }, error = function(e) {
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addControl(
          html = paste(
            "<strong>Não foi possível carregar a malha de bairros.</strong><br>",
            conditionMessage(e)
          ),
          position = "topright"
        )
    })
  })

  output$dengue_bairros_barra <- renderPlotly({
    df <- dengue_bairros_filtrado() %>%
      slice_max(Casos, n = 25, with_ties = FALSE) %>%
      arrange(Casos)
    
    if(nrow(df) == 0) {
      return(plot_ly() %>% layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "Sem bairros informados para exibir.",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
      ))
    }
    
    paleta <- colorRampPalette(c("#facc15", "#f97316", "#ef4444", "#b91c1c", "#7f1d1d"))(nrow(df))
    plot_ly(df) %>%
      add_bars(
        x = ~Casos,
        y = ~factor(NM_BAIRRO, levels = NM_BAIRRO),
        orientation = "h",
        marker = list(color = paleta),
        text = ~format_number(Casos),
        textposition = "outside",
        cliponaxis = FALSE,
        hovertemplate = "<b>%{y}</b><br>Casos: %{x}<extra></extra>"
      ) %>%
      layout(
        title = list(text = "Top 25 bairros com mais registros de dengue"),
        xaxis = list(title = "Casos"),
        yaxis = list(title = "", automargin = TRUE),
        showlegend = FALSE,
        margin = list(l = 175, r = 70, t = 50, b = 45)
      )
  })
  
  criar_card <- function(df_reativo, coluna, rotulo, extra_class = NULL) {
    renderUI({
      valor <- sum(df_reativo()[[coluna]], na.rm = TRUE)
      card_cls <- paste(c("custom-card", extra_class), collapse = " ")
      div(class = card_cls,
          div(class = "card-value", format_number(valor)),
          div(class = "card-label", rotulo)
      )
    })
  }

  criar_card_incidencia <- function(df_reativo) {
    renderUI({
      taxa <- incidencia_periodo(df_reativo())
      div(class = "custom-card",
          div(class = "card-value", if(is.finite(taxa)) format_number(round(taxa)) else "Indisponível"),
          div(class = "card-label", "INCIDÊNCIA/100 MIL")
      )
    })
  }

  criar_card_populacao <- function(df_reativo) {
    renderUI({
      df_pop <- adicionar_populacao(df_reativo())
      pop_media <- mean(df_pop$Populacao, na.rm = TRUE)
      div(class = "custom-card",
          div(class = "card-value", if(is.finite(pop_media)) format_number(round(pop_media)) else "Indisponível"),
          div(class = "card-label", "POPULAÇÃO MÉDIA SIDRA")
      )
    })
  }

  render_qualidade <- function(df_reativo) {
    renderUI({
      q <- qualidade_dados(df_reativo())
      div(class = "quality-grid",
        lapply(seq_len(nrow(q)), function(i) {
          div(class = "quality-card",
            div(class = "quality-label", q$Variavel[i]),
            div(class = "quality-value", format_percent(q$Percentual[i])),
            div(class = "quality-note", paste0(format_number(q$Ignorado_Branco_Ausente[i]), " de ", format_number(q$Total[i]), " registros"))
          )
        })
      )
    })
  }
  
  # Chikungunya cards (sem valor forçado)
  output$chik_card_cura <- criar_card(chik_filtrado, "Cura_evolucao", "CURAS")
  output$chik_card_obitos <- criar_card(chik_filtrado, "Obitos_Agr", "\u00d3BITOS", extra_class = "card-obitos")
  output$chik_card_descartados <- criar_card(chik_filtrado, "Descartado_casos", "DESCARTADOS")
  output$chik_card_ignorados <- criar_card(chik_filtrado, "Ign_Branco_casos", "IGN/BRANCO")
  output$chik_card_confirmados <- criar_card(chik_filtrado, "Confirmado_casos", "CONFIRMADOS")
  output$chik_card_inconclusivos <- criar_card(chik_filtrado, "Inconclusivo_casos", "INCONCLUSIVOS")
  output$chik_card_incidencia <- criar_card_incidencia(chik_filtrado)
  output$chik_card_populacao <- criar_card_populacao(chik_filtrado)
  output$chik_qualidade <- render_qualidade(chik_filtrado)
  
  # Dengue cards
  output$dengue_card_cura <- criar_card(dengue_filtrado, "Cura_evolucao", "CURAS")
  output$dengue_card_obitos <- criar_card(dengue_filtrado, "Obitos_Agr", "\u00d3BITOS", extra_class = "card-obitos")
  output$dengue_card_descartados <- criar_card(dengue_filtrado, "Descartado_casos", "DESCARTADOS")
  output$dengue_card_ignorados <- criar_card(dengue_filtrado, "Ign_Branco_casos", "IGN/BRANCO")
  output$dengue_card_confirmados <- criar_card(dengue_filtrado, "Confirmado_casos", "CONFIRMADOS")
  output$dengue_card_inconclusivos <- criar_card(dengue_filtrado, "Inconclusivo_casos", "INCONCLUSIVOS")
  output$dengue_card_incidencia <- criar_card_incidencia(dengue_filtrado)
  output$dengue_card_populacao <- criar_card_populacao(dengue_filtrado)
  output$dengue_qualidade <- render_qualidade(dengue_filtrado)
  
  # Zika cards
  output$zika_card_cura <- criar_card(zika_filtrado, "Cura_evolucao", "CURAS")
  output$zika_card_obitos <- criar_card(zika_filtrado, "Obitos_Agr", "\u00d3BITOS", extra_class = "card-obitos")
  output$zika_card_descartados <- criar_card(zika_filtrado, "Descartado_casos", "DESCARTADOS")
  output$zika_card_ignorados <- criar_card(zika_filtrado, "Ign_Branco_casos", "IGN/BRANCO")
  output$zika_card_confirmados <- criar_card(zika_filtrado, "Confirmado_casos", "CONFIRMADOS")
  output$zika_card_inconclusivos <- criar_card(zika_filtrado, "Inconclusivo_casos", "INCONCLUSIVOS")
  output$zika_card_incidencia <- criar_card_incidencia(zika_filtrado)
  output$zika_card_populacao <- criar_card_populacao(zika_filtrado)
  output$zika_qualidade <- render_qualidade(zika_filtrado)
  
  # Gráficos
  output$chik_serie <- renderPlotly({ criar_grafico_serie(dados$Chikungunya, "#2E86AB", "Chikungunya") })
  output$chik_incidencia <- renderPlotly({ criar_grafico_incidencia(chik_filtrado(), "#2E86AB", "Chikungunya") })
  output$chik_etnia <- renderPlotly({ criar_grafico_etnia(chik_filtrado()) })
  output$chik_sexo <- renderPlotly({ criar_grafico_sexo(chik_filtrado(), destacar_feminino_2024 = TRUE) })
  output$chik_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(chik_filtrado()) })
  output$chik_gestacao <- renderPlotly({ criar_grafico_gestacao(chik_filtrado()) })
  output$chik_escolaridade <- renderPlotly({ criar_grafico_escolaridade(chik_filtrado()) })
  
  output$dengue_serie <- renderPlotly({ criar_grafico_serie(dados$Dengue, "#C73E1D", "Dengue") })
  output$dengue_incidencia <- renderPlotly({ criar_grafico_incidencia(dengue_filtrado(), "#C73E1D", "Dengue") })
  output$dengue_etnia <- renderPlotly({ criar_grafico_etnia(dengue_filtrado()) })
  output$dengue_sexo <- renderPlotly({ criar_grafico_sexo(dengue_filtrado(), organizar_rotulos = TRUE) })
  output$dengue_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(dengue_filtrado()) })
  output$dengue_gestacao <- renderPlotly({ criar_grafico_gestacao(dengue_filtrado()) })
  output$dengue_escolaridade <- renderPlotly({ criar_grafico_escolaridade(dengue_filtrado()) })
  
  output$zika_serie <- renderPlotly({ criar_grafico_serie(dados$Zika, "#A23B72", "Zika") })
  output$zika_incidencia <- renderPlotly({ criar_grafico_incidencia(zika_filtrado(), "#A23B72", "Zika") })
  output$zika_etnia <- renderPlotly({ criar_grafico_etnia(zika_filtrado()) })
  output$zika_sexo <- renderPlotly({ criar_grafico_sexo(zika_filtrado()) })
  output$zika_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(zika_filtrado()) })
  output$zika_gestacao <- renderPlotly({ criar_grafico_gestacao(zika_filtrado()) })
  output$zika_escolaridade <- renderPlotly({ criar_grafico_escolaridade(zika_filtrado()) })
  
  
  
  # ---- Badge de periodo ativo ----
  render_periodo <- function(df_full, input_id) {
    renderUI({
      ano <- ano_selecionado(input_id)
      if (ano == "Todos") {
        anos <- df_full$Ano
        lbl  <- paste0("Todos os anos (", min(anos), "–", max(anos), ")")
      } else {
        lbl <- paste0("Ano selecionado: ", ano)
      }
      
      tags$span(class = "periodo-badge",
        tags$i(class = "fa fa-calendar-o"), " ", lbl
      )
    })
  }
  output$chik_periodo   <- render_periodo(dados$Chikungunya, "chik_ano")
  output$dengue_periodo <- render_periodo(dados$Dengue, "dengue_ano")
  output$zika_periodo   <- render_periodo(dados$Zika, "zika_ano")
  
  
  session$onSessionEnded(function() {
    registrar_log(LOG_SESSAO, data.frame(
      evento = "fim_sessao",
      detalhes = "Sessao encerrada"
    ))
  })
}

# EXECUTAR
