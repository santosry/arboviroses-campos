# ============================================================
# SERVER
# ============================================================

# Thresholds centralizados de qualidade (usados em alertas e auditoria)
QUALIDADE_THRESHOLD_CRITICO  <- 60   # % ignorado/branco acima disso = critico
QUALIDADE_THRESHOLD_ALERTA   <- 40   # % ignorado/branco acima disso = alerta
QUALIDADE_THRESHOLD_ATENCAO  <- 30   # % ignorado/branco acima disso = atencao

server <- function(input, output, session) {
  
  # Tutorial: fecha com removeUI do proprio Shiny (persiste na sessao)
  observeEvent(input$fechar_tutorial, {
    removeUI(selector = "#tutorial-box")
  })
  
  observeEvent(input$sidebarItemExpanded, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = input$sidebarItemExpanded,
      acao = "navegacao",
      detalhes = paste("Navegou para aba:", input$sidebarItemExpanded)
    ))
  })

  # Seleção de anos por aba: o usuário pode marcar um ou vários anos.
  anos_disponiveis <- function(df) {
    sort(unique(as.integer(df$Ano[!is.na(df$Ano)])))
  }

  anos_selecionados <- function(input_id, df) {
    disponiveis <- anos_disponiveis(df)
    valor <- input[[input_id]]
    if (is.null(valor) || length(valor) == 0) return(disponiveis)

    valor_num <- suppressWarnings(as.integer(valor))
    valor_num <- valor_num[!is.na(valor_num)]
    if (length(valor_num) == 0) return(disponiveis)

    selecionados <- sort(intersect(disponiveis, valor_num))
    if (length(selecionados) == 0) return(disponiveis)
    selecionados
  }

  rotulo_periodo <- function(anos, disponiveis = anos) {
    anos <- sort(unique(anos[!is.na(anos)]))
    disponiveis <- sort(unique(disponiveis[!is.na(disponiveis)]))
    if (length(anos) == 0) return("Sem dados no período")
    if (length(disponiveis) > 0 && setequal(anos, disponiveis)) {
      return(paste0("Todos os anos (", min(disponiveis), "-", max(disponiveis), ")"))
    }
    if (length(anos) == 1) return(paste0("Ano selecionado: ", anos))
    paste0("Anos selecionados: ", paste(anos, collapse = ", "))
  }

  rotulo_periodo_df <- function(df) {
    anos <- sort(unique(df$Ano[!is.na(df$Ano)]))
    rotulo_periodo(anos, anos)
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
    if (nrow(df) == 0) {
      return(list(
        total = 0, obitos = 0, pico = "N/D", predominante = "N/D",
        incidencia = NA_real_, atualizado = as.character(APP_DATA_ATUALIZACAO)
      ))
    }
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
    pico_ano <- if (nrow(por_ano) > 0 && any(por_ano$Casos > 0, na.rm = TRUE)) {
      as.character(por_ano$Ano[which.max(por_ano$Casos)])
    } else {
      "N/D"
    }
    predominante_agravo <- if (nrow(por_agravo) > 0 && any(por_agravo$Casos > 0, na.rm = TRUE)) {
      por_agravo$Agravo[which.max(por_agravo$Casos)]
    } else {
      "N/D"
    }
    list(
      total = sum(por_agravo$Casos, na.rm = TRUE),
      obitos = sum(por_agravo$Obitos, na.rm = TRUE),
      pico = pico_ano,
      predominante = predominante_agravo,
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
  output$home_card_incidencia <- criar_home_card("incidencia", "INCIDÊNCIA/100 MIL", function(x) if(is.finite(x)) format_number(round(x)) else "Indisponível")
  output$home_card_obitos <- criar_home_card("obitos", "ÓBITOS", format_number)
  output$home_card_pico <- criar_home_card("pico", "ANO DE PICO", as.character)
  output$home_card_predominante <- criar_home_card("predominante", "AGRAVO PREDOMINANTE", as.character)
  output$home_card_atualizacao <- criar_home_card("atualizado", "ÚLTIMA ATUALIZAÇÃO", as.character)

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

  output$home_comparador_plot <- renderPlot({
    tab <- tabela_comparador_home()
    if (nrow(tab) == 0) {
      return(ggplot() + annotate("text", x=1, y=1, label="Sem dados") + theme_void())
    }
    
    cores <- CORES_AGRAVOS
    
    # Ordena categorias por total de casos (decrescente)
    ordem <- tab %>%
      group_by(Categoria) %>%
      summarise(total = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(Categoria)
    tab$Categoria <- factor(tab$Categoria, levels = ordem)
    
    ggplot(tab, aes(x = Casos, y = Categoria, fill = Agravo)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.92) +
      geom_text(
        aes(label = ifelse(Casos > 0, format_number(Casos), ""), group = Agravo),
        position = position_dodge(width = 0.8),
        hjust = -0.1, size = 2.8, color = "#334155"
      ) +
      scale_fill_manual(values = cores) +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.20)),
        labels = format_number
      ) +
      labs(
        title = "Comparação entre agravos por variável selecionada",
        subtitle = "Número de casos confirmados — barras agrupadas por doença",
        x = "Casos confirmados",
        y = NULL,
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15, color = "#1A2535"),
        plot.subtitle = element_text(size = 11, color = "#64748B", margin = margin(b = 8)),
        axis.text.y = element_text(size = 12, color = "#1A2535", face = "bold"),
        axis.text.x = element_text(size = 10, color = "#475569"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 60, 10, 10)
      )
  })

  output$home_comparador_tabela <- renderDT({
    datatable(tabela_comparador_home(), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  # Painel de qualidade na pagina inicial
  dados_filtrados_global <- reactive({
    dados
  })

  # Tabela de qualidade na pagina inicial (substitui o grafico com log)
  home_qualidade_tabela_dados <- reactive({
    agravo <- input$home_qualidade_agravo
    if (is.null(agravo)) agravo <- "Todos"
    df_list <- dados_filtrados_global()
    if (!is.null(agravo) && agravo != "Todos") {
      df_list <- df_list[names(df_list) == agravo]
    }

    dplyr::bind_rows(lapply(names(df_list), function(nome) {
      df <- df_list[[nome]]
      if (nrow(df) == 0) return(data.frame())
      q <- qualidade_dados(df)
      q$Agravo <- nome
      q
    })) %>%
      select(Agravo, Variável = Variavel, `Total de registros` = Total,
             `Ignorados/Brancos` = Ignorado_Branco_Ausente,
             `% Incompletude` = Percentual) %>%
      mutate(`% Incompletude` = round(`% Incompletude`, 1))
  })

  output$home_qualidade_tabela <- renderDT({
    df <- home_qualidade_tabela_dados()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensagem = "Selecione um agravo para visualizar.")))
    }
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 4, render = DT::JS(
            "function(data, type, row) {
              if (data === null || isNaN(data)) return 'N/D';
              var val = parseFloat(data);
              var color = val > 60 ? '#440154' : val > 30 ? '#3B528B' : val > 10 ? '#21918C' : '#5EC962';
              return '<span style=\"font-weight:700;color:' + color + '\">' + val.toFixed(1) + '%</span>';
            }"
          ))
        )
      ),
      escape = FALSE
    ) %>%
      formatStyle(
        5,
        backgroundColor = styleInterval(
          c(10, 30, 60),
          c('#F0F9E8', '#E8F4D9', '#D7EFC4', '#C2E6AE')
        )
      )
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
        cores <- c(critico = "#440154", alerta = "#3B528B", atencao = "#21918C")
        div(class = "quality-card", style = paste0("border-left: 3px solid ", cores[nivel]),
          div(class = "quality-label", paste(icone, nome, ":", v)),
          div(class = "quality-value", if (is.na(p)) "?" else paste0(format_percent(p))),
          div(class = "quality-note", if (nivel == "critico") "Campo crítico: inferências não confiáveis" else if (nivel == "alerta") "Atenção: alta incompletude" else "Verificar preenchimento")
        )
      })
    })

    alertas <- unlist(alertas, recursive = FALSE)
    if (length(alertas) == 0) {
      return(div(class = "quality-grid",
        div(class = "quality-card", style = "grid-column: 1 / -1; text-align: center;",
          div(class = "quality-label", paste0("\u2705 Nenhum campo com incompletude superior a ", QUALIDADE_THRESHOLD_ATENCAO, "% no período selecionado."))
        )
      ))
    }
    div(class = "quality-grid", alertas)
  })
  
  observeEvent(input$chik_anos, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "chikungunya",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", paste(input$chik_anos, collapse = ", "))
    ))
  })
  observeEvent(input$dengue_anos, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "dengue",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", paste(input$dengue_anos, collapse = ", "))
    ))
  })
  observeEvent(input$zika_anos, {
    registrar_log(LOG_ACESSOS, data.frame(
      pagina = "zika",
      acao = "filtro_ano",
      detalhes = paste("Filtro alterado para:", paste(input$zika_anos, collapse = ", "))
    ))
  })
  
  chik_filtrado <- reactive({ filter_year(dados$Chikungunya, anos_selecionados("chik_anos", dados$Chikungunya)) })
  dengue_filtrado <- reactive({ filter_year(dados$Dengue, anos_selecionados("dengue_anos", dados$Dengue)) })
  zika_filtrado <- reactive({ filter_year(dados$Zika, anos_selecionados("zika_anos", dados$Zika)) })
  dengue_bairros_filtrado <- reactive({
    df <- dengue_bairros
    if(nrow(df) == 0) return(df)
    anos <- anos_selecionados("dengue_anos", dados$Dengue)
    df <- df[df$Ano %in% anos, , drop = FALSE]
    # Agrega por bairro_key (mesma logica da tabela de correspondencia)
    df %>%
      mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
      group_by(bairro_key, NM_BAIRRO) %>%
      summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
      group_by(bairro_key) %>%
      mutate(
        Casos_total = sum(Casos),
        NM_BAIRRO = first(NM_BAIRRO)  # usa o nome mais frequente
      ) %>%
      ungroup() %>%
      distinct(bairro_key, NM_BAIRRO, Casos = Casos_total) %>%
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
        periodo_label <- rotulo_periodo_df(df_reativo())
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
        periodo_label <- rotulo_periodo_df(df_plot)
        p <- grafico_publicacao(df_plot, visualizacao, paste(nome_doenca, "-", names(opcoes_visualizacao)[opcoes_visualizacao == visualizacao]), nome_doenca, periodo_label)
        ggsave(file, plot = p, device = formato, width = 9, height = 6, dpi = 600, units = "in")
      }
    )
  }
  
  registrar_downloads("chik", chik_filtrado, "chikungunya")
  registrar_downloads("dengue", dengue_filtrado, "dengue")
  registrar_downloads("zika", zika_filtrado, "zika")

  salvar_png_alta_resolucao <- function(plot, file, width = 9, height = 6) {
    ggsave(file, plot = plot, device = "png", width = width, height = height, dpi = 600, units = "in", bg = "white")
  }

  registrar_download_grafico <- function(output_id, df_func, nome_doenca, visualizacao, titulo,
                                         width = 9, height = 6, benchmarks = NULL) {
    output[[output_id]] <- downloadHandler(
      filename = function() {
        paste0(nome_doenca, "_", visualizacao, "_alta_resolucao_", Sys.Date(), ".png")
      },
      content = function(file) {
        df_plot <- df_func()
        periodo_label <- rotulo_periodo_df(df_plot)
        p <- grafico_publicacao(df_plot, visualizacao, titulo, nome_doenca, periodo_label,
                                benchmarks = benchmarks)
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
      paste(nome_doenca, "- Incidência por 100 mil"),
      benchmarks = obter_benchmarks(nome_doenca)
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
      paste(nome_doenca, "- Faixa etária")
    )
    registrar_download_grafico(
      paste0(prefixo, "_download_escolaridade"),
      df_filtrado,
      nome_doenca,
      "escolaridade",
      paste(nome_doenca, "- Escolaridade")
    )
  }

  registrar_downloads_graficos_doenca("chik", chik_filtrado, chik_filtrado, "chikungunya")
  registrar_downloads_graficos_doenca("dengue", dengue_filtrado, dengue_filtrado, "dengue")
  registrar_downloads_graficos_doenca("zika", zika_filtrado, zika_filtrado, "zika")

  output$dengue_download_mapa_bairros <- downloadHandler(
    filename = function() {
      paste0("dengue_mapa_bairros_alta_resolucao_", Sys.Date(), ".png")
    },
    content = function(file) {
      periodo_label <- rotulo_periodo_df(dengue_bairros_filtrado())
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
        slice_max(Casos, n = 20, with_ties = FALSE) %>%
        arrange(Casos)

      p <- ggplot(df, aes(x = Casos, y = reorder(NM_BAIRRO, Casos))) +
        geom_col(fill = cor_agravo("Dengue")) +
        geom_text(aes(label = format_number(Casos)), hjust = -0.08, size = 3.4, color = "#1e293b") +
        scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
        labs(title = "Top 20 bairros com mais registros de dengue", x = "Casos", y = "") +
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
    periodo_label <- rotulo_periodo_df(dengue_bairros_filtrado())
    
    if(nrow(dengue_bairros_filtrado()) == 0) {
      return(div(class = "context-box", "Não há registros com bairro informado nas planilhas locais para o período selecionado."))
    }
    
    tagList(
      leafletOutput("dengue_mapa_bairros_plot", height = "620px"),
      botao_download_grafico("dengue_download_mapa_bairros"),
      div(class = "context-box",
        tags$strong("Nota metodológica: "),
        "o mapa usa a malha de bairros de 2010 disponibilizada pelo geobr/IBGE. A vinculação com as planilhas é feita por nome normalizado do bairro; divergências de grafia podem deixar alguns bairros sem correspondência espacial."
      ),
      plotOutput("dengue_bairros_barra", height = "430px"),
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
    periodo_label <- rotulo_periodo_df(dengue_bairros_filtrado())
    tryCatch({
      mapa_leaflet_bairros_geobr(dengue_mapa_geobr()$mapa, periodo_label)
    }, error = function(e) {
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldGrayCanvas") %>%
        leaflet::addControl(
          html = paste(
            "<strong>Não foi possível carregar a malha de bairros.</strong><br>",
            conditionMessage(e)
          ),
          position = "topright"
        )
    })
  })

  output$dengue_bairros_barra <- renderPlot({
    df <- dengue_bairros_filtrado() %>%
      slice_max(Casos, n = 20, with_ties = FALSE) %>%
      arrange(Casos)
    
    if(nrow(df) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Sem bairros informados para exibir.") + theme_void())
    }
    
    df$NM_BAIRRO <- factor(df$NM_BAIRRO, levels = df$NM_BAIRRO)
    max_casos <- max(df$Casos, na.rm = TRUE)
    
    paleta <- paleta_viridis(nrow(df), begin = 0.08, end = 0.92)
    
    ggplot(df, aes(x = Casos, y = NM_BAIRRO)) +
      geom_col(aes(fill = NM_BAIRRO), width = 0.72) +
      geom_text(
        aes(label = format_number(Casos)),
        hjust = -0.15, size = 3.6, color = "#1e293b", fontface = "bold"
      ) +
      scale_fill_manual(values = paleta, guide = "none") +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.22)),
        limits = c(0, max_casos * 1.18)
      ) +
      labs(
        title = "Distribuição dos registros de dengue por bairro — 20 bairros com maior volume de notificações, Campos dos Goytacazes",
        x = "Número de notificações",
        y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#1A2535", margin = margin(b = 10)),
        axis.title.x = element_text(size = 12, color = "#334155"),
        axis.text.y = element_text(size = 11, color = "#1A2535"),
        axis.text.x = element_text(size = 10, color = "#475569"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 55, 10, 10)
      )
  }, height = 430)
  
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
  output$chik_serie <- renderPlotly({ criar_grafico_serie(chik_filtrado(), CORES_AGRAVOS[["Chikungunya"]], "Chikungunya") })
  output$chik_incidencia <- renderPlotly({ criar_grafico_incidencia(chik_filtrado(), CORES_AGRAVOS[["Chikungunya"]], "Chikungunya", benchmarks = obter_benchmarks("Chikungunya")) })
  output$chik_etnia <- renderPlotly({ criar_grafico_etnia(chik_filtrado()) })
  output$chik_sexo <- renderPlotly({ criar_grafico_sexo(chik_filtrado(), destacar_feminino_2024 = TRUE) })
  output$chik_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(chik_filtrado()) })
  output$chik_gestacao <- renderPlotly({ criar_grafico_gestacao(chik_filtrado()) })
  output$chik_escolaridade <- renderPlotly({ criar_grafico_escolaridade(chik_filtrado()) })
  
  output$dengue_serie <- renderPlotly({ criar_grafico_serie(dengue_filtrado(), CORES_AGRAVOS[["Dengue"]], "Dengue") })
  output$dengue_incidencia <- renderPlotly({ criar_grafico_incidencia(dengue_filtrado(), CORES_AGRAVOS[["Dengue"]], "Dengue", benchmarks = obter_benchmarks("Dengue")) })
  output$dengue_etnia <- renderPlotly({ criar_grafico_etnia(dengue_filtrado()) })
  output$dengue_sexo <- renderPlotly({ criar_grafico_sexo(dengue_filtrado(), organizar_rotulos = TRUE) })
  output$dengue_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(dengue_filtrado()) })
  output$dengue_gestacao <- renderPlotly({ criar_grafico_gestacao(dengue_filtrado()) })
  output$dengue_escolaridade <- renderPlotly({ criar_grafico_escolaridade(dengue_filtrado()) })
  
  output$zika_serie <- renderPlotly({ criar_grafico_serie(zika_filtrado(), CORES_AGRAVOS[["Zika"]], "Zika") })
  output$zika_incidencia <- renderPlotly({ criar_grafico_incidencia(zika_filtrado(), CORES_AGRAVOS[["Zika"]], "Zika", benchmarks = obter_benchmarks("Zika")) })
  output$zika_etnia <- renderPlotly({ criar_grafico_etnia(zika_filtrado()) })
  output$zika_sexo <- renderPlotly({ criar_grafico_sexo(zika_filtrado()) })
  output$zika_faixa_etaria <- renderPlotly({ criar_grafico_faixa_etaria(zika_filtrado()) })
  output$zika_gestacao <- renderPlotly({ criar_grafico_gestacao(zika_filtrado()) })
  output$zika_escolaridade <- renderPlotly({ criar_grafico_escolaridade(zika_filtrado()) })
  
  # ---- Títulos dinâmicos com os anos ativos (#9) ----
  output$chik_titulo_ano <- renderUI({
    disponiveis <- anos_disponiveis(dados$Chikungunya)
    anos <- anos_selecionados("chik_anos", dados$Chikungunya)
    if (!setequal(anos, disponiveis)) {
      tags$span(class = "titulo-ano-badge", paste0("(", paste(anos, collapse = ", "), ")"))
    }
  })
  output$dengue_titulo_ano <- renderUI({
    disponiveis <- anos_disponiveis(dados$Dengue)
    anos <- anos_selecionados("dengue_anos", dados$Dengue)
    if (!setequal(anos, disponiveis)) {
      tags$span(class = "titulo-ano-badge", paste0("(", paste(anos, collapse = ", "), ")"))
    }
  })
  output$zika_titulo_ano <- renderUI({
    disponiveis <- anos_disponiveis(dados$Zika)
    anos <- anos_selecionados("zika_anos", dados$Zika)
    if (!setequal(anos, disponiveis)) {
      tags$span(class = "titulo-ano-badge", paste0("(", paste(anos, collapse = ", "), ")"))
    }
  })
  
  # ---- Badge de periodo ativo ----
  render_periodo <- function(df_full, input_id) {
    renderUI({
      disponiveis <- anos_disponiveis(df_full)
      anos <- anos_selecionados(input_id, df_full)
      lbl <- rotulo_periodo(anos, disponiveis)
      
      tags$span(class = "periodo-badge",
        tags$i(class = "fa fa-calendar-o"), " ", lbl
      )
    })
  }
  output$chik_periodo   <- render_periodo(dados$Chikungunya, "chik_anos")
  output$dengue_periodo <- render_periodo(dados$Dengue, "dengue_anos")
  output$zika_periodo   <- render_periodo(dados$Zika, "zika_anos")

  # ============================================================
  # CLASSIFICACAO DE RISCO / GRAVIDADE (MS)
  # ============================================================

  resumo_grupos_dengue <- function(df) {
    cores <- c(A = COR_GRUPO_A, B = COR_GRUPO_B, C = COR_GRUPO_C, D = COR_GRUPO_D)
    rotulos <- c(
      A = "GRUPO A — sem sinais de alarme",
      B = "GRUPO B — risco intermediário",
      C = "GRUPO C — sinais de alarme",
      D = "GRUPO D — dengue grave"
    )
    base <- data.frame(Grupo = c("A", "B", "C", "D"), stringsAsFactors = FALSE)
    agg <- if (nrow(df) > 0 && "Grupo" %in% names(df)) {
      df %>% group_by(Grupo) %>% summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop")
    } else {
      data.frame(Grupo = character(), Casos = integer(), stringsAsFactors = FALSE)
    }
    base <- base %>%
      left_join(agg, by = "Grupo") %>%
      mutate(Casos = ifelse(is.na(Casos), 0L, as.integer(Casos)))
    base$Cor <- unname(cores[base$Grupo])
    base$Rotulo <- unname(rotulos[base$Grupo])
    base
  }

  risco_dengue_filtrado <- reactive({
    df <- dengue_risco
    anos <- anos_selecionados("risco_dengue_anos", df)
    df[df$Ano %in% anos, , drop = FALSE]
  })

  render_risco_card <- function(grupo) {
    renderUI({
      res <- resumo_grupos_dengue(risco_dengue_filtrado())
      linha <- res[res$Grupo == grupo, , drop = FALSE]
      if (nrow(linha) == 0) return(NULL)
      div(class = "custom-card", style = paste0("border-top: 3px solid ", linha$Cor, " !important;"),
        div(class = "card-value", style = paste0("color: ", linha$Cor, ";"), format_number(linha$Casos)),
        div(class = "card-label", style = paste0("color: ", linha$Cor, ";"), linha$Rotulo)
      )
    })
  }

  output$risco_card_A <- render_risco_card("A")
  output$risco_card_B <- render_risco_card("B")
  output$risco_card_C <- render_risco_card("C")
  output$risco_card_D <- render_risco_card("D")

  output$risco_dengue_donut <- renderPlotly({
    res <- resumo_grupos_dengue(risco_dengue_filtrado())
    criar_donut(res$Casos, res$Rotulo, res$Cor, exibir_rotulo_detalhado = TRUE)
  })

  output$risco_dengue_serie <- renderPlotly({
    df <- risco_dengue_filtrado()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
        annotations = list(list(text = "Sem dados de risco para o período selecionado.", x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE))
      ))
    }
    df$Ano <- as.integer(df$Ano)
    df$Grupo <- factor(df$Grupo, levels = c("A", "B", "C", "D"))
    plot_ly(df, x = ~Ano, y = ~Casos, color = ~Grupo, type = "bar",
      colors = c(COR_GRUPO_A, COR_GRUPO_B, COR_GRUPO_C, COR_GRUPO_D),
      hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y}<extra></extra>") %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", tickvals = sort(unique(df$Ano))),
        yaxis = list(title = "Casos"),
        legend = list(orientation = "h", y = 1.15, x = 0.5, xanchor = "center"),
        margin = list(l = 50, r = 20, t = 20, b = 50)
      )
  })

  risco_dengue_bairros_filtrado <- reactive({
    df <- dengue_risco_bairros
    anos <- anos_selecionados("risco_dengue_anos", dengue_risco)
    df[df$Ano %in% anos, , drop = FALSE]
  })

  output$risco_dengue_bairros_dt <- renderDT({
    df <- risco_dengue_bairros_filtrado()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensagem = "Sem dados de risco por bairro para o período selecionado."), rownames = FALSE, options = list(dom = "t")))
    }
    tab <- df %>%
      group_by(NM_BAIRRO, Grupo) %>%
      summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Grupo, values_from = Casos, values_fill = list(Casos = 0L))
    for (g in c("A", "B", "C", "D")) {
      if (!g %in% names(tab)) tab[[g]] <- 0L
    }
    tab$Total <- tab$A + tab$B + tab$C + tab$D
    tab$Graves_alarme <- tab$C + tab$D
    tab <- tab %>%
      select(NM_BAIRRO, A, B, C, D, Graves_alarme, Total) %>%
      arrange(desc(Total), desc(Graves_alarme), NM_BAIRRO) %>%
      rename(
        Bairro = NM_BAIRRO,
        "Grupo A" = A,
        "Grupo B" = B,
        "Grupo C" = C,
        "Grupo D" = D,
        "C+D (alarme/grave)" = Graves_alarme
      )
    datatable(tab, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })

  output$risco_dengue_bairros_download <- downloadHandler(
    filename = function() paste0("dengue_risco_bairros_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- risco_dengue_bairros_filtrado()
      tab <- if (nrow(df) == 0) {
        data.frame(Bairro = character(), stringsAsFactors = FALSE)
      } else {
        df %>%
          group_by(NM_BAIRRO, Grupo) %>%
          summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = Grupo, values_from = Casos, values_fill = list(Casos = 0L))
      }
      for (g in c("A", "B", "C", "D")) if (!g %in% names(tab)) tab[[g]] <- 0L
      tab$Total <- tab$A + tab$B + tab$C + tab$D
      write.csv(tab, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ============================================================
  # AVALIAÇÃO DE USABILIDADE / CO-DESIGN
  # ============================================================
  feedback_status_msg <- reactiveVal(NULL)
  feedback_ultimo <- reactiveVal(NULL)

  output$feedback_status <- renderUI({
    msg <- feedback_status_msg()
    if (is.null(msg) || is.null(msg$texto) || !nzchar(msg$texto)) return(NULL)
    div(class = paste("feedback-status", msg$tipo),
      tags$strong(msg$titulo), tags$br(), msg$texto
    )
  })

  output$feedback_download <- downloadHandler(
    filename = function() paste0("avaliacao_painel_arboviroses_", Sys.Date(), ".csv"),
    content = function(file) {
      registro <- feedback_ultimo()
      if (is.null(registro)) {
        registro <- data.frame(Mensagem = "Nenhuma avaliação enviada nesta sessão.", stringsAsFactors = FALSE)
      }
      utils::write.csv(registro, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  mostrar_fallback_feedback <- function(registro, erro_envio = NULL) {
    email_dest <- Sys.getenv("ARBOVIROSES_FEEDBACK_EMAIL", FEEDBACK_EMAIL_PADRAO)
    assunto <- utils::URLencode("Avaliação do Painel de Arboviroses — Campos/RJ", reserved = TRUE)
    corpo <- paste(
      "Avaliação de utilidade e usabilidade do Painel de Arboviroses",
      "",
      paste(names(registro), unlist(registro), sep = ": ", collapse = "\n"),
      "",
      "Enviado pelo formulário do painel.",
      sep = "\n"
    )
    link_mailto <- paste0(
      "mailto:", email_dest,
      "?subject=", assunto,
      "&body=", utils::URLencode(corpo, reserved = TRUE)
    )
    feedback_status_msg(list(tipo = "warn", titulo = "Respostas registradas nesta sessão",
      texto = paste0(
        "O envio automático não foi concluído",
        if (!is.null(erro_envio) && nzchar(as.character(erro_envio)[1])) {
          paste0(" (", as.character(erro_envio)[1], ")")
        } else "",
        ". Use o botão de e-mail ou baixe o CSV para encaminhar à equipe."
      )))
    showModal(modalDialog(
      title = "Respostas registradas",
      easyClose = TRUE,
      footer = modalButton("Fechar"),
      p("Obrigado pela avaliação. Para concluir o envio à equipe, abra o e-mail pré-preenchido abaixo ou baixe o CSV e anexe na mensagem."),
      p(tags$a(href = link_mailto, class = "contact-button", target = "_blank",
               tags$i(class = "fa fa-envelope"), " Abrir e-mail pré-preenchido")),
      p(class = "feedback-help", "Destinatário: ", tags$strong(email_dest)),
      if (!is.null(erro_envio)) p(class = "feedback-help", "Detalhe técnico do envio automático: ", erro_envio)
    ))
  }

  observeEvent(input$feedback_enviar, {
    ids_q <- paste0("feedback_q", 1:8)
    respostas_q <- vapply(ids_q, function(id) {
      v <- input[[id]]
      if (is.null(v) || !nzchar(as.character(v))) NA_character_ else as.character(v)
    }, character(1))

    if (any(is.na(respostas_q))) {
      feedback_status_msg(list(tipo = "err", titulo = "Avaliação incompleta",
        texto = "Responda às oito afirmações da escala Likert antes de enviar."))
      showNotification("Responda às oito afirmações da avaliação.", type = "warning", duration = 6)
      return()
    }
    if (!isTRUE(input$feedback_consentimento)) {
      feedback_status_msg(list(tipo = "err", titulo = "Consentimento necessário",
        texto = "Marque a caixa de consentimento para enviar as respostas."))
      showNotification("Marque o consentimento para enviar.", type = "warning", duration = 6)
      return()
    }

    valores_num <- suppressWarnings(as.integer(respostas_q))
    registro <- data.frame(
      data_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      perfil = as.character(input$feedback_perfil %||% ""),
      instituicao = as.character(input$feedback_instituicao %||% ""),
      email = as.character(input$feedback_email %||% ""),
      tempo_uso = as.character(input$feedback_tempo_uso %||% ""),
      q1_navegacao = valores_num[1],
      q2_clareza_graficos = valores_num[2],
      q3_utilidade_rotina = valores_num[3],
      q4_confianca_dados = valores_num[4],
      q5_compreensao_limites = valores_num[5],
      q6_documentacao = valores_num[6],
      q7_acessibilidade_cores = valores_num[7],
      q8_recomendacao = valores_num[8],
      media_likert = round(mean(valores_num, na.rm = TRUE), 2),
      comentarios = as.character(input$feedback_comentarios %||% ""),
      consentimento = TRUE,
      stringsAsFactors = FALSE
    )
    feedback_ultimo(registro)

    # Persistência local (auditoria) e envio automático via endpoint.
    # O endpoint padrão usa FormSubmit (sem chave de API). O primeiro envio
    # dispara um e-mail de ativação; depois disso as respostas chegam direto
    # à caixa configurada. TESTTHAT evita envios reais durante os testes.
    tryCatch(registrar_log(LOG_FEEDBACK, registro), error = function(e) NULL)
    autosend <- !identical(tolower(Sys.getenv("ARBOVIROSES_FEEDBACK_AUTOSEND", "true")), "false") &&
      !identical(Sys.getenv("TESTTHAT"), "true")
    endpoint_env <- Sys.getenv("ARBOVIROSES_FEEDBACK_ENDPOINT", unset = NA_character_)
    endpoint <- if (is.na(endpoint_env)) FEEDBACK_ENDPOINT_PADRAO else endpoint_env
    if (!autosend) endpoint <- ""
    if (!autosend || !nzchar(endpoint)) {
      mostrar_fallback_feedback(registro, "envio automático desativado")
      return()
    }

    payload <- as.list(registro)
    payload[["_subject"]] <- "Nova avaliação do Painel de Arboviroses — Campos/RJ"
    payload[["_template"]] <- "table"
    payload[["_captcha"]] <- "false"
    if (nzchar(registro$email)) payload[["_replyto"]] <- registro$email

    feedback_status_msg(list(tipo = "warn", titulo = "Enviando avaliação...",
      texto = "Aguarde enquanto as respostas são enviadas ao FormSubmit."))
    session$sendCustomMessage("feedback_formsubmit", list(
      endpoint = endpoint,
      payload = payload
    ))
  })

  observeEvent(input$feedback_js_result, {
    res <- input$feedback_js_result
    if (is.null(res) || is.null(feedback_ultimo())) return()
    registro <- feedback_ultimo()
    if (isTRUE(res$ok)) {
      feedback_status_msg(list(tipo = "ok", titulo = "Avaliação enviada com sucesso",
        texto = paste0(
          "Obrigado! As respostas foram registradas e serão usadas no relatório de usabilidade e nas melhorias do painel. ",
          "O envio automático foi concluído via FormSubmit."
        )))
      showNotification("Avaliação enviada. Obrigado!", type = "message", duration = 6)
    } else {
      detalhe <- paste0(
        "HTTP ", if (is.null(res$status)) "?" else res$status,
        if (!is.null(res$message) && nzchar(as.character(res$message)[1])) paste0(": ", as.character(res$message)[1]) else ""
      )
      mostrar_fallback_feedback(registro, detalhe)
    }
  })

  session$onSessionEnded(function() {
    registrar_log(LOG_SESSAO, data.frame(
      evento = "fim_sessao",
      detalhes = "Sessão encerrada"
    ))
  })
}

# EXECUTAR
