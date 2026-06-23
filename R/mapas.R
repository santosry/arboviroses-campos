normalizar_chave_bairro <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("\\bparq\\b|\\bpq\\b", "parque", x)
  x <- gsub("\\bjd\\b", "jardim", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

primeira_coluna_existente <- function(df, candidatas) {
  candidatas[candidatas %in% names(df)][1]
}

carregar_malha_bairros_campos <- function(
  cache_path = file.path("data", "app_cache", "geobr_bairros_campos_2010.rds"),
  gpkg_path = file.path("data", "app_cache", "neighborhoods_2010_simplified.gpkg")
) {
  preparar_malha <- function(malha) {
    col_bairro <- primeira_coluna_existente(malha, c("name_neighborhood", "name_neighbourhood", "NM_BAIRRO", "bairro", "NM_BAIR", "NM_BAIRRO_GEO"))
    if(is.na(col_bairro)) {
      stop("Não encontrei uma coluna de nome de bairro na malha do geobr.", call. = FALSE)
    }
    
    malha$NM_BAIRRO_GEO <- normalizar_bairro(malha[[col_bairro]])
    malha$bairro_key <- normalizar_chave_bairro(malha[[col_bairro]])
    sf::st_make_valid(malha)
  }
  
  if(file.exists(cache_path)) {
    malha_cache <- readRDS(cache_path)
    if(!all(c("NM_BAIRRO_GEO", "bairro_key") %in% names(malha_cache))) {
      malha_cache <- preparar_malha(malha_cache)
    }
    return(malha_cache)
  }

  if(file.exists(gpkg_path) && file.info(gpkg_path)$size > 0) {
    malha <- sf::read_sf(gpkg_path)
  } else {
    stop(
      "Malha de bairros nao encontrada em cache local. ",
      "Execute scripts/update_data.R antes de publicar; o app nao baixa geobr no startup.",
      call. = FALSE
    )
  }
  
  if(is.null(malha) || nrow(malha) == 0) {
    stop("O geobr não retornou a malha de bairros.", call. = FALSE)
  }
  
  col_codigo <- primeira_coluna_existente(malha, c("code_muni", "code_muni_7", "CD_MUN", "CD_GEOCMU"))
  col_municipio <- primeira_coluna_existente(malha, c("name_muni", "NM_MUN", "municipio"))
  if(!is.na(col_codigo)) {
    malha <- malha[as.character(malha[[col_codigo]]) %in% c("3301009", "330100"), , drop = FALSE]
  } else if(!is.na(col_municipio)) {
    malha <- malha[grepl("Campos dos Goytacazes", as.character(malha[[col_municipio]]), ignore.case = TRUE), , drop = FALSE]
  } else {
    stop("Não encontrei coluna de município na malha do geobr para filtrar Campos dos Goytacazes.", call. = FALSE)
  }
  
  if(nrow(malha) == 0) {
    stop("A base de bairros do geobr não trouxe polígonos para Campos dos Goytacazes.", call. = FALSE)
  }
  
  malha <- preparar_malha(malha)
  malha
}

preparar_mapa_bairros_geobr <- function(df_bairros) {
  malha <- carregar_malha_bairros_campos()
  dados_bairro <- df_bairros %>%
    mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
    group_by(bairro_key) %>%
    summarise(
      NM_BAIRRO_DADOS = first(NM_BAIRRO),
      Casos = sum(Casos, na.rm = TRUE),
      .groups = "drop"
    )
  
  mapa <- malha %>%
    left_join(dados_bairro, by = "bairro_key") %>%
    mutate(
      Casos = ifelse(is.na(Casos), 0, Casos),
      rotulo_bairro = dplyr::coalesce(NM_BAIRRO_DADOS, NM_BAIRRO_GEO)
    )
  
  nao_mapeados <- dados_bairro %>%
    filter(!bairro_key %in% mapa$bairro_key) %>%
    arrange(desc(Casos))

  list(mapa = mapa, nao_mapeados = nao_mapeados)
}

preparar_correspondencia_bairros <- function(df_bairros) {
  if(is.null(df_bairros) || nrow(df_bairros) == 0) {
    return(data.frame(
      NM_BAIRRO_PLANILHA = character(),
      bairro_key = character(),
      Casos = integer(),
      NM_BAIRRO_GEOBR = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  malha <- carregar_malha_bairros_campos() %>%
    sf::st_drop_geometry() %>%
    mutate(NM_BAIRRO_GEOBR = dplyr::coalesce(
      if("NM_BAIRRO_GEOBR" %in% names(.)) NM_BAIRRO_GEOBR else NA_character_,
      if("NM_BAIRRO_GEO" %in% names(.)) NM_BAIRRO_GEO else NA_character_
    )) %>%
    select(bairro_key, NM_BAIRRO_GEOBR) %>%
    distinct()
  
  df_bairros %>%
    mutate(bairro_key = normalizar_chave_bairro(NM_BAIRRO)) %>%
    group_by(NM_BAIRRO_PLANILHA = NM_BAIRRO, bairro_key) %>%
    summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
    left_join(malha, by = "bairro_key") %>%
    mutate(
      Status = ifelse(is.na(NM_BAIRRO_GEOBR), "Nao mapeado", "Mapeado"),
      NM_BAIRRO_GEOBR = ifelse(is.na(NM_BAIRRO_GEOBR), "", NM_BAIRRO_GEOBR)
    ) %>%
    arrange(Status, desc(Casos), NM_BAIRRO_PLANILHA)
}

grafico_mapa_bairros_geobr <- function(mapa_sf, periodo_label) {
  ggplot(mapa_sf) +
    geom_sf(aes(fill = Casos), color = "#ffffff", linewidth = 0.16) +
    scale_fill_gradientn(
      colours = c("#fff7bc", "#fec44f", "#fe9929", "#d95f0e", "#7f0000"),
      name = "Casos",
      labels = format_number
    ) +
    labs(
      title = "Dengue por bairro em Campos dos Goytacazes",
      subtitle = periodo_label,
      caption = "Fonte espacial: geobr/IBGE, bairros 2010. Fonte epidemiológica: planilhas locais SINAN-DENGUE."
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#1A2535"),
      plot.subtitle = element_text(size = 11, color = "#475569"),
      plot.caption = element_text(size = 9, color = "#64748b"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(34, "pt")
    )
}

mapa_leaflet_bairros_geobr <- function(mapa_sf, periodo_label) {
  mapa_sf <- sf::st_transform(mapa_sf, 4326)
  valores <- mapa_sf$Casos
  dominio <- valores[is.finite(valores)]
  if(length(dominio) == 0) dominio <- c(0, 1)
  
  pal <- leaflet::colorNumeric(
    palette = c("#fff7bc", "#fec44f", "#fe9929", "#d95f0e", "#7f0000"),
    domain = dominio,
    na.color = "#e2e8f0"
  )
  
  labels <- paste0(
    "<strong>", mapa_sf$rotulo_bairro, "</strong><br>",
    "Casos: ", format_number(mapa_sf$Casos), "<br>",
    periodo_label
  )
  
  leaflet::leaflet(mapa_sf, options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Base clara") %>%
    leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
    leaflet::addPolygons(
      group = "Dengue por bairro",
      fillColor = ~ifelse(Casos > 0, pal(Casos), "#e2e8f0"),
      fillOpacity = ~ifelse(Casos > 0, 0.82, 0.34),
      color = "#ffffff",
      opacity = 0.95,
      weight = 0.7,
      smoothFactor = 0.25,
      label = lapply(labels, htmltools::HTML),
      popup = lapply(labels, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#1B3A6B",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = dominio,
      title = "Casos de dengue",
      opacity = 0.85
    ) %>%
    leaflet::addScaleBar(
      position = "bottomleft",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("Base clara", "OpenStreetMap"),
      overlayGroups = c("Dengue por bairro"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}
