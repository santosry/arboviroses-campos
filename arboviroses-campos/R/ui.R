# UI

# Aviso metodológico/legal reutilizável. Presente em todas as abas para
# atender ao requisito de guardrails de comunicação (Bloco A, item 1).
aviso_uso_dados <- function(compacto = FALSE) {
  div(class = if (compacto) "tab-disclaimer tab-disclaimer-compact" else "tab-disclaimer",
    role = "note",
    tags$i(class = "fa fa-shield-alt", `aria-hidden` = "true"),
    tags$div(
      tags$strong("Uso responsável: "),
      if (compacto) {
        "dados de notificação do SINAN/SVS, sujeitos a subnotificação e atraso. Indicadores descritivos não provam causalidade e não substituem avaliação clínica individual."
      } else {
        tagList(
          "este painel apresenta registros notificados, não a incidência real. Subnotificação, atraso de digitação, encerramento incompleto e mudanças de critério afetam os números. ",
          tags$strong("Os indicadores são descritivos e não estabelecem causalidade."),
          " Não use os resultados para decisões clínicas individuais nem para concluir risco sem validação local. Ao citar, informe período, agravo, fonte e recorte territorial."
        )
      }
    )
  )
}

campo_likert <- function(id, pergunta) {
  div(class = "likert-row",
    div(class = "likert-question", pergunta),
    radioButtons(
      id, NULL,
      choices = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5"),
      selected = character(0), inline = TRUE, width = "100%"
    )
  )
}

seletor_anos <- function(id, anos, rotulo = "PERÍODO — selecione um ou mais anos") {
  anos <- sort(unique(anos[!is.na(anos)]))
  anos <- as.character(anos)
  div(class = "seletor-anos",
    div(class = "seletor-anos-label", rotulo),
    checkboxGroupInput(
      id, NULL,
      choices = stats::setNames(anos, anos),
      selected = anos,
      inline = TRUE
    ),
    div(class = "seletor-anos-help", "Deixe todos marcados para visualizar o período completo.")
  )
}

ui <- dashboardPage(skin = "black",
  dashboardHeader(
    title = "",
    titleWidth = 0,
    disable = FALSE
  ),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("INÍCIO", tabName = "inicio", icon = icon("home")),
      menuItem("CHIKUNGUNYA", tabName = "chik"),
      menuItem("DENGUE", tabName = "dengue"),
      menuItem("ZIKA", tabName = "zika"),
      menuItem("AVALIAÇÃO", tabName = "feedback", icon = icon("clipboard-check")),
      menuItem("MÉTODOS", tabName = "metodos", icon = icon("flask")),
      menuItem("EQUIPE", tabName = "equipe", icon = icon("users"))
    ),
    hr(),
    tags$div(class = "sidebar-fonte",
      tags$strong("Fonte de Dados"),
      "SINAN/SVS: 2020-2025"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,300;0,400;0,600;0,700;1,400&family=Source+Serif+4:wght@400;600;700&display=swap');
        
        * { font-family: 'Source Sans 3', 'Helvetica Neue', Arial, sans-serif; box-sizing: border-box; }
        
        /* ===== HEADER ===== */
        .skin-black .main-header .logo,
        .skin-black .main-header .logo:hover {
          background-color: #1B3A6B !important;
          font-family: 'Source Serif 4', Georgia, serif !important;
          font-size: 0 !important;
          font-weight: 700 !important;
          letter-spacing: 1.5px !important;
          text-transform: uppercase !important;
          color: transparent !important;
          border-bottom: 3px solid #122856 !important;
        }
        .skin-black .main-header .logo {
          width: 0 !important;
          padding: 0 !important;
        }
        .skin-black .main-header .navbar {
          background-color: #1B3A6B !important;
          border-bottom: 3px solid #122856 !important;
          margin-left: 0 !important;
        }
        .skin-black .main-header .navbar::after {
          content: none !important;
          display: none !important;
        }
        .fixed-app-title {
          position: fixed !important;
          top: 0 !important;
          left: 50% !important;
          transform: translateX(-50%);
          height: 50px;
          display: flex;
          align-items: center;
          justify-content: center;
          color: #ffffff;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 21px;
          font-weight: 700;
          letter-spacing: 0.3px;
          white-space: nowrap;
          pointer-events: none;
          z-index: 2000 !important;
        }
        .skin-black .main-header .sidebar-toggle {
          color: rgba(255,255,255,0.85) !important;
          width: 50px !important;
          text-align: left;
          padding-left: 9px !important;
          padding-right: 0 !important;
          position: fixed !important;
          left: 0 !important;
          top: 0 !important;
          z-index: 2100 !important;
        }
        .skin-black .main-header .sidebar-toggle:hover {
          background: rgba(0,0,0,0.18) !important;
          color: white !important;
        }
        
        /* ===== SIDEBAR ===== */
        .skin-black .main-sidebar,
        .skin-black .left-side {
          background-color: #1A2535 !important;
        }
        .skin-black .sidebar a { color: #B8C8DC !important; }
        .skin-black .sidebar-menu > li > a {
          color: #B8C8DC !important;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 1.4px;
          text-transform: uppercase;
          padding: 14px 18px;
          border-left: 3px solid transparent !important;
          border-bottom: 1px solid rgba(255,255,255,0.04) !important;
          transition: all 0.18s ease;
        }
        .skin-black .sidebar-menu > li:hover > a {
          color: #ffffff !important;
          background: rgba(27,58,107,0.28) !important;
          border-left-color: #1B3A6B !important;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li.active > a:hover {
          color: #ffffff !important;
          background: rgba(27,58,107,0.55) !important;
          border-left-color: #ffffff !important;
        }
        .skin-black .sidebar hr {
          border-color: rgba(255,255,255,0.1);
          margin: 8px 18px;
        }
        .sidebar .form-group { padding: 0 14px 10px 14px; }
        .sidebar .form-group label {
          color: #9E9E9E !important;
          font-size: 10px;
          font-weight: 700;
          letter-spacing: 1.5px;
          text-transform: uppercase;
        }
        .sidebar .selectize-input {
          background: rgba(255,255,255,0.07) !important;
          border: 1px solid rgba(255,255,255,0.18) !important;
          color: #ffffff !important;
          border-radius: 4px !important;
          font-size: 13px !important;
        }
        .sidebar .selectize-input .item { color: #fff !important; }
        .sidebar .selectize-dropdown {
          background: #1E3048 !important;
          border: 1px solid rgba(255,255,255,0.12) !important;
        }
        .sidebar .selectize-dropdown-content .option { color: #B8C8DC !important; }
        .sidebar .selectize-dropdown-content .option.active,
        .sidebar .selectize-dropdown-content .option:hover {
          background: rgba(27,58,107,0.55) !important;
          color: white !important;
        }
        /* Global filter inside sidebar */
        .sidebar-filtro-global {
          padding: 10px 18px 10px 18px;
        }
        .sidebar-filtro-global strong {
          display: block;
          color: #9E9E9E;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 1.2px;
          text-transform: uppercase;
          margin-bottom: 6px;
        }
        .sidebar-filtro-global .form-group {
          padding: 0;
          margin-bottom: 0;
        }
        .sidebar-filtro-global label {
          color: #9E9E9E !important;
          font-size: 10px;
          font-weight: 700;
          letter-spacing: 1.5px;
          text-transform: uppercase;
        }
        .sidebar-filtro-global .selectize-input {
          background: rgba(255,255,255,0.07) !important;
          border: 1px solid rgba(255,255,255,0.18) !important;
          color: #ffffff !important;
          border-radius: 4px !important;
          font-size: 13px !important;
        }
        .sidebar-filtro-global .selectize-input .item { color: #fff !important; }
        .sidebar-filtro-global .selectize-dropdown {
          background: #1E3048 !important;
          border: 1px solid rgba(255,255,255,0.12) !important;
        }
        .sidebar-filtro-global .selectize-dropdown-content .option { color: #B8C8DC !important; }
        .sidebar-filtro-global .selectize-dropdown-content .option.active,
        .sidebar-filtro-global .selectize-dropdown-content .option:hover {
          background: rgba(27,58,107,0.55) !important;
          color: white !important;
        }
        /* Data source footnote inside sidebar */
        .sidebar-fonte {
          padding: 10px 18px 14px 18px;
          color: #4A6580;
          font-size: 10px;
          line-height: 1.5;
          border-top: 1px solid rgba(255,255,255,0.07);
          margin-top: 6px;
        }
        .sidebar-fonte strong {
          display: block;
          color: #9E9E9E;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 1.2px;
          text-transform: uppercase;
          margin-bottom: 3px;
        }
        
        /* ===== CONTENT AREA ===== */
        .content-wrapper, .right-side { background-color: #F4F4F4 !important; }
        .content { padding: 22px 24px 14px 24px; }
        
        /* ===== DISEASE TITLE ===== */
        .doenca-titulo {
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 34px;
          font-weight: 700;
          color: #1A2535;
          margin: 0 0 16px 0;
          padding-bottom: 10px;
          border-bottom: 3px solid #1B3A6B;
          display: flex;
          align-items: center;
          letter-spacing: -0.2px;
        }
        .mosquito-icon {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 40px;
          height: 40px;
          background: #1B3A6B;
          border-radius: 6px;
          margin-right: 14px;
          box-shadow: 0 2px 8px rgba(27,58,107,0.35);
          flex-shrink: 0;
        }
        .mosquito-icon img {
          width: 28px;
          height: 28px;
        }
        .mosquito-emoji { font-size: 22px; line-height: 1; }
        
        /* ===== COLLAPSIBLE TUTORIAL ===== */
        .tutorial-collapsible {
          background: #EBF5FB;
          border: 2px solid #1B3A6B;
          border-radius: 8px;
          padding: 20px 24px;
          margin-bottom: 18px;
          box-shadow: 0 3px 12px rgba(27,58,107,0.12);
        }
        .tutorial-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin-bottom: 12px;
        }
        .tutorial-header h3 {
          margin: 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 22px;
        }
        .tutorial-close-btn {
          background: #1B3A6B !important;
          color: #ffffff !important;
          border: none !important;
          border-radius: 4px;
          padding: 6px 14px;
          font-size: 13px;
          font-weight: 600;
          cursor: pointer;
          box-shadow: 0 1px 4px rgba(27,58,107,0.25);
          transition: background 0.15s ease;
          text-decoration: none !important;
          display: inline-block;
        }
        .tutorial-close-btn:hover,
        .tutorial-close-btn:focus {
          background: #122856 !important;
          color: #ffffff !important;
          border: none !important;
          text-decoration: none !important;
        }
        .tutorial-content {
          color: #334155;
          font-size: 14px;
          line-height: 1.55;
        }
        .tutorial-content h4 {
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
          margin-top: 14px;
          margin-bottom: 6px;
        }
        .tutorial-content ol, .tutorial-content ul {
          padding-left: 20px;
          margin: 6px 0;
        }
        .tutorial-content li {
          margin-bottom: 4px;
        }
        .tutorial-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px;
          margin-top: 14px;
        }
        @media (max-width: 900px) {
          .tutorial-grid { grid-template-columns: 1fr; }
        }
        
        /* ===== METRIC CARDS ===== */
        .custom-card {
          background: #ffffff;
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          border: none !important;
          border-top: 3px solid #1B3A6B !important;
          width: 100%;
          padding: 14px 10px 12px 10px;
          text-align: center;
          margin-bottom: 14px;
        }
        .card-value {
          font-size: 34px;
          font-weight: 700;
          color: #1A2535;
          line-height: 1.1;
          font-variant-numeric: tabular-nums;
        }
        .card-label {
          font-size: 11px;
          font-weight: 700;
          color: #1B3A6B;
          text-transform: uppercase;
          letter-spacing: 1.2px;
          margin-top: 5px;
        }
        
        /* ===== GRAPH BOXES ===== */
        .graph-box {
          background: #ffffff;
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          border: 1px solid #B8C8D8 !important;
          border-top: 3px solid #1B3A6B !important;
          padding: 14px 10px 10px 10px;
          margin-bottom: 14px;
        }
        .graph-box .plotly, .graph-box .js-plotly-plot,
        .graph-box .plot-container, .graph-box .svg-container {
          width: 100% !important;
        }
        .graph-title {
          font-size: 13px;
          font-weight: 600;
          color: #1A2535;
          margin-bottom: 8px;
          padding-bottom: 5px;
          border-bottom: 1px solid #EDEDED;
        }
        .plot-download {
          display: flex;
          justify-content: center;
          margin: 8px 0 10px 0;
        }
        .plot-download .btn {
          background: #1B3A6B !important;
          border: 1px solid #122856 !important;
          border-radius: 4px !important;
          color: #ffffff !important;
          font-size: 12px !important;
          font-weight: 600 !important;
          padding: 6px 12px !important;
          box-shadow: 0 1px 4px rgba(27,58,107,0.18);
        }
        .plot-download .btn:hover,
        .plot-download .btn:focus {
          background: #122856 !important;
          color: #ffffff !important;
        }
        .plot-download .fa {
          margin-right: 6px;
        }
        
        /* ===== FIGURE CAPTIONS ===== */
        .fig-caption {
          font-size: 16px;
          color: #777;
          font-style: italic;
          margin-top: 7px;
          line-height: 1.45;
          padding-left: 8px;
          border-left: 2px solid #B8C8D8;
        }
        .landing-section, .context-box, .download-panel, .team-profile {
          background: #ffffff;
          border: 1px solid #B8C8D8;
          border-top: 3px solid #1B3A6B;
          border-radius: 6px;
          padding: 16px;
          margin-bottom: 14px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        .landing-section h3, .landing-section h4, .team-profile h4 {
          margin-top: 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
        }
        .context-box {
          color: #334155;
          line-height: 1.45;
        }
        .download-panel .dt-container {
          font-size: 12px;
        }
        .home-hero {
          background: #ffffff;
          border: 1px solid #B8C8D8;
          border-top: 4px solid #1B3A6B;
          border-radius: 8px;
          padding: 22px;
          margin-bottom: 14px;
          box-shadow: 0 2px 10px rgba(15,23,42,0.06);
        }
        .home-hero h3 {
          margin: 0 0 8px 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 26px;
        }
        .home-hero p {
          color: #334155;
          font-size: 15px;
          line-height: 1.5;
          max-width: 980px;
        }
        .hero-metrics {
          display: grid;
          grid-template-columns: repeat(4, minmax(0, 1fr));
          gap: 10px;
          margin-top: 16px;
        }
        .hero-metric {
          background: #F8FAFC;
          border: 1px solid #D8E0EA;
          border-radius: 6px;
          padding: 12px;
        }
        .hero-metric strong {
          display: block;
          color: #1B3A6B;
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.8px;
          margin-bottom: 4px;
        }
        .hero-metric span {
          color: #1A2535;
          font-weight: 700;
          font-size: 15px;
        }
        .contact-button {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          background: #1B3A6B;
          color: #ffffff !important;
          border: 1px solid #122856;
          border-radius: 4px;
          padding: 9px 14px;
          font-size: 13px;
          font-weight: 700;
          text-decoration: none !important;
          margin-top: 8px;
          box-shadow: 0 1px 5px rgba(27,58,107,0.2);
        }
        .contact-button:hover,
        .contact-button:focus {
          background: #122856;
          color: #ffffff !important;
        }
        .quality-panel {
          background: #ffffff;
          border: 1px solid #B8C8D8;
          border-top: 3px solid #1B3A6B;
          border-radius: 6px;
          padding: 16px;
          margin-bottom: 14px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        .quality-panel h4 {
          margin-top: 0;
          color: #1A2535;
          font-family: 'Source Serif 4', Georgia, serif;
        }
        .quality-grid {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 10px;
          margin-top: 10px;
        }
        .quality-card {
          background: #F8FAFC;
          border: 1px solid #D8E0EA;
          border-radius: 6px;
          padding: 11px 10px;
          min-height: 92px;
        }
        .quality-label {
          color: #475569;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.8px;
          text-transform: uppercase;
        }
        .quality-value {
          color: #1A2535;
          font-size: 24px;
          font-weight: 700;
          line-height: 1.15;
          margin-top: 5px;
        }
        .quality-note {
          color: #64748B;
          font-size: 11px;
          margin-top: 3px;
        }
        .team-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px;
          margin-top: 14px;
        }
        .team-card {
          background: #ffffff;
          border: 1px solid #D8E0EA;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(15,23,42,0.06);
          padding: 18px 18px 16px 18px;
          text-align: center;
          min-height: 245px;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: flex-start;
        }
        .team-avatar {
          width: 96px;
          height: 96px;
          border-radius: 50%;
          background: #EEF3F8;
          border: 1px solid #D8E0EA;
          color: #1B3A6B;
          font-family: 'Source Serif 4', Georgia, serif;
          font-size: 28px;
          font-weight: 700;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-bottom: 14px;
        }
        .team-card h4 {
          margin: 0 0 6px 0;
          color: #0F172A;
          font-family: 'Source Sans 3', 'Helvetica Neue', Arial, sans-serif;
          font-size: 17px;
          font-weight: 700;
        }
        .team-role {
          color: #64748B;
          font-size: 13px;
          margin-bottom: 10px;
        }
        .team-bio {
          color: #334155;
          font-size: 13px;
          line-height: 1.45;
          margin: 0 0 12px 0;
          max-width: 92%;
        }
        .team-links {
          margin-top: auto;
          display: flex;
          justify-content: center;
        }
        .team-link-btn {
          display: inline-flex;
          align-items: center;
          gap: 7px;
          border: 1px solid #D8E0EA;
          border-radius: 18px;
          padding: 7px 13px;
          color: #0F172A;
          background: #FFFFFF;
          font-size: 13px;
          font-weight: 600;
          text-decoration: none !important;
          box-shadow: 0 1px 4px rgba(15,23,42,0.04);
        }
        .team-link-btn:hover,
        .team-link-btn:focus {
          color: #ffffff;
          background: #1B3A6B;
          border-color: #1B3A6B;
        }
        @media (max-width: 900px) {
          .fixed-app-title {
            font-size: 15px;
            max-width: calc(100vw - 105px);
            overflow: hidden;
            text-overflow: ellipsis;
          }
          .content {
            padding: 14px 10px 10px 10px;
          }
          .doenca-titulo {
            font-size: 24px;
            align-items: flex-start;
          }
          .hero-metrics,
          .quality-grid {
            grid-template-columns: 1fr;
          }
          .graph-box {
            padding: 12px 8px 9px 8px;
          }
          .fig-caption {
            font-size: 13px;
          }
          .team-grid {
            grid-template-columns: 1fr;
          }
        }
        .mapa-bairros {
          width: 100%;
          min-height: 380px;
          border-radius: 6px;
          border: 1px solid #B8C8D8;
          overflow: hidden;
          background: #e5e7eb;
        }
        .mapa-legenda {
          background: rgba(255,255,255,0.94);
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.18);
          padding: 10px 12px;
          font-size: 12px;
          color: #1e293b;
          line-height: 1.4;
        }
        .mapa-legenda-item {
          display: flex;
          align-items: center;
          gap: 7px;
          margin-top: 4px;
        }
        .mapa-legenda-cor {
          width: 14px;
          height: 14px;
          border-radius: 3px;
          display: inline-block;
          border: 1px solid rgba(0,0,0,0.15);
        }
        
        /* ===== LAYOUT ===== */
        .col-sm-6, .col-sm-3, .col-sm-4, .col-sm-12 { padding-left: 6px; padding-right: 6px; }
        .row { margin-left: -6px; margin-right: -6px; margin-bottom: 4px; }
        .graph-row { margin-bottom: 0 !important; }
        .cards-destaque { margin-top: 0; }
        
        /* ===== SCROLLBAR ===== */
        ::-webkit-scrollbar { width: 6px; height: 6px; }
        ::-webkit-scrollbar-track { background: #f0f0f0; }
        ::-webkit-scrollbar-thumb { background: #1B3A6B; border-radius: 3px; }
        
        /* ===== TAB CONTENT PADDING ===== */
        .tab-content { padding-top: 2px; }
        
        /* ===== CARD OBITOS DESTAQUE ===== */
        .custom-card.card-obitos {
          border-top-color: #D97706 !important;
        }
        .custom-card.card-obitos .card-label {
          color: #B45309 !important;
        }
        .custom-card.card-obitos .card-value {
          color: #92400E;
        }
        
        /* ===== SELETOR MULTIPLO DE ANOS ===== */
        .seletor-anos {
          background: #F8FAFC;
          border: 1px solid #DCE5EF;
          border-radius: 8px;
          padding: 10px 14px 8px 14px;
          margin-bottom: 12px;
        }
        .seletor-anos-label {
          color: #1B3A6B;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 1.1px;
          text-transform: uppercase;
          margin-bottom: 6px;
        }
        .seletor-anos .form-group {
          margin-bottom: 0;
        }
        .seletor-anos .checkbox-inline {
          margin-left: 0;
          margin-right: 16px;
          color: #1A2535;
          font-size: 13px;
          font-weight: 600;
        }
        .seletor-anos .checkbox-inline input {
          margin-top: 2px;
        }
        .seletor-anos-help {
          color: #64748B;
          font-size: 11px;
          margin-top: 4px;
        }

        /* ===== PERIODO BADGE ===== */
        .periodo-badge {
          display: inline-flex;
          align-items: center;
          gap: 5px;
          background: #EBF0F8;
          color: #1B3A6B;
          font-size: 11px;
          font-weight: 600;
          letter-spacing: 0.8px;
          text-transform: uppercase;
          padding: 4px 12px;
          border-radius: 20px;
          border: 1px solid #B8C8D8;
          white-space: nowrap;
          margin-bottom: 14px;
        }
        /* ===== ANO ATIVO NO TITULO ===== */
        .titulo-ano-badge {
          font-family: 'Source Sans 3', 'Helvetica Neue', Arial, sans-serif;
          font-size: 22px;
          font-weight: 600;
          color: #1B3A6B;
          margin-left: 10px;
          background: #EBF0F8;
          padding: 2px 12px;
          border-radius: 6px;
          vertical-align: middle;
        }

        /* ===== ACESSIBILIDADE VISUAL E CLAREZA (VIRIDIS) ===== */
        :root {
          --viridis-1: #440154;
          --viridis-2: #3B528B;
          --viridis-3: #21918C;
          --viridis-4: #5EC962;
          --viridis-5: #FDE725;
          --ink: #0F172A;
          --muted: #475569;
          --line: #DCE3EA;
          --surface: #FFFFFF;
          --bg: #F7F9FB;
          --accent: #21918C;
          --accent-dark: #146C6A;
        }
        body { color: var(--ink); }
        .content-wrapper, .right-side { background: var(--bg) !important; }
        .content { padding: 20px 22px 12px 22px; }
        .skin-black .main-header .logo,
        .skin-black .main-header .navbar {
          background-color: #17313B !important;
          border-bottom: 3px solid var(--accent) !important;
        }
        .skin-black .main-sidebar, .skin-black .left-side { background-color: #10252D !important; }
        .skin-black .sidebar-menu > li > a {
          color: #C7D7DE !important;
          font-size: 11px;
          letter-spacing: 1.2px;
          padding: 13px 18px;
          border-bottom: 1px solid rgba(255,255,255,0.05) !important;
        }
        .skin-black .sidebar-menu > li:hover > a {
          background: rgba(33,145,140,0.20) !important;
          border-left-color: var(--viridis-4) !important;
          color: #FFFFFF !important;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li.active > a:hover {
          background: rgba(33,145,140,0.32) !important;
          border-left-color: var(--viridis-5) !important;
          color: #FFFFFF !important;
        }
        .doenca-titulo {
          font-size: 31px;
          color: var(--ink);
          border-bottom: 3px solid var(--accent);
          margin-bottom: 12px;
        }
        .mosquito-icon { background: var(--accent-dark); box-shadow: 0 2px 6px rgba(15,23,42,0.18); }
        .custom-card, .graph-box, .landing-section, .context-box,
        .download-panel, .quality-panel, .team-profile, .home-hero,
        .feedback-card {
          background: var(--surface);
          border: 1px solid var(--line) !important;
          border-top-color: var(--accent) !important;
          border-radius: 10px;
          box-shadow: 0 1px 3px rgba(15,23,42,0.06);
        }
        .custom-card { border-top-width: 3px !important; }
        .custom-card .card-label { color: var(--accent-dark); }
        .custom-card .card-value { color: var(--ink); }
        .custom-card.card-obitos { border-top-color: var(--viridis-2) !important; }
        .custom-card.card-obitos .card-label { color: var(--viridis-2) !important; }
        .custom-card.card-obitos .card-value { color: var(--viridis-1); }
        .graph-title { color: var(--ink); border-bottom: 1px solid var(--line); font-weight: 700; }
        .fig-caption {
          font-size: 12px;
          color: var(--muted);
          border-left: 2px solid var(--viridis-3);
          line-height: 1.45;
          margin-top: 8px;
          padding-left: 9px;
        }
        .context-box { color: #334155; line-height: 1.5; }
        .context-box strong, .method-note dt { color: var(--ink); }
        .home-hero { border-top-width: 4px; }
        .home-hero h3, .landing-section h3, .landing-section h4,
        .team-profile h4, .quality-panel h4, .feedback-card h4 {
          color: var(--ink);
        }
        .hero-metric { background: #F8FAFC; border-color: var(--line); }
        .hero-metric strong { color: var(--accent-dark); }
        .periodo-badge, .titulo-ano-badge {
          background: #E8F4F2; color: var(--accent-dark); border-color: #B9DDD9;
        }
        .plot-download .btn, .contact-button, .tutorial-close-btn {
          background: var(--accent-dark) !important;
          border-color: #0F5B59 !important;
          color: #FFFFFF !important;
        }
        .plot-download .btn:hover, .contact-button:hover, .tutorial-close-btn:hover {
          background: #0F5B59 !important;
        }
        .tutorial-collapsible { background: #F2FAF8; border-color: var(--accent); }
        .team-avatar { color: var(--accent-dark); background: #E8F4F2; border-color: #B9DDD9; }
        .team-link-btn:hover, .team-link-btn:focus { background: var(--accent-dark); border-color: var(--accent-dark); }
        .quality-card { background: #F8FAFC; border-color: var(--line); }
        .quality-label { color: var(--muted); }
        .quality-value { color: var(--ink); }
        .quality-note { color: #64748B; }
        ::-webkit-scrollbar-thumb { background: var(--accent); }

        /* Avisos e rótulos de escopo */
        .global-disclaimer {
          display: flex;
          align-items: flex-start;
          gap: 10px;
          background: #FFF9E8;
          border: 1px solid #E8D48A;
          border-left: 5px solid var(--viridis-5);
          border-radius: 8px;
          padding: 10px 14px;
          margin: 0 0 14px 0;
          color: #3F3A1F;
          font-size: 13px;
          line-height: 1.45;
        }
        .global-disclaimer .fa { color: #8A6D00; margin-top: 2px; }
        .global-disclaimer-badge {
          margin-left: auto;
          background: #8A6D00;
          color: #FFFFFF;
          border-radius: 20px;
          padding: 2px 10px;
          font-size: 10px;
          font-weight: 700;
          letter-spacing: .6px;
          text-transform: uppercase;
          white-space: nowrap;
        }
        .tab-disclaimer {
          display: flex;
          align-items: flex-start;
          gap: 9px;
          background: #F4F8FB;
          border: 1px solid var(--line);
          border-left: 4px solid var(--viridis-2);
          border-radius: 8px;
          padding: 10px 12px;
          margin: 0 0 12px 0;
          color: var(--muted);
          font-size: 12px;
          line-height: 1.45;
        }
        .tab-disclaimer .fa { color: var(--viridis-2); margin-top: 2px; }
        .tab-disclaimer-compact { font-size: 11.5px; padding: 8px 11px; }
        .benchmark-note {
          background: #F2FAF8;
          border: 1px dashed #9CCFC8;
          border-radius: 6px;
          padding: 7px 9px;
          margin: 7px 0 0 0;
          font-size: 11px;
          color: #335C59;
          line-height: 1.4;
        }
        .app-footer {
          margin: 10px 0 2px 0;
          padding: 14px 16px;
          border-top: 1px solid var(--line);
          color: var(--muted);
          font-size: 12px;
          line-height: 1.5;
          background: transparent;
        }
        .app-footer a { color: var(--accent-dark); font-weight: 600; }
        .app-footer-note { color: #64748B; font-size: 11px; margin-top: 3px; }

        /* Formulário de avaliação */
        .feedback-intro { border-top-color: var(--viridis-3) !important; }
        .feedback-meta { display: flex; flex-wrap: wrap; gap: 14px; margin-top: 8px; color: var(--muted); font-size: 12px; }
        .feedback-meta .fa { color: var(--accent-dark); margin-right: 4px; }
        .feedback-card { padding: 18px; margin-bottom: 14px; }
        .feedback-card h4 { margin-top: 0; }
        .feedback-help { color: var(--muted); font-size: 12.5px; }
        .feedback-card hr { border-top: 1px solid var(--line); margin: 18px 0 14px 0; }
        .likert-scale { display: flex; justify-content: space-between; gap: 10px; color: var(--muted); font-size: 11px; margin: 4px 0 8px 0; }
        .likert-row {
          display: grid;
          grid-template-columns: minmax(0, 1fr) auto;
          gap: 14px;
          align-items: center;
          padding: 8px 0;
          border-bottom: 1px solid #EEF2F6;
        }
        .likert-row:last-child { border-bottom: 0; }
        .likert-question { font-size: 13px; color: var(--ink); line-height: 1.35; }
        .likert-row .form-group { margin-bottom: 0; }
        .likert-row .shiny-options-group { display: flex; gap: 6px; align-items: center; margin: 0; }
        .likert-row .radio { margin: 0; }
        .likert-row .radio label { padding-left: 0; }
        .likert-row .radio input { position: static; margin: 0 3px 0 0; }
        .feedback-actions { display: flex; flex-wrap: wrap; gap: 10px; align-items: center; margin-top: 14px; }
        .feedback-actions .btn-primary {
          background: var(--accent-dark) !important;
          border-color: var(--accent-dark) !important;
          color: #FFFFFF !important;
          font-weight: 700;
        }
        .feedback-actions .btn-default { border-color: var(--line); color: var(--ink); }
        .feedback-status { margin-top: 14px; padding: 11px 13px; border-radius: 8px; font-size: 13px; line-height: 1.5; }
        .feedback-status.ok { background: #E8F7EE; border: 1px solid #A7DDB9; color: #14532D; }
        .feedback-status.warn { background: #FFF9E8; border: 1px solid #E8D48A; color: #5B4A08; }
        .feedback-status.err { background: #FDECEC; border: 1px solid #F2B8B8; color: #7F1D1D; }
        .feedback-status a { font-weight: 700; color: var(--accent-dark); }
        .sr-only { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0); white-space: nowrap; border: 0; }
        *:focus-visible { outline: 3px solid var(--viridis-5) !important; outline-offset: 2px; }
        @media (max-width: 900px) {
          .global-disclaimer { flex-wrap: wrap; }
          .global-disclaimer-badge { margin-left: 0; }
          .likert-row { grid-template-columns: 1fr; gap: 4px; }
          .likert-row .shiny-options-group { justify-content: flex-start; }
          .feedback-actions { flex-direction: column; align-items: stretch; }
          .doenca-titulo { font-size: 24px; }
        }
      ")),
      tags$script(HTML("
        // FormSubmit é chamado pelo navegador do usuário, não pelo servidor.
        // Isso evita bloqueios de IP de datacenter e mantém o Referer/Origin
        // do app publicado, requisitos do serviço.
        Shiny.addCustomMessageHandler('feedback_formsubmit', function(msg) {
          if (!msg || !msg.endpoint) {
            Shiny.setInputValue('feedback_js_result', {
              ok: false, status: 0, message: 'Endpoint ausente'
            }, {priority: 'event'});
            return;
          }
          // mode: 'no-cors' envia um simple request form-encoded, sem preflight.
          // O navegador não expõe a resposta, mas o FormSubmit recebe o POST.
          var body = new URLSearchParams();
          var payload = msg.payload || {};
          Object.keys(payload).forEach(function(k) {
            if (payload[k] !== null && payload[k] !== undefined) {
              body.append(k, String(payload[k]));
            }
          });
          var done = false;
          var finish = function(ok, message) {
            if (done) return;
            done = true;
            Shiny.setInputValue('feedback_js_result', {
              ok: ok, status: ok ? 200 : 0, message: message
            }, {priority: 'event'});
          };
          var attempt = function(restantes) {
            var controller = new AbortController();
            var timer = setTimeout(function() { controller.abort(); }, 8000);
            fetch(msg.endpoint, {
              method: 'POST',
              mode: 'no-cors',
              body: body,
              signal: controller.signal
            }).then(function() {
              clearTimeout(timer);
              finish(true, 'Requisição enviada ao FormSubmit');
            }).catch(function(err) {
              clearTimeout(timer);
              if (restantes > 1) {
                setTimeout(function() { attempt(restantes - 1); }, 1200);
              } else {
                finish(false, String(err));
              }
            });
          };
          attempt(2);
        });
      "))
    ),
    tags$div(class = "fixed-app-title", "Painel Arboviroses — Campos/RJ"),
    div(class = "global-disclaimer", role = "note",
      tags$i(class = "fa fa-info-circle", `aria-hidden` = "true"),
      tags$div(
        tags$strong("Dados de notificação não são incidência real. "),
        "Os indicadores são descritivos, não estabelecem causalidade e não substituem avaliação clínica ou decisão sanitária local. Consulte a aba ",
        tags$strong("Métodos"), " e a nota de licença/FAIR no README antes de reutilizar."
      ),
      tags$span(class = "global-disclaimer-badge", "Uso responsável")
    ),
    tabItems(
      tabItem(
        tabName = "inicio",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("Painel de Inteligência Epidemiológica para Chikungunya, Dengue e Zika em Campos dos Goytacazes (2020-2025)")
        ),
        aviso_uso_dados(),
        # TUTORIAL COLLAPSIBLE - Integrated into Início tab
        div(class = "tutorial-collapsible", id = "tutorial-box",
          div(class = "tutorial-header",
            h3("\U0001F4D6 Tutorial e Interpretação"),
            actionButton("fechar_tutorial", label = "\u2715 Fechar", class = "tutorial-close-btn")
          ),
          div(class = "tutorial-content",
            p("Este painel foi criado para apoiar a leitura epidemiológica das arboviroses em Campos dos Goytacazes. Ele reúne indicadores descritivos de Chikungunya, Dengue e Zika, permitindo observar a magnitude dos registros, o perfil dos casos, a completude das notificações e a distribuição espacial da dengue por bairro. Para detalhes sobre fontes, métodos e limitações, consulte a aba Métodos."),
            tags$ol(
              tags$li("Escolha a doença no menu lateral: Chikungunya, Dengue ou Zika."),
              tags$li("Use o seletor de período dentro de cada aba de doença para escolher um ou vários anos."),
              tags$li("Comece pelos cartões-resumo para entender rapidamente curas, óbitos, casos confirmados, descartados, inconclusivos e campos ignorados/brancos."),
              tags$li("Compare os gráficos de série temporal, sexo, faixa etária, gestação, raça/cor e escolaridade para reconhecer padrões e diferenças entre os grupos."),
              tags$li("Na aba de Dengue, explore também o mapa por bairros e o ranking dos bairros com maior número de registros."),
              tags$li("Use os botões abaixo de cada gráfico para baixar figuras em alta resolução e use o painel de downloads para exportar tabelas e plots em formatos de relatório.")
            ),
            div(class = "tutorial-grid",
              div(
                h4("Como interpretar os gráficos"),
                p("A série temporal mostra como os casos confirmados variam ao longo dos anos e ajuda a identificar picos, quedas e possíveis períodos de maior transmissão. Os gráficos por sexo e faixa etária ajudam a observar quais grupos aparecem com maior frequência nos registros."),
                p("Os recortes por raça/cor, escolaridade e situação gestacional devem ser lidos com cuidado: eles podem sugerir desigualdades, padrões de exposição, acesso ao serviço de saúde e diferenças de preenchimento, mas não provam causalidade isoladamente."),
                p("Campos ignorados/brancos são indicadores importantes de qualidade da informação. Quando aparecem em grande volume, eles reduzem a confiança de interpretações sobre o perfil social ou biológico dos casos.")
              ),
              div(
                h4("Para que o site serve"),
                p("O aplicativo serve como ferramenta de apoio para ensino, pesquisa, vigilância epidemiológica e comunicação científica. Ele organiza dados brutos e agregados em visualizações que facilitam a comparação entre doenças, anos e perfis populacionais."),
                p("Na prática, o painel pode ajudar a construir relatórios, selecionar figuras para apresentações, levantar hipóteses de investigação, discutir qualidade de preenchimento das fichas e orientar perguntas para análises futuras mais profundas.")
              ),
              div(
                h4("Exemplos de inferência epidemiológica"),
                tags$ul(
                  tags$li("Aumento em crianças e adolescentes pode sugerir mudanças de exposição domiciliar, escolar ou comunitária."),
                  tags$li("Diferenças por sexo podem refletir exposição, comportamento de busca por cuidado ou vigilância em gestantes."),
                  tags$li("Maior proporção de idosos pode levantar hipóteses sobre gravidade e risco de evolução desfavorável."),
                  tags$li("Sub-registro de escolaridade ou raça/cor limita inferências sociais e deve ser relatado.")
                )
              ),
              div(
                h4("Boas práticas de uso"),
                tags$ul(
                  tags$li("Compare categorias sempre considerando o período selecionado no filtro."),
                  tags$li("Evite concluir que um grupo tem maior risco apenas porque aparece mais no gráfico; diferenças de população, acesso e registro também influenciam."),
                  tags$li("Ao usar uma figura em relatório, cite o período, a doença, a fonte SINAN/SVS e o recorte analisado."),
                  tags$li("Quando houver muitos registros ignorados/brancos, destaque essa limitação junto da interpretação.")
                )
              )
            )
          )
        ),
        div(class = "home-hero",
          h3("Painel de Inteligência Epidemiológica para Arboviroses em Campos dos Goytacazes"),
          p("Leitura integrada de Chikungunya, Dengue e Zika em Campos dos Goytacazes, com foco em vigilância epidemiológica, qualidade da informação, comparação temporal, perfil sociodemográfico e apoio à produção de relatórios e apresentações."),
          div(class = "hero-metrics",
            div(class = "hero-metric", tags$strong("Doenças"), span("Chikungunya, Dengue e Zika")),
            div(class = "hero-metric", tags$strong("Período"), span("2020-2025")),
            div(class = "hero-metric", tags$strong("Fonte"), span("SINAN/SVS, microdatasus, SIDRA e dados locais")),
            div(class = "hero-metric", tags$strong("Uso"), span("Ensino, pesquisa e vigilância"))
          )
        ),
        fluidRow(
          column(4, uiOutput("home_card_total")),
          column(4, uiOutput("home_card_incidencia")),
          column(4, uiOutput("home_card_obitos"))
        ),
        fluidRow(
          column(4, uiOutput("home_card_pico")),
          column(4, uiOutput("home_card_predominante")),
          column(4, uiOutput("home_card_atualizacao"))
        ),
        div(class = "landing-section",
          h4("Comparador entre agravos"),
          fluidRow(
            column(4,
              selectInput(
                "home_comparador_variavel",
                "Variável",
                choices = c(
                  "Ano" = "ano",
                  "Sexo" = "sexo",
                  "Faixa etária" = "faixa",
                  "Raça/cor" = "raca",
                  "Gestação" = "gestacao"
                ),
                selected = "ano"
              )
            )
          ),
          plotOutput("home_comparador_plot", height = "400px"),
          DTOutput("home_comparador_tabela")
        ),
        div(class = "landing-section",
          h4("Qualidade dos Dados : Completude por Variável"),
          p("Tabela com o percentual de registros ignorados, brancos ou ausentes por agravo e variável. O período e agravo selecionados abaixo filtram os dados exibidos. Valores acima de 30% indicam atenção; acima de 40%, alerta; acima de 60%, situação crítica para inferências."),
          fluidRow(
            column(4,
              selectInput(
                "home_qualidade_agravo",
                "Agravo",
                choices = c("Todos", "Chikungunya", "Dengue", "Zika"),
                selected = "Todos"
              )
            )
          ),
          DTOutput("home_qualidade_tabela"),
          uiOutput("home_alertas_qualidade")
        ),
        div(class = "landing-section",
          h3("Projeto"),
          p("Este aplicativo Shiny organiza dados de arboviroses em uma experiência interativa para leitura epidemiológica, sociodemográfica e biológica. As abas de dengue e zika utilizam dados baixados pelo pacote microdatasus, filtrados e agregados para Campos dos Goytacazes."),
          p("A plataforma integra um projeto de iniciação científica do Instituto Federal de Educação, Ciência e Tecnologia Fluminense, Campus Campos Guarus, financiado pelo CNPq e vinculado ao curso de Bacharelado em Enfermagem do IFF Guarus. O projeto é coordenado pela Profa. Dra. Karla Rangel Ribeiro e articula vigilância em saúde, ciência de dados, formação em enfermagem e comunicação científica aplicada ao território."),
          p("O site foi desenvolvido com o intuito de funcionar como um painel de apoio à decisão estratégica e baseada em dados para a gestão de saúde do município, oferecendo uma leitura organizada dos registros de arboviroses e servindo também como exemplo metodológico para outras iniciativas de vigilância, ensino, extensão e pesquisa aplicada.")
        ),
        fluidRow(
          column(4, div(class = "landing-section",
            h4("Objetivos"),
            tags$ul(
              tags$li("Explorar padrões temporais, etários, sexuais, gestacionais e sociodemográficos."),
              tags$li("Avaliar qualidade de preenchimento por campos ignorados ou brancos."),
              tags$li("Gerar tabelas e figuras exportáveis para relatório, apresentação e publicação.")
            )
          )),
          column(4, div(class = "landing-section",
            h4("Significado biológico"),
            p("Diferenças por idade, sexo, gestação e raça/cor podem sugerir padrões de exposição, vulnerabilidade, acesso ao cuidado, busca por diagnóstico e gravidade potencial. Essas associações não provam causalidade, mas ajudam a gerar hipóteses.")
          )),
          column(4, div(class = "landing-section",
            h4("O que explorar"),
            p("Use os filtros de ano, gráficos e tabelas para comparar incidência observada, perfil dos casos, completude das notificações e mudanças no tempo. Em seguida, baixe as figuras e tabelas da visualização selecionada.")
          ))
        ),
        div(class = "landing-section",
          h4("Estado atual e perspectivas"),
          p("No estágio atual, o painel apresenta principalmente indicadores sociodemográficos e epidemiológicos descritivos para cada arbovirose, como distribuição por sexo, idade, escolaridade, raça/cor, situação gestacional, evolução, classificação final e série temporal. Esses recortes permitem reconhecer grupos mais registrados, lacunas de preenchimento, possíveis desigualdades de exposição e padrões que merecem investigação complementar."),
          p("As perspectivas futuras incluem a integração de modelos de machine learning interpretável para aprofundar a análise dos dados, identificar combinações de fatores associadas a maior risco, apoiar a detecção de padrões temporais e espaciais e produzir explicações compreensíveis para profissionais de saúde, gestores e pesquisadores. A proposta é que o painel evolua de um ambiente descritivo para uma ferramenta de inteligência epidemiológica, mantendo transparência metodológica, rastreabilidade dos dados e utilidade prática para a tomada de decisão.")
        ),
        div(class = "landing-section",
          h4("Sugestões"),
          p("Estamos abertos a sugestões, críticas, correções e ideias para aprimorar este painel. Contribuições sobre novas visualizações, melhorias de interpretação, ajustes metodológicos ou possibilidades de uso em ensino, pesquisa e vigilância em saúde são muito bem-vindas."),
          p(
            "Para avaliar utilidade, clareza, confiança e usabilidade, responda ao formulário estruturado na aba ",
            tags$strong("Avaliação"),
            ". Para contato direto, escreva para ",
            tags$a("ryan.paulo@gsuite.iff.edu.br", href = "mailto:ryan.paulo@gsuite.iff.edu.br"),
            "."
          ),
          tags$a(
            href = "#shiny-tab-feedback",
            `data-toggle` = "tab",
            `data-value` = "feedback",
            class = "contact-button",
            tags$i(class = "fa fa-clipboard-check"),
            "Responder à avaliação"
          ),
          tags$a(
            href = "mailto:ryan.paulo@gsuite.iff.edu.br?subject=Sugest%C3%A3o%20para%20o%20Painel%20de%20Arboviroses",
            class = "contact-button",
            style = "margin-left:8px; background:#475569 !important;",
            tags$i(class = "fa fa-envelope"),
            "Enviar e-mail"
          )
        )
      ),
      # CHIKUNGUNYA
      tabItem(
        tabName = "chik",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("CHIKUNGUNYA"),
          uiOutput("chik_titulo_ano")
        ),
        aviso_uso_dados(compacto = TRUE),
        fluidRow(
          column(8, seletor_anos("chik_anos", dados$Chikungunya$Ano)),
          column(4, uiOutput("chik_periodo"))
        ),
        fluidRow(
          column(3, uiOutput("chik_card_cura")),
          column(3, uiOutput("chik_card_obitos")),
          column(3, uiOutput("chik_card_descartados")),
          column(3, uiOutput("chik_card_ignorados"))
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("chik_card_confirmados")),
          column(6, uiOutput("chik_card_inconclusivos"))
        ),
        div(class = "context-box",
          tags$strong("Leitura guiada: "),
          "observe a evolução temporal e os perfis por sexo, idade, gestação, escolaridade e raça/cor. Campos ignorados/brancos indicam limites de completude e devem moderar inferências causais."
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("chik_card_incidencia")),
          column(6, uiOutput("chik_card_populacao"))
        ),
        nota_metodologica_doenca(
          "Chikungunya",
          "SINAN/SVS em tabela agregada do projeto.",
          "Casos classificados como confirmados na base agregada disponível no painel.",
          "Dados anuais agregados; não há granularidade mensal/semanal nesta aba. Notificações podem sofrer sub-registro, atraso de digitação e incompletude de campos."
        ),
        dashboard_qualidade_dados("chik_qualidade"),
        div(class = "graph-box",
          div(class = "graph-title", "Incidência anual por 100 mil habitantes"),
          plotlyOutput("chik_incidencia", height = "300px"),
          botao_download_grafico("chik_download_incidencia"),
          div(class = "benchmark-note", tags$strong("Linhas tracejadas: "), "referência do Estado do RJ e do Brasil (SINAN/SVS; população IBGE/SIDRA). ", tags$em("Comparação contextual, não diagnóstico local.")),
          div(class="fig-caption","O que mostra: casos confirmados de chikungunya padronizados pela população estimada de Campos dos Goytacazes. Como interpretar: compare anos com maior carga relativa. Atenção metodológica: a taxa usa casos notificados e população SIDRA/IBGE.")
        ),
        painel_downloads("chik"),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Raça/Cor"),
                 plotlyOutput("chik_etnia", height = "280px"),
                 botao_download_grafico("chik_download_etnia")
              ,div(class="fig-caption","Figura 1. Distribuição proporcional dos casos notificados de chikungunya segundo raça/cor autodeclarada. Os segmentos refletem a proporção relativa de cada categoria em relação ao total de casos com raça informada. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Sexo e Ano"),
                 plotlyOutput("chik_sexo", height = "280px"),
                 botao_download_grafico("chik_download_sexo")
              ,div(class="fig-caption","Figura 2. Número absoluto de casos notificados de chikungunya por sexo (masculino, feminino e ignorado/branco) e ano de notificação. Permite identificar a predominância do sexo feminino ao longo da série. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
               div(class = "graph-title", "Evolução Temporal dos Casos"),
               plotlyOutput("chik_serie", height = "280px"),
               botao_download_grafico("chik_download_serie")
              ,div(class="fig-caption","Figura 3. Série histórica anual de casos confirmados de chikungunya. O pico de 2024 (n=1.460) reflete o maior surto do período analisado. Período: 2020-2025. Fonte: SINAN/SVS.")
          )
        ),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
               div(class = "graph-title", "Situação Gestacional"),
               plotlyOutput("chik_gestacao", height = "320px"),
               botao_download_grafico("chik_download_gestacao")
              ,div(class="fig-caption","Figura 4. Casos de chikungunya segundo situação gestacional: 1º, 2º e 3º trimestres, não gestante, não se aplica (sexo masculino/crianças), idade gestacional ignorada e campo ignorado/branco. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
            div(class = "graph-title", "Distribuição por Faixa Etária"),
            plotlyOutput("chik_faixa_etaria", height = "320px"),
            botao_download_grafico("chik_download_faixa")
              ,div(class="fig-caption","Figura 5. Casos confirmados de chikungunya distribuídos por faixa etária (em anos: <1, 1-4, 5-9, 10-14, 15-19, 20-39, 40-59, 60-64, 65-69, 70-79, 80+). Adultos de 40-59 anos concentram a maior carga. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
              div(class = "graph-title", "Distribuição por Escolaridade"),
              plotlyOutput("chik_escolaridade", height = "320px"),
              botao_download_grafico("chik_download_escolaridade")
              ,div(class="fig-caption","Figura 6. Casos de chikungunya segundo nível de escolaridade do paciente. A elevada frequência de ignorado/branco reflete sub-registro e limita interpretações causais. Período: 2020-2025. Fonte: SINAN/SVS.")
          )
        )
      ),
      
      # DENGUE
      tabItem(
        tabName = "dengue",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("DENGUE"),
          uiOutput("dengue_titulo_ano")
        ),
        aviso_uso_dados(compacto = TRUE),
        fluidRow(
          column(8, seletor_anos("dengue_anos", dados$Dengue$Ano)),
          column(4, uiOutput("dengue_periodo"))
        ),
        fluidRow(
          column(3, uiOutput("dengue_card_cura")),
          column(3, uiOutput("dengue_card_obitos")),
          column(3, uiOutput("dengue_card_descartados")),
          column(3, uiOutput("dengue_card_ignorados"))
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("dengue_card_confirmados")),
          column(6, uiOutput("dengue_card_inconclusivos"))
        ),
        texto_contexto_dengue,
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("dengue_card_incidencia")),
          column(6, uiOutput("dengue_card_populacao"))
        ),
        nota_metodologica_doenca(
          "Dengue",
          "SINAN-DENGUE/SVS baixado pelo pacote microdatasus; população IBGE/SIDRA para incidência.",
          "Classificação final compatível com dengue ou febre hemorrágica, excluindo descartados e registros classificados como chikungunya.",
          "Dados notificados não equivalem a todos os casos ocorridos; há risco de subnotificação, atraso de encerramento, mudanças de critério e divergências de nomes de bairros."
        ),
        dashboard_qualidade_dados("dengue_qualidade"),
        div(class = "graph-box",
          div(class = "graph-title", "Incidência anual por 100 mil habitantes"),
          plotlyOutput("dengue_incidencia", height = "300px"),
          botao_download_grafico("dengue_download_incidencia"),
          div(class = "benchmark-note", tags$strong("Linhas tracejadas: "), "referência do Estado do RJ e do Brasil (SINAN/SVS; população IBGE/SIDRA). ", tags$em("Comparação contextual, não diagnóstico local.")),
          div(class="fig-caption","O que mostra: casos confirmados de dengue padronizados pela população estimada de Campos dos Goytacazes. Como interpretar: compare anos com maior carga relativa. Atenção metodológica: a taxa usa casos notificados e população SIDRA/IBGE.")
        ),
        painel_downloads("dengue"),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Raça/Cor"),
                 plotlyOutput("dengue_etnia", height = "280px"),
                 botao_download_grafico("dengue_download_etnia")
              ,div(class="fig-caption","Figura 1. Distribuição proporcional dos casos notificados de dengue segundo raça/cor autodeclarada, agregada automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Sexo e Ano"),
                 plotlyOutput("dengue_sexo", height = "280px"),
                 botao_download_grafico("dengue_download_sexo")
              ,div(class="fig-caption","Figura 2. Número absoluto de casos notificados de dengue por sexo e ano. Os valores são calculados a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
               div(class = "graph-title", "Evolução Temporal dos Casos"),
               plotlyOutput("dengue_serie", height = "280px"),
               botao_download_grafico("dengue_download_serie")
              ,div(class="fig-caption","Figura 3. Série histórica anual de casos confirmados de dengue, calculada automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020-2025. Fonte: SINAN/SVS.")
          )
        ),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
               div(class = "graph-title", "Situação Gestacional"),
               plotlyOutput("dengue_gestacao", height = "320px"),
               botao_download_grafico("dengue_download_gestacao")
              ,div(class="fig-caption","Figura 4. Casos de dengue segundo situação gestacional (1º, 2º e 3º trimestres, não gestante, não se aplica e dados ignorados/brancos). Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
            div(class = "graph-title", "Distribuição por Faixa Etária"),
            plotlyOutput("dengue_faixa_etaria", height = "320px"),
            botao_download_grafico("dengue_download_faixa")
              ,div(class="fig-caption","Figura 5. Casos confirmados de dengue por faixa etária, agregados automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
              div(class = "graph-title", "Distribuição por Escolaridade"),
              plotlyOutput("dengue_escolaridade", height = "320px"),
              botao_download_grafico("dengue_download_escolaridade")
              ,div(class="fig-caption","Figura 6. Casos de dengue por nível de escolaridade, agregados automaticamente a partir dos registros baixados do SINAN-DENGUE pelo pacote microdatasus. Período: 2020-2025. Fonte: SINAN/SVS.")
          )
        ),
        div(class = "landing-section",
          h4("Mapa de dengue por bairro"),
          p("Distribuição espacial dos registros de dengue por bairro no período selecionado, calculada a partir das planilhas locais e unida à malha de bairros do geobr/IBGE. A escala de cores representa o número de notificações."),
          uiOutput("dengue_mapa_bairros"),
          painel_correspondencia_bairros()
        ),

        # ---- DENGUE ----
        div(class = "landing-section",
          h4("Classificação de risco — Grupos oficiais A/B/C/D"),
          fluidRow(
            column(12, seletor_anos("risco_dengue_anos", dados$Dengue$Ano))
          ),
          fluidRow(
            column(3, uiOutput("risco_card_A")),
            column(3, uiOutput("risco_card_B")),
            column(3, uiOutput("risco_card_C")),
            column(3, uiOutput("risco_card_D"))
          ),
          fluidRow(
            column(6, class = "graph-box",
              div(class = "graph-title", "Distribuição por grupo de risco"),
              plotlyOutput("risco_dengue_donut", height = "320px")
            ),
            column(6, class = "graph-box",
              div(class = "graph-title", "Evolução anual por grupo de risco"),
              plotlyOutput("risco_dengue_serie", height = "320px")
            )
          ),
          div(class = "graph-box",
            div(class = "graph-title", "Tabela por bairro — casos por grupo de risco"),
            p("Distribuição dos casos classificados por bairro de residência. Bairros com casos nos Grupos C (sinais de alarme) e D (dengue grave) concentram maior gravidade."),
            downloadButton("risco_dengue_bairros_download", "Baixar CSV"),
            DTOutput("risco_dengue_bairros_dt")
          ),
          div(class = "context-box",
            tags$strong("Nota metodológica: "),
            "a classificação é recalculada a partir dos campos de sinais de alarme (ALRM_*) e gravidade (GRAV_*) do SINAN-DENGUE, além de idade, gestação, comorbidades, petéquias (sangramento de pele) e prova do laço. Casos com campos de alarme/gravidade não preenchidos são classificados como A ou B conforme os demais fatores de risco presentes."
          )
        )
      ),
      
      # ZIKA
      tabItem(
        tabName = "zika",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("ZIKA"),
          uiOutput("zika_titulo_ano")
        ),
        aviso_uso_dados(compacto = TRUE),
        fluidRow(
          column(8, seletor_anos("zika_anos", dados$Zika$Ano)),
          column(4, uiOutput("zika_periodo"))
        ),
        fluidRow(
          column(3, uiOutput("zika_card_cura")),
          column(3, uiOutput("zika_card_obitos")),
          column(3, uiOutput("zika_card_descartados")),
          column(3, uiOutput("zika_card_ignorados"))
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("zika_card_confirmados")),
          column(6, uiOutput("zika_card_inconclusivos"))
        ),
        div(class = "context-box",
          tags$strong("Leitura guiada: "),
          "interprete a distribuição de Zika considerando vigilância em gestantes, investigação clínica e alta sensibilidade a sub-registro. As visualizações ajudam a localizar padrões e lacunas de preenchimento."
        ),
        fluidRow(class = "cards-destaque",
          column(6, uiOutput("zika_card_incidencia")),
          column(6, uiOutput("zika_card_populacao"))
        ),
        nota_metodologica_doenca(
          "Zika",
          "SINAN-ZIKA/SVS baixado pelo pacote microdatasus; população IBGE/SIDRA para incidência.",
          "Classificação final compatível com confirmação de Zika após processamento do SINAN-ZIKA, excluindo descartados e inconclusivos.",
          "Zika é sensível a sub-registro, investigação em gestantes, mudanças de vigilância e baixa confirmação laboratorial; interpretar perfis sociais junto da incompletude dos campos."
        ),
        dashboard_qualidade_dados("zika_qualidade"),
        div(class = "graph-box",
          div(class = "graph-title", "Incidência anual por 100 mil habitantes"),
          plotlyOutput("zika_incidencia", height = "300px"),
          botao_download_grafico("zika_download_incidencia"),
          div(class = "benchmark-note", tags$strong("Linhas tracejadas: "), "referência do Estado do RJ e do Brasil (SINAN/SVS; população IBGE/SIDRA). ", tags$em("Comparação contextual, não diagnóstico local.")),
          div(class="fig-caption","O que mostra: casos confirmados de Zika vírus padronizados pela população estimada de Campos dos Goytacazes. Como interpretar: compare anos com maior carga relativa. Atenção metodológica: a taxa usa casos notificados e população SIDRA/IBGE.")
        ),
        painel_downloads("zika"),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Raça/Cor"),
                 plotlyOutput("zika_etnia", height = "280px"),
                 botao_download_grafico("zika_download_etnia")
              ,div(class="fig-caption","Figura 1. Distribuição proporcional dos casos notificados de Zika vírus segundo raça/cor autodeclarada. O alto percentual de ignorado/branco compromete a representatividade dos demais grupos. Período: 2021-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
                 div(class = "graph-title", "Distribuição por Sexo e Ano"),
                 plotlyOutput("zika_sexo", height = "280px"),
                 botao_download_grafico("zika_download_sexo")
              ,div(class="fig-caption","Figura 2. Número absoluto de casos notificados de Zika vírus por sexo e ano. O predomínio feminino é consistente em todo o período, possivelmente relacionado à maior busca por assistência durante a gestação. Período: 2021-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
               div(class = "graph-title", "Evolução Temporal dos Casos"),
               plotlyOutput("zika_serie", height = "280px"),
               botao_download_grafico("zika_download_serie")
              ,div(class="fig-caption","Figura 3. Série histórica anual de casos confirmados de Zika vírus. O período mostra tendência de queda após o pico epidêmico; os anos de 2023 e 2024 registraram leve elevação. Período: 2021-2025. Fonte: SINAN/SVS.")
          )
        ),
        fluidRow(class = "graph-row",
          column(4, class = "graph-box",
               div(class = "graph-title", "Situação Gestacional"),
               plotlyOutput("zika_gestacao", height = "320px"),
               botao_download_grafico("zika_download_gestacao")
              ,div(class="fig-caption","Figura 4. Casos de Zika vírus segundo situação gestacional. Destaque para o volume de idade gestacional não informada em 2022 (n = 26), sugerindo possível sub-registro do trimestre. Período: 2021-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
            div(class = "graph-title", "Distribuição por Faixa Etária"),
            plotlyOutput("zika_faixa_etaria", height = "320px"),
            botao_download_grafico("zika_download_faixa")
              ,div(class="fig-caption","Figura 5. Casos confirmados de Zika vírus por faixa etária. Adultos de 20-59 anos concentram a maior parte dos casos; a presença em crianças (≤1 ano) pode indicar transmissão vertical. Período: 2021-2025. Fonte: SINAN/SVS.")
          ),
          column(4, class = "graph-box",
              div(class = "graph-title", "Distribuição por Escolaridade"),
              plotlyOutput("zika_escolaridade", height = "320px"),
              botao_download_grafico("zika_download_escolaridade")
              ,div(class="fig-caption","Figura 6. Casos de Zika vírus por nível de escolaridade. O ensino médio completo e o grupo ignorado/branco predominam; interpretações sobre vulnerabilidade socioeconômica devem considerar o elevado sub-registro. Período: 2021-2025. Fonte: SINAN/SVS.")
          )
        )
      ),
      # AVALIAÇÃO E CO-DESIGN
      tabItem(
        tabName = "feedback",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("AVALIAÇÃO E CO-DESIGN")
        ),
        aviso_uso_dados(compacto = TRUE),
        div(class = "landing-section feedback-intro",
          h3("Ajude a melhorar o painel"),
          p("Esta avaliação estruturada tem como objetivo medir utilidade, clareza, confiança e usabilidade percebida por profissionais da vigilância, gestão, ensino e pesquisa. O questionário leva cerca de 4 minutos e segue uma escala Likert de 5 pontos."),
          div(class = "feedback-meta",
            tags$span(tags$i(class = "fa fa-clock"), " 4 minutos"),
            tags$span(tags$i(class = "fa fa-lock"), " Respostas voluntárias"),
            tags$span(tags$i(class = "fa fa-envelope"), " Retorno para ", tags$a(href = "mailto:ryan.paulo@gsuite.iff.edu.br", "ryan.paulo@gsuite.iff.edu.br"))
          )
        ),
        div(class = "feedback-card",
          h4("1. Sobre você"),
          p(class = "feedback-help", "O e-mail é opcional e serve apenas para retorno sobre melhorias ou esclarecimentos."),
          fluidRow(
            column(6, selectInput("feedback_perfil", "Perfil principal",
              choices = c(
                "Vigilância epidemiológica" = "vigilancia",
                "Gestão de saúde" = "gestao",
                "Ensino" = "ensino",
                "Pesquisa" = "pesquisa",
                "Assistência clínica" = "assistencia",
                "Comunidade / outro" = "outro"
              )
            )),
            column(6, textInput("feedback_instituicao", "Instituição ou território (opcional)"))
          ),
          fluidRow(
            column(6, textInput("feedback_email", "E-mail para retorno (opcional)")),
            column(6, selectInput("feedback_tempo_uso", "Tempo de uso do painel",
              choices = c("Primeiro acesso" = "primeiro", "Menos de 1 mês" = "<1m", "1 a 6 meses" = "1-6m", "Mais de 6 meses" = ">6m")
            ))
          ),
          hr(),
          h4("2. Avaliação de utilidade e usabilidade"),
          p(class = "feedback-help", "Escolha de 1 (discordo totalmente) a 5 (concordo totalmente)."),
          div(class = "likert-scale",
            tags$span("1 = discordo totalmente"),
            tags$span("3 = neutro"),
            tags$span("5 = concordo totalmente")
          ),
          campo_likert("feedback_q1", "Consigo navegar e encontrar as informações com facilidade."),
          campo_likert("feedback_q2", "Os gráficos e mapas são claros e fáceis de interpretar."),
          campo_likert("feedback_q3", "O painel é útil para a rotina de vigilância ou gestão em saúde."),
          campo_likert("feedback_q4", "Confio nos dados e nos indicadores apresentados."),
          campo_likert("feedback_q5", "Compreendo os limites metodológicos e o risco de subnotificação."),
          campo_likert("feedback_q6", "A documentação (Métodos, legendas e notas) é suficiente para usar o painel."),
          campo_likert("feedback_q7", "As cores e a apresentação são acessíveis, inclusive para pessoas daltônicas."),
          campo_likert("feedback_q8", "Eu recomendaria este painel a colegas de trabalho ou pesquisa."),
          hr(),
          h4("3. Comentários e sugestões"),
          textAreaInput("feedback_comentarios", "O que deve ser mantido, melhorado ou incluído?",
                        rows = 4, placeholder = "Ex.: incluir filtro por bairro, melhorar legenda do mapa, comparar com anos anteriores..."),
          checkboxInput("feedback_consentimento",
            "Concordo em enviar estas respostas para avaliação do painel e melhoria contínua.", FALSE),
          div(class = "feedback-actions",
            actionButton("feedback_enviar", "Enviar avaliação", class = "btn-primary"),
            downloadButton("feedback_download", "Baixar respostas em CSV")
          ),
          uiOutput("feedback_status")
        ),
        div(class = "landing-section",
          h4("Como as respostas são usadas"),
          p("As respostas alimentam um relatório quantitativo de satisfação e usabilidade e orientam melhorias de co-design. Nenhum dado clínico ou identificável de paciente é solicitado. O envio automático usa o FormSubmit e chega a ", tags$strong("ryan.paulo@gsuite.iff.edu.br"), "; no primeiro envio pode ser necessária uma ativação única por e-mail. Se o envio automático falhar, o painel oferece e-mail pré-preenchido e CSV."),
          p(tags$strong("Privacidade: "), "não informe dados pessoais de pacientes, prontuários ou informações sensíveis neste formulário.")
        )
      ),
      tabItem(
        tabName = "metodos",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("MÉTODOS")
        ),
        aviso_uso_dados(compacto = TRUE),
        div(class = "landing-section",
          h3("Métodos, fontes e reprodutibilidade"),
          p("Esta aba documenta as fontes, os filtros aplicados, os critérios de agregação e as limitações usadas no painel."),
          fluidRow(
            column(6, div(class = "method-note",
              h4("Recorte analítico"),
              tags$dl(
                tags$dt("Município"),
                tags$dd(APP_MUNICIPIO),
                tags$dt("Período"),
                tags$dd(APP_PERIODO_PADRAO),
                tags$dt("Unidade de análise"),
                tags$dd(APP_UNIDADE_ANALISE),
                tags$dt("Filtros aplicados"),
                tags$dd("Dengue e Zika: UF RJ, município 330100, anos 2020-2025, registros processados pelo microdatasus. Chikungunya: tabela agregada do projeto.")
              )
            )),
            column(6, div(class = "method-note",
              h4("Pacotes e atualização"),
              tags$dl(
                tags$dt("R"),
                tags$dd(R.version.string),
                tags$dt("Pacotes principais"),
                tags$dd(paste(
                  "shiny", as.character(packageVersion("shiny")),
                  "| shinydashboard", as.character(packageVersion("shinydashboard")),
                  "| microdatasus", as.character(packageVersion("microdatasus")),
                  "| plotly", as.character(packageVersion("plotly")),
                  "| viridisLite", as.character(packageVersion("viridisLite")),
                  "| sf", as.character(packageVersion("sf")),
                  "| geobr", as.character(packageVersion("geobr"))
                )),
                tags$dt("Data de atualização"),
                tags$dd(as.character(APP_DATA_ATUALIZACAO))
              )
            ))
          )
        ),
        div(class = "landing-section",
          h4("Critérios de confirmação e agregação"),
          tags$ul(
            tags$li("Dengue: registros do SINAN-DENGUE classificados como dengue ou febre hemorrágica, excluindo descartados e registros classificados como chikungunya."),
            tags$li("Zika: registros do SINAN-ZIKA com classificação compatível com confirmação após processamento pelo microdatasus, excluindo descartados e inconclusivos."),
            tags$li("Chikungunya: casos confirmados conforme tabela agregada do projeto."),
            tags$li("Incidência: casos confirmados divididos pela população estimada do IBGE/SIDRA e multiplicados por 100 mil."),
            tags$li("Qualidade dos dados: percentual de ignorado, branco ou ausente por sexo, idade, raça/cor, escolaridade, gestação e classificação final.")
          )
        ),
        div(class = "landing-section",
          h4("Séries temporais e mapa"),
          tags$ul(
            tags$li("Séries mensal e semanal: calculadas a partir da data de notificação dos registros brutos de Dengue e Zika, quando disponíveis em cache."),
            tags$li("Mapa de bairros: une registros locais de dengue por NM_BAIRRO com a malha geobr/IBGE de bairros 2010 por chave textual normalizada."),
            tags$li("Correspondência de bairros: bairros não mapeados devem ser revisados manualmente, pois podem representar grafias alternativas, localidades sem polígono ou nomes não presentes na malha.")
          )
        ),
        div(class = "landing-section",
          h4("Limitações"),
          tags$ul(
            tags$li("Os dados são notificações registradas e podem subestimar a ocorrência real."),
            tags$li("Atrasos de digitação, encerramento e investigação podem alterar anos recentes, especialmente 2025."),
            tags$li("Campos ignorados/brancos reduzem a interpretabilidade dos perfis sociodemográficos."),
            tags$li("As visualizações são descritivas e não estabelecem causalidade."),
            tags$li("A malha de bairros de 2010 pode não refletir todos os limites ou nomes territoriais usados nos registros atuais.")
          )
        ),
        div(class = "landing-section",
          h4("Comparação com o Estado do RJ e o Brasil"),
          p("As linhas tracejadas dos gráficos de incidência usam referências anuais do Estado do Rio de Janeiro e do Brasil. Elas servem para contextualizar a magnitude local, não para substituir a análise municipal nem para inferir causalidade."),
          tags$ul(
            tags$li("Dengue e Zika: casos confirmados agregados do SINAN/SVS (TabNet/DataSUS), por ano de notificação."),
            tags$li("Chikungunya: microdados nacionais do SINAN-CHIKUNGUNYA via microdatasus, filtrando residência no RJ (código IBGE 33) ou total Brasil."),
            tags$li("Denominadores: população residente do IBGE/SIDRA (Censo 2022 para 2022-2023; estimativas para os demais anos).")
          ),
          p(tags$strong("Atualização: "), "o arquivo data/cache_app/benchmarks_uf_br.csv é gerado por scripts/08_benchmarks.R e pode ser atualizado semestralmente pelo GitHub Actions.")
        ),
        div(class = "landing-section",
          h4("Licença, citação e reuso (princípios FAIR)"),
          p("O código está sob licença MIT, permitindo reuso, adaptação e redistribuição com atribuição. Os dados agregados podem ser reutilizados com citação da fonte original (SINAN/SVS) e deste painel. Dados brutos individuais e planilhas locais restritas não são publicados."),
          tags$ul(
            tags$li(tags$strong("Findable: "), "metadados, fontes, período e versão documentados no README e em data/cache_app/metadata.json."),
            tags$li(tags$strong("Accessible: "), "aplicativo público, repositório aberto e arquivos de cache versionados."),
            tags$li(tags$strong("Interoperable: "), "nomes de variáveis padronizados, CSV/JSON e código R modular."),
            tags$li(tags$strong("Reusable: "), "licença MIT, script de atualização, testes automatizados e nota de citação (CITATION.cff).")
          ),
          p(
            "Como citar: SANTOS, R. P. et al. ",
            tags$em("Painel de Inteligência Epidemiológica para Arboviroses em Campos dos Goytacazes (2020-2025)"),
            ". IFF Guarus, 2026. Disponível em: ",
            tags$a(href = "https://github.com/santosry/arboviroses-campos", target = "_blank", "github.com/santosry/arboviroses-campos"),
            ". Licença MIT."
          )
        )
      ),
      tabItem(
        tabName = "equipe",
        div(class = "doenca-titulo",
          div(class = "mosquito-icon", tags$img(src = "mosquito.svg", width = "28", height = "28", alt = "Mosquito")),
          span("EQUIPE E DESENVOLVEDORES")
        ),
        aviso_uso_dados(compacto = TRUE),
        div(class = "landing-section",
          h4("Equipe do projeto"),
          p("Esta empreitada reúne bacharelandos em Enfermagem pelo Instituto Federal Fluminense, Campus Campos Guarus, sob orientação da Profa. Dra. Karla Rangel Ribeiro. A proposta integra formação em saúde, vigilância epidemiológica, ciência de dados e comunicação científica aplicada às arboviroses."),
          div(class = "team-grid",
            perfil_equipe(
              "Ryan de Paulo Santos",
              "Bacharelando em Enfermagem",
              "Bacharelando em Enfermagem pelo IFF Guarus, integrante da equipe de desenvolvimento, organização dos dados e análise das visualizações do painel.",
              "http://lattes.cnpq.br/7503796642571978",
              "RS"
            ),
            perfil_equipe(
              "Brenda Velasco Moreira",
              "Bacharelanda em Enfermagem",
              "Bacharelanda em Enfermagem pelo IFF Guarus, integrante da equipe do projeto e colaboradora na leitura epidemiológica e organização científica dos dados.",
              "http://lattes.cnpq.br/2823380252102590",
              "BM"
            ),
            perfil_equipe(
              "Mirella Guimarães Lourenço de Souza",
              "Bacharelanda em Enfermagem",
              "Bacharelanda em Enfermagem pelo IFF Guarus, integrante da equipe do projeto e colaboradora na interpretação dos indicadores de arboviroses.",
              "http://lattes.cnpq.br/1000563712788651",
              "MS"
            ),
            perfil_equipe(
              "Marcelly Rangel de Mello",
              "Bacharelanda em Enfermagem",
              "Bacharelanda em Enfermagem pelo IFF Guarus, integrante da equipe do projeto e colaboradora na análise, revisão e comunicação dos resultados.",
              "http://lattes.cnpq.br/9803145019193571",
              "MM"
            ),
            perfil_equipe(
              "Karla Rangel Ribeiro",
              "Orientadora",
              "Docente do IFF Guarus e orientadora desta empreitada, articulando vigilância em saúde, pesquisa aplicada, formação em enfermagem e análise de dados.",
              "http://lattes.cnpq.br/6725528158895476",
              "KR"
            )
          )
        )
      )
    ),
    div(class = "app-footer",
      tags$div(
        tags$strong("Painel de Arboviroses — Campos dos Goytacazes/RJ"),
        " · Dados: SINAN/SVS, microdatasus, IBGE/SIDRA · Código: MIT · ",
        tags$a(href = "https://github.com/santosry/arboviroses-campos", target = "_blank", "Repositório"),
        " · ",
        tags$a(href = "mailto:ryan.paulo@gsuite.iff.edu.br", "Contato")
      ),
      tags$div(class = "app-footer-note",
        "Indicadores descritivos. Notificação não é incidência real. Consulte Métodos e a licença/FAIR no README."
      )
    )
  )
)
