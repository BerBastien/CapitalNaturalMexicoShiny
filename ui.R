library(shiny)
library(leaflet)
library(shinyWidgets)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600;9..144,700&family=Manrope:wght@400;500;700&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  fluidRow(
    column(
      width = 12,
      div(
        class = "dashboard-panel",
        h1("Capital Natural de Mexico ante el Cambio Climatico", class = "app-title"),
        div("Base de datos del proyecto SECIHTI CBF-2025-I-345", class = "app-subtitle")
      )
    )
  ),

  hidden(
    div(
      id = "loading_banner",
      class = "loading-banner",
      "Cargando datos..."
    )
  ),

  br(),

  fluidRow(
    column(
      width = 3,
      div(
        class = "dashboard-panel",
        div("Configuracion", class = "sidebar-title"),
        switchInput("selector_mode", "", value = FALSE,
                    onLabel = "Arbol", offLabel = "Lista"),
        conditionalPanel(
          condition = "!input.selector_mode",
          pickerInput("variable_sel", "Variable", choices = NULL, options = list(`live-search` = TRUE))
        ),
        conditionalPanel(
          condition = "input.selector_mode",
          pickerInput("domain_sel", "Dominio", choices = NULL),
          pickerInput("variable_tree_sel", "Variable", choices = NULL, options = list(`live-search` = TRUE)),
          pickerInput("scenario_sel", "Escenario", choices = NULL),
          pickerInput("year_sel", "Año", choices = NULL)
        ),
        pickerInput("estado_sel", "Estado", choices = NULL, options = list(`live-search` = TRUE)),
        br(), br(),
        downloadButton("descarga_malla", "Descarga la malla geoespacial", class = "btn-download btn-block"),
        br(),
        downloadButton("descarga_datos", "Descarga los datos mostrados", class = "btn-download btn-block")
      )
    ),

    column(
      width = 9,
      fluidRow(
        column(
          width = 12,
          div(
            class = "dashboard-panel",
            div(
              class = "map-wrap",
              leafletOutput("mapa", height = "560px"),
              hidden(
                div(
                  id = "map_loading_overlay",
                  class = "map-loading-overlay",
                  div(class = "map-loading-spinner"),
                  div("Actualizando mapa...", class = "map-loading-text")
                )
              )
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          div(
            class = "dashboard-panel",
            plotOutput("histograma", height = "260px")
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 3,
          div(class = "meta-box", div("Variable", class = "meta-title"), htmlOutput("meta_variable", class = "meta-body"))
        ),
        column(
          width = 3,
          div(class = "meta-box", div("Descripcion", class = "meta-title"), htmlOutput("meta_descripcion", class = "meta-body"))
        ),
        column(
          width = 3,
          div(class = "meta-box", div("Fuente", class = "meta-title"), htmlOutput("meta_fuente", class = "meta-body"))
        ),
        column(
          width = 3,
          div(class = "meta-box", div("Referencia", class = "meta-title"), htmlOutput("meta_referencia", class = "meta-body"))
        )
      )
    )
  ),

  # ── Repo info card — outside the main fluidRow so Bootstrap doesn't interfere ──
  tags$style(HTML("
    .repo-info-shell {
      margin: 20px 15px 16px 15px;
      border-radius: 18px;
      background: #17384f !important;
      border-left: 6px solid #d99a2b;
      padding: 28px 32px;
      box-shadow: 0 12px 40px rgba(0,0,0,0.28);
      color: #ffffff !important;
    }
    .repo-info-shell .repo-info-title { color: #f6c64e !important; }
    .repo-info-shell .repo-info-text,
    .repo-info-shell p,
    .repo-info-shell div { color: #ffffff !important; }
    .repo-info-shell .repo-info-kicker {
      color: #f6c64e !important;
      background: rgba(246,198,78,0.18) !important;
      border: 1px solid rgba(246,198,78,0.35) !important;
    }
    .repo-info-shell .repo-side-title { color: #7dd3db !important; }
    .repo-info-shell .repo-info-contact { color: #7dd3db !important; }
    .repo-info-shell .repo-info-citation { color: rgba(255,255,255,0.72) !important; }
    .repo-info-shell .repo-info-side {
      background: rgba(255,255,255,0.07) !important;
      border: 1px solid rgba(255,255,255,0.16) !important;
    }
    .repo-info-shell .repo-pill {
      color: #ffffff !important;
      background: rgba(255,255,255,0.10) !important;
      border: 1px solid rgba(255,255,255,0.22) !important;
    }
    .repo-info-shell .repo-info-divider {
      background: rgba(255,255,255,0.18) !important;
    }
  ")),
  div(
    class = "repo-info-shell",
    div(class = "repo-info-kicker", "Información del proyecto"),
    div("", class = "repo-info-title"),
    div(
      class = "repo-info-grid",
      div(
        class = "repo-info-main",
        p(
          "Repositorio interactivo, público y de acceso abierto con datos estandarizados sobre capital natural en México, ",
          "organizados en una malla hexagonal de 10 km —y de 2 km para casos específicos que requieren mayor resolución—. ",
          "El repositorio integra información descargable en tres dominios clave para el análisis socioecosistémico: ",
          "variables climáticas, humanas y ecológicas. Todos los datos provienen de fuentes públicas, fueron descargados, ",
          "procesados, armonizados espacialmente y transformados mediante operaciones geográficas y analíticas para generar información nueva, ",
          "comparable y lista para su uso en investigación, planeación territorial y toma de decisiones.",
          class = "repo-info-text"
        )
      ),
      div(
        class = "repo-info-side",
        div("Responsable del proyecto", class = "repo-side-title"),
        p(
          "Dr. Bernardo A. Bastien Olvera, Investigador Asociado C. de T. C. del Instituto de Ciencias de la Atmósfera y Cambio Climático de la UNAM, Departamento de Cambio Climático, Grupo Clima y Sociedad, Hub Ecosistemas.",
          class = "repo-info-text"
        ),
        p("bbastien@atmosfera.unam.mx", class = "repo-info-text repo-info-contact")
      )
    ),
    div(class = "repo-info-divider"),
    p("Colaboradores: Xarhini Garcia Cepeda, Mariana Figueroa Soriano, Samantha Cuevas Antunez, Regina Sánchez Martínez, Alma Mendoza-Ponce, Enrique Martínez Meyer, Viridiana Lizardo, Pierre Mokondoko, Aurora Tristán Flores, Araceli Sánchez, Amparo Martínez Arroyo, Francisco Estrada Porrua, Oscar Calderon Bustamante, Miguel Altamirano Del Carmen, Rodrigo Muñoz Sanchez", class = "repo-info-text"),
    p(
      "Como citar: Bastien-Olvera et al. (2026) Capital Natural de Mexico ante el Cambio Climatico: Base de datos del proyecto SECIHTI CBF-2025-I-345.",
      class = "repo-info-text repo-info-citation"
    )
  )
)
