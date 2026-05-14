suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(sf)
  library(dplyr)
  library(readxl)
  library(leaflet)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(purrr)
  library(stringr)
})

options(shiny.maxRequestSize = 200 * 1024^2)

app_dir        <- getwd()
local_data_dir <- file.path(app_dir, "data", "indicadores")
preferred_start_file <- "E__uso_de_vegetacion_y_uso_suelo__Presente__2020.xlsx"

# --- Hex resolution config --------------------------------------------------
hex_resolutions <- list(
  "10km" = list(file = "master_hex_10km.gpkg", layer = "hex_10km"),
  "2km"  = list(file = "master_hex_2km.gpkg",  layer = "hex_2km")
)
default_resolution <- "10km"

resolve_hex_resolution <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value) || trimws(value) == "") {
    return(default_resolution)
  }

  x <- tolower(trimws(as.character(value)[1]))
  x <- gsub("\\s+", "", x)

  if (x %in% names(hex_resolutions)) return(x)
  if (grepl("^[0-9]+$", x)) {
    k <- paste0(x, "km")
    if (k %in% names(hex_resolutions)) return(k)
  }
  if (grepl("^[0-9]+km$", x) && x %in% names(hex_resolutions)) return(x)

  warning(sprintf("Resolucion no reconocida '%s'. Se usa %s.", as.character(value)[1], default_resolution))
  default_resolution
}

load_hex_data <- local({
  cache <- new.env(parent = emptyenv())

  function(resolution_key) {
    key <- resolve_hex_resolution(resolution_key)
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }

    hex_cfg  <- hex_resolutions[[key]]
    hex_path <- file.path(app_dir, "data", hex_cfg$file)

    if (!file.exists(hex_path)) {
      stop("No se encuentra ", hex_cfg$file, ". Verifica los geopackage en data/.")
    }

    hex_sf <- st_read(hex_path, layer = hex_cfg$layer, quiet = TRUE)
    if (!"hex_id" %in% names(hex_sf)) stop("El geopackage maestro debe contener la columna hex_id.")
    if (!"estado" %in% names(hex_sf)) hex_sf$estado <- "Sin estado"
    if (!"tipo"   %in% names(hex_sf)) hex_sf$tipo   <- "Continental"

    # Simplify geometry for smoother browser interaction with large hex sets.
    display_hex_sf <- {
      crs_hex <- "+proj=laea +lat_0=23 +lon_0=-102 +datum=WGS84 +units=m +no_defs"
      x <- st_transform(hex_sf, crs_hex)
      st_geometry(x) <- st_simplify(st_geometry(x), dTolerance = 150, preserveTopology = TRUE)
      st_transform(x, 4326)
    }

    out <- list(
      resolution_key = key,
      hex_path = hex_path,
      hex_sf = hex_sf,
      display_hex_sf = display_hex_sf
    )
    assign(key, out, envir = cache)
    out
  }
})

# Validate default hex on startup.
invisible(load_hex_data(default_resolution))

# Paletas daybreak
pal_daybreak_cont <- c("#123E59", "#1D6D77", "#4E9F8A", "#E5C35A", "#D97A2B", "#B84A3B")
pal_daybreak_cat  <- c("#123E59", "#1D6D77", "#4E9F8A", "#E5C35A", "#D97A2B", "#B84A3B", "#7E3B6F", "#3F5F36")

build_daybreak_cat_palette <- function(n) {
  if (is.na(n) || n <= 0) return(character(0))
  if (n <= length(pal_daybreak_cat)) return(pal_daybreak_cat[seq_len(n)])
  grDevices::colorRampPalette(pal_daybreak_cat)(n)
}

# ---------------------------------------------------------------------------
# Each xlsx = one variable.
# Sheet 1 (index): datos  — columns include columna_id_hex and columna_valor
# Sheet "catalogo_variable": ONE row with variable metadata
# Sheet "metadatos":         source / institution info  (optional)
# Sheet "diccionario_categorias": category labels/colors (optional)
# ---------------------------------------------------------------------------
safe_sheet <- function(path, sheet) {
  tryCatch(readxl::read_excel(path, sheet = sheet), error = function(e) NULL)
}

read_variable_from_excel <- function(xlsx_path) {
  # Read all columns as text to avoid logical coercion on NA-leading columns
  datos <- tryCatch(
    readxl::read_excel(xlsx_path, sheet = 1, col_types = "text"),
    error = function(e) NULL
  )

  # Accept both singular and plural sheet name
  catalog <- safe_sheet(xlsx_path, "catalogo_variable")
  if (is.null(catalog)) catalog <- safe_sheet(xlsx_path, "catalogo_variables")

  meta <- safe_sheet(xlsx_path, "metadatos")
  if (is.null(meta)) meta <- safe_sheet(xlsx_path, "metadatos_dataset")

  dicc <- safe_sheet(xlsx_path, "diccionario_categorias")

  if (is.null(datos) || nrow(datos) == 0) {
    warning(sprintf("Sin datos en hoja 1: %s", basename(xlsx_path)))
    return(NULL)
  }
  if (is.null(catalog) || nrow(catalog) == 0) {
    warning(sprintf("Sin catalogo_variable: %s", basename(xlsx_path)))
    return(NULL)
  }

  req_cols <- c("variable_id", "variable_nombre", "tipo_dato", "unidades",
                "descripcion", "fuente", "referencia", "columna_id_hex", "columna_valor")
  missing  <- setdiff(req_cols, names(catalog))
  if (length(missing) > 0) {
    warning(sprintf("Faltan columnas en %s: %s", basename(xlsx_path), paste(missing, collapse = ", ")))
    return(NULL)
  }

  row <- catalog %>% slice(1)

  if (is.null(meta) || nrow(meta) == 0) {
    meta <- tibble::tibble(
      titulo       = as.character(row$variable_nombre),
      resumen      = NA_character_,
      institucion  = NA_character_,
      fecha_version = NA_character_
    )
  }
  if (is.null(dicc)) dicc <- tibble::tibble()

  list(
    variable_id     = as.character(row$variable_id),
    variable_nombre = as.character(row$variable_nombre),
    tipo_dato       = as.character(row$tipo_dato),
    unidades        = as.character(row$unidades),
    descripcion     = as.character(row$descripcion),
    fuente          = as.character(row$fuente),
    referencia      = as.character(row$referencia),
    columna_id_hex  = as.character(row$columna_id_hex),
    columna_valor   = as.character(row$columna_valor),
    resolution_key  = resolve_hex_resolution(if ("resolucion_hex" %in% names(row)) row$resolucion_hex else NA_character_),
    datos           = as.data.frame(datos),
    meta            = as.data.frame(meta),
    diccionario     = as.data.frame(dicc),
    xlsx_path       = xlsx_path
  )
}

build_repository <- function() {
  xlsx_files <- list.files(
    local_data_dir,
    pattern = "^[CHE]__.+\\.xlsx$",
    full.names = TRUE,
    ignore.case = FALSE
  )
  if (length(xlsx_files) == 0) return(tibble::tibble())

  domain_labels <- c(C = "Climatica", H = "Humana", E = "Ecologica")

  parse_one <- function(path) {
    file_base <- tools::file_path_sans_ext(basename(path))
    m <- stringr::str_match(file_base, "^([CHE])__(.+?)__(.+?)__(\\d{4})$")

    if (is.na(m[1, 1])) {
      pref <- toupper(substr(file_base, 1, 1))
      dom  <- if (pref %in% names(domain_labels)) domain_labels[[pref]] else "Otros"
      return(tibble::tibble(
        xlsx_path = path,
        file_base = file_base,
        parsed = FALSE,
        domain_code = pref,
        domain_label = dom,
        variable_key = file_base,
        variable_label = file_base,
        scenario_key = NA_character_,
        scenario_label = NA_character_,
        year_label = NA_character_,
        display_label = file_base
      ))
    }

    variable_key <- m[1, 3]
    variable_group_key <- variable_key %>%
      str_replace("_(19|20)\\d{2}$", "") %>%
      str_replace("_(tendencial|verde|green|bau)$", "")
    scenario_key <- m[1, 4]
    year_label   <- m[1, 5]
    variable_label <- variable_key %>%
      str_replace_all("_", " ") %>%
      str_to_title()
    variable_group_label <- variable_group_key %>%
      str_replace_all("_", " ") %>%
      str_to_title()
    scenario_label <- toupper(scenario_key)

    tibble::tibble(
      xlsx_path = path,
      file_base = file_base,
      parsed = TRUE,
      domain_code = m[1, 2],
      domain_label = domain_labels[[m[1, 2]]],
      variable_key = variable_key,
      variable_label = variable_label,
      variable_group_key = variable_group_key,
      variable_group_label = variable_group_label,
      scenario_key = scenario_key,
      scenario_label = scenario_label,
      year_label = year_label,
      display_label = paste0(variable_label, " - ", scenario_label, " - ", year_label)
    )
  }

  index <- purrr::map_dfr(xlsx_files, parse_one)

  domain_order <- c("Climatica", "Humana", "Ecologica", "Otros")
  index %>%
    mutate(
      domain_order = match(domain_label, domain_order),
      year_num = suppressWarnings(as.integer(year_label))
    ) %>%
    arrange(domain_order, variable_label, scenario_label, year_num, display_label) %>%
    dplyr::select(-domain_order, -year_num)
}

# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  # On startup, show the global "Cargando datos..." banner. It will be
  # hidden by the mapped_sf observer once the first map render completes.
  show("loading_banner")

  build_variable_choices <- function(index_tbl) {
    if (nrow(index_tbl) == 0) return(list())

    grouped <- split(index_tbl, index_tbl$domain_label)
    ordered_sections <- c("Climatica", "Humana", "Ecologica", "Otros")
    ordered_sections <- ordered_sections[ordered_sections %in% names(grouped)]

    out <- vector("list", length(ordered_sections))
    names(out) <- ordered_sections

    for (sec in ordered_sections) {
      g <- grouped[[sec]]
      out[[sec]] <- setNames(g$xlsx_path, g$display_label)
    }
    out
  }

  repo <- reactivePoll(
    intervalMillis = 2000,
    session = session,
    checkFunc = function() {
      files <- list.files(
        local_data_dir,
        pattern = "^[CHE]__.+\\.xlsx$",
        full.names = TRUE,
        ignore.case = FALSE
      )
      if (length(files) == 0) return("empty")

      info <- file.info(files)
      stamp <- paste(
        files,
        as.numeric(info$size),
        as.numeric(info$mtime),
        sep = "::",
        collapse = "||"
      )
      stamp
    },
    valueFunc = function() {
      build_repository()
    }
  )

  with_placeholder <- function(choices, label = "Selecciona") {
    if (length(choices) == 0) return(setNames("", label))

    if (is.null(names(choices))) {
      out <- c("", as.character(choices))
      names(out) <- c(label, as.character(choices))
      return(out)
    }

    c(setNames("", label), choices)
  }

  # Cache parsed excel contents by absolute file path.
  cfg_cache <- local({
    cache <- new.env(parent = emptyenv())
    function(xlsx_path) {
      finfo <- file.info(xlsx_path)
      if (nrow(finfo) == 0 || is.na(finfo$mtime[[1]]) || is.na(finfo$size[[1]])) {
        return(NULL)
      }

      file_stamp <- paste(as.numeric(finfo$size[[1]]), as.numeric(finfo$mtime[[1]]), sep = "::")

      if (exists(xlsx_path, envir = cache, inherits = FALSE)) {
        cached <- get(xlsx_path, envir = cache, inherits = FALSE)
        if (!is.null(cached$stamp) && identical(cached$stamp, file_stamp)) {
          return(cached$cfg)
        }
      }

      cfg <- read_variable_from_excel(xlsx_path)
      assign(xlsx_path, list(stamp = file_stamp, cfg = cfg), envir = cache)
      cfg
    }
  })

  observeEvent(repo(), {
    idx <- repo()
    if (nrow(idx) == 0) {
      updatePickerInput(session, "variable_sel", choices = character(0), selected = character(0))
      updatePickerInput(session, "domain_sel", choices = with_placeholder(character(0)), selected = "")
      updatePickerInput(session, "variable_tree_sel", choices = with_placeholder(character(0)), selected = "")
      updatePickerInput(session, "scenario_sel", choices = with_placeholder(character(0)), selected = "")
      updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    # List mode choices
    choices <- build_variable_choices(idx)
    all_paths <- unname(unlist(choices, recursive = TRUE, use.names = FALSE))
    current_list <- isolate(input$variable_sel)
    preferred_path <- file.path(local_data_dir, preferred_start_file)
    selected_list <- if (!is.null(current_list) && current_list %in% all_paths) {
      current_list
    } else if (preferred_path %in% all_paths) {
      preferred_path
    } else {
      all_paths[[1]]
    }
    updatePickerInput(session, "variable_sel", choices = choices, selected = selected_list)

    # Tree mode - domains
    tree_idx <- idx %>% filter(parsed)
    domains <- unique(tree_idx$domain_label)
    if (length(domains) == 0) {
      updatePickerInput(session, "domain_sel", choices = with_placeholder(character(0)), selected = "")
    } else {
      current_domain <- isolate(input$domain_sel)
      selected_domain <- if (!is.null(current_domain) && current_domain %in% domains) current_domain else domains[[1]]
      updatePickerInput(session, "domain_sel", choices = with_placeholder(domains), selected = selected_domain)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$domain_sel, {
    if (is.null(input$domain_sel) || identical(input$domain_sel, "")) {
      updatePickerInput(session, "variable_tree_sel", choices = with_placeholder(character(0)), selected = "")
      updatePickerInput(session, "scenario_sel", choices = with_placeholder(character(0)), selected = "")
      updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    idx <- repo() %>% filter(parsed)
    req(nrow(idx) > 0)

    vars <- idx %>%
      filter(domain_label == input$domain_sel) %>%
      distinct(variable_group_key, variable_group_label) %>%
      arrange(variable_group_label)

    # Reset child branches whenever parent branch changes.
    updatePickerInput(session, "scenario_sel", choices = with_placeholder(character(0)), selected = "")
    updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")

    if (nrow(vars) == 0) {
      updatePickerInput(session, "variable_tree_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    choices <- setNames(vars$variable_group_key, vars$variable_group_label)
    updatePickerInput(session, "variable_tree_sel", choices = with_placeholder(choices), selected = "")
  }, ignoreInit = FALSE)

  observeEvent(input$variable_tree_sel, {
    if (is.null(input$variable_tree_sel) || identical(input$variable_tree_sel, "")) {
      updatePickerInput(session, "scenario_sel", choices = with_placeholder(character(0)), selected = "")
      updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    idx <- repo() %>% filter(parsed)
    req(nrow(idx) > 0)
    req(input$domain_sel)

    scn <- idx %>%
      filter(domain_label == input$domain_sel, variable_group_key == input$variable_tree_sel) %>%
      distinct(scenario_key, scenario_label) %>%
      arrange(scenario_label)

    updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")

    if (nrow(scn) == 0) {
      updatePickerInput(session, "scenario_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    choices <- setNames(scn$scenario_key, scn$scenario_label)
    updatePickerInput(session, "scenario_sel", choices = with_placeholder(choices), selected = "")
  }, ignoreInit = FALSE)

  observeEvent(input$scenario_sel, {
    if (is.null(input$scenario_sel) || identical(input$scenario_sel, "")) {
      updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    idx <- repo() %>% filter(parsed)
    req(nrow(idx) > 0)
    req(input$domain_sel, input$variable_tree_sel)

    yrs <- idx %>%
      filter(
        domain_label == input$domain_sel,
        variable_group_key == input$variable_tree_sel,
        scenario_key == input$scenario_sel
      ) %>%
      distinct(year_label) %>%
      arrange(suppressWarnings(as.integer(year_label)))

    if (nrow(yrs) == 0) {
      updatePickerInput(session, "year_sel", choices = with_placeholder(character(0)), selected = "")
      return()
    }

    choices <- yrs$year_label
    updatePickerInput(session, "year_sel", choices = with_placeholder(choices), selected = "")
  }, ignoreInit = FALSE)

  selection_complete <- reactive({
    idx <- repo()
    if (nrow(idx) == 0) return(FALSE)

    if (!isTRUE(input$selector_mode)) {
      return(!is.null(input$variable_sel) && nzchar(input$variable_sel) && input$variable_sel %in% idx$xlsx_path)
    }

    all(!sapply(list(input$domain_sel, input$variable_tree_sel, input$scenario_sel, input$year_sel), function(x) is.null(x) || !nzchar(x)))
  })

  selected_xlsx_path <- reactive({
    idx <- repo()
    validate(need(nrow(idx) > 0, "No hay variables cargadas en data/indicadores/."))

    if (!isTRUE(input$selector_mode)) {
      req(input$variable_sel)
      validate(need(input$variable_sel %in% idx$xlsx_path, "Variable no encontrada."))
      return(input$variable_sel)
    }

    req(input$domain_sel, input$variable_tree_sel, input$scenario_sel, input$year_sel)
    req(input$domain_sel != "", input$variable_tree_sel != "", input$scenario_sel != "", input$year_sel != "")
    hit <- idx %>%
      filter(
        parsed,
        domain_label == input$domain_sel,
        variable_group_key == input$variable_tree_sel,
        scenario_key == input$scenario_sel,
        year_label == input$year_sel
      )

    validate(need(nrow(hit) > 0, "No se encontro el archivo para la seleccion actual."))
    hit$xlsx_path[[1]]
  })

  observe({
    cfg <- selected_config()
    hx  <- load_hex_data(cfg$resolution_key)

    estados <- c("Todos", sort(unique(hx$hex_sf$estado)))
    current <- isolate(input$estado_sel)
    selected <- if (!is.null(current) && current %in% estados) current else "Todos"
    updatePickerInput(session, "estado_sel", choices = estados, selected = selected)
  })

  # Show the map overlay only when the user actively changes a selection.
  # Never hide here — only the mapped_sf observer is allowed to hide
  # overlays (after the new layer has been pushed to leaflet). This avoids
  # the "blip" where overlays disappeared mid-cascade while pickers updated.
  observeEvent(list(
    input$selector_mode,
    input$variable_sel,
    input$domain_sel,
    input$variable_tree_sel,
    input$scenario_sel,
    input$year_sel,
    input$estado_sel
  ), {
    if (isTRUE(selection_complete())) {
      show("map_loading_overlay")
    }
  }, ignoreInit = TRUE, priority = 1000)

  selected_config <- reactive({
    cfg <- cfg_cache(selected_xlsx_path())
    validate(need(!is.null(cfg), "No se pudo leer el Excel seleccionado."))
    cfg
  })

  mapped_sf <- reactive({
    cfg <- selected_config()
    hx  <- load_hex_data(cfg$resolution_key)

    dat       <- cfg$datos
    id_col    <- cfg$columna_id_hex
    val_col   <- cfg$columna_valor

    validate(need(id_col  %in% names(dat), paste("Falta columna ID:", id_col)))
    validate(need(val_col %in% names(dat), paste("Falta columna valor:", val_col)))

    dat2 <- dat %>%
      transmute(
        .hex       = as.character(.data[[id_col]]),
        .valor_raw = .data[[val_col]]
      )

    dat2 <- dat2 %>%
      mutate(
        .valor_raw = trimws(as.character(.valor_raw)),
        .valor_raw = if_else(.valor_raw == "", NA_character_, .valor_raw)
      )

    out <- hx$display_hex_sf %>% left_join(dat2, by = c("hex_id" = ".hex"))

    if (!is.null(input$estado_sel) && input$estado_sel != "Todos") {
      out <- out %>% filter(estado == input$estado_sel)
    }

    tipo_dato <- tolower(trimws(as.character(cfg$tipo_dato)))
    es_continua <- tipo_dato %in% c("continua", "continuo")

    if (es_continua) {
      out <- out %>% mutate(valor = suppressWarnings(as.numeric(.valor_raw)))
    } else {
      out <- out %>% mutate(valor = as.character(.valor_raw))
    }
    out
  })

  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Etiquetas") %>%
      setView(lng = -102.5, lat = 23.5, zoom = 5)
  })

  observeEvent(mapped_sf(), {
    sf_data     <- mapped_sf()
    cfg         <- selected_config()
    units_label <- cfg$unidades
    tipo_dato   <- tolower(trimws(as.character(cfg$tipo_dato)))
    es_continua <- tipo_dato %in% c("continua", "continuo")

    proxy <- leafletProxy("mapa", data = sf_data) %>% clearShapes() %>% clearControls()

    # Delay the hide so Leaflet has time to actually render the polygons
    # in the browser before we remove the overlay. Without this, the
    # overlay disappears while the map is still black/empty.
    on.exit({
      shinyjs::delay(250, {
        hide("loading_banner")
        hide("map_loading_overlay")
      })
    })

    if (nrow(sf_data) == 0) {
      showNotification("No hay celdas para mostrar con el filtro actual.", type = "warning")
      return()
    }

    if (es_continua) {
      vals <- sf_data$valor
      pal  <- colorNumeric(palette = pal_daybreak_cont, domain = vals, na.color = "#cfd8dc")

      proxy %>%
        addPolygons(
          fillColor  = ~pal(valor),
          stroke     = FALSE,
          color      = "#ffffff",
          weight     = 0.15,
          fillOpacity = 0.78,
          smoothFactor = 0,
          popup = ~paste0(
            "<b>", hex_id, "</b><br>",
            "Tipo: ", tipo, "<br>",
            "Estado: ", estado, "<br>",
            "Valor: ", ifelse(is.na(valor), "NA", comma(valor)), " ", units_label
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = vals,
                  title = paste0(cfg$variable_nombre, " (", units_label, ")"), opacity = 0.9)

    } else {
      vals <- as.character(sf_data$valor)
      dicc <- cfg$diccionario

      if (nrow(dicc) > 0 && all(c("categoria", "color_hex") %in% names(dicc))) {
        lvls       <- as.character(dicc$categoria)
        pal_values <- as.character(dicc$color_hex)
        missing_color <- is.na(pal_values) | trimws(pal_values) == ""
        if (any(missing_color)) {
          fallback <- build_daybreak_cat_palette(length(lvls))
          pal_values[missing_color] <- fallback[missing_color]
        }
      } else {
        lvls       <- sort(unique(vals[!is.na(vals)]))
        pal_values <- build_daybreak_cat_palette(length(lvls))
      }

      sf_data$valor <- factor(sf_data$valor, levels = lvls)
      pal <- colorFactor(palette = unname(pal_values), domain = lvls, na.color = "#cfd8dc")

      proxy %>%
        addPolygons(
          fillColor  = ~pal(valor),
          stroke     = FALSE,
          color      = "#ffffff",
          weight     = 0.15,
          fillOpacity = 0.82,
          smoothFactor = 0,
          popup = ~paste0(
            "<b>", hex_id, "</b><br>",
            "Tipo: ", tipo, "<br>",
            "Estado: ", estado, "<br>",
            "Categoria: ", ifelse(is.na(valor), "NA", as.character(valor))
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = vals,
                  title = cfg$variable_nombre, opacity = 0.9)
    }
  })

  output$histograma <- renderPlot({
    sf_data   <- mapped_sf()
    cfg       <- selected_config()
    tipo_dato <- tolower(trimws(as.character(cfg$tipo_dato)))
    es_continua <- tipo_dato %in% c("continua", "continuo")

    if (!es_continua) {
      dicc <- cfg$diccionario
      dat  <- sf_data %>% st_drop_geometry() %>% filter(!is.na(valor)) %>% count(valor, sort = TRUE)

      if (nrow(dicc) > 0 && "categoria" %in% names(dicc)) {
        level_order <- as.character(dicc$categoria)
        dat         <- dat %>% mutate(valor = factor(valor, levels = level_order)) %>% arrange(valor)
        fill_values <- setNames(
          if ("color_hex" %in% names(dicc)) {
            cols <- as.character(dicc$color_hex)
            missing_color <- is.na(cols) | trimws(cols) == ""
            if (any(missing_color)) {
              fallback <- build_daybreak_cat_palette(length(level_order))
              cols[missing_color] <- fallback[missing_color]
            }
            cols
          } else {
            build_daybreak_cat_palette(length(level_order))
          },
          level_order
        )
      } else {
        dat         <- dat %>% mutate(valor = fct_reorder(valor, n))
        fill_values <- setNames(build_daybreak_cat_palette(nrow(dat)), as.character(dat$valor))
      }

      validate(need(nrow(dat) > 0, "No hay datos categoricos para graficar."))

      ggplot(dat, aes(x = valor, y = n, fill = valor)) +
        geom_col(width = 0.85, alpha = 0.9) +
        scale_fill_manual(values = fill_values, guide = "none", drop = FALSE) +
        coord_flip() +
        labs(x = "Categoria", y = "Numero de celdas") +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
              axis.title = element_text(size = 11, face = "bold"))

    } else {
      dat <- sf_data %>% st_drop_geometry() %>% filter(!is.na(valor))
      validate(need(nrow(dat) > 0, "No hay datos continuos para graficar."))

      mn          <- mean(dat$valor,   na.rm = TRUE)
      md          <- median(dat$valor, na.rm = TRUE)
      units_label <- cfg$unidades

      ggplot(dat, aes(x = valor)) +
        geom_histogram(fill = "#1D6D77", color = "#ffffff", bins = 40, alpha = 0.92) +
        geom_vline(xintercept = mn, color = "#D97A2B", linewidth = 1.1) +
        geom_vline(xintercept = md, color = "#B84A3B", linewidth = 1.1, linetype = "dashed") +
        annotate("label", x = mn, y = Inf, vjust = 1.8, label = paste0("Media: ",   comma(mn), " ", units_label), fill = "#fff3d6") +
        annotate("label", x = md, y = Inf, vjust = 3.9, label = paste0("Mediana: ", comma(md), " ", units_label), fill = "#ffe8df") +
        labs(x = paste0(cfg$variable_nombre, " (", units_label, ")"), y = "Numero de celdas") +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.minor = element_blank(), axis.title = element_text(size = 11, face = "bold"))
    }
  })

  output$meta_variable <- renderUI({
    cfg <- selected_config()
    meta <- cfg$meta
    titulo <- if ("titulo" %in% names(meta) && !is.na(meta$titulo[1])) meta$titulo[1]
              else cfg$variable_nombre
    resumen <- if ("resumen" %in% names(meta) && !is.na(meta$resumen[1])) meta$resumen[1] else ""
    inst    <- if ("institucion" %in% names(meta) && !is.na(meta$institucion[1])) meta$institucion[1] else ""
    fecha   <- if ("fecha_version" %in% names(meta) && !is.na(meta$fecha_version[1])) meta$fecha_version[1] else ""

    HTML(paste0(
      "<b>", titulo, "</b>",
      if (nchar(resumen) > 0) paste0("<br><br>", resumen) else "",
      if (nchar(inst)   > 0) paste0("<br><br><b>Institucion:</b> ", inst) else "",
      if (nchar(fecha)  > 0) paste0("<br><b>Version:</b> ", fecha) else ""
    ))
  })

  output$meta_descripcion <- renderUI({
    cfg <- selected_config()
    HTML(ifelse(is.na(cfg$descripcion) || cfg$descripcion == "", "Sin descripcion.", cfg$descripcion))
  })

  output$meta_fuente <- renderUI({
    cfg <- selected_config()
    HTML(ifelse(is.na(cfg$fuente) || cfg$fuente == "", "Sin fuente.", cfg$fuente))
  })

  output$meta_referencia <- renderUI({
    cfg <- selected_config()
    HTML(ifelse(is.na(cfg$referencia) || cfg$referencia == "", "Sin referencia.", cfg$referencia))
  })

  output$descarga_malla <- downloadHandler(
    filename = function() {
      cfg <- selected_config()
      paste0("malla_hex_", cfg$resolution_key, ".gpkg")
    },
    content  = function(file) {
      cfg <- selected_config()
      hx  <- load_hex_data(cfg$resolution_key)
      file.copy(hx$hex_path, file)
    },
    contentType = "application/octet-stream"
  )

  output$descarga_datos <- downloadHandler(
    filename = function() {
      cfg <- selected_config()
      paste0(cfg$variable_id, ".xlsx")
    },
    content = function(file) {
      cfg <- selected_config()
      file.copy(cfg$xlsx_path, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}
