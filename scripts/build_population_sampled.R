# build_population_xlsx_sampled.R - FAST TEST VERSION
# Processes only every Nth hexagon for quick testing
suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(dplyr)
  library(openxlsx)
})

get_project_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    return(dirname(dirname(script_path)))
  }
  getwd()
}

project_dir <- normalizePath(get_project_dir(), mustWork = FALSE)
hex_gpkg <- file.path(project_dir, "data", "master_hex_10km.gpkg")
out_dir <- file.path(project_dir, "data", "indicadores")
pop_root <- "H:/My Drive/Data/SSPs/Gridded/Pop"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# TEST: Only SSP2 2050 for quick testing
scenarios <- c("SSP2")
years <- c(2050)
SAMPLE_EVERY <- 100  # Only process every 100th hexagon for quick testing

article_name <- "Projecting 1 km-grid population distributions from 2020 to 2100 globally under shared socioeconomic pathways"
dataset_name <- "Global 1 KM-Grid Population Distributions from 2020 to 2100"
doi_url <- "https://doi.org/10.6084/m9.figshare.19608594"
data_last_update <- "2022-08-29"
authors <- "Xinyu Wang; Xiangfeng Meng; Ying Long"

descripcion_base <- "Dataset global de poblacion en grilla de 1 km (TEST SAMPLE)"

resolve_tif_path <- function(pop_root_dir, scenario, year) {
  scenario_dir <- file.path(pop_root_dir, scenario)
  if (!dir.exists(scenario_dir)) return(list(path = NA_character_, reason = "no_dir"))
  sub_names <- if (scenario == "SSP1") "SSP1" else paste0("SPP", substr(scenario, 4, 4))
  scenario_dir <- file.path(scenario_dir, sub_names)
  if (!dir.exists(scenario_dir)) return(list(path = NA_character_, reason = "no_subdir"))
  hits <- list.files(scenario_dir, pattern = paste0("^", scenario, "_", year, ".tif$"), full.names = TRUE)
  if (length(hits) == 0) return(list(path = NA_character_, reason = "no_file"))
  hit <- hits[[1]]
  fi <- file.info(hit)
  if (is.na(fi$size) || fi$size <= 0) return(list(path = NA_character_, reason = "empty"))
  list(path = hit, reason = "ok")
}

extract_hex_sums_sampled <- function(tif_path, hex_sf, sample_every = 100) {
  r <- raster::raster(tif_path)
  
  message("    [SAMPLED TEST] Processing every ", sample_every, "th hexagon")
  
  # Sample hexagons
  sample_indices <- seq(1, nrow(hex_sf), by = sample_every)
  hex_sf_sampled <- hex_sf[sample_indices, ]
  
  px_res_lon <- 360 / 43200
  px_res_lat <- 180 / 18720
  
  # Create full output with NAs initially
  out <- data.frame(
    hex_id = as.character(hex_sf$hex_id),
    poblacion_personas = NA_real_,
    stringsAsFactors = FALSE
  )

  n <- nrow(hex_sf_sampled)
  message("    [SAMPLED] Processing ", n, " hexagons out of ", nrow(hex_sf), " total")

  for (i in seq_len(n)) {
    hex_geom <- hex_sf_sampled[i, ]
    hex_id <- as.character(hex_geom$hex_id)
    orig_idx <- sample_indices[i]
    
    tryCatch({
      centroid <- sf::st_centroid(hex_geom)
      coords <- sf::st_coordinates(centroid)
      lon <- coords[1, 1]
      lat <- coords[1, 2]
      
      col_idx <- as.integer((lon + 180) / px_res_lon)
      row_idx <- as.integer((90 - lat) / px_res_lat)
      col_1based <- col_idx + 1
      row_1based <- row_idx + 1
      
      col_1based <- pmax(1, pmin(ncol(r), col_1based))
      row_1based <- pmax(1, pmin(nrow(r), row_1based))
      
      row_vals <- raster::getValues(r, row = row_1based, nrows = 1)
      if (length(row_vals) >= col_1based) {
        val <- as.numeric(row_vals[col_1based])
        if (!is.na(val) && val > 0) {
          out$poblacion_personas[orig_idx] <- val
        }
      }
      
      if (i %% 10 == 0) message("      Processed ", i, "/", n)
    }, error = function(e) {})
  }

  out
}

message("Cargando malla hexagonal...")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)

processed <- 0
for (scenario in scenarios) {
  for (year in years) {
    tif_info <- resolve_tif_path(pop_root, scenario, year)
    if (is.na(tif_info$path)) {
      message("[SKIP] ", scenario, " ", year)
      next
    }

    message("Procesando ", basename(tif_info$path), "...")

    datos <- tryCatch(
      extract_hex_sums_sampled(tif_info$path, hex_sf, sample_every = SAMPLE_EVERY),
      error = function(e) {
        message("ERROR: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(datos)) next

    variable_nombre <- paste0("Poblacion ", year, " - ", scenario, " (TEST SAMPLE)")
    catalogo <- data.frame(
      variable_id = paste0("poblacion_", year, "_", tolower(scenario)),
      variable_nombre = variable_nombre,
      tipo_dato = "continuo",
      unidades = "personas",
      descripcion = "TEST SAMPLE - Every 100th hexagon",
      fuente = dataset_name,
      referencia = paste0(article_name, " | DOI: ", doi_url),
      columna_id_hex = "hex_id",
      columna_valor = "poblacion_personas",
      resolucion_hex = "10km",
      stringsAsFactors = FALSE
    )

    metadatos <- data.frame(
      titulo = variable_nombre,
      resumen = "Test sample extraction",
      institucion = "Wang et al.",
      fecha_version = data_last_update,
      stringsAsFactors = FALSE
    )

    out_path <- file.path(out_dir, paste0("TEST_H__poblacion__", scenario, "__", year, ".xlsx"))

    wb <- createWorkbook()
    addWorksheet(wb, "datos")
    addWorksheet(wb, "catalogo_variable")
    addWorksheet(wb, "metadatos")
    writeData(wb, "datos", datos)
    writeData(wb, "catalogo_variable", catalogo)
    writeData(wb, "metadatos", metadatos)
    saveWorkbook(wb, out_path, overwrite = TRUE)

    n_valid <- sum(!is.na(datos$poblacion_personas) & datos$poblacion_personas > 0)
    message("  [OK] ", basename(out_path))
    message("  Hexagons with data: ", n_valid)
    processed <- processed + 1
  }
}

message("\nFinalizado - Archivos: ", processed)
