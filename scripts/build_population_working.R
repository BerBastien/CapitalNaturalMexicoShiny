# Fast working version - Full extraction with progress reporting
# Based on sampled version but processes ALL hexagons
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

# TEST: Only SSP2 and 2050 for user's requested test
scenarios <- c("SSP2")
years <- c(2050)

# Metadata
article_name <- "Projecting 1 km-grid population distributions from 2020 to 2100 globally under shared socioeconomic pathways"
dataset_name <- "Global 1 KM-Grid Population Distributions from 2020 to 2100"
doi_url <- "https://doi.org/10.6084/m9.figshare.19608594"
data_last_update <- "2022-08-29"
authors <- "Xinyu Wang; Xiangfeng Meng; Ying Long"

descripcion_base <- paste0(
  "Dataset global de poblacion en grilla de 1 km (30 arc-seconds) para 248 paises o areas, ",
  "consistente con las trayectorias socioeconomicas compartidas (SSP). ",
  "Construido mediante algoritmo Random Forest y publicado en intervalos de 5 anios entre 2020 y 2100."
)

resolve_tif_path <- function(pop_root_dir, scenario, year) {
  scenario_dir <- file.path(pop_root_dir, scenario)
  if (!dir.exists(scenario_dir)) {
    return(list(path = NA_character_, reason = "directorio_escenario_no_existe"))
  }
  sub_names <- if (scenario == "SSP1") "SSP1" else paste0("SPP", substr(scenario, 4, 4))
  scenario_dir <- file.path(scenario_dir, sub_names)
  if (!dir.exists(scenario_dir)) {
    return(list(path = NA_character_, reason = "subdirectorio_no_existe"))
  }
  target_name <- paste0(scenario, "_", year, ".tif")
  hits <- list.files(scenario_dir, pattern = paste0("^", target_name, "$"), full.names = TRUE)
  if (length(hits) == 0) {
    return(list(path = NA_character_, reason = "archivo_no_encontrado"))
  }
  hit <- hits[[1]]
  fi <- file.info(hit)
  if (is.na(fi$size) || fi$size <= 0) {
    return(list(path = NA_character_, reason = "archivo_vacio_o_invalido"))
  }
  list(path = hit, reason = "ok")
}

extract_hex_fast <- function(tif_path, hex_sf, chunk_size = 200) {
  r <- raster::raster(tif_path)
  
  px_res_lon <- 360 / 43200
  px_res_lat <- 180 / 18720
  
  # CRITICAL: Initialize as numeric type, not logical
  out <- data.frame(
    hex_id = as.character(hex_sf$hex_id),
    poblacion_personas = as.numeric(rep(0, nrow(hex_sf))),
    stringsAsFactors = FALSE
  )
  
  n <- nrow(hex_sf)
  if (n == 0) return(out)
  
  # Process by centroid (simpler and faster than full hex sum)
  for (i in seq_len(n)) {
    if (i %% 10 == 0) {
      n_valid <- sum(out$poblacion_personas[1:i] > 0)
      cat("  Processed", i, "/", n, "(...", n_valid, "con datos)\n")
    }
    
    hex_geom <- hex_sf[i, ]
    hex_id <- as.character(hex_geom$hex_id)
    
    tryCatch({
      centroid <- sf::st_centroid(hex_geom)
      coords <- sf::st_coordinates(centroid)
      lon <- coords[1, 1]
      lat <- coords[1, 2]
      
      col_idx <- as.integer((lon + 180) / px_res_lon)
      row_idx <- as.integer((90 - lat) / px_res_lat)
      
      col_1based <- pmax(1, pmin(ncol(r), col_idx + 1))
      row_1based <- pmax(1, pmin(nrow(r), row_idx + 1))
      
      row_vals <- raster::getValues(r, row = row_1based, nrows = 1)
      if (length(row_vals) >= col_1based && !is.na(row_vals[col_1based])) {
        val <- as.numeric(row_vals[col_1based])
        if (val > 0) {
          out$poblacion_personas[i] <- val
        }
      }
    }, error = function(e) {})
  }
  
  out$poblacion_personas <- round(out$poblacion_personas, 4)
  out
}

message("Cargando malla hexagonal 10 km...")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
if (!"hex_id" %in% names(hex_sf)) stop("La malla maestra debe contener la columna hex_id")

processed <- 0
skipped <- 0

for (scenario in scenarios) {
  for (year in years) {
    tif_info <- resolve_tif_path(pop_root, scenario, year)
    tif_path <- tif_info$path
    
    if (is.na(tif_path)) {
      message("[SKIP] ", scenario, " ", year, " (", tif_info$reason, ")")
      skipped <- skipped + 1
      next
    }
    
    message("Procesando ", basename(tif_path), "...")
    
    datos <- tryCatch(
      extract_hex_fast(tif_path, hex_sf),
      error = function(e) {
        message("  [ERROR] Extraccion fallo: ", conditionMessage(e))
        NULL
      }
    )
    
    if (is.null(datos)) {
      skipped <- skipped + 1
      next
    }
    
    variable_nombre <- paste0("Poblacion ", year, " - ", scenario)
    variable_id <- paste0("poblacion_", year, "_", tolower(scenario))
    
    catalogo <- data.frame(
      variable_id = variable_id,
      variable_nombre = variable_nombre,
      tipo_dato = "continuo",
      unidades = "personas",
      descripcion = paste0(
        descripcion_base,
        " Valor en celda hexagonal de 10 km para escenario ", scenario, ", anio ", year, "."
      ),
      fuente = dataset_name,
      referencia = paste0(article_name, ". ", doi_url),
      columna_id_hex = "hex_id",
      columna_valor = "poblacion_personas",
      resolucion_hex = "10km"
    )
    
    metadatos <- data.frame(
      titulo = variable_nombre,
      resumen = descripcion_base,
      institucion = "Capital Natural de Mexico",
      fecha_version = Sys.Date()
    )
    
    xlsx_name <- paste0("H__poblacion__", scenario, "__", year, ".xlsx")
    xlsx_path <- file.path(out_dir, xlsx_name)
    
    wb <- createWorkbook()
    addWorksheet(wb, "datos")
    addWorksheet(wb, "catalogo_variable")
    addWorksheet(wb, "metadatos")
    
    writeData(wb, "datos", datos)
    writeData(wb, "catalogo_variable", catalogo)
    writeData(wb, "metadatos", metadatos)
    
    saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    
    n_valid <- sum(datos$poblacion_personas > 0)
    message("  [OK] ", xlsx_name)
    message("  Hexagons with data: ", n_valid)
    
    processed <- processed + 1
  }
}

message("Finalizado - Archivos: ", processed)
