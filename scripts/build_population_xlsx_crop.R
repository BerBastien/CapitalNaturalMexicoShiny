# build_population_xlsx.R - MEXICO CROP APPROACH
suppressPackageStartupMessages({
  library(sf)
  library(terra)
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

# TEST: Only SSP2 for debugging
scenarios <- c("SSP2")
years <- c(2050)

# Metadatos
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
  target_name <- paste0(scenario, "_", year, ".tif")
  hits <- list.files(scenario_dir, pattern = paste0("^", target_name, "$"), recursive = TRUE, full.names = TRUE)
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

open_raster_safe <- function(tif_path) {
  if (!file.exists(tif_path)) {
    stop("No existe el raster: ", tif_path)
  }
  candidates <- unique(c(
    tif_path,
    normalizePath(tif_path, winslash = "/", mustWork = FALSE),
    normalizePath(tif_path, winslash = "\\", mustWork = FALSE)
  ))
  last_err <- NULL
  for (p in candidates) {
    rr <- tryCatch(terra::rast(p), error = function(e) {
      last_err <<- conditionMessage(e)
      NULL
    })
    if (!is.null(rr)) return(rr)
  }
  stop("[rast] no se pudo abrir el raster. Ultimo error: ", last_err)
}

extract_hex_sums_chunked <- function(tif_path, hex_sf, chunk_size = 200) {
  # Load with raster package for direct indexing access
  r_ras <- raster::raster(tif_path)
  
  # Get Mexico bounding box from hex grids
  hex_bbox <- sf::st_bbox(hex_sf)
  
  # Add buffer to ensure we capture all hexagons
  buf <- 0.1
  bbox_cropped <- c(
    xmin = hex_bbox["xmin"] - buf,
    ymin = hex_bbox["ymin"] - buf,
    xmax = hex_bbox["xmax"] + buf,
    ymax = hex_bbox["ymax"] + buf
  )
  
  message("    [INFO] Cropping to Mexico bbox: ", 
          round(bbox_cropped["xmin"], 2), " to ", round(bbox_cropped["xmax"], 2), " lon, ",
          round(bbox_cropped["ymin"], 2), " to ", round(bbox_cropped["ymax"], 2), " lat")
  
  # Crop raster to Mexico region
  e <- raster::extent(bbox_cropped)
  r_crop <- tryCatch(
    raster::crop(r_ras, e),
    error = function(err) {
      message("    [WARN] Cropping failed: ", conditionMessage(err), " - using full raster")
      r_ras
    }
  )
  
  # Load cropped raster into memory as matrix
  message("    [INFO] Loading cropped raster into memory...")
  raster_matrix <- raster::as.matrix(r_crop)
  nr <- nrow(raster_matrix)
  nc <- ncol(raster_matrix)
  message("    [LOADED] Matrix: ", nr, " x ", nc)
  
  # Get extent info for coordinate-to-index mapping
  ext <- raster::extent(r_crop)
  ext_xmin <- ext@xmin
  ext_ymax <- ext@ymax
  pixel_size_x <- (ext@xmax - ext@xmin) / nc
  pixel_size_y <- (ext@ymax - ext@ymin) / nr
  
  message("    [GEOTRANS] Pixel size: ", round(pixel_size_x, 6), "° x ", round(pixel_size_y, 6), 
          "° | Extent: (", round(ext_xmin, 2), ",", round(ext@ymin, 2), ") to (", 
          round(ext@xmax, 2), ",", round(ext_ymax, 2), ")")
  
  out <- data.frame(
    hex_id = as.character(hex_sf$hex_id),
    poblacion_personas = 0.0,
    stringsAsFactors = FALSE
  )

  n <- nrow(hex_sf)
  if (n == 0) return(out)

  starts <- seq(1, n, by = chunk_size)

  for (s in starts) {
    e_chunk <- min(s + chunk_size - 1, n)
    hexes <- hex_sf[s:e_chunk, ]
    hex_ids_chunk <- as.character(hexes$hex_id)

    for (i in seq_len(nrow(hexes))) {
      hex_geom <- hexes[i, ]
      hex_id <- hex_ids_chunk[i]
      
      # Get centroid coordinates
      centroid <- sf::st_centroid(hex_geom)
      coords <- sf::st_coordinates(centroid)
      lon <- coords[1, 1]
      lat <- coords[1, 2]
      
      # Check if centroid is in cropped extent
      if (lon >= ext_xmin && lon <= ext@xmax && lat >= ext@ymin && lat <= ext_ymax) {
        # Convert geographic coordinates to matrix indices
        col_idx <- round((lon - ext_xmin) / pixel_size_x) + 1
        row_idx <- round((ext_ymax - lat) / pixel_size_y) + 1
        
        # Ensure within bounds
        col_idx <- pmax(1, pmin(nc, col_idx))
        row_idx <- pmax(1, pmin(nr, row_idx))
        
        # Direct matrix access
        val <- tryCatch({
          v <- raster_matrix[row_idx, col_idx]
          if (!is.na(v) && v > 0) as.numeric(v) else 0.0
        }, error = function(e) 0.0)
        
        if (val > 0) {
          hex_idx <- which(out$hex_id == hex_id)
          if (length(hex_idx) > 0) {
            out$poblacion_personas[hex_idx] <- val
          }
        }
      }
    }

    if ((e_chunk %% (chunk_size * 5)) == 0 || e_chunk == n) {
      n_valid <- sum(out$poblacion_personas[1:e_chunk] > 0)
      message("    chunk ", s, "-", e_chunk, " de ", n, " (...", n_valid, " con datos)")
    }
    gc(verbose = FALSE)
  }

  out$poblacion_personas <- round(out$poblacion_personas, 4)
  out
}

message("Cargando malla hexagonal 10 km...")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
if (!"hex_id" %in% names(hex_sf)) stop("La malla maestra debe contener la columna hex_id")

processed <- 0
skipped <- 0
skip_details <- character(0)

for (scenario in scenarios) {
  for (year in years) {
    tif_info <- resolve_tif_path(pop_root, scenario, year)
    tif_path <- tif_info$path

    if (is.na(tif_path)) {
      msg <- paste0("[SKIP] ", scenario, " ", year, " (", tif_info$reason, ")")
      message(msg)
      skip_details <- c(skip_details, msg)
      skipped <- skipped + 1
      next
    }

    message("Procesando ", basename(tif_path), "...")

    datos <- tryCatch(
      extract_hex_sums_chunked(tif_path, hex_sf, chunk_size = 200),
      error = function(e) {
        message("  [ERROR] Extraccion fallo para ", scenario, " ", year, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(datos)) {
      skip_details <- c(skip_details, paste0("[SKIP] ", scenario, " ", year, " (fallo_extraccion)"))
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
        " Valor reportado como poblacion por celda hexagonal de 10 km para el escenario ",
        scenario, " en el anio ", year, "."
      ),
      fuente = dataset_name,
      referencia = paste0(article_name, " | DOI: ", doi_url, " | Actualizacion: ", data_last_update, 
                          " | Autores: ", authors),
      columna_id_hex = "hex_id",
      columna_valor = "poblacion_personas",
      resolucion_hex = "10km",
      stringsAsFactors = FALSE
    )

    metadatos <- data.frame(
      titulo = variable_nombre,
      resumen = paste0("Distribucion global de poblacion (1 km) agregada a malla hexagonal de 10 km. ",
                       "Escenario ", scenario, ", horizonte ", year, "."),
      institucion = "Wang et al. (Figshare)",
      fecha_version = data_last_update,
      stringsAsFactors = FALSE
    )

    out_name <- paste0("H__poblacion__", scenario, "__", year, ".xlsx")
    out_path <- file.path(out_dir, out_name)

    wb <- createWorkbook()
    addWorksheet(wb, "datos")
    addWorksheet(wb, "catalogo_variable")
    addWorksheet(wb, "metadatos")

    writeData(wb, "datos", datos)
    writeData(wb, "catalogo_variable", catalogo)
    writeData(wb, "metadatos", metadatos)

    saveWorkbook(wb, out_path, overwrite = TRUE)

    n_valid <- sum(datos$poblacion_personas > 0)
    message("  [OK] ", out_name, " (", n_valid, " celdas con datos)")
    processed <- processed + 1
  }
}

message("\nFinalizado")
message("  Archivos generados: ", processed)
message("  Omitidos: ", skipped)
if (length(skip_details) > 0) {
  message("  Detalle omisiones:")
  for (x in skip_details) message("    - ", x)
}
message("  Carpeta salida: ", out_dir)
