# build_population_xlsx.R
#
# Extrae poblacion de raster globales SSP (1 km) hacia la malla hexagonal de 10 km
# y genera un xlsx por escenario/anio en data/indicadores/.
#
# Escenarios: SSP1, SSP2, SSP3, SSP4, SSP5
# Anios:      2020, 2050, 2070, 2100
#
# Salida (formato canonico para el arbol de la app):
#   H__poblacion__SSP2__2020.xlsx

suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(sp)
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

# All scenarios and years
scenarios <- c("SSP2")
years     <- c(2050)

# Metadatos fuente (espanol)
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

# ---------------------------------------------------------------------------
# Pixel grid constants (global 1 km grid: 43200 cols x 18720 rows)
#   col = floor((lon + 180) / px_res_lon) + 1   [1 = west edge, col increases east]
#   row = floor((90  - lat) / px_res_lat) + 1   [1 = north pole, row increases south]
# Confirmed correct by diagnostic: NYC=218, MexCity=3-11, Pacific=0 (diag_raster.R)
# ---------------------------------------------------------------------------
PX_RES_LON <- 360 / 43200   # 0.008333 deg/pixel
PX_RES_LAT <- 180 / 18720   # 0.009615 deg/pixel
NCOLS_R    <- 43200L
NROWS_R    <- 18720L

extract_hex_sums_block <- function(tif_path, hex_sf) {
  r <- raster::raster(tif_path)

  # ---- 1. Compute raster extent that covers all hexes ----------------------
  all_bb <- sf::st_bbox(hex_sf)

  g_col_min <- max(1L, as.integer(floor((all_bb[["xmin"]] + 180) / PX_RES_LON)))
  g_col_max <- min(NCOLS_R, as.integer(ceiling((all_bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
  g_row_min <- max(1L, as.integer(floor((90 - all_bb[["ymax"]]) / PX_RES_LAT)))
  g_row_max <- min(NROWS_R, as.integer(ceiling((90 - all_bb[["ymin"]]) / PX_RES_LAT)) + 1L)

  n_rb <- g_row_max - g_row_min + 1L
  n_cb <- g_col_max - g_col_min + 1L

  message(sprintf("    Block: rows %d-%d, cols %d-%d  (%d x %d = %.1f MB)",
    g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb, n_rb * n_cb * 4 / 1e6))

  # ---- 2. Read the entire block once from disk into memory -----------------
  block_vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb,
                                           col = g_col_min, ncols = n_cb)
  # matrix[row_local, col_local]; row 1 = g_row_min
  block_mat <- matrix(block_vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
  rm(block_vals); gc(verbose = FALSE)

  # Pre-compute pixel-center geographic coordinates for every block col
  # cell center = left-edge + 0.5 pixel
  block_lons <- -180 + (seq.int(g_col_min, g_col_max) - 0.5) * PX_RES_LON

  # ---- 3. Pre-extract polygon vertex coords (avoid repeated sf calls) ------
  n <- nrow(hex_sf)
  hex_coords_list <- vector("list", n)
  for (i in seq_len(n)) {
    coords <- sf::st_coordinates(sf::st_geometry(hex_sf[i, ])[[1]])
    if ("L1" %in% colnames(coords))
      coords <- coords[coords[, "L1"] == 1L, , drop = FALSE]
    hex_coords_list[[i]] <- list(
      x = as.numeric(coords[, "X"]),
      y = as.numeric(coords[, "Y"]),
      bb = sf::st_bbox(hex_sf[i, ])
    )
  }

  # ---- 4. Per-hex pixel summation entirely from memory ---------------------

  out <- data.frame(
    hex_id = as.character(hex_sf$hex_id),
    poblacion_personas = as.numeric(rep(0, n)),
    stringsAsFactors = FALSE
  )

  sum_hex <- function(hc) {
    bb <- hc$bb
    x_poly <- hc$x
    y_poly <- hc$y
    if (length(x_poly) < 4) return(0)

    # Raster indices for hex bbox
    col_min <- max(1L, as.integer(floor((bb[["xmin"]] + 180) / PX_RES_LON)) + 1L)
    col_max <- min(NCOLS_R, as.integer(ceiling((bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
    row_min <- max(1L, as.integer(floor((90 - bb[["ymax"]]) / PX_RES_LAT)) + 1L)
    row_max <- min(NROWS_R, as.integer(ceiling((90 - bb[["ymin"]]) / PX_RES_LAT)) + 1L)
    if (col_min > col_max || row_min > row_max) return(0)

    # Convert to block-local indices
    bc_min <- col_min - g_col_min + 1L
    bc_max <- col_max - g_col_min + 1L
    br_min <- row_min - g_row_min + 1L
    br_max <- row_max - g_row_min + 1L
    bc_min <- max(1L, bc_min); bc_max <- min(n_cb, bc_max)
    br_min <- max(1L, br_min); br_max <- min(n_rb, br_max)
    if (bc_min > bc_max || br_min > br_max) return(0)

    local_cols <- seq.int(bc_min, bc_max)
    lons <- block_lons[local_cols]
    total <- 0

    for (br in seq.int(br_min, br_max)) {
      # pixel center latitude: cell row in global grid = g_row_min + br - 1
      lat_c <- 90 - (g_row_min + br - 1L - 0.5) * PX_RES_LAT

      pip <- sp::point.in.polygon(
        point.x = lons,
        point.y = rep(lat_c, length(lons)),
        pol.x = x_poly, pol.y = y_poly
      )
      inside <- pip > 0L
      if (!any(inside)) next

      vals <- block_mat[br, local_cols]
      total <- total + sum(vals[inside], na.rm = TRUE)
    }
    as.numeric(total)
  }

  for (i in seq_len(n)) {
    if (i %% 2000L == 0L || i == n) {
      n_valid <- sum(out$poblacion_personas[1:i] > 0, na.rm = TRUE)
      message(sprintf("    %d / %d  (%d con datos)", i, n, n_valid))
    }
    out$poblacion_personas[i] <- tryCatch(sum_hex(hex_coords_list[[i]]), error = function(e) 0)
  }

  out$poblacion_personas <- round(as.numeric(out$poblacion_personas), 4)
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

    out_name_check <- paste0("H__poblacion__", scenario, "__", year, ".xlsx")
    out_path_check <- file.path(out_dir, out_name_check)
    if (file.exists(out_path_check)) {
      message("[SKIP] ", out_name_check, " ya existe, omitiendo")
      skipped <- skipped + 1
      next
    }

    message("Procesando ", basename(tif_path), "...")

    datos <- tryCatch(
      extract_hex_sums_block(tif_path, hex_sf),
      error = function(e) {
        message("  [ERROR] Extraccion/suma fallo para ", scenario, " ", year, ": ", conditionMessage(e))
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
        " Valor reportado como suma de poblacion por celda hexagonal de 10 km (agregacion zonal) para el escenario ",
        scenario,
        " en el anio ",
        year,
        "."
      ),
      fuente = dataset_name,
      referencia = paste0(
        article_name,
        " | DOI: ", doi_url,
        " | Actualizacion: ", data_last_update,
        " | Autores: ", authors
      ),
      columna_id_hex = "hex_id",
      columna_valor = "poblacion_personas",
      resolucion_hex = "10km",
      stringsAsFactors = FALSE
    )

    metadatos <- data.frame(
      titulo = variable_nombre,
      resumen = paste0(
        "Distribucion global de poblacion (1 km) agregada a malla hexagonal de 10 km. ",
        "Escenario ", scenario, ", horizonte ", year, "."
      ),
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

    n_valid <- sum(!is.na(datos$poblacion_personas) & datos$poblacion_personas > 0)
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

