# Regenerate all population XLSX files with cleaned extraction (Marine hexes as NA)
# This ensures all 19 files are consistent and clean

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

scenarios <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
years <- c(2020, 2050, 2070, 2100)

resolve_tif_path <- function(pop_root_dir, scenario, year) {
  scenario_dir <- file.path(pop_root_dir, scenario)
  if (!dir.exists(scenario_dir)) {
    return(list(path = NA_character_, reason = "dir_not_found"))
  }
  target_name <- paste0(scenario, "_", year, ".tif")
  hits <- list.files(scenario_dir, pattern = paste0("^", target_name, "$"), recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0) {
    return(list(path = NA_character_, reason = "file_not_found"))
  }
  hit <- hits[[1]]
  fi <- file.info(hit)
  if (is.na(fi$size) || fi$size <= 0) {
    return(list(path = NA_character_, reason = "file_empty"))
  }
  list(path = hit, reason = "ok")
}

PX_RES_LON <- 360 / 43200   # 0.008333 deg/pixel (1/120 degree)
PX_RES_LAT <- 360 / 43200   # 0.008333 deg/pixel — square pixels, NOT 180/18720
LAT_TOP    <- 84             # raster top is 84N (covers 84N to -72S, 156 deg)
NCOLS_R <- 43200L
NROWS_R <- 18720L

extract_hex_sums_block <- function(tif_path, hex_sf) {
  r <- raster::raster(tif_path)
  all_bb <- sf::st_bbox(hex_sf)

  g_col_min <- max(1L, as.integer(floor((all_bb[["xmin"]] + 180) / PX_RES_LON)))
  g_col_max <- min(NCOLS_R, as.integer(ceiling((all_bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
  g_row_min <- max(1L, as.integer(floor((LAT_TOP - all_bb[["ymax"]]) / PX_RES_LAT)))
  g_row_max <- min(NROWS_R, as.integer(ceiling((LAT_TOP - all_bb[["ymin"]]) / PX_RES_LAT)) + 1L)

  n_rb <- g_row_max - g_row_min + 1L
  n_cb <- g_col_max - g_col_min + 1L

  message(sprintf("    Block: rows %d-%d, cols %d-%d  (%d x %d)", g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb))

  block_vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb, col = g_col_min, ncols = n_cb)
  block_mat <- matrix(block_vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
  rm(block_vals); gc(verbose = FALSE)

  block_lons <- -180 + (seq.int(g_col_min, g_col_max) - 0.5) * PX_RES_LON

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

  marine_mask <- if ("tipo" %in% names(hex_sf)) hex_sf$tipo == "Oceano" else rep(FALSE, n)

  out <- data.frame(
    hex_id = as.character(hex_sf$hex_id),
    poblacion_personas = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )

  sum_hex <- function(hc) {
    bb <- hc$bb
    x_poly <- hc$x
    y_poly <- hc$y
    if (length(x_poly) < 4) return(0)

    col_min <- max(1L, as.integer(floor((bb[["xmin"]] + 180) / PX_RES_LON)) + 1L)
    col_max <- min(NCOLS_R, as.integer(ceiling((bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
    row_min <- max(1L, as.integer(floor((LAT_TOP - bb[["ymax"]]) / PX_RES_LAT)) + 1L)
    row_max <- min(NROWS_R, as.integer(ceiling((LAT_TOP - bb[["ymin"]]) / PX_RES_LAT)) + 1L)
    if (col_min > col_max || row_min > row_max) return(0)

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
      lat_c <- LAT_TOP - (g_row_min + br - 1L - 0.5) * PX_RES_LAT
      pip <- sp::point.in.polygon(point.x = lons, point.y = rep(lat_c, length(lons)), 
                                   pol.x = x_poly, pol.y = y_poly)
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
    if (marine_mask[i]) next
    out$poblacion_personas[i] <- tryCatch(sum_hex(hex_coords_list[[i]]), error = function(e) 0)
  }

  out$poblacion_personas <- round(as.numeric(out$poblacion_personas), 4)
  out
}

message("Cargando malla hexagonal 10 km...")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
if (!"hex_id" %in% names(hex_sf)) stop("La malla debe contener hex_id")

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

    out_name <- paste0("H__poblacion__", scenario, "__", year, ".xlsx")
    out_path <- file.path(out_dir, out_name)

    # Always regenerate (remove skip-if-exists)
    message("Procesando ", basename(tif_path), "...")

    datos <- tryCatch(
      extract_hex_sums_block(tif_path, hex_sf),
      error = function(e) {
        message("  [ERROR] ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(datos)) {
      message("[SKIP] ", scenario, " ", year, " (extraction_failed)")
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
      descripcion = paste0("Poblacion para escenario ", scenario, " año ", year),
      fuente = "Wang et al. SSP rasters",
      referencia = "https://doi.org/10.6084/m9.figshare.19608594",
      columna_id_hex = "hex_id",
      columna_valor = "poblacion_personas",
      resolucion_hex = "10km",
      stringsAsFactors = FALSE
    )

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "datos")
    openxlsx::writeData(wb, sheet = "datos", x = datos)
    
    openxlsx::addWorksheet(wb, "catalogo_variable")
    openxlsx::writeData(wb, sheet = "catalogo_variable", x = catalogo)
    
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
    
    n_valid <- sum(datos$poblacion_personas > 0, na.rm = TRUE)
    total <- sum(datos$poblacion_personas, na.rm = TRUE)
    message(sprintf("[OK] %s (%d celdas con datos, total=%.0f)", out_name, n_valid, total))
    processed <- processed + 1
  }
}

message(sprintf("\nFinalizado: %d generados, %d omitidos", processed, skipped))
