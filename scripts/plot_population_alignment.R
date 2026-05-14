suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(sp)
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

parse_args <- function(args) {
  out <- list(
    scenario = "SSP5",
    year = 2100L,
    hex_id = NA_character_
  )

  if (length(args) == 0) return(out)

  for (arg in args) {
    if (grepl("^--scenario=", arg)) out$scenario <- sub("^--scenario=", "", arg)
    if (grepl("^--year=", arg)) out$year <- as.integer(sub("^--year=", "", arg))
    if (grepl("^--hex-id=", arg)) out$hex_id <- sub("^--hex-id=", "", arg)
  }

  out
}

resolve_tif_path <- function(pop_root_dir, scenario, year) {
  scenario_dir <- file.path(pop_root_dir, scenario)
  if (!dir.exists(scenario_dir)) {
    stop("No existe el directorio del escenario: ", scenario_dir)
  }

  target_name <- paste0(scenario, "_", year, ".tif")
  hits <- list.files(scenario_dir, pattern = paste0("^", target_name, "$"), recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0) {
    stop("No se encontro el raster: ", target_name)
  }

  hits[[1]]
}

PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 180 / 18720
NCOLS_R <- 43200L
NROWS_R <- 18720L

global_col_from_lon <- function(lon) {
  max(1L, min(NCOLS_R, as.integer(floor((lon + 180) / PX_RES_LON)) + 1L))
}

global_row_from_lat <- function(lat) {
  max(1L, min(NROWS_R, as.integer(floor((90 - lat) / PX_RES_LAT)) + 1L))
}

cell_x_edges <- function(col_min, col_max) {
  left <- -180 + (seq.int(col_min, col_max) - 1L) * PX_RES_LON
  c(left, left[length(left)] + PX_RES_LON)
}

cell_y_edges_desc <- function(row_min, row_max) {
  top <- 90 - (seq.int(row_min, row_max) - 1L) * PX_RES_LAT
  c(top, top[length(top)] - PX_RES_LAT)
}

make_block_raster <- function(block_mat, g_col_min, g_col_max, g_row_min, g_row_max) {
  xmn <- -180 + (g_col_min - 1L) * PX_RES_LON
  xmx <- -180 + g_col_max * PX_RES_LON
  ymx <- 90 - (g_row_min - 1L) * PX_RES_LAT
  ymn <- 90 - g_row_max * PX_RES_LAT
  raster::raster(block_mat, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = "+proj=longlat +datum=WGS84 +no_defs")
}

pick_default_hex <- function(hex_sf) {
  bb <- sf::st_bbox(hex_sf)
  target <- sf::st_sfc(sf::st_point(c((bb[["xmin"]] + bb[["xmax"]]) / 2, (bb[["ymin"]] + bb[["ymax"]]) / 2)), crs = sf::st_crs(hex_sf))
  centroids <- sf::st_centroid(hex_sf)
  idx <- which.min(as.numeric(sf::st_distance(centroids, target)))
  idx[[1]]
}

build_zoom_cells <- function(hex_geom, block_mat, g_col_min, g_row_min) {
  coords <- sf::st_coordinates(hex_geom[[1]])
  if ("L1" %in% colnames(coords)) {
    coords <- coords[coords[, "L1"] == 1L, , drop = FALSE]
  }

  bb <- sf::st_bbox(hex_geom)
  col_min <- global_col_from_lon(bb[["xmin"]]) - 1L
  col_max <- global_col_from_lon(bb[["xmax"]]) + 1L
  row_min <- global_row_from_lat(bb[["ymax"]]) - 1L
  row_max <- global_row_from_lat(bb[["ymin"]]) + 1L

  col_min <- max(1L, col_min)
  col_max <- min(NCOLS_R, col_max)
  row_min <- max(1L, row_min)
  row_max <- min(NROWS_R, row_max)

  bc_min <- max(1L, col_min - g_col_min + 1L)
  bc_max <- min(ncol(block_mat), col_max - g_col_min + 1L)
  br_min <- max(1L, row_min - g_row_min + 1L)
  br_max <- min(nrow(block_mat), row_max - g_row_min + 1L)

  local_cols <- seq.int(bc_min, bc_max)
  local_rows <- seq.int(br_min, br_max)
  global_cols <- g_col_min + local_cols - 1L
  global_rows <- g_row_min + local_rows - 1L

  x_left <- -180 + (global_cols - 1L) * PX_RES_LON
  x_right <- x_left + PX_RES_LON
  y_top <- 90 - (global_rows - 1L) * PX_RES_LAT
  y_bottom <- y_top - PX_RES_LAT
  x_center <- x_left + PX_RES_LON / 2
  y_center <- y_bottom + PX_RES_LAT / 2

  rows <- vector("list", length(local_rows) * length(local_cols))
  k <- 1L
  for (row_idx in seq_along(local_rows)) {
    lat_c <- y_center[row_idx]
    pip <- sp::point.in.polygon(
      point.x = x_center,
      point.y = rep(lat_c, length(x_center)),
      pol.x = as.numeric(coords[, "X"]),
      pol.y = as.numeric(coords[, "Y"])
    )

    for (col_idx in seq_along(local_cols)) {
      rows[[k]] <- data.frame(
        xleft = x_left[col_idx],
        xright = x_right[col_idx],
        ybottom = y_bottom[row_idx],
        ytop = y_top[row_idx],
        xcenter = x_center[col_idx],
        ycenter = lat_c,
        value = block_mat[local_rows[row_idx], local_cols[col_idx]],
        inside = pip[col_idx] > 0L,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  do.call(rbind, rows)
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
project_dir <- normalizePath(get_project_dir(), mustWork = FALSE)
hex_gpkg <- file.path(project_dir, "data", "master_hex_10km.gpkg")
pop_root <- "H:/My Drive/Data/SSPs/Gridded/Pop"
diag_dir <- file.path(project_dir, "diagnostics")
dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
status_path <- file.path(diag_dir, sprintf("population_block_%s_%s_status.txt", args$scenario, args$year))
writeLines(character(0), status_path)
log_status <- function(...) {
  cat(paste0(..., collapse = ""), "\n", file = status_path, append = TRUE)
}

message("Leyendo malla hexagonal...")
log_status("Leyendo malla hexagonal")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
if (!"hex_id" %in% names(hex_sf)) stop("La malla maestra debe contener hex_id")

tif_path <- resolve_tif_path(pop_root, args$scenario, args$year)
message("Leyendo raster: ", tif_path)
log_status("Leyendo raster: ", tif_path)
r <- raster::raster(tif_path)

all_bb <- sf::st_bbox(hex_sf)
g_col_min <- max(1L, as.integer(floor((all_bb[["xmin"]] + 180) / PX_RES_LON)))
g_col_max <- min(NCOLS_R, as.integer(ceiling((all_bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((90 - all_bb[["ymax"]]) / PX_RES_LAT)))
g_row_max <- min(NROWS_R, as.integer(ceiling((90 - all_bb[["ymin"]]) / PX_RES_LAT)) + 1L)

n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L
message(sprintf("Bloque usado por el extractor: rows %d-%d, cols %d-%d (%d x %d)", g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb))
log_status(sprintf("Bloque usado por el extractor: rows %d-%d, cols %d-%d (%d x %d)", g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb))

block_vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb, col = g_col_min, ncols = n_cb)
block_mat <- matrix(block_vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
block_r <- make_block_raster(block_mat, g_col_min, g_col_max, g_row_min, g_row_max)

plot_r <- raster::calc(block_r, fun = function(x) log1p(pmax(x, 0)))
plot_r_overview <- raster::aggregate(plot_r, fact = 6, fun = mean, expand = TRUE)

sel_idx <- if (!is.na(args$hex_id) && args$hex_id %in% hex_sf$hex_id) {
  match(args$hex_id, hex_sf$hex_id)
} else {
  pick_default_hex(hex_sf)
}
sel_hex <- hex_sf[sel_idx, ]
sel_hex_id <- as.character(sel_hex$hex_id[[1]])

overview_path <- file.path(diag_dir, sprintf("population_block_%s_%s_overview.png", args$scenario, args$year))
zoom_path <- file.path(diag_dir, sprintf("population_block_%s_%s_hex_%s_zoom.png", args$scenario, args$year, gsub("[^A-Za-z0-9_-]", "_", sel_hex_id)))

cols <- grDevices::colorRampPalette(c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"))(80)

png(overview_path, width = 1800, height = 1400, res = 180)
par(mar = c(4, 4, 4, 6))
plot(plot_r_overview,
     col = cols,
     axes = TRUE,
     box = FALSE,
     legend.args = list(text = "log1p(poblacion)", side = 4, line = 2.5),
     main = sprintf("Bloque del raster usado por el extractor: %s %s", args$scenario, args$year),
     xlab = "Longitud",
     ylab = "Latitud")
plot(sf::st_geometry(sel_hex), add = TRUE, border = "#d7301f", lwd = 2)
mtext(sprintf("Hex resaltado para zoom: %s", sel_hex_id), side = 3, line = 0.2, cex = 0.8)
dev.off()
log_status("Overview generado: ", overview_path)

zoom_cells <- build_zoom_cells(sf::st_geometry(sel_hex), block_mat, g_col_min, g_row_min)
log_status("Zoom cells construidas: ", nrow(zoom_cells))
hex_bb <- sf::st_bbox(sel_hex)
pad_x <- PX_RES_LON * 2
pad_y <- PX_RES_LAT * 2

tryCatch({
  png(zoom_path, width = 1800, height = 1800, res = 220)
  par(mar = c(4, 4, 4, 1))
  plot(NA,
    xlim = c(hex_bb[["xmin"]] - pad_x, hex_bb[["xmax"]] + pad_x),
    ylim = c(hex_bb[["ymin"]] - pad_y, hex_bb[["ymax"]] + pad_y),
    asp = 1,
    xlab = "Longitud",
    ylab = "Latitud",
    main = sprintf("Celdas raster evaluadas para hex %s (%s %s)", sel_hex_id, args$scenario, args$year))

  val_fill <- cols[cut(log1p(pmax(zoom_cells$value, 0)), breaks = length(cols), include.lowest = TRUE)]
  rect(zoom_cells$xleft, zoom_cells$ybottom, zoom_cells$xright, zoom_cells$ytop,
    col = val_fill, border = grDevices::adjustcolor("grey20", alpha.f = 0.35), lwd = 0.4)

  inside_cells <- zoom_cells[zoom_cells$inside, , drop = FALSE]
  if (nrow(inside_cells) > 0) {
    rect(inside_cells$xleft, inside_cells$ybottom, inside_cells$xright, inside_cells$ytop,
      col = grDevices::adjustcolor("#fdae6b", alpha.f = 0.5), border = "#e6550d", lwd = 1)
    points(inside_cells$xcenter, inside_cells$ycenter, pch = 16, cex = 0.5, col = "#a63603")
  }

  plot(sf::st_geometry(sel_hex), add = TRUE, border = "#d7301f", lwd = 2.5)
  plot(sf::st_centroid(sel_hex), add = TRUE, pch = 4, cex = 1.2, lwd = 2, col = "#54278f")
  legend("topright",
      legend = c("Hex seleccionado", "Centroide del hex", "Celdas contadas por point-in-polygon"),
      col = c("#d7301f", "#54278f", "#e6550d"),
      lwd = c(2.5, 2, 1.5),
      pch = c(NA, 4, 15),
      pt.cex = c(NA, 1, 1.2),
      bty = "n")
  dev.off()
  log_status("Zoom generado: ", zoom_path)
}, error = function(e) {
  try(dev.off(), silent = TRUE)
  log_status("ERROR_ZOOM: ", conditionMessage(e))
  stop(e)
})

message("Listo.")
message("  Overview: ", overview_path)
message("  Zoom:     ", zoom_path)
message("  Hex usado para el zoom: ", sel_hex_id)
log_status("Listo")
log_status("Hex usado para el zoom: ", sel_hex_id)