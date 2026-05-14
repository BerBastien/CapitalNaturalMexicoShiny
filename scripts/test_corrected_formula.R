# Quick validation: test corrected formula on SSP2 2020, then check Mexico City hexes
suppressPackageStartupMessages({
  library(sf); library(raster); library(sp); library(dplyr)
})

PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 360 / 43200   # square pixels
LAT_TOP    <- 84
NCOLS_R <- 43200L
NROWS_R <- 18720L

tif_path <- "H:/My Drive/Data/SSPs/Gridded/Pop/SSP2/SPP2/SSP2_2020.tif"
hex_gpkg  <- "H:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg"

hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
r <- raster::raster(tif_path)

# Test single pixels first
cities <- data.frame(
  name=c("Mexico City","Guadalajara","Monterrey","San Luis Potosi","Open Pacific"),
  lon= c(-99.133,     -103.35,      -100.32,    -100.97,         -110.0),
  lat= c( 19.433,       20.667,       25.686,     22.15,           20.0)
)
cat("=== PIXEL VALUES WITH CORRECTED FORMULA ===\n")
for (i in seq_len(nrow(cities))) {
  col <- floor((cities$lon[i] + 180) / PX_RES_LON) + 1L
  row <- floor((LAT_TOP - cities$lat[i]) / PX_RES_LAT) + 1L
  val <- raster::getValues(r, row=row, nrows=1)[col]
  cat(sprintf("  %-20s row=%d col=%d  val=%.2f\n", cities$name[i], row, col, val))
}

# Extract Mexico City area hexes only (for speed)
cat("\n=== EXTRACTING MEXICO CITY AREA HEXES ===\n")
hex_cents <- st_coordinates(st_centroid(hex_sf))
mx_idx <- which(
  hex_cents[,"X"] > -99.6 & hex_cents[,"X"] < -98.9 &
  hex_cents[,"Y"] > 19.0  & hex_cents[,"Y"] < 19.7  &
  hex_sf$tipo == "Continental"
)
cat("Testing", length(mx_idx), "hexes in CDMX area\n")

mx_hex_sf <- hex_sf[mx_idx, ]
all_bb <- sf::st_bbox(mx_hex_sf)
g_col_min <- max(1L, as.integer(floor((all_bb[["xmin"]] + 180) / PX_RES_LON)))
g_col_max <- min(NCOLS_R, as.integer(ceiling((all_bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((LAT_TOP - all_bb[["ymax"]]) / PX_RES_LAT)))
g_row_max <- min(NROWS_R, as.integer(ceiling((LAT_TOP - all_bb[["ymin"]]) / PX_RES_LAT)) + 1L)
n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L
cat(sprintf("Block: rows %d-%d, cols %d-%d (%d x %d)\n", g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb))

block_vals <- raster::getValuesBlock(r, row=g_row_min, nrows=n_rb, col=g_col_min, ncols=n_cb)
block_mat  <- matrix(block_vals, nrow=n_rb, ncol=n_cb, byrow=TRUE)
block_lons <- -180 + (seq.int(g_col_min, g_col_max) - 0.5) * PX_RES_LON

for (i in seq_len(nrow(mx_hex_sf))) {
  coords <- sf::st_coordinates(sf::st_geometry(mx_hex_sf[i,])[[1]])
  if ("L1" %in% colnames(coords)) coords <- coords[coords[,"L1"]==1L,,drop=FALSE]
  x_poly <- as.numeric(coords[,"X"]); y_poly <- as.numeric(coords[,"Y"])
  bb <- sf::st_bbox(mx_hex_sf[i,])
  col_min <- max(1L, as.integer(floor((bb[["xmin"]]+180)/PX_RES_LON))+1L)
  col_max <- min(NCOLS_R, as.integer(ceiling((bb[["xmax"]]+180)/PX_RES_LON))+1L)
  row_min <- max(1L, as.integer(floor((LAT_TOP-bb[["ymax"]])/PX_RES_LAT))+1L)
  row_max <- min(NROWS_R, as.integer(ceiling((LAT_TOP-bb[["ymin"]])/PX_RES_LAT))+1L)
  bc_min <- max(1L, col_min-g_col_min+1L); bc_max <- min(n_cb, col_max-g_col_min+1L)
  br_min <- max(1L, row_min-g_row_min+1L); br_max <- min(n_rb, row_max-g_row_min+1L)
  local_cols <- seq.int(bc_min, bc_max)
  lons <- block_lons[local_cols]
  total <- 0
  for (br in seq.int(br_min, br_max)) {
    lat_c <- LAT_TOP - (g_row_min + br - 1L - 0.5) * PX_RES_LAT
    pip <- sp::point.in.polygon(lons, rep(lat_c,length(lons)), x_poly, y_poly)
    inside <- pip > 0L
    if (any(inside)) total <- total + sum(block_mat[br, local_cols][inside], na.rm=TRUE)
  }
  hid <- mx_hex_sf$hex_id[i]
  cx <- hex_cents[mx_idx[i],"X"]; cy <- hex_cents[mx_idx[i],"Y"]
  estado <- if ("estado" %in% names(mx_hex_sf)) mx_hex_sf$estado[i] else "?"
  cat(sprintf("  %-12s lon=%7.3f lat=%6.3f  %-20s  pop=%.1f\n", hid, cx, cy, estado, total))
}
