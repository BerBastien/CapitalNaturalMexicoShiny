suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(sp)
})

tif_path <- "H:/My Drive/Data/SSPs/Gridded/Pop/SSP2/SPP2/SSP2_2020.tif"
hex_gpkg  <- "H:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg"

r <- raster::raster(tif_path)

# The raster has broken georef - extent is pixel indices, not degrees.
# We assume:
#   Row 1   = top row    = 90 N
#   Col 1   = left col   = -180 W
#   nrow = 18720,  nLAT per row  = ?
#   ncol = 43200,  nLON per col  = ?

NCOLS_R <- raster::ncol(r)   # 43200
NROWS_R <- raster::nrow(r)   # 18720

# Two possible interpretations:
# A) 43200 cols × 360° = 0.008333°/col,  18720 rows × 180° = 0.009615°/row (non-square)
# B) 43200 cols × 360° = 0.008333°/col,  21600 rows × 180° = 0.008333°/row (square) -- but nrow=18720, not 21600

PX_RES_LON <- 360 / NCOLS_R   # 0.008333
PX_RES_LAT <- 180 / NROWS_R   # 0.009615

cat("NCOLS:", NCOLS_R, "  NROWS:", NROWS_R, "\n")
cat("PX_RES_LON:", PX_RES_LON, "  PX_RES_LAT:", PX_RES_LAT, "\n\n")

# --- Test: extract single pixels at known cities ---
cities <- data.frame(
  name = c("Mexico City", "Guadalajara", "Monterrey", "Acapulco"),
  lon  = c(-99.133,      -103.350,      -100.316,   -99.821),
  lat  = c( 19.433,        20.667,        25.686,    16.860)
)

cat("=== SINGLE PIXEL VALUES AT KNOWN CITIES ===\n")
for (i in seq_len(nrow(cities))) {
  lon <- cities$lon[i]; lat <- cities$lat[i]
  col <- floor((lon + 180) / PX_RES_LON) + 1L
  row <- floor((90 - lat) / PX_RES_LAT) + 1L
  val <- raster::getValues(r, row = row, nrows = 1)[col]
  # Also pixel center
  lat_c <- 90 - (row - 0.5) * PX_RES_LAT
  lon_c <- -180 + (col - 0.5) * PX_RES_LON
  cat(sprintf("  %-15s lon=%.3f lat=%.3f -> row=%d col=%d | center=(%.4f,%.4f) | val=%.2f\n",
              cities$name[i], lon, lat, row, col, lon_c, lat_c, val))
}
cat("\n")

# --- Also test: 5x5 patch around Mexico City ---
cat("=== 5x5 PIXEL PATCH AROUND MEXICO CITY ===\n")
cx_r <- floor((90 - 19.433) / PX_RES_LAT) + 1L
cx_c <- floor((-99.133 + 180) / PX_RES_LON) + 1L
patch <- raster::getValuesBlock(r, row = cx_r - 2L, nrows = 5L, col = cx_c - 2L, ncols = 5L)
mat <- matrix(patch, nrow = 5, ncol = 5, byrow = TRUE)
cat("Row range:", cx_r-2, "to", cx_r+2, "  Col range:", cx_c-2, "to", cx_c+2, "\n")
print(round(mat, 1))
cat("\n")

# --- Compare: hex extraction for Mexico City hex ---
cat("=== HEX-LEVEL EXTRACTION FOR MEXICO CITY HEX ===\n")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)

mx_pt  <- st_sfc(st_point(c(-99.133, 19.433)), crs = 4326)
idx    <- which(as.logical(st_intersects(mx_pt, hex_sf)))
if (length(idx) == 0) {
  # try larger search
  dists <- st_distance(mx_pt, st_centroid(hex_sf))
  idx   <- which.min(dists)
}
mx_hex <- hex_sf[idx, ]
cat("Hex:", mx_hex$hex_id, "  tipo:", mx_hex$tipo, "\n")

bb <- st_bbox(mx_hex)
cat("Hex bbox: xmin=", bb["xmin"], "xmax=", bb["xmax"], "ymin=", bb["ymin"], "ymax=", bb["ymax"], "\n")

# pixel range in hex
g_col_min <- max(1L, as.integer(floor((bb[["xmin"]] + 180) / PX_RES_LON)))
g_col_max <- min(NCOLS_R, as.integer(ceiling((bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((90 - bb[["ymax"]]) / PX_RES_LAT)))
g_row_max <- min(NROWS_R, as.integer(ceiling((90 - bb[["ymin"]]) / PX_RES_LAT)) + 1L)

n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L
cat("Pixel block: rows", g_row_min, "-", g_row_max, "  cols", g_col_min, "-", g_col_max, "\n")
cat("Block size:", n_rb, "x", n_cb, "\n")

block_vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb, col = g_col_min, ncols = n_cb)
block_mat  <- matrix(block_vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
cat("Block non-NA count:", sum(!is.na(block_mat)), "\n")
cat("Block non-zero count:", sum(block_mat > 0, na.rm = TRUE), "\n")
cat("Block max:", max(block_mat, na.rm = TRUE), "\n")
cat("Block min (nonzero):", min(block_mat[block_mat > 0], na.rm = TRUE), "\n\n")

# compute lon/lat centers for each pixel in block
block_lons <- -180 + (seq.int(g_col_min, g_col_max) - 0.5) * PX_RES_LON
block_lats <- 90 - (seq.int(g_row_min, g_row_max) - 0.5) * PX_RES_LAT

cat("Block lon range:", range(block_lons), "\n")
cat("Block lat range:", range(block_lats), "\n\n")

# get hex polygon coords
coords <- sf::st_coordinates(sf::st_geometry(mx_hex)[[1]])
x_poly <- as.numeric(coords[, "X"])
y_poly <- as.numeric(coords[, "Y"])

# test all pixels in block
total <- 0
n_inside <- 0
for (r_idx in seq_len(n_rb)) {
  lat_c  <- block_lats[r_idx]
  pip    <- sp::point.in.polygon(block_lons, rep(lat_c, n_cb), x_poly, y_poly)
  inside <- pip > 0
  if (any(inside)) {
    n_inside <- n_inside + sum(inside)
    total    <- total + sum(block_mat[r_idx, inside], na.rm = TRUE)
  }
}
cat("Pixels inside hex:", n_inside, "\n")
cat("Total population in hex:", round(total, 1), "\n")
