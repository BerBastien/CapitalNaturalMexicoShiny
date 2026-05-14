library(raster)
library(sf)

tif <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                  recursive=TRUE, full.names=TRUE)[1]
r <- raster::raster(tif)

# Read a strip around known high-density areas
# Sample a wide latitude band (-105 to -87W, 12 to 25N) to see where population is
PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 180 / 18720

# Compute block for broader Mexico region
lon1 <- -107; lon2 <- -86; lat1 <- 12; lat2 <- 26

g_col_min <- max(1L, as.integer(floor((lon1 + 180) / PX_RES_LON)) + 1L)
g_col_max <- min(43200L, as.integer(ceiling((lon2 + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((90 - lat2) / PX_RES_LAT)) + 1L)
g_row_max <- min(18720L, as.integer(ceiling((90 - lat1) / PX_RES_LAT)) + 1L)

n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L
cat(sprintf("Block: rows %d-%d, cols %d-%d (%d x %d)\n", g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb))

block_vals <- raster::getValuesBlock(r, row=g_row_min, nrows=n_rb, col=g_col_min, ncols=n_cb)
block_mat <- matrix(block_vals, nrow=n_rb, ncol=n_cb, byrow=TRUE)
block_mat[is.na(block_mat)] <- 0

# Find top 20 pixels by value
top_idx <- order(block_vals, na.last=TRUE, decreasing=TRUE)[1:20]
top_vals <- block_vals[top_idx]
# Convert flat index to row/col in block
top_rows <- ((top_idx - 1) %/% n_cb) + 1
top_cols <- ((top_idx - 1) %% n_cb) + 1
# Convert to geographic
top_lats <- 90 - (g_row_min + top_rows - 1 - 0.5) * PX_RES_LAT
top_lons <- -180 + (g_col_min + top_cols - 1 - 0.5) * PX_RES_LON

cat("\n=== Top 20 pixels by value in the Mexico region (12-26N, 107-86W) ===\n")
for (i in 1:20) {
  cat(sprintf("  value=%7.0f  lat=%.3f  lon=%.3f\n", top_vals[i], top_lats[i], top_lons[i]))
}

# Also: what's the value at specific Mexican cities?
cities <- list(
  list(name="Mexico City",      lon=-99.13, lat=19.43),
  list(name="Guadalajara",      lon=-103.35, lat=20.66),
  list(name="Monterrey",        lon=-100.32, lat=25.67),
  list(name="Puebla",           lon=-98.20,  lat=19.04),
  list(name="Tijuana",          lon=-117.00, lat=32.52),
  list(name="Acapulco",         lon=-99.90,  lat=16.85),
  list(name="OceanPacific1",    lon=-99.02,  lat=15.40),  # top ocean hex centroid
  list(name="OceanPacific2",    lon=-99.12,  lat=15.40)
)

cat("\n=== Values at major Mexican cities ===\n")
for (city in cities) {
  col <- max(1L, min(43200L, as.integer(floor((city$lon + 180) / PX_RES_LON)) + 1L))
  row <- max(1L, min(18720L, as.integer(floor((90 - city$lat) / PX_RES_LAT)) + 1L))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  cat(sprintf("  %-20s lon=%7.2f lat=%.2f -> row=%5d col=%5d val=%.0f\n",
              city$name, city$lon, city$lat, row, col, ifelse(is.na(v),0,v)))
}
