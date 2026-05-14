library(raster)

tif <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                  recursive=TRUE, full.names=TRUE)[1]
r <- raster::raster(tif)
cat("File:", tif, "\n")
cat("Dims:", nrow(r), "x", ncol(r), "\n")
cat("Extent:", paste(as.vector(raster::extent(r)), collapse=" "), "\n")

# Read column 9705 (our formula says lon=-99.13W) across all rows and find peaks
# This will show the latitude profile of population along Mexico City's longitude
col_mc <- 9705

cat("\nReading full latitude strip at col", col_mc, "(lon~-99.13W)...\n")

# Read the entire column -- raster doesn't support column-by-column easily
# Read in strips of 500 rows
row_start <- 5000; row_end <- 10000  # covers ~5N to ~42N approximately

strip_vals <- raster::getValuesBlock(r, row=row_start, nrows=(row_end-row_start+1),
                                      col=col_mc, ncols=1)
strip_vals[is.na(strip_vals)] <- 0

PX_RES_LAT <- 180/18720

# Find max value and its row
max_val <- max(strip_vals)
max_row <- which.max(strip_vals) + row_start - 1
max_lat <- 90 - (max_row - 0.5) * PX_RES_LAT
cat(sprintf("Max value along col %d: %.0f at row %d (lat=%.2fN)\n", col_mc, max_val, max_row, max_lat))

# Print top 20 non-zero values with their latitudes
top_n <- 30
top_idx <- order(strip_vals, decreasing=TRUE)[1:top_n]
cat(sprintf("\nTop %d values at col %d (lon~-99.13W):\n", top_n, col_mc))
for (i in 1:top_n) {
  g_row <- top_idx[i] + row_start - 1
  lat <- 90 - (g_row - 0.5) * PX_RES_LAT
  cat(sprintf("  row=%5d lat=%6.2fN  val=%7.0f\n", g_row, lat, strip_vals[top_idx[i]]))
}

# Also check: what is AT Mexico City's row (7340)?
mc_row <- 7340
mc_local_row <- mc_row - row_start + 1
cat(sprintf("\nValue at Mexico City row (%d, lat=%.2fN): %.0f\n", mc_row, max_lat, strip_vals[mc_local_row]))

# Show values around row 7340 ±50
cat("\nValues around row 7340 (lat=19.43N) ±50 rows:\n")
for (offset in seq(-50, 50, 10)) {
  g_row <- mc_row + offset
  if (g_row >= row_start && g_row <= row_end) {
    lat <- 90 - (g_row - 0.5) * PX_RES_LAT
    val <- strip_vals[g_row - row_start + 1]
    cat(sprintf("  row=%5d lat=%5.2fN val=%5.0f\n", g_row, lat, val))
  }
}
