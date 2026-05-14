library(terra)
library(raster)

tif_path <- "H:\\My Drive\\Data\\SSPs\\Gridded\\Pop\\SSP2\\SPP2\\SSP2_2050.tif"

r <- raster::raster(tif_path)

cat("=== RASTER DIAGNOSTICS ===\n")
cat("Dimensions: ", nrow(r), " rows x ", ncol(r), " cols\n")
cat("Extent: ", as.vector(r@extent), "\n")
cat("CRS: ", as.character(r@crs), "\n")
cat("Min value:", r@data@min, "\n")
cat("Max value:", r@data@max, "\n")

# Get some statistics
vals <- raster::values(r)
cat("Summary of all values:\n")
print(summary(vals))

cat("\nNumber of NA values:", sum(is.na(vals)), "\n")
cat("Number of non-NA values:", sum(!is.na(vals)), "\n")
cat("Number of values > 0:", sum(vals > 0, na.rm=TRUE), "\n")

# Find a location with data
idx_with_data <- which(!is.na(vals) & vals > 0)[1]
if (!is.na(idx_with_data)) {
  cat("\nFirst non-zero cell index:", idx_with_data, "\n")
  cat("Value at that index:", vals[idx_with_data], "\n")
}
