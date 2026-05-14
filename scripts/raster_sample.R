library(raster)

tif_path <- "H:\\My Drive\\Data\\SSPs\\Gridded\\Pop\\SSP2\\SPP2\\SSP2_2050.tif"
r <- raster::raster(tif_path)

cat("Raster info:\n")
cat("Dims: ", nrow(r), " x ", ncol(r), "\n")
cat("Extent (pixel coords): ", as.vector(r@extent), "\n")
cat("Min/Max: ", r@data@min, "/", r@data@max, "\n\n")

# Sample a single row to see if we can get values
cat("Sampling row 100...\n")
row_vals <- raster::getValues(r, row = 100, nrows = 1)
cat("Row 100 length:", length(row_vals), "\n")
cat("Row 100 non-NA count:", sum(!is.na(row_vals)), "\n")
cat("Row 100 values > 0:", sum(row_vals > 0, na.rm=TRUE), "\n")
cat("Row 100 sample (cols 1-20):", paste(row_vals[1:20], collapse=", "), "\n")

# Try accessing a specific cell
cat("\nDirect access test:\n")
cat("r[100, 100] =", r[100, 100], "\n")
cat("r[5000, 5000] =", r[5000, 5000], "\n")

# Try a different approach: use cell number
cat("\nCell number approach:\n")
cell_num <- 100 * ncol(r) + 100
val_by_cell <- raster::getValues(r, cell = cell_num, buffer = 0)
cat("getValues(cell=", cell_num, ") =", val_by_cell, "\n")
