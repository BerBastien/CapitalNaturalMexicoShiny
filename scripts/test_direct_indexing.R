library(terra)
library(raster)

# Test direct indexing approach
tif_path <- "H:\\\\My Drive\\\\Data\\\\SSPs\\\\Gridded\\\\Pop\\\\SSP2\\\\SPP2\\\\SSP2_2050.tif"

cat("Testing indexing methods on:", tif_path, "\n\n")

# Load with raster package
r_ras <- raster::raster(tif_path)

# Try direct indexing
message("Raster dimensions: ", nrow(r_ras), " x ", ncol(r_ras))
message("Raster extent: ", r_ras@extent)

# Test 1: Try bracket indexing
val1 <- tryCatch({
  r_ras[100, 100]
}, error = function(e) {
  message("ERROR with [100, 100]: ", conditionMessage(e))
  NA
})
message("r_ras[100, 100] = ", val1)

# Test 2: Try extracting specific cells
val2 <- tryCatch({
  raster::extract(r_ras, data.frame(x = 100, y = 100))
}, error = function(e) {
  message("ERROR with extract(100,100): ", conditionMessage(e))
  NA
})
message("raster::extract(100,100) = ", val2)

# Test 3: Get values from raster
vals_sample <- tryCatch({
  raster::getValues(r_ras, row = 100, nrows = 1)[1:10]
}, error = function(e) {
  message("ERROR with getValues: ", conditionMessage(e))
  NA
})
message("getValues sample: ", paste(vals_sample, collapse = ", "))

# Test 4: Try with terra
r_terra <- terra::rast(tif_path)
message("\nTerra raster dims: ", nrow(r_terra), " x ", ncol(r_terra))
message("Terra extent: ", terra::ext(r_terra))

# Try terra bracket
val_terra <- tryCatch({
  r_terra[100, 100]
}, error = function(e) {
  message("ERROR with terra[100,100]: ", conditionMessage(e))
  NA
})
message("terra[100, 100] = ", val_terra)

# Try terra extract
val_terra_ext <- tryCatch({
  terra::extract(r_terra, cbind(100, 100))[1]
}, error = function(e) {
  message("ERROR with terra extract: ", conditionMessage(e))
  NA
})
message("terra extract(100, 100) = ", val_terra_ext)
