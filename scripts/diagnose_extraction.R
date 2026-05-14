suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(raster)
  library(sp)
})

project_dir <- "H:/My Drive/CapitalNaturalMexicoShiny"
hex_gpkg   <- file.path(project_dir, "data", "master_hex_10km.gpkg")
pop_root   <- "H:/My Drive/Data/SSPs/Gridded/Pop"

# Use SSP2 2020 as test case
tif_path <- file.path(pop_root, "SSP2", "SPP2", "SSP2_2020.tif")
cat("TIF:", tif_path, "\n")
cat("Exists:", file.exists(tif_path), "\n\n")

# ---- 1. Actual raster metadata ----
cat("=== RASTER METADATA ===\n")
r_terra <- terra::rast(tif_path)
cat("terra dimensions (nrow, ncol, nlyr):", dim(r_terra), "\n")
cat("terra resolution (x, y):", terra::res(r_terra), "\n")
cat("terra extent:", as.character(terra::ext(r_terra)), "\n")
cat("terra CRS:", terra::crs(r_terra, proj=TRUE), "\n\n")

r_raster <- raster::raster(tif_path)
cat("raster dimensions:", raster::nrow(r_raster), "x", raster::ncol(r_raster), "\n")
cat("raster resolution:", raster::res(r_raster), "\n")
cat("raster extent:", as.character(raster::extent(r_raster)), "\n\n")

# ---- 2. What the code ASSUMES ----
cat("=== CODE ASSUMPTIONS ===\n")
PX_RES_LON_assumed <- 360 / 43200
PX_RES_LAT_assumed <- 180 / 18720
NCOLS_assumed <- 43200L
NROWS_assumed <- 18720L
cat("Assumed NCOLS:", NCOLS_assumed, "  PX_RES_LON:", PX_RES_LON_assumed, "\n")
cat("Assumed NROWS:", NROWS_assumed, "  PX_RES_LAT:", PX_RES_LAT_assumed, "\n\n")

# ---- 3. ACTUAL values from raster ----
actual_ncols <- raster::ncol(r_raster)
actual_nrows <- raster::nrow(r_raster)
actual_res   <- raster::res(r_raster)
actual_ext   <- raster::extent(r_raster)
PX_RES_LON_actual <- actual_res[1]
PX_RES_LAT_actual <- actual_res[2]

cat("=== DISCREPANCIES ===\n")
cat("NCOLS match:", actual_ncols == NCOLS_assumed, "(actual:", actual_ncols, "assumed:", NCOLS_assumed, ")\n")
cat("NROWS match:", actual_nrows == NROWS_assumed, "(actual:", actual_nrows, "assumed:", NROWS_assumed, ")\n")
cat("PX_RES_LON match:", isTRUE(all.equal(PX_RES_LON_actual, PX_RES_LON_assumed, tol=1e-7)),
    "(actual:", PX_RES_LON_actual, "assumed:", PX_RES_LON_assumed, ")\n")
cat("PX_RES_LAT match:", isTRUE(all.equal(PX_RES_LAT_actual, PX_RES_LAT_assumed, tol=1e-7)),
    "(actual:", PX_RES_LAT_actual, "assumed:", PX_RES_LAT_assumed, ")\n\n")

# ---- 4. Ground-truth extraction at known cities via terra ----
cat("=== TERRA EXTRACTION AT KNOWN CITIES ===\n")
cities <- data.frame(
  name = c("Mexico City", "Guadalajara", "Monterrey", "Tijuana", "Merida"),
  lon  = c(-99.133,      -103.350,     -100.316,   -117.003, -89.623),
  lat  = c( 19.433,        20.667,       25.686,     32.514,  20.967)
)
pts <- terra::vect(cities, geom = c("lon", "lat"), crs = "EPSG:4326")
vals <- terra::extract(r_terra, pts)
cities$terra_val <- vals[, 2]
print(cities)
cat("\n")

# ---- 5. Manual block extraction at Mexico City ----
cat("=== MANUAL BLOCK EXTRACTION AT MEXICO CITY (using ACTUAL raster dims) ===\n")
mx_lon <- -99.133; mx_lat <- 19.433

# Using ACTUAL raster properties
ext_xmin <- actual_ext@xmin
ext_ymax <- actual_ext@ymax

col_actual <- floor((mx_lon - ext_xmin) / PX_RES_LON_actual) + 1L
row_actual <- floor((ext_ymax - mx_lat) / PX_RES_LAT_actual) + 1L
cat("Using actual raster properties:\n")
cat("  col:", col_actual, "  row:", row_actual, "\n")

# Read single pixel
v_actual <- raster::getValues(r_raster, row = row_actual, nrows = 1)[col_actual]
cat("  Value at Mexico City (actual formula):", v_actual, "\n\n")

# Using ASSUMED raster properties (code formula)
col_assumed <- floor((mx_lon + 180) / PX_RES_LON_assumed) + 1L
row_assumed <- floor((90 - mx_lat) / PX_RES_LAT_assumed) + 1L
cat("Using ASSUMED raster properties (code formula):\n")
cat("  col:", col_assumed, "  row:", row_assumed, "\n")

v_assumed <- tryCatch(
  raster::getValues(r_raster, row = row_assumed, nrows = 1)[col_assumed],
  error = function(e) paste("ERROR:", e$message)
)
cat("  Value at Mexico City (assumed formula):", v_assumed, "\n\n")

# ---- 6. Check hex for Mexico City ----
cat("=== HEX CONTAINING MEXICO CITY ===\n")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
mx_pt  <- st_sfc(st_point(c(mx_lon, mx_lat)), crs = 4326)
mx_hex_idx <- which(st_contains(hex_sf, mx_pt, sparse = FALSE))
if (length(mx_hex_idx) > 0) {
  mx_hex <- hex_sf[mx_hex_idx, ]
  cat("Found hex:", mx_hex$hex_id, "  tipo:", mx_hex$tipo, "\n")
  cat("Hex bbox:", st_bbox(mx_hex), "\n")
  
  # Count pixels in hex using actual raster res
  bb <- st_bbox(mx_hex)
  col_min <- floor((bb[["xmin"]] - ext_xmin) / PX_RES_LON_actual) + 1L
  col_max <- floor((bb[["xmax"]] - ext_xmin) / PX_RES_LON_actual) + 1L
  row_min <- floor((ext_ymax - bb[["ymax"]]) / PX_RES_LAT_actual) + 1L
  row_max <- floor((ext_ymax - bb[["ymin"]]) / PX_RES_LAT_actual) + 1L
  cat("Pixel range in hex (actual res): rows", row_min, "-", row_max, ", cols", col_min, "-", col_max, "\n")
  cat("Block size:", row_max - row_min + 1, "rows x", col_max - col_min + 1, "cols\n")
} else {
  cat("No hex found containing Mexico City coords\n")
}
