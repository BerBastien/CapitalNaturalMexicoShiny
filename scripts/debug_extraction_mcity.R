library(sf)
library(raster)
library(sp)

# Load data
hex_gpkg <- "h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg"
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
tif_path <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                       recursive=TRUE, full.names=TRUE)[1]
r <- raster::raster(tif_path)

# Find hex containing Mexico City
mc_pt <- st_sfc(st_point(c(-99.13, 19.43)), crs = 4326)
mc_hex <- hex_sf[st_contains(hex_sf, mc_pt, sparse = FALSE)[,1], ]
mc_hex_id <- mc_hex$hex_id[1]
cat("Mexico City hex:", mc_hex_id, "\n")
cat("Tipo:", mc_hex$tipo[1], "\n")

# Constants from extraction script
PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 180 / 18720
NCOLS_R <- 43200L
NROWS_R <- 18720L

# Get hex bounding box
bb <- sf::st_bbox(mc_hex)
cat("Hex bbox: xmin=", bb$xmin, "xmax=", bb$xmax, "ymin=", bb$ymin, "ymax=", bb$ymax, "\n")

# Compute global block indices (matching extraction code)
g_col_min <- max(1L, as.integer(floor((bb[["xmin"]] + 180) / PX_RES_LON)))
g_col_max <- min(NCOLS_R, as.integer(ceiling((bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((90 - bb[["ymax"]]) / PX_RES_LAT)))
g_row_max <- min(NROWS_R, as.integer(ceiling((90 - bb[["ymin"]]) / PX_RES_LAT)) + 1L)

cat("\nBlock indices: rows", g_row_min, "-", g_row_max, ", cols", g_col_min, "-", g_col_max, "\n")

n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L
cat("Block size:", n_rb, "x", n_cb, "\n")

# Read block (exact copy of extraction code)
block_vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb, 
                                      col = g_col_min, ncols = n_cb)
block_mat <- matrix(block_vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
block_mat[is.na(block_mat)] <- 0

cat("\nBlock statistics:\n")
cat("  Min:", min(block_mat), "\n")
cat("  Max:", max(block_mat), "\n")
cat("  Sum:", sum(block_mat), "\n")
cat("  Nonzero pixels:", sum(block_mat > 0), "\n")

# Compute pixel center coordinates (exact copy of extraction code)
block_lons <- -180 + (seq.int(g_col_min, g_col_max) - 0.5) * PX_RES_LON

# Get hex polygon coordinates
hex_coords <- sf::st_coordinates(sf::st_geometry(mc_hex)[[1]])
if ("L1" %in% colnames(hex_coords))
  hex_coords <- hex_coords[hex_coords[, "L1"] == 1L, , drop = FALSE]
x_poly <- as.numeric(hex_coords[, "X"])
y_poly <- as.numeric(hex_coords[, "Y"])

cat("\nHex polygon vertices:", nrow(hex_coords), "points\n")
cat("X range:", min(x_poly), "to", max(x_poly), "\n")
cat("Y range:", min(y_poly), "to", max(y_poly), "\n")

# Now manually sum using PIP test (exact copy of extraction logic)
total <- 0
pixels_inside <- 0

for (br in seq.int(1L, n_rb)) {
  lat_c <- 90 - (g_row_min + br - 1L - 0.5) * PX_RES_LAT
  
  # Test each column in this row
  for (bc in seq.int(1L, n_cb)) {
    lon_c <- -180 + (g_col_min + bc - 1L - 0.5) * PX_RES_LON
    
    pip <- sp::point.in.polygon(lon_c, lat_c, x_poly, y_poly)
    if (pip > 0) {
      val <- block_mat[br, bc]
      total <- total + val
      if (val > 0) pixels_inside <- pixels_inside + 1
      if (pixels_inside <= 10) {
        cat(sprintf("    Inside: row=%d col=%d (global %d, %d) lat=%.3f lon=%.3f val=%.0f\n",
          br, bc, g_row_min+br-1, g_col_min+bc-1, lat_c, lon_c, val))
      }
    }
  }
}

cat("\nExtraction result:\n")
cat("  Pixels inside hex:", pixels_inside, "\n")
cat("  Total population:", total, "\n")

# Compare with direct terra polygon extraction
cat("\n=== Comparison with terra::extract ===\n")
library(terra)
r_terra <- terra::rast(tif_path)
terra::ext(r_terra) <- terra::ext(-180, 180, -90, 90)
terra::crs(r_terra) <- "EPSG:4326"

hex_vect <- terra::vect(mc_hex)
terra_sum <- terra::extract(r_terra, hex_vect, fun=sum)[1,2]
cat("terra polygon sum:", terra_sum, "\n")
cat("Extraction algorithm sum:", total, "\n")
cat("Match:", abs(terra_sum - total) < 1, "\n")
