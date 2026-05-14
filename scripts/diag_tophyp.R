library(raster)
library(terra)

tif <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                  recursive=TRUE, full.names=TRUE)[1]

cat("=== Testing with terra (ignores broken CRS, uses geographic extraction) ===\n")
# terra can extract by geographic point even with a broken CRS — 
# but first let's check if terra reads the file differently

r_terra <- terra::rast(tif)
cat("terra extent:", as.character(terra::ext(r_terra)), "\n")
cat("terra nrow:", terra::nrow(r_terra), "ncol:", terra::ncol(r_terra), "\n")
cat("terra crs:", terra::crs(r_terra, proj=TRUE), "\n")

# Now: extract at known points using terra::extract (cell index, not geographic)
# Use terra::cellFromXY to get cell from raster's native (pixel) coordinates
# The raster is in pixel coords: x from -0.49 to 43200, y from 0 to -18720

# In pixel space, what coordinates correspond to NYC and Mexico City?
# If raster starts at 90N, -180W and resolution is 180/18720 in lat, 360/43200 in lon:
# Mexico City: pixel_x = (lon+180)/(360/43200) - 0.5 = (99.13-(-180))/0.008333 - 0.5 = 80.87/0.008333 - 0.5 = 9704 - 0.5 = 9703.5... 
# Hmm this is getting confusing. Let me just test row values.

PX_RES_LAT <- 180/18720
PX_RES_LON <- 360/43200

# Test at various "top of raster" assumptions:
# What value is at row 6924, col 9705? (Mexico City with 86N start)
r_raster <- raster::raster(tif)

cat("\n=== Values at candidate Mexico City rows (col=9705, lon~-99.13W) ===\n")
col_mc <- 9705L
# Read rows 5000-9000 at once
block <- raster::getValuesBlock(r_raster, row=5000, nrows=4000, col=col_mc, ncols=1)
block[is.na(block)] <- 0

# Print values at specific rows corresponding to Mexico City under different top-of-raster assumptions
test_tops <- c(90, 88, 86, 84, 80)
cat("\nRow and value at Mexico City latitude (19.43N) for different raster top latitudes:\n")
for (top in test_tops) {
  mc_row <- as.integer(floor((top - 19.43) / PX_RES_LAT)) + 1L
  block_idx <- mc_row - 5000 + 1
  if (block_idx >= 1 && block_idx <= length(block)) {
    cat(sprintf("  Top=%d: row=%d -> val=%.0f\n", top, mc_row, block[block_idx]))
  }
}

# Also test NYC under different assumptions
block2 <- raster::getValuesBlock(r_raster, row=3000, nrows=4000, col=12721, ncols=1)
block2[is.na(block2)] <- 0
cat("\nRow and value at NYC latitude (40.7N) for different raster top latitudes:\n")
for (top in test_tops) {
  nyc_row <- as.integer(floor((top - 40.7) / PX_RES_LAT)) + 1L
  block_idx <- nyc_row - 3000 + 1
  if (block_idx >= 1 && block_idx <= length(block2)) {
    cat(sprintf("  Top=%d: row=%d -> val=%.0f\n", top, nyc_row, block2[block_idx]))
  }
}

# And Tokyo (35.69N, 139.69E)
col_tokyo <- as.integer(floor((139.69 + 180) / PX_RES_LON)) + 1L
block3 <- raster::getValuesBlock(r_raster, row=3000, nrows=4000, col=col_tokyo, ncols=1)
block3[is.na(block3)] <- 0
cat("\nRow and value at Tokyo latitude (35.69N) for different raster top latitudes:\n")
for (top in test_tops) {
  tokyo_row <- as.integer(floor((top - 35.69) / PX_RES_LAT)) + 1L
  block_idx <- tokyo_row - 3000 + 1
  if (block_idx >= 1 && block_idx <= length(block3)) {
    cat(sprintf("  Top=%d: row=%d -> val=%.0f\n", top, tokyo_row, block3[block_idx]))
  }
}

# What IS at row 5128, col 12721 (our formula's NYC)?
nyc_val_formula <- raster::getValues(r_raster, row=5128, nrows=1)[12721]
cat(sprintf("\nDirect: row=5128 col=12721 (our formula's NYC) = %.0f\n", ifelse(is.na(nyc_val_formula),0,nyc_val_formula)))

# What IS at row 5128-416, col 12721? (86N formula's NYC)
nyc_val_86 <- raster::getValues(r_raster, row=5128-416, nrows=1)[12721]
cat(sprintf("Direct: row=%d col=12721 (86N formula's NYC) = %.0f\n", 5128-416, ifelse(is.na(nyc_val_86),0,nyc_val_86)))
