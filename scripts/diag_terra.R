library(terra)

tif <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                  recursive=TRUE, full.names=TRUE)[1]

# Load raster and OVERRIDE its broken pixel-coordinate extent with the true geographic extent
r <- terra::rast(tif)
cat("Original extent:", as.character(terra::ext(r)), "\n")
cat("Dims:", terra::nrow(r), "x", terra::ncol(r), "\n")

# Set the CORRECT geographic extent (global, WGS84)
terra::ext(r) <- terra::ext(-180, 180, -90, 90)
terra::crs(r) <- "EPSG:4326"
cat("Fixed extent:", as.character(terra::ext(r)), "\n")

# Now extract geographically at known cities
pts <- data.frame(
  name = c("Mexico City", "Guadalajara", "Monterrey", "Puebla", 
           "Tijuana", "Acapulco", "NYC", "Tokyo",
           "OceanPacific15N", "SanLuisPotosi"),
  lon  = c(-99.13, -103.35, -100.32, -98.20, 
           -117.00, -99.90, -74.00, 139.69,
           -99.02, -100.1),
  lat  = c(19.43, 20.66, 25.67, 19.04, 
           32.52, 16.85, 40.70, 35.69,
           15.40, 22.7)
)

pts_vect <- terra::vect(pts, geom=c("lon","lat"), crs="EPSG:4326")
vals <- terra::extract(r, pts_vect)

cat("\n=== Geographic extractions with corrected CRS ===\n")
for (i in 1:nrow(pts)) {
  cat(sprintf("  %-20s lon=%7.2f lat=%5.2f -> val=%.0f\n",
    pts$name[i], pts$lon[i], pts$lat[i], 
    ifelse(is.na(vals[i,2]), 0, vals[i,2])))
}

# Also: what's the max and where is it globally?
cat("\nRaster global max:", terra::minmax(r)[2], "\n")

# Sample the Mexico region (14-25N, 86-120W) at 0.5 degree resolution to find peaks
lons_grid <- seq(-120, -86, 0.5)
lats_grid <- seq(14, 25, 0.5)
grid_pts <- expand.grid(lon=lons_grid, lat=lats_grid)
grid_vect <- terra::vect(grid_pts, geom=c("lon","lat"), crs="EPSG:4326")
grid_vals <- terra::extract(r, grid_vect)

grid_pts$val <- ifelse(is.na(grid_vals[,2]), 0, grid_vals[,2])
top_grid <- grid_pts[order(-grid_pts$val),][1:15,]
cat("\nTop 15 grid points in Mexico region (0.5 deg grid):\n")
print(top_grid)
