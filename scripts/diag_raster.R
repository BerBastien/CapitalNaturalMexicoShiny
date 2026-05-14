library(raster)
tif_path <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif", recursive=TRUE, full.names=TRUE)[1]
cat("File:", tif_path, "\n")
r <- raster::raster(tif_path)
cat("Extent xmin/xmax/ymin/ymax:", r@extent@xmin, r@extent@xmax, r@extent@ymin, r@extent@ymax, "\n")
cat("Dims (nrow x ncol):", nrow(r), "x", ncol(r), "\n")
cat("CRS:", as.character(crs(r)), "\n")
cat("Min/Max values:", minValue(r), "/", maxValue(r), "\n\n")

px_lon <- 360/43200
px_lat <- 180/18720

test_pts <- list(
  list(name="Mexico City", lon=-99.13, lat=19.43),
  list(name="Tokyo",       lon=139.69, lat=35.69),
  list(name="Pacific",     lon=-170.0, lat=0.0),
  list(name="Sahara",      lon=20.0,   lat=23.0),
  list(name="NYC",         lon=-74.0,  lat=40.7)
)

cat("--- Formula A: row=floor((90-lat)/px_lat)+1  [top=north] ---\n")
for (pt in test_pts) {
  col <- max(1L, min(43200L, as.integer(floor((pt$lon + 180) / px_lon)) + 1L))
  row <- max(1L, min(18720L, as.integer(floor((90 - pt$lat)  / px_lat)) + 1L))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  cat(sprintf("  %-15s col=%5d row=%5d val=%.0f\n", pt$name, col, row, ifelse(is.na(v), 0, v)))
}

cat("\n--- Formula B: row=floor((lat+90)/px_lat)+1  [bottom=south, flipped] ---\n")
for (pt in test_pts) {
  col <- max(1L, min(43200L, as.integer(floor((pt$lon + 180) / px_lon)) + 1L))
  row <- max(1L, min(18720L, as.integer(floor((pt$lat + 90)  / px_lat)) + 1L))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  cat(sprintf("  %-15s col=%5d row=%5d val=%.0f\n", pt$name, col, row, ifelse(is.na(v), 0, v)))
}

cat("\n--- Formula C: row=nrows - floor((90-lat)/px_lat)  [mirror row A] ---\n")
for (pt in test_pts) {
  col <- max(1L, min(43200L, as.integer(floor((pt$lon + 180) / px_lon)) + 1L))
  row <- max(1L, min(18720L, 18720L - as.integer(floor((90 - pt$lat) / px_lat))))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  cat(sprintf("  %-15s col=%5d row=%5d val=%.0f\n", pt$name, col, row, ifelse(is.na(v), 0, v)))
}

cat("\n--- Formula D: col = ncols - floor((lon+180)/px_lon) [mirror col, lon reversed] ---\n")
for (pt in test_pts) {
  col <- max(1L, min(43200L, 43200L - as.integer(floor((pt$lon + 180) / px_lon))))
  row <- max(1L, min(18720L, as.integer(floor((90 - pt$lat)  / px_lat)) + 1L))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  cat(sprintf("  %-15s col=%5d row=%5d val=%.0f\n", pt$name, col, row, ifelse(is.na(v), 0, v)))
}

# Also: check what values appear at exact pixel coords 1,1 and center of raster
cat("\n--- Corner/center pixel values ---\n")
v_topleft  <- raster::getValues(r, row=1,     nrows=1)[1]
v_topright <- raster::getValues(r, row=1,     nrows=1)[43200]
v_botleft  <- raster::getValues(r, row=18720, nrows=1)[1]
v_center   <- raster::getValues(r, row=9360,  nrows=1)[21600]
cat(sprintf("  Top-left    (row=1,    col=1):     %.0f\n", ifelse(is.na(v_topleft),  0, v_topleft)))
cat(sprintf("  Top-right   (row=1,    col=43200): %.0f\n", ifelse(is.na(v_topright), 0, v_topright)))
cat(sprintf("  Bottom-left (row=18720,col=1):     %.0f\n", ifelse(is.na(v_botleft),  0, v_botleft)))
cat(sprintf("  Center      (row=9360, col=21600): %.0f\n", ifelse(is.na(v_center),   0, v_center)))

# Sample a 20-pixel strip around Mexico City's expected row in formula A
cat("\n--- Row strip around Mexico City (Formula A row=7332, cols 8000-8020) ---\n")
mex_row_A <- max(1L, min(18720L, as.integer(floor((90 - 19.43) / px_lat)) + 1L))
mex_col_A <- max(1L, min(43200L, as.integer(floor((-99.13 + 180) / px_lon)) + 1L))
cat(sprintf("  Expected row=%d col=%d\n", mex_row_A, mex_col_A))
strip <- raster::getValues(r, row=mex_row_A, nrows=1)[(mex_col_A-5):(mex_col_A+5)]
cat("  Values +/-5 cols:", paste(round(ifelse(is.na(strip),0,strip)), collapse=" "), "\n")

# Also try the B formula row
mex_row_B <- max(1L, min(18720L, as.integer(floor((19.43 + 90) / px_lat)) + 1L))
cat(sprintf("  Formula B row=%d\n", mex_row_B))
strip2 <- raster::getValues(r, row=mex_row_B, nrows=1)[(mex_col_A-5):(mex_col_A+5)]
cat("  Values +/-5 cols:", paste(round(ifelse(is.na(strip2),0,strip2)), collapse=" "), "\n")
