library(raster)

# Compare SSP2 2020 vs 2050 for key cities
tif_2020 <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2020.tif",
                       recursive=TRUE, full.names=TRUE)[1]
tif_2050 <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                       recursive=TRUE, full.names=TRUE)[1]

cat("2020:", tif_2020, "\n")
cat("2050:", tif_2050, "\n")

r20 <- raster::raster(tif_2020)
r50 <- raster::raster(tif_2050)

PX_RES_LON <- 360/43200; PX_RES_LAT <- 180/18720

get_val <- function(r, lon, lat) {
  col <- max(1L, min(43200L, as.integer(floor((lon+180)/PX_RES_LON))+1L))
  row <- max(1L, min(18720L, as.integer(floor((90-lat)/PX_RES_LAT))+1L))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  ifelse(is.na(v), 0, v)
}

cities <- data.frame(
  name=c("Mexico City","Guadalajara","Monterrey","Puebla","SLP","OceanPacific","Acapulco","NYC"),
  lon =c(-99.13,-103.35,-100.32,-98.20,-100.1,-99.02,-99.90,-74.00),
  lat =c(19.43,20.66,25.67,19.04,22.7,15.40,16.85,40.70)
)

cat("\n City               lon      lat   SSP2-2020  SSP2-2050\n")
cat(paste(rep("-",56),collapse=""),"\n")
for(i in 1:nrow(cities)){
  v20 <- get_val(r20, cities$lon[i], cities$lat[i])
  v50 <- get_val(r50, cities$lon[i], cities$lat[i])
  cat(sprintf(" %-18s %7.2f %6.2f  %9.0f  %9.0f\n",
      cities$name[i], cities$lon[i], cities$lat[i], v20, v50))
}

# Also: what is the 2020 max value for Mexico region?
cat("\n=== Scanning Mexico region for 2020 max (12-26N, 86-120W) ===\n")
g_col_min <- max(1L, as.integer(floor((-120+180)/PX_RES_LON)))
g_col_max <- min(43200L, as.integer(ceiling((-86+180)/PX_RES_LON))+1L)
g_row_min <- max(1L, as.integer(floor((90-26)/PX_RES_LAT)))
g_row_max <- min(18720L, as.integer(ceiling((90-12)/PX_RES_LAT))+1L)

block20 <- raster::getValuesBlock(r20, row=g_row_min, nrows=(g_row_max-g_row_min+1),
                                   col=g_col_min, ncols=(g_col_max-g_col_min+1))
block20[is.na(block20)] <- 0
top_idx <- order(block20, decreasing=TRUE)[1:10]
n_cb <- g_col_max-g_col_min+1
n_rb <- g_row_max-g_row_min+1
top_rows <- ((top_idx-1)%/%n_cb)+1; top_cols <- ((top_idx-1)%%n_cb)+1
top_lats <- 90-(g_row_min+top_rows-1-0.5)*PX_RES_LAT
top_lons <- -180+(g_col_min+top_cols-1-0.5)*PX_RES_LON
cat("Top 10 pixels in 2020 raster for Mexico region:\n")
for(i in 1:10) cat(sprintf("  val=%.0f at lat=%.3f lon=%.3f\n", block20[top_idx[i]], top_lats[i], top_lons[i]))
