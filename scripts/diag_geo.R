library(sf)
library(raster)
library(readxl)
library(dplyr)

# Load data
hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg",
               layer = "hex_10km", quiet = TRUE)

# Direct raster spot-check at known coordinates 
tif <- list.files("H:/My Drive/Data/SSPs/Gridded/Pop", pattern="SSP2_2050.tif",
                  recursive=TRUE, full.names=TRUE)[1]
cat("TIF:", tif, "\n")
r <- raster::raster(tif)

PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 180 / 18720

get_val <- function(lon, lat) {
  col <- max(1L, min(43200L, as.integer(floor((lon + 180) / PX_RES_LON)) + 1L))
  row <- max(1L, min(18720L, as.integer(floor((90 - lat) / PX_RES_LAT)) + 1L))
  v <- raster::getValues(r, row=row, nrows=1)[col]
  list(col=col, row=row, val=v)
}

# 1. What value is at the centroid of top ocean hex?
cat("\n=== Direct raster lookups ===\n")
h1 <- get_val(-99.025, 15.399)  # HEX0041123 centroid (ocean hex with 1.1M pop)
cat(sprintf("HEX0041123 centroid (-99.025, 15.399): row=%d col=%d val=%.0f\n", h1$row, h1$col, ifelse(is.na(h1$val), 0, h1$val)))

mc <- get_val(-99.13, 19.43)  # Mexico City
cat(sprintf("Mexico City (-99.13, 19.43): row=%d col=%d val=%.0f\n", mc$row, mc$col, ifelse(is.na(mc$val), 0, mc$val)))

# 2. Look at the actual geometry of the top ocean hex
cat("\n=== Geometry of HEX0041123 ===\n")
h_geom <- hex %>% filter(hex_id == "HEX0041123")
cat("Tipo:", h_geom$tipo, "\n")
cat("Zona:", h_geom$zona, "\n")
cat("Estado:", h_geom$estado, "\n")
bb <- st_bbox(h_geom)
cat("BBox: xmin=", bb$xmin, "xmax=", bb$xmax, "ymin=", bb$ymin, "ymax=", bb$ymax, "\n")

# 3. How many raster pixels have value > 0 in the HEX0041123 bounding box?
cat("\n=== Pixels in HEX0041123 bbox area ===\n")
col_min <- max(1L, as.integer(floor((bb$xmin + 180) / PX_RES_LON)) + 1L)
col_max <- min(43200L, as.integer(ceiling((bb$xmax + 180) / PX_RES_LON)) + 1L)
row_min2 <- max(1L, as.integer(floor((90 - bb$ymax) / PX_RES_LAT)) + 1L)
row_max2 <- min(18720L, as.integer(ceiling((90 - bb$ymin) / PX_RES_LAT)) + 1L)
cat(sprintf("Rows %d-%d, Cols %d-%d\n", row_min2, row_max2, col_min, col_max))

# Read that block
block <- raster::getValuesBlock(r, row=row_min2, nrows=(row_max2-row_min2+1),
                                col=col_min, ncols=(col_max-col_min+1))
block <- ifelse(is.na(block), 0, block)
cat("Total pixels:", length(block), "\n")
cat("Nonzero pixels:", sum(block > 0), "\n")
cat("Sum:", sum(block), "\n")

# 4. Check what hexes contain Mexico City (19.43N, 99.13W)
cat("\n=== What hex contains Mexico City? ===\n")
mc_pt <- st_sfc(st_point(c(-99.13, 19.43)), crs = 4326)
mc_in_hex <- hex[st_contains(hex, mc_pt, sparse = FALSE)[,1], ]
if (nrow(mc_in_hex) > 0) {
  cat("Mexico City is in hex:", mc_in_hex$hex_id, "tipo:", mc_in_hex$tipo, "\n")
  dat <- read_excel("h:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/H__poblacion__SSP2__2050.xlsx", sheet=1)
  mc_pop <- dat %>% filter(hex_id == mc_in_hex$hex_id)
  cat("Population assigned to that hex:", mc_pop$poblacion_personas, "\n")
} else {
  cat("Mexico City not found in any hex!\n")
}
