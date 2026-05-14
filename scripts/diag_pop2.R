library(sf)
library(readxl)
library(dplyr)
library(raster)

# 1. Check actual raster extent/resolution
tif <- "H:/My Drive/Data/SSPs/Gridded/Pop/SSP2/SSP2_2050.tif"
if (file.exists(tif)) {
  r <- raster::raster(tif)
  cat("=== Raster info ===\n")
  cat("ncols:", ncol(r), "\n")
  cat("nrows:", nrow(r), "\n")
  cat("extent:", paste(as.vector(raster::extent(r)), collapse=" "), "\n")
  cat("CRS:", as.character(raster::crs(r)), "\n")
  cat("resolution:", raster::res(r), "\n")
} else {
  cat("TIF not found:", tif, "\n")
}

# 2. Check tipo distribution of nonzero hexes
hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg",
               layer = "hex_10km", quiet = TRUE)
dat <- read_excel("h:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/H__poblacion__SSP2__2050.xlsx", sheet = 1)
joined <- hex %>% left_join(dat, by = "hex_id")

cat("\n=== Tipo breakdown for nonzero pop hexes ===\n")
nonzero <- joined %>% filter(!is.na(poblacion_personas) & poblacion_personas > 0)
print(table(nonzero$tipo))
cat("Total nonzero:", nrow(nonzero), "\n")

cat("\n=== Tipo breakdown for all hexes ===\n")
print(table(hex$tipo))

# 3. Where is the max population area vs Mexico City?
cat("\n=== Hexes near Mexico City (18-21N, 97-101W) ===\n")
mc_hex <- joined %>%
  filter(!is.na(poblacion_personas) & poblacion_personas > 0) %>%
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  filter(lat >= 18 & lat <= 21 & lon >= -101 & lon <= -97) %>%
  arrange(desc(poblacion_personas))
cat("Count in box:", nrow(mc_hex), "\n")
if (nrow(mc_hex) > 0) {
  cat("Max pop in Mexico City box:", max(mc_hex$poblacion_personas), "\n")
  print(head(mc_hex %>% select(hex_id, tipo, lon, lat, poblacion_personas), 5))
}
