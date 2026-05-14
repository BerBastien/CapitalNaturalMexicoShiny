library(sf)
library(readxl)
library(dplyr)

hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg",
               layer = "hex_10km", quiet = TRUE)
dat <- read_excel("h:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/H__poblacion__SSP2__2050.xlsx", sheet = 1)
joined <- hex %>% left_join(dat, by = "hex_id")

ocean_pop <- joined %>% 
  filter(tipo == "Oceano" & !is.na(poblacion_personas) & poblacion_personas > 0) %>%
  mutate(centroid = st_centroid(geom)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  st_drop_geometry() %>%
  select(hex_id, zona, estado, lon, lat, poblacion_personas) %>%
  arrange(desc(poblacion_personas))

cat("=== Ocean hexes with nonzero population ===\n")
cat("Count:", nrow(ocean_pop), "\n")
cat("Total population in ocean hexes:", sum(ocean_pop$poblacion_personas), "\n")
cat("Total population in ALL hexes:", sum(dat$poblacion_personas, na.rm=TRUE), "\n")
cat("\nTop 20 ocean hexes by population:\n")
print(head(ocean_pop, 20))

cat("\n=== Distribution of ocean hexes with pop by zona ===\n")
print(table(ocean_pop$zona))

cat("\n=== Top 5 ocean hexes - are they near known coastal cities? ===\n")
top5 <- head(ocean_pop, 5)
for (i in 1:nrow(top5)) {
  cat(sprintf("Hex %s: lon=%.3f, lat=%.3f, pop=%.0f\n",
    top5$hex_id[i], top5$lon[i], top5$lat[i], top5$poblacion_personas[i]))
}
