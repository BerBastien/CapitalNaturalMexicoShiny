library(sf)
library(readxl)
library(dplyr)

hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg",
               layer = "hex_10km", quiet = TRUE)
cat("Hex columns:", paste(names(hex), collapse = ", "), "\n")
cat("Hex rows:", nrow(hex), "\n")

dat <- read_excel("h:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/H__poblacion__SSP2__2050.xlsx", sheet = 1)
cat("XLSX columns:", paste(names(dat), collapse = ", "), "\n")
cat("XLSX rows:", nrow(dat), "\n")

pop_col <- "poblacion_personas"
id_col  <- names(dat)[1]
cat("Population column:", pop_col, "\n")
cat("ID column:", id_col, "\n")
cat("Nonzero cells:", sum(dat[[pop_col]] > 0, na.rm = TRUE), "\n")

# Top 10 by population
top10 <- dat %>% arrange(desc(.data[[pop_col]])) %>% head(10)
cat("\nTop 10 hex_ids by population:\n")
print(top10)

# Join to geometry and get centroids
joined <- hex %>%
  left_join(dat, by = c("hex_id" = id_col)) %>%
  filter(!is.na(.data[[pop_col]]) & .data[[pop_col]] > 0)

cat("\nNonzero hexes after join:", nrow(joined), "\n")

# Centroid coordinates of top populated hexes
top_ids <- top10[[id_col]]
top_hex <- joined %>% filter(hex_id %in% top_ids)
if (nrow(top_hex) > 0) {
  centroids <- st_centroid(st_geometry(top_hex))
  coords <- st_coordinates(centroids)
  result <- data.frame(hex_id = top_hex$hex_id,
                       lon = round(coords[,1], 3),
                       lat = round(coords[,2], 3),
                       pop = round(top_hex[[pop_col]]))
  cat("\nTop hexes with centroids (lon, lat):\n")
  print(result)
  cat("\nAre these near Mexico City (~19.4N, 99.1W)? Gulf of Mexico? Pacific?\n")
} else {
  cat("No matching hexes found in geometry!\n")
}

# Check if any XLSX hex_ids are missing from hex geometry
missing <- setdiff(dat[[1]], hex$hex_id)
cat("\nXLSX hex_ids missing from geometry:", length(missing), "\n")
if (length(missing) > 0) cat("First 5 missing:", paste(head(missing, 5), collapse=", "), "\n")
