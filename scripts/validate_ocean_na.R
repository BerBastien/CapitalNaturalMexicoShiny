suppressPackageStartupMessages({
  library(sf)
  library(openxlsx)
})

project_dir <- "H:/My Drive/CapitalNaturalMexicoShiny"
hex_gpkg <- file.path(project_dir, "data", "master_hex_10km.gpkg")
xlsx_path <- file.path(project_dir, "data", "indicadores", "H__poblacion__SSP1__2050.xlsx")

hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
dat <- openxlsx::read.xlsx(xlsx_path, sheet = "datos")
marine_ids <- hex_sf$hex_id[hex_sf$tipo == "Oceano"]
marine_vals <- dat$poblacion_personas[dat$hex_id %in% marine_ids]
land_vals <- dat$poblacion_personas[!dat$hex_id %in% marine_ids]

cat("Marine count:", length(marine_vals), "\n")
cat("Marine NA count:", sum(is.na(marine_vals)), "\n")
cat("Marine zero count:", sum(marine_vals == 0, na.rm = TRUE), "\n")
cat("Marine positive count:", sum(marine_vals > 0, na.rm = TRUE), "\n")
cat("Land NA count:", sum(is.na(land_vals)), "\n")
cat("Land positive count:", sum(land_vals > 0, na.rm = TRUE), "\n")
cat("Top 10 values:\n")
print(head(dat[order(-dat$poblacion_personas), c("hex_id", "poblacion_personas")], 10))
