suppressPackageStartupMessages({
  library(sf)
  library(openxlsx)
  library(dplyr)
})

project_dir <- "H:/My Drive/CapitalNaturalMexicoShiny"
hex_gpkg   <- file.path(project_dir, "data", "master_hex_10km.gpkg")
xlsx_path  <- file.path(project_dir, "data", "indicadores", "H__poblacion__SSP1__2050.xlsx")

cat("=== XLSX TOP 20 HEXES (SSP1 2050) ===\n")
datos <- openxlsx::read.xlsx(xlsx_path, sheet = "datos")
top20 <- datos %>% arrange(desc(poblacion_personas)) %>% head(20)
print(top20)
cat("\nTotal hexes with data:", sum(datos$poblacion_personas > 0, na.rm=TRUE), "\n")
cat("Grand total:", sum(datos$poblacion_personas, na.rm=TRUE), "\n\n")

# Load hex geometry to see where the top hexes are
cat("=== GEOGRAPHIC LOCATION OF TOP 10 HEXES ===\n")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
# get centroids
hex_cents <- st_centroid(hex_sf)
hex_pts <- st_coordinates(hex_cents)

top10_ids <- top20$hex_id[1:10]
for (hid in top10_ids) {
  idx <- which(hex_sf$hex_id == hid)
  if (length(idx) > 0) {
    lon <- hex_pts[idx, "X"]
    lat <- hex_pts[idx, "Y"]
    tipo <- hex_sf$tipo[idx]
    estado <- if ("estado" %in% names(hex_sf)) hex_sf$estado[idx] else "?"
    pop <- datos$poblacion_personas[datos$hex_id == hid]
    cat(sprintf("  %-15s  lon=%7.3f lat=%6.3f  tipo=%-12s estado=%-20s pop=%.1f\n",
                hid, lon, lat, tipo, estado, pop))
  }
}

# Also check Mexico City area hexes
cat("\n=== MEXICO CITY AREA HEXES (19-20N, -99.5 to -98.5W) ===\n")
mx_city_hexes <- which(
  hex_pts[, "X"] > -99.5 & hex_pts[, "X"] < -98.5 &
  hex_pts[, "Y"] > 19.0  & hex_pts[, "Y"] < 20.0
)
if (length(mx_city_hexes) > 0) {
  for (idx in mx_city_hexes) {
    hid <- hex_sf$hex_id[idx]
    pop <- datos$poblacion_personas[datos$hex_id == hid]
    if (length(pop) == 0) pop <- 0
    estado <- if ("estado" %in% names(hex_sf)) hex_sf$estado[idx] else "?"
    tipo <- hex_sf$tipo[idx]
    cat(sprintf("  %-15s  lon=%7.3f lat=%6.3f  tipo=%-12s estado=%-20s pop=%.1f\n",
                hid, hex_pts[idx,"X"], hex_pts[idx,"Y"], tipo, estado, pop))
  }
} else {
  cat("No hexes found in Mexico City bbox\n")
}

# Also compare SSP2 2020 (marine-fixed only, not regenerated) vs SSP1 2050 (regenerated)
cat("\n=== COMPARE SSP2 2020 TOP 5 (marine-fixed) vs SSP1 2050 (regenerated) ===\n")
xlsx2 <- file.path(project_dir, "data", "indicadores", "H__poblacion__SSP2__2020.xlsx")
if (file.exists(xlsx2)) {
  datos2 <- openxlsx::read.xlsx(xlsx2, sheet = "datos")
  top5_ssp2 <- datos2 %>% arrange(desc(poblacion_personas)) %>% head(5)
  top5_ssp1 <- datos %>% arrange(desc(poblacion_personas)) %>% head(5)
  cat("SSP2 2020 top 5:\n"); print(top5_ssp2)
  cat("SSP1 2050 top 5:\n"); print(top5_ssp1)
}
