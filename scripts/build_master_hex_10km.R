suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(rnaturalearthdata)
  library(stringr)
})

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  getwd()
}

project_dir   <- normalizePath(file.path(get_script_dir(), ".."), mustWork = FALSE)
output_gpkg   <- file.path(project_dir, "data", "master_hex_10km.gpkg")

eez_poly_path <- "/Users/dr.bastien/Library/CloudStorage/GoogleDrive-bastien.oba@gmail.com/My Drive/Data/Geography/eez_v11.gpkg"
states_path   <- "/Users/dr.bastien/Library/CloudStorage/GoogleDrive-bastien.oba@gmail.com/My Drive/Data/Geography/Mexico/Division_Politica_Estatal/dest22gw.shp"

message("1) Leyendo frontera terrestre de Mexico (rnaturalearthdata)...")
data("countries110", package = "rnaturalearthdata", envir = environment())
mx_land <- countries110 %>%
  filter(admin == "Mexico") %>%
  st_make_valid() %>%
  st_transform(4326)

message("2) Leyendo EEZ (poligono completo) y filtrando Mexico...")
ee_raw <- st_read(eez_poly_path, layer = "eez_v11", quiet = TRUE)
ee_mex <- ee_raw %>%
  filter(
    toupper(SOVEREIGN1) %in% c("MEX", "MEXICO") |
    toupper(TERRITORY1) %in% c("MEX", "MEXICO")
  ) %>%
  st_make_valid() %>%
  st_transform(4326)

if (nrow(ee_mex) == 0) stop("No se encontro el poligono EEZ de Mexico en ", basename(eez_poly_path))
message("   EEZ Mexico area km2: ", round(ee_mex$AREA_KM2[1]))

message("3) Leyendo estados...")
states <- st_read(states_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

state_name_candidates <- c("NOMGEO", "NOMBRE", "ESTADO", "NOM_ENT", "nomgeo", "nombre")
state_name_col        <- state_name_candidates[state_name_candidates %in% names(states)][1]
if (is.na(state_name_col)) {
  states$estado_nombre <- paste0("Estado_", seq_len(nrow(states)))
} else {
  states$estado_nombre <- as.character(states[[state_name_col]])
}

message("4) Construyendo mascara territorial (tierra + EEZ completa como poligono)...")
crs_hex <- "+proj=laea +lat_0=23 +lon_0=-102 +datum=WGS84 +units=m +no_defs"

mx_land_proj  <- st_transform(mx_land, crs_hex) %>% st_make_valid()
ee_mex_proj   <- st_transform(ee_mex, crs_hex)  %>% st_make_valid()
states_proj   <- st_transform(states, crs_hex)  %>% st_make_valid()

mask_parts <- rbind(
  st_sf(tipo = "Continental", geometry = st_geometry(mx_land_proj)),
  st_sf(tipo = "Oceano",      geometry = st_geometry(ee_mex_proj))
) %>% st_make_valid()

message("5) Generando malla hexagonal de 10 km...")
bbox_poly <- st_as_sfc(st_bbox(mask_parts))
hex_grid  <- st_make_grid(bbox_poly, cellsize = 10000, square = FALSE, what = "polygons")

message("6) Recortando hexagonos a la mascara Mexico + EEZ completa...")
sel <- lengths(st_intersects(hex_grid, mask_parts)) > 0
hex <- st_sf(hex_id = sprintf("HEX%07d", seq_len(sum(sel))), geometry = hex_grid[sel])
message("   -> ", sum(sel), " hexagonos seleccionados")

message("7) Clasificando tipo: Continental vs Oceano...")
mx_land_union <- st_union(mx_land_proj) %>% st_make_valid() %>% st_as_sf()
hex$tipo <- ifelse(
  lengths(st_intersects(hex, mx_land_union)) > 0,
  "Continental",
  "Oceano"
)
hex$zona <- ifelse(hex$tipo == "Continental", "Terrestre", "Marina")

message("   Continental: ", sum(hex$tipo == "Continental"))
message("   Oceano:      ", sum(hex$tipo == "Oceano"))

message("8) Asignando estado a hexagonos continentales (union espacial por centroide)...")
cent <- st_centroid(hex)
suppressWarnings({
  joined <- st_join(cent, states_proj %>% select(estado_nombre), left = TRUE)
})
hex$estado <- joined$estado_nombre

message("9) Asignando estado a hexagonos oceanicos (estado costero mas cercano)...")
marine_idx <- which(hex$tipo == "Oceano")
if (length(marine_idx) > 0) {
  nearest_idx              <- st_nearest_feature(cent[marine_idx, ], states_proj)
  hex$estado[marine_idx]   <- states_proj$estado_nombre[nearest_idx]
}

na_idx <- which(is.na(hex$estado))
if (length(na_idx) > 0) {
  nearest_idx              <- st_nearest_feature(cent[na_idx, ], states_proj)
  hex$estado[na_idx]       <- states_proj$estado_nombre[nearest_idx]
}

message("10) Reproyectando a EPSG:4326 y guardando...")
hex_out <- hex %>%
  select(hex_id, tipo, zona, estado) %>%
  st_transform(4326)

if (file.exists(output_gpkg)) file.remove(output_gpkg)
dir.create(dirname(output_gpkg), recursive = TRUE, showWarnings = FALSE)
st_write(hex_out, output_gpkg, layer = "hex_10km", quiet = TRUE)

message("Listo. Geopackage en: ", output_gpkg)
message("Total hexagonos: ", nrow(hex_out))
message("Continental: ", sum(hex_out$tipo == "Continental"))
message("Oceano:      ", sum(hex_out$tipo == "Oceano"))
message("Estados unicos: ", length(unique(hex_out$estado)))
