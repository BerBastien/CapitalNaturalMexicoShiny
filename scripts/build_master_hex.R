suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(rnaturalearthdata)
  library(stringr)
})

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  getwd()
}

project_dir <- normalizePath(file.path(get_script_dir(), ".."), mustWork = FALSE)
output_gpkg <- file.path(project_dir, "data", "master_hex_2km.gpkg")

eez_path <- "/Users/dr.bastien/Library/CloudStorage/GoogleDrive-bastien.oba@gmail.com/My Drive/Data/Geography/eez_boundaries_v11.gpkg"
states_path <- "/Users/dr.bastien/Library/CloudStorage/GoogleDrive-bastien.oba@gmail.com/My Drive/Data/Geography/Mexico/Division_Politica_Estatal/dest22gw.shp"

guess_mexico_rows <- function(x) {
  if (nrow(x) == 0) {
    return(x)
  }

  preferred_cols <- c(
    "ISO_TER1", "ISO_SOV1", "ISO3", "ISO3_1", "ADM0_A3",
    "TERRITORY1", "SOVEREIGN1", "SOVEREIGN", "GEONUNIT", "COUNTRY"
  )

  for (col_name in preferred_cols) {
    if (col_name %in% names(x)) {
      vals <- toupper(as.character(x[[col_name]]))
      idx <- vals %in% c("MEX", "MEXICO", "UNITED MEXICAN STATES")
      if (any(idx, na.rm = TRUE)) {
        return(x[idx, , drop = FALSE])
      }
    }
  }

  text_cols <- names(x)[vapply(x, is.character, logical(1))]
  if (length(text_cols) == 0) {
    stop("No se pudo identificar a Mexico en la capa EEZ.")
  }

  idx <- rep(FALSE, nrow(x))
  for (col_name in text_cols) {
    idx <- idx | str_detect(tolower(x[[col_name]]), "mex")
  }

  if (!any(idx, na.rm = TRUE)) {
    stop("No se pudo filtrar EEZ para Mexico. Revisa columnas en el geopackage EEZ.")
  }

  x[idx, , drop = FALSE]
}

message("1) Leyendo frontera terrestre de Mexico desde rnaturalearthdata local...")
data("countries110", package = "rnaturalearthdata", envir = environment())
mx_land <- countries110 %>%
  filter(admin == "Mexico") %>%
  st_make_valid() %>%
  st_transform(4326)

message("2) Leyendo EEZ y filtrando Mexico...")
ee_layers <- st_layers(eez_path)$name

ee_raw <- NULL
for (ly in ee_layers) {
  candidate <- suppressWarnings(st_read(eez_path, layer = ly, quiet = TRUE))
  if (nrow(candidate) > 0) {
    ee_raw <- candidate
    break
  }
}
if (is.null(ee_raw)) {
  stop("No fue posible leer capas del geopackage EEZ.")
}

ee_mex <- guess_mexico_rows(ee_raw) %>%
  st_make_valid() %>%
  st_transform(4326)

message("3) Leyendo estados y detectando campo de nombre...")
states <- st_read(states_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

state_name_candidates <- c("NOMGEO", "NOMBRE", "ESTADO", "NOM_ENT", "nomgeo", "nombre")
state_name_col <- state_name_candidates[state_name_candidates %in% names(states)][1]
if (is.na(state_name_col)) {
  states$estado_nombre <- paste0("Estado_", seq_len(nrow(states)))
} else {
  states$estado_nombre <- as.character(states[[state_name_col]])
}

message("4) Construyendo mascara territorial (tierra + EEZ)...")
mask_parts <- rbind(
  st_sf(origen = "Tierra", geometry = st_geometry(mx_land)),
  st_sf(origen = "EEZ", geometry = st_geometry(ee_mex))
) %>%
  st_make_valid()

# Proyeccion local azimutal equivalente centrada en Mexico para celdas de 2 km.
crs_hex <- "+proj=laea +lat_0=23 +lon_0=-102 +datum=WGS84 +units=m +no_defs"
mask_proj <- st_transform(mask_parts, crs_hex)
states_proj <- st_transform(states, crs_hex)

message("5) Generando malla hexagonal de 2 km (puede tardar varios minutos)...")
bbox_proj <- st_bbox(mask_proj)
bbox_poly <- st_as_sfc(bbox_proj)
hex_grid <- st_make_grid(
  bbox_poly,
  cellsize = 2000,
  square = FALSE,
  what = "polygons"
)

message("6) Recortando hexagonos a la mascara Mexico + EEZ...")
sel <- lengths(st_intersects(hex_grid, mask_proj)) > 0
hex <- st_sf(hex_id = sprintf("HEX%07d", seq_len(sum(sel))), geometry = hex_grid[sel])

# Clasificacion tierra/mar usando interseccion con frontera terrestre.
mx_land_proj <- st_transform(mx_land, crs_hex) %>% st_union() %>% st_as_sf()
hex$zona <- ifelse(lengths(st_intersects(hex, mx_land_proj)) > 0, "Terrestre", "Marina")

# Estado por centroide para celdas terrestres; marinas quedan como "Marino".
cent <- st_centroid(hex)
hex_states <- st_join(cent, states_proj %>% select(estado_nombre), left = TRUE)
hex$estado <- ifelse(is.na(hex_states$estado_nombre), "Marino", hex_states$estado_nombre)

message("7) Reproyectando a EPSG:4326 y guardando geopackage maestro...")
hex_out <- st_transform(hex, 4326)

if (file.exists(output_gpkg)) {
  file.remove(output_gpkg)
}

dir.create(dirname(output_gpkg), recursive = TRUE, showWarnings = FALSE)
st_write(hex_out, output_gpkg, layer = "hex_2km", quiet = TRUE)

message("Listo. Geopackage generado en: ", output_gpkg)
message("Total de hexagonos: ", nrow(hex_out))
