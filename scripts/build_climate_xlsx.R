# build_climate_xlsx.R
#
# Extracts climate raster values from GFDL-ESM4 CMIP6 projections onto the
# 10-km hexagonal grid and writes one xlsx per variable-year-scenario
# combination into data/indicadores/.
#
# TIF naming convention: {var}_{scenario}_{year}.tif
#   var:      tas | pr | rsds
#   scenario: ssp126 | ssp370 | ssp585
#   year:     2020 | 2050 | 2070 | 2100
#
# Unit conversions applied:
#   tas  : already in C in provided rasters (no conversion)
#   pr   : already in mm/anio in provided rasters (no conversion)
#   rsds : W/m2 -> no conversion needed
#
# Output xlsx naming: C_{NombreLegible} {year} - {SCENARIO_UPPER}.xlsx
# e.g.  C_Temperatura 2020 - SSP585.xlsx

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(openxlsx)
})

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
get_project_dir <- function() {
  # When run via Rscript --file=, the script lives in scripts/ -> go up one level
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg  <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    return(dirname(dirname(script_path)))  # scripts/ -> project root
  }
  # When sourced interactively, working directory is already the project root
  getwd()
}

project_dir <- normalizePath(get_project_dir(), mustWork = FALSE)
hex_gpkg    <- file.path(project_dir, "data", "master_hex_10km.gpkg")
tif_dir     <- "H:/My Drive/PFT patterns/Data/output/climate_horizons_tifs"
out_dir     <- file.path(project_dir, "data", "indicadores")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Variable metadata lookup
# ---------------------------------------------------------------------------
var_meta <- list(
  tas = list(
    nombre_legible = "Temperatura",
    col_valor      = "temperatura_c",
    unidades       = "C",
    descripcion    = paste0(
      "Temperatura del aire en superficie (tas) proyectada por el modelo GFDL-ESM4 ",
      "del CMIP6. El raster de entrada ya se encuentra en grados Celsius, ",
      "por lo que no se aplica conversion adicional. ",
      "Promedio espacial por celda hexagonal de 10 km."
    ),
    conversion     = function(x) x
  ),
  pr = list(
    nombre_legible = "Precipitacion",
    col_valor      = "precipitacion_mm_anio",
    unidades       = "mm/anio",
    descripcion    = paste0(
      "Precipitacion total (pr) proyectada por el modelo GFDL-ESM4 del CMIP6. ",
      "Los raster de entrada ya vienen en mm por anio, ",
      "por lo que no se aplica conversion adicional. ",
      "Promedio espacial por celda hexagonal de 10 km."
    ),
    conversion     = function(x) x
  ),
  rsds = list(
    nombre_legible = "Radiacion Solar",
    col_valor      = "radiacion_solar_wm2",
    unidades       = "W/m2",
    descripcion    = paste0(
      "Radiacion solar de onda corta descendente en superficie (rsds) proyectada ",
      "por el modelo GFDL-ESM4 del CMIP6. ",
      "Promedio espacial por celda hexagonal de 10 km."
    ),
    conversion     = function(x) x
  )
)

scenario_labels <- c(
  ssp126 = "SSP126",
  ssp370 = "SSP370",
  ssp585 = "SSP585"
)

# ---------------------------------------------------------------------------
# Load hex polygons (reproject to raster CRS on the fly during extraction)
# ---------------------------------------------------------------------------
message("Cargando malla hexagonal 10 km...")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
if (!"estado" %in% names(hex_sf)) hex_sf$estado <- "Sin estado"
if (!"tipo"   %in% names(hex_sf)) hex_sf$tipo   <- "Continental"

# SpatVector for terra extraction
hex_vect <- vect(hex_sf)

# ---------------------------------------------------------------------------
# Helper: extract mean raster value per hex polygon
# ---------------------------------------------------------------------------
extract_hex_means <- function(tif_path, conversion_fn) {
  r   <- rast(tif_path)
  # Reproject hex to raster CRS for extraction
  hv  <- project(hex_vect, crs(r))
  ex  <- terra::extract(r, hv, fun = mean, na.rm = TRUE, ID = TRUE)
  # Column 1 = ID, column 2 = value
  vals <- conversion_fn(ex[[2]])
  vals <- round(vals, 4)
  data.frame(
    hex_id = hex_sf$hex_id,
    valor  = vals,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

if (length(tif_files) == 0) stop("No se encontraron archivos .tif en: ", tif_dir)
message(sprintf("Encontrados %d archivos TIF.", length(tif_files)))

processed <- 0
skipped   <- 0

for (tif_path in sort(tif_files)) {
  fname <- tools::file_path_sans_ext(basename(tif_path))   # e.g. tas_ssp585_2020
  parts <- strsplit(fname, "_")[[1]]

  # Handle multi-part variable names like rsds -> parts[1] = "rsds"
  # Pattern: {var}_{scenario}_{year}
  if (length(parts) < 3) {
    message("  [SKIP] Nombre no reconocido: ", basename(tif_path))
    skipped <- skipped + 1
    next
  }

  var_key  <- parts[1]                           # tas | pr | rsds
  scenario <- paste(parts[2:(length(parts)-1)], collapse = "_")  # ssp126 etc.
  year     <- parts[length(parts)]               # 2020 etc.

  if (!var_key %in% names(var_meta)) {
    message("  [SKIP] Variable no registrada: ", var_key)
    skipped <- skipped + 1
    next
  }
  if (!scenario %in% names(scenario_labels)) {
    message("  [SKIP] Escenario no reconocido: ", scenario)
    skipped <- skipped + 1
    next
  }

  meta      <- var_meta[[var_key]]
  scen_up   <- scenario_labels[[scenario]]
  nombre_legible <- paste0(meta$nombre_legible, " ", year, " - ", scen_up)

  # variable_id: safe snake_case identifier
  var_id <- paste0(
    tolower(gsub(" ", "_", meta$nombre_legible)), "_",
    year, "_",
    tolower(scen_up)
  )

  # Output file
  out_file <- file.path(out_dir, paste0("C_", nombre_legible, ".xlsx"))

  message(sprintf("  Procesando: %s", basename(tif_path)))

  # --- Extract values ---
  datos_raw <- tryCatch(
    extract_hex_means(tif_path, meta$conversion),
    error = function(e) {
      message("    ERROR extrayendo: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(datos_raw)) { skipped <- skipped + 1; next }

  # Rename valor column to the variable-specific name
  names(datos_raw)[names(datos_raw) == "valor"] <- meta$col_valor

  # --- catalogo_variable sheet (all required server columns + resolucion_hex) ---
  catalogo <- data.frame(
    variable_id     = var_id,
    variable_nombre = nombre_legible,
    tipo_dato       = "continuo",
    unidades        = meta$unidades,
    descripcion     = paste0(
      meta$descripcion,
      " Escenario: ", scen_up, ". Horizonte temporal: ", year, "."
    ),
    fuente          = "GFDL-ESM4 (CMIP6) - procesado para Capital Natural de Mexico",
    referencia      = paste0(
      "Held, I. M. et al. (2019). Structure and performance of GFDL's ESM4.0 ",
      "Earth System Model. JAMES 11, 3167-3211. https://doi.org/10.1029/2019MS001829"
    ),
    columna_id_hex  = "hex_id",
    columna_valor   = meta$col_valor,
    resolucion_hex  = "10km",
    stringsAsFactors = FALSE
  )

  # --- metadatos sheet ---
  metadatos <- data.frame(
    titulo        = nombre_legible,
    resumen       = paste0(
      "Proyeccion climatica de ", meta$nombre_legible,
      " para el horizonte ", year, " bajo el escenario ", scen_up,
      " generada con el modelo GFDL-ESM4 del proyecto CMIP6."
    ),
    institucion   = "GFDL / NOAA - CMIP6",
    fecha_version = "2026",
    stringsAsFactors = FALSE
  )

  # --- Write workbook ---
  wb <- createWorkbook()
  addWorksheet(wb, "datos")
  addWorksheet(wb, "catalogo_variable")
  addWorksheet(wb, "metadatos")

  writeData(wb, "datos",             datos_raw)
  writeData(wb, "catalogo_variable", catalogo)
  writeData(wb, "metadatos",         metadatos)

  saveWorkbook(wb, out_file, overwrite = TRUE)

  n_valid <- sum(!is.na(datos_raw[[meta$col_valor]]))
  message(sprintf("    -> %s  (%d celdas con datos)", basename(out_file), n_valid))
  processed <- processed + 1
}

message(sprintf("\nListo. %d archivos generados, %d omitidos.", processed, skipped))
message("Archivos en: ", out_dir)
