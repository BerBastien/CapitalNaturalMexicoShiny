# fix_marine_population.R
#
# Post-processes all H__poblacion__*.xlsx files in data/indicadores/
# to set poblacion_personas = NA for hexes classified as tipo = "Oceano"
# (zona = "Marina") in the master hex grid.
#
# ROOT CAUSE: The Wang et al. SSP rasters have data quality artifacts where
# coastal/ocean pixels can inherit spurious values. Marine hexes should be
# treated as missing data in the workbook.

suppressPackageStartupMessages({
  library(sf)
  library(readxl)
  library(openxlsx)
  library(dplyr)
})

get_project_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
    return(dirname(dirname(script_path)))
  }
  getwd()
}

project_dir <- normalizePath(get_project_dir(), mustWork = FALSE)
hex_gpkg <- file.path(project_dir, "data", "master_hex_10km.gpkg")
out_dir   <- file.path(project_dir, "data", "indicadores")

message("Cargando malla hexagonal...")
hex_sf <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE)
marine_ids <- hex_sf$hex_id[hex_sf$tipo == "Oceano"]
message(sprintf("  Hexagonos Oceano: %d", length(marine_ids)))

xlsx_files <- list.files(out_dir, pattern = "^H__poblacion__.*\\.xlsx$", full.names = TRUE)
message(sprintf("Archivos a procesar: %d\n", length(xlsx_files)))

for (xf in xlsx_files) {
  wb <- openxlsx::loadWorkbook(xf)
  dat <- openxlsx::readWorkbook(wb, sheet = 1)

  pop_col <- "poblacion_personas"
  if (!pop_col %in% names(dat)) {
    message("  [SKIP] ", basename(xf), " — sin columna poblacion_personas")
    next
  }

  n_before <- sum(dat[[pop_col]] > 0, na.rm = TRUE)
  ocean_sum_before <- sum(dat[[pop_col]][dat$hex_id %in% marine_ids], na.rm = TRUE)

  # Mark Marine hexes as missing data
  dat[[pop_col]][dat$hex_id %in% marine_ids] <- NA_real_

  n_after <- sum(dat[[pop_col]] > 0, na.rm = TRUE)
  total_after <- sum(dat[[pop_col]], na.rm = TRUE)

  # Write back to sheet 1, preserving other sheets
  openxlsx::writeData(wb, sheet = 1, x = dat)
  openxlsx::saveWorkbook(wb, xf, overwrite = TRUE)

  message(sprintf("  [OK] %-40s  celdas_antes=%d  marine_pop_eliminada=%.0f  celdas_despues=%d  total=%.0f",
    basename(xf), n_before, ocean_sum_before, n_after, total_after))
}

message("\nListo.")
