# fix_climate_xlsx_units.R
#
# Repairs already-generated climate xlsx files in data/indicadores/:
# 1) Temperature files: add 273.15 back (to undo incorrect Kelvin->C subtraction)
# 2) Precipitation files: mark as mm/anio and rename value column from
#    precipitacion_mm_dia -> precipitacion_mm_anio when present

suppressPackageStartupMessages({
  library(openxlsx)
  library(readxl)
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
out_dir <- file.path(project_dir, "data", "indicadores")

xlsx_files <- list.files(out_dir, pattern = "^C_.*\\.xlsx$", full.names = TRUE)
if (length(xlsx_files) == 0) stop("No se encontraron archivos C_*.xlsx en: ", out_dir)

num_or_na <- function(x) {
  y <- suppressWarnings(as.numeric(as.character(x)))
  y
}

fixed_temp <- 0
fixed_pr <- 0
skipped <- 0
write_errors <- 0

for (xlsx_path in xlsx_files) {
  catalog <- tryCatch(readxl::read_excel(xlsx_path, sheet = "catalogo_variable"), error = function(e) NULL)
  datos <- tryCatch(readxl::read_excel(xlsx_path, sheet = 1), error = function(e) NULL)
  meta <- tryCatch(readxl::read_excel(xlsx_path, sheet = "metadatos"), error = function(e) NULL)

  if (is.null(catalog) || nrow(catalog) == 0 || is.null(datos)) {
    message("[SKIP] Estructura invalida: ", basename(xlsx_path))
    skipped <- skipped + 1
    next
  }

  variable_nombre <- tolower(as.character(catalog$variable_nombre[1]))
  variable_id <- if ("variable_id" %in% names(catalog)) tolower(as.character(catalog$variable_id[1])) else ""
  col_valor <- as.character(catalog$columna_valor[1])

  changed <- FALSE

  # Fix temperature correction: add 273.15 back to stored values.
  if (grepl("temperatura", variable_nombre) || grepl("temperatura", variable_id)) {
    if (col_valor %in% names(datos)) {
      vals <- num_or_na(datos[[col_valor]])
      med <- suppressWarnings(stats::median(vals, na.rm = TRUE))
      if (is.finite(med) && med < -100) {
        datos[[col_valor]] <- ifelse(is.na(vals), NA_real_, vals + 273.15)
        fixed_temp <- fixed_temp + 1
        changed <- TRUE
      }
      catalog$unidades[1] <- "C"
      if ("descripcion" %in% names(catalog)) {
        catalog$descripcion[1] <- paste0(
          "Temperatura del aire en superficie (tas) proyectada por el modelo GFDL-ESM4 del CMIP6. ",
          "El raster de entrada ya se encuentra en grados Celsius, por lo que no se aplica conversion adicional. ",
          "Promedio espacial por celda hexagonal de 10 km."
        )
        changed <- TRUE
      }
    }
  }

  # Fix precipitation metadata/column naming to mm/anio.
  if (grepl("precipitacion", variable_nombre) || grepl("precipitacion", variable_id)) {
    if (col_valor %in% names(datos)) {
      if (identical(col_valor, "precipitacion_mm_dia")) {
        names(datos)[names(datos) == "precipitacion_mm_dia"] <- "precipitacion_mm_anio"
        catalog$columna_valor[1] <- "precipitacion_mm_anio"
      }
      catalog$unidades[1] <- "mm/anio"
      if ("descripcion" %in% names(catalog)) {
        catalog$descripcion[1] <- paste0(
          "Precipitacion total (pr) proyectada por el modelo GFDL-ESM4 del CMIP6. ",
          "Los raster de entrada ya vienen en mm por anio, por lo que no se aplica conversion adicional. ",
          "Promedio espacial por celda hexagonal de 10 km."
        )
      }
      fixed_pr <- fixed_pr + 1
      changed <- TRUE
    }
  }

  if (!changed) next

  wb <- createWorkbook()
  addWorksheet(wb, "datos")
  addWorksheet(wb, "catalogo_variable")
  writeData(wb, "datos", as.data.frame(datos))
  writeData(wb, "catalogo_variable", as.data.frame(catalog))

  if (!is.null(meta)) {
    addWorksheet(wb, "metadatos")
    writeData(wb, "metadatos", as.data.frame(meta))
  }

  ok <- tryCatch({
    saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    message("[ERROR] No se pudo guardar ", basename(xlsx_path), ": ", conditionMessage(e))
    FALSE
  })

  if (ok) {
    message("[OK] ", basename(xlsx_path))
  } else {
    write_errors <- write_errors + 1
  }
}

message("\nCorreccion finalizada")
message("  Temperatura corregida: ", fixed_temp)
message("  Precipitacion actualizada: ", fixed_pr)
message("  Omitidos: ", skipped)
message("  Errores de escritura: ", write_errors)
