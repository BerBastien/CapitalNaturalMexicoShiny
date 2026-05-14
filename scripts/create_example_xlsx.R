# Creates example xlsx files in data/indicadores/ using the new one-xlsx-per-variable format.
# Sheet 1 = datos (hex_id + valor column)
# Sheet "catalogo_variable" = variable metadata (1 row)
# Sheet "metadatos"         = source info
# Sheet "diccionario_categorias" = (not used for continuous variables)
#
# Run AFTER build_master_hex_10km.R

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(openxlsx)
})

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  getwd()
}

project_dir    <- normalizePath(file.path(get_script_dir(), ".."), mustWork = FALSE)
hex_gpkg       <- file.path(project_dir, "data", "master_hex_10km.gpkg")
out_dir        <- file.path(project_dir, "data", "indicadores")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Cargando hex grid...")
hex <- st_read(hex_gpkg, layer = "hex_10km", quiet = TRUE) %>% st_drop_geometry()
cat("Total hexagonos:", nrow(hex), "\n")
cat("Continental:", sum(hex$tipo == "Continental"), "\n")
cat("Oceano:     ", sum(hex$tipo == "Oceano"), "\n")

set.seed(42)

# ---------------------------------------------------------------------------
# 1. MANGLAR — Superficie de manglar (ha) por hexagono
#    Solo Continental. Oceano = NA.
# ---------------------------------------------------------------------------
message("Creando manglar.xlsx...")

manglar_datos <- hex %>%
  mutate(
    hex_id          = hex_id,
    manglar_sup_ha  = ifelse(tipo == "Continental",
                             round(runif(n(), 0, 500) * rbeta(n(), 0.5, 3), 2),
                             NA_real_)
  ) %>%
  select(hex_id, manglar_sup_ha)

manglar_catalog <- data.frame(
  variable_id    = "manglar_sup_ha",
  variable_nombre = "Superficie de Manglar",
  tipo_dato      = "continua",
  unidades       = "ha",
  descripcion    = "Superficie cubierta por manglar dentro de cada celda hexagonal de 10 km.",
  fuente         = "Ejemplo didactico",
  referencia     = "Dato simulado para uso en clase.",
  columna_id_hex = "hex_id",
  columna_valor  = "manglar_sup_ha",
  stringsAsFactors = FALSE
)

manglar_meta <- data.frame(
  titulo        = "Superficie de Manglar",
  resumen       = "Variable de ejemplo. Solo celdas continentales tienen valores.",
  institucion   = "UNAM - Capital Natural de Mexico",
  fecha_version = "2026",
  stringsAsFactors = FALSE
)

wb_manglar <- createWorkbook()
addWorksheet(wb_manglar, "datos")
addWorksheet(wb_manglar, "catalogo_variable")
addWorksheet(wb_manglar, "metadatos")
writeData(wb_manglar, "datos",             manglar_datos)
writeData(wb_manglar, "catalogo_variable", manglar_catalog)
writeData(wb_manglar, "metadatos",         manglar_meta)
saveWorkbook(wb_manglar, file.path(out_dir, "manglar.xlsx"), overwrite = TRUE)
message("  -> manglar.xlsx guardado (", sum(!is.na(manglar_datos$manglar_sup_ha)), " celdas con datos)")

# ---------------------------------------------------------------------------
# 2. CLOROFILA — Concentracion de clorofila (mg/m3) por hexagono
#    Solo Oceano. Continental = NA.
# ---------------------------------------------------------------------------
message("Creando clorofila.xlsx...")

clorofila_datos <- hex %>%
  mutate(
    hex_id        = hex_id,
    clorofila_mgm3 = ifelse(tipo == "Oceano",
                            round(rlnorm(n(), meanlog = 0.5, sdlog = 0.8), 3),
                            NA_real_)
  ) %>%
  select(hex_id, clorofila_mgm3)

clorofila_catalog <- data.frame(
  variable_id    = "clorofila_mgm3",
  variable_nombre = "Concentracion de Clorofila",
  tipo_dato      = "continua",
  unidades       = "mg/m3",
  descripcion    = "Concentracion de clorofila-a en la superficie del oceano por celda hexagonal de 10 km.",
  fuente         = "Ejemplo didactico",
  referencia     = "Dato simulado para uso en clase.",
  columna_id_hex = "hex_id",
  columna_valor  = "clorofila_mgm3",
  stringsAsFactors = FALSE
)

clorofila_meta <- data.frame(
  titulo        = "Concentracion de Clorofila",
  resumen       = "Variable de ejemplo. Solo celdas oceanicas tienen valores.",
  institucion   = "UNAM - Capital Natural de Mexico",
  fecha_version = "2026",
  stringsAsFactors = FALSE
)

wb_cloro <- createWorkbook()
addWorksheet(wb_cloro, "datos")
addWorksheet(wb_cloro, "catalogo_variable")
addWorksheet(wb_cloro, "metadatos")
writeData(wb_cloro, "datos",             clorofila_datos)
writeData(wb_cloro, "catalogo_variable", clorofila_catalog)
writeData(wb_cloro, "metadatos",         clorofila_meta)
saveWorkbook(wb_cloro, file.path(out_dir, "clorofila.xlsx"), overwrite = TRUE)
message("  -> clorofila.xlsx guardado (", sum(!is.na(clorofila_datos$clorofila_mgm3)), " celdas con datos)")

message("\nListo. Archivos en: ", out_dir)
