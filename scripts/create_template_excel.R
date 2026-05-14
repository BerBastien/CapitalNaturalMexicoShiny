suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(openxlsx)
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
hex_path <- file.path(project_dir, "data", "master_hex_2km.gpkg")
out_xlsx <- file.path(project_dir, "data", "indicadores", "plantilla_indicadores.xlsx")
out_csv <- file.path(project_dir, "data", "indicadores", "ejemplo_manglar.csv")
out_csv_cat <- file.path(project_dir, "data", "indicadores", "ejemplo_uso_suelo.csv")

if (!file.exists(hex_path)) {
  stop("No existe el geopackage maestro. Corre scripts/build_master_hex.R primero.")
}

hex <- st_read(hex_path, layer = "hex_2km", quiet = TRUE) %>%
  st_drop_geometry() %>%
  select(hex_id, estado)

set.seed(20260501)
example <- hex %>%
  mutate(valor_manglar_ha = round(runif(n(), min = 0, max = 250), 2))

example_cat <- hex %>%
  mutate(
    clase_uso = sample(
      c("Forestal", "Agricola", "Urbano"),
      size = n(),
      replace = TRUE,
      prob = c(0.58, 0.29, 0.13)
    )
  )

write.csv(example %>% select(hex_id, valor_manglar_ha), out_csv, row.names = FALSE)
write.csv(example_cat %>% select(hex_id, clase_uso), out_csv_cat, row.names = FALSE)

catalogo_variables <- data.frame(
  variable_id = c("manglar_ha", "uso_suelo"),
  variable_nombre = c("Area de manglar", "Tipo de uso de suelo"),
  tipo_dato = c("continua", "categorica"),
  unidades = c("ha", "categoria"),
  descripcion = c(
    "Hectareas estimadas de manglar por hexagono.",
    "Categoria dominante de uso de suelo por hexagono."
  ),
  fuente = c("Ejemplo de plantilla", "Ejemplo de plantilla"),
  referencia = c("Sin referencia", "Sin referencia"),
  archivo_csv = c("ejemplo_manglar.csv", "ejemplo_uso_suelo.csv"),
  columna_id_hex = c("hex_id", "hex_id"),
  columna_valor = c("valor_manglar_ha", "clase_uso"),
  paleta = c("daybreak_cont", "daybreak_cat"),
  stringsAsFactors = FALSE
)

metadatos_dataset <- data.frame(
  dataset_id = "dataset_estudiante_01",
  titulo_dataset = "Plantilla de indicadores para Capital Natural",
  resumen = "Sustituye con descripcion breve del dataset.",
  institucion = "Tu institucion",
  persona_contacto = "Nombre Apellido",
  correo_contacto = "correo@dominio.com",
  fecha_version = as.character(Sys.Date()),
  stringsAsFactors = FALSE
)

diccionario_categorias <- data.frame(
  variable_id = c("uso_suelo", "uso_suelo", "uso_suelo"),
  categoria = c("Forestal", "Agricola", "Urbano"),
  orden = c(1, 2, 3),
  color_hex = c("#1F5D3A", "#D89B30", "#A63D40"),
  stringsAsFactors = FALSE
)

wb <- createWorkbook()
addWorksheet(wb, "catalogo_variables")
addWorksheet(wb, "metadatos_dataset")
addWorksheet(wb, "diccionario_categorias")

writeData(wb, "catalogo_variables", catalogo_variables)
writeData(wb, "metadatos_dataset", metadatos_dataset)
writeData(wb, "diccionario_categorias", diccionario_categorias)

hdr_style <- createStyle(textDecoration = "bold", fgFill = "#F6C64E", halign = "center")
for (sheet in c("catalogo_variables", "metadatos_dataset", "diccionario_categorias")) {
  addStyle(wb, sheet, hdr_style, rows = 1, cols = 1:50, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet, cols = 1:20, widths = "auto")
}

dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Plantilla creada: ", out_xlsx)
message("CSV de ejemplo creado: ", out_csv)
message("CSV categorico de ejemplo creado: ", out_csv_cat)
