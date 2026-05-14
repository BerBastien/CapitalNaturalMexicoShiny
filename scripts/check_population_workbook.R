suppressPackageStartupMessages({
  library(sf)
  library(readxl)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(args) >= 1) args[[1]] else "SSP5"
year <- if (length(args) >= 2) as.integer(args[[2]]) else 2100L

project_dir <- "h:/My Drive/CapitalNaturalMexicoShiny"
hex_path <- file.path(project_dir, "data", "master_hex_10km.gpkg")
xlsx_path <- file.path(project_dir, "data", "indicadores", paste0("H__poblacion__", scenario, "__", year, ".xlsx"))

hex <- st_read(hex_path, layer = "hex_10km", quiet = TRUE)
dat <- read_excel(xlsx_path, sheet = 1)

dat$hex_id <- as.character(dat$hex_id)
dat$poblacion_personas <- suppressWarnings(as.numeric(dat$poblacion_personas))

joined <- dplyr::left_join(hex, dat, by = "hex_id")
summary_tbl <- joined %>%
  st_drop_geometry() %>%
  mutate(
    tipo = as.character(tipo),
    nonzero = !is.na(poblacion_personas) & poblacion_personas > 0
  ) %>%
  count(tipo, nonzero, name = "n") %>%
  arrange(tipo, nonzero)

cat("Workbook:", xlsx_path, "\n")
print(summary_tbl)
cat("nonzero total:", sum(joined$poblacion_personas > 0, na.rm = TRUE), "\n")

suspicious <- joined %>%
  st_drop_geometry() %>%
  filter(!is.na(poblacion_personas), poblacion_personas > 0, !is.na(tipo), tipo != "Continental") %>%
  select(hex_id, estado, tipo, poblacion_personas) %>%
  arrange(desc(poblacion_personas))

cat("nonzero outside Continental:", nrow(suspicious), "\n")
if (nrow(suspicious) > 0) {
  print(head(suspicious, 20))
}