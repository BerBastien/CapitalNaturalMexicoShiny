# rename_indicator_excels.R
#
# Renames indicator Excel files in data/indicadores to:
#   D__variable_name__scenario__year.xlsx
# where D in {C,H,E} (domain code)
#
# Rules:
# - variable_name uses lowercase snake_case
# - scenario uses uppercase like SSP585
# - year uses 4 digits
# - if no climate scenario exists, scenario defaults to PRESENTE and year to
#   2025 (or to a year detected in referencia)

suppressPackageStartupMessages({
  library(readxl)
  library(stringr)
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

slugify <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

extract_year <- function(x) {
  years <- stringr::str_extract_all(x, "(19|20)\\d{2}")[[1]]
  if (length(years) == 0) return(NA_character_)
  years[[1]]
}

extract_year_reference <- function(x) {
  years <- stringr::str_extract_all(x, "(19|20)\\d{2}")[[1]]
  if (length(years) == 0) return(NA_character_)
  years <- suppressWarnings(as.integer(years))
  years <- years[!is.na(years) & years >= 1900 & years <= 2100]
  if (length(years) == 0) return(NA_character_)
  as.character(max(years))
}

extract_scenario <- function(x) {
  xu <- toupper(x)
  m <- str_match(xu, "(SSP\\d{3}|RCP\\d{2,3}|BAU|GREEN|HISTORICAL|BASELINE|PRESENTE)")
  ifelse(is.na(m[, 1]), NA_character_, m[, 1])
}

project_dir <- normalizePath(get_project_dir(), mustWork = FALSE)
indir <- file.path(project_dir, "data", "indicadores")

files <- list.files(indir, pattern = "\\.xlsx$", full.names = TRUE)
if (length(files) == 0) stop("No se encontraron archivos xlsx en: ", indir)

renamed <- 0
skipped <- 0
errors <- 0

for (path in files) {
  old_name <- basename(path)
  old_base <- tools::file_path_sans_ext(old_name)

  # Ignore temporary lock files created by Excel
  if (startsWith(old_name, "~$")) {
    message("[SKIP] Temporal de Excel: ", old_name)
    skipped <- skipped + 1
    next
  }

  # Domain code from first token/prefix
  domain_code <- toupper(substr(old_base, 1, 1))
  if (!domain_code %in% c("C", "H", "E")) {
    message("[SKIP] Dominio no reconocido en nombre: ", old_name)
    skipped <- skipped + 1
    next
  }

  # Fix malformed climate names produced by previous parser versions:
  # C__temperatura_2100_ssp126__SSP126__2019.xlsx -> C__temperatura__SSP126__2100.xlsx
  malformed <- stringr::str_match(
    old_base,
    "^([CHE])__([a-z0-9_]+)_(\\d{4})_(ssp\\d{3})__(SSP\\d{3})__(\\d{4})$"
  )
  if (!is.na(malformed[1, 1])) {
    code <- malformed[1, 2]
    var_slug_fix <- malformed[1, 3]
    year_fix <- malformed[1, 4]
    scen_fix <- toupper(malformed[1, 5])
    new_name_fix <- paste0(code, "__", var_slug_fix, "__", scen_fix, "__", year_fix, ".xlsx")
    new_path_fix <- file.path(indir, new_name_fix)
    if (!file.exists(new_path_fix) && file.rename(path, new_path_fix)) {
      message("[OK] ", old_name, " -> ", new_name_fix)
      renamed <- renamed + 1
      next
    }
  }

  # Hard safeguard: if filename is already canonical, never try to rebuild it
  # from Excel metadata (prevents accidental regressions on re-runs).
  is_canonical <- str_detect(
    old_base,
    "^[CHE]__[a-z0-9_]+__(SSP\\d{3}|RCP\\d{2,3}|BAU|GREEN|HISTORICAL|BASELINE|PRESENTE)__\\d{4}$"
  )
  if (is_canonical) {
    message("[OK] Ya en formato: ", old_name)
    next
  }

  catalog <- tryCatch(readxl::read_excel(path, sheet = "catalogo_variable"), error = function(e) NULL)
  if (is.null(catalog) || nrow(catalog) == 0 || !"variable_nombre" %in% names(catalog)) {
    message("[SKIP] Sin catalogo_variable/variable_nombre: ", old_name)
    skipped <- skipped + 1
    next
  }

  var_name_raw <- as.character(catalog$variable_nombre[1])
  referencia_raw <- if ("referencia" %in% names(catalog)) as.character(catalog$referencia[1]) else ""

  # Remove trailing " year - scenario" if present
  var_core <- var_name_raw %>%
    str_remove("\\s+(19|20)\\d{2}\\s*-\\s*SSP\\d{3}.*$") %>%
    str_remove("_(19|20)\\d{2}_ssp\\d{3}$") %>%
    str_trim()
  var_slug <- slugify(var_core)

  # Scenario/year from filename first, then variable label.
  scenario <- extract_scenario(old_base)
  year <- extract_year(old_base)

  if (is.na(scenario)) scenario <- extract_scenario(var_name_raw)
  if (is.na(year)) year <- extract_year(var_name_raw)

  # For present/baseline products (no explicit scenario), use PRESENTE.
  if (is.na(scenario)) scenario <- "PRESENTE"

  # If no year in variable/file name, try referencia and then default 2025.
  if (is.na(year)) year <- extract_year_reference(referencia_raw)
  if (is.na(year)) year <- "2025"

  if (var_slug == "") {
    message("[SKIP] Falta year/scenario/variable en: ", old_name)
    skipped <- skipped + 1
    next
  }

  new_name <- paste0(domain_code, "__", var_slug, "__", scenario, "__", year, ".xlsx")
  new_path <- file.path(indir, new_name)

  if (normalizePath(path, winslash = "/", mustWork = FALSE) == normalizePath(new_path, winslash = "/", mustWork = FALSE)) {
    message("[OK] Ya en formato: ", old_name)
    next
  }

  if (file.exists(new_path)) {
    message("[ERROR] Ya existe destino: ", new_name)
    errors <- errors + 1
    next
  }

  ok <- file.rename(path, new_path)
  if (ok) {
    message("[OK] ", old_name, " -> ", new_name)
    renamed <- renamed + 1
  } else {
    message("[ERROR] No se pudo renombrar: ", old_name)
    errors <- errors + 1
  }
}

message("\nRenombrado finalizado")
message("  Renombrados: ", renamed)
message("  Omitidos: ", skipped)
message("  Errores: ", errors)
