library(readxl)
library(dplyr)

# Check the most recent population Excel file
excel_path <- "h:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/TEST_H__poblacion__SSP2__2050.xlsx"

cat("Reading", excel_path, "\n\n")

# Read datos sheet
datos <- read_excel(excel_path, sheet = "datos")
cat("Datos sheet:\n")
print(head(datos, 20))
cat("\nSummary:\n")
print(summary(datos$poblacion_personas))

# Count non-NA values
n_na <- sum(is.na(datos$poblacion_personas))
n_valid <- sum(!is.na(datos$poblacion_personas))
n_with_data <- sum(datos$poblacion_personas > 0, na.rm = TRUE)

cat("\nRow count:", nrow(datos), "\n")
cat("NA values:", n_na, "\n")
cat("Valid values:", n_valid, "\n")
cat("Values > 0:", n_with_data, "\n")
cat("Data type:", class(datos$poblacion_personas), "\n")

# Check a few non-NA values
if (n_valid > 0) {
  cat("\nFirst 5 non-NA values:\n")
  non_na_rows <- which(!is.na(datos$poblacion_personas) & datos$poblacion_personas > 0)[1:5]
  print(datos[non_na_rows, ])
}
