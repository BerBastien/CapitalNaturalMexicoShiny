library(readxl)

dat <- read_xlsx("H:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/H__poblacion__SSP2__2050.xlsx", sheet="datos")

cat("=== Excel Data Summary ===\n")
cat("Rows:", nrow(dat), "\n")
cat("Columns:", paste(names(dat), collapse=", "), "\n\n")

cat("=== poblacion_personas column ===\n")
cat("Class:", class(dat$poblacion_personas), "\n")
cat("NAs:", sum(is.na(dat$poblacion_personas)), "/", nrow(dat), "\n")
cat("Min:", min(dat$poblacion_personas, na.rm=T), "\n")
cat("Max:", max(dat$poblacion_personas, na.rm=T), "\n")
cat("Mean:", mean(dat$poblacion_personas, na.rm=T), "\n")
cat("Non-NA, non-zero:", sum(!is.na(dat$poblacion_personas) & dat$poblacion_personas > 0), "\n\n")

cat("First 20 values:\n")
print(dat$poblacion_personas[1:20])

cat("\nLast 20 values:\n")
print(tail(dat$poblacion_personas, 20))
