library(readxl)
library(dplyr)

dat <- read_excel("h:/My Drive/CapitalNaturalMexicoShiny/data/indicadores/H__poblacion__SSP2__2050.xlsx", sheet=1)

# Check Mexico City hex
mc_hex <- dat %>% filter(hex_id == "HEX0040889")
cat("Mexico City hex (HEX0040889) population:", mc_hex$poblacion_personas, "\n")

# Count nonzero and zero
nonzero <- sum(dat$poblacion_personas > 0, na.rm=TRUE)
zero <- sum(dat$poblacion_personas == 0 | is.na(dat$poblacion_personas), na.rm=TRUE)
cat("\nNonzero cells:", nonzero, "\n")
cat("Zero/NA cells:", zero, "\n")
cat("Total:", nrow(dat), "\n")

# Top 10 cells
top10 <- dat %>% arrange(desc(poblacion_personas)) %>% head(10)
cat("\nTop 10 by population:\n")
print(top10)
