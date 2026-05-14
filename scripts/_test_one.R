# Quick test: extract SSP2 2020, check Mexico City hex
suppressPackageStartupMessages({
  library(sf); library(raster); library(sp); library(dplyr); library(openxlsx)
})

source("h:/My Drive/CapitalNaturalMexicoShiny/scripts/regenerate_all_population.R", local=TRUE)
