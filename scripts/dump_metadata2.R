suppressPackageStartupMessages(library(readxl))
files <- c(
  "data/indicadores/H__huella_de_los_edificios__PRESENTE__2025.xlsx",
  "data/indicadores/H__poblacion__SSP2__2050.xlsx",
  "data/indicadores/H__producto_interno_bruto__PRESENTE__2020.xlsx",
  "data/indicadores/H__producto_interno_bruto_per_capita__PRESENTE__2020.xlsx",
  "data/indicadores/H__producto_interno_bruto__SSP2__2050.xlsx",
  "data/indicadores/E__uso_de_vegetacion_y_uso_suelo__Presente__2020.xlsx"
)
out <- file("metadata_extra.txt", "w", encoding = "UTF-8")
for (f in files) {
  cat("\n===== ", basename(f), " =====\n", file = out)
  sn <- excel_sheets(f)
  cat("SHEETS:", paste(sn, collapse = "|"), "\n", file = out)
  for (s in c("catalogo_variable", "metadatos")) {
    if (s %in% sn) {
      d <- suppressMessages(read_excel(f, sheet = s, col_names = FALSE))
      cat("-- ", s, " --\n", file = out)
      for (i in seq_len(nrow(d))) {
        r <- as.character(unlist(d[i, ]))
        r <- r[!is.na(r) & r != ""]
        if (length(r) > 0) cat(paste(r, collapse = " | "), "\n", file = out)
      }
    }
  }
}
close(out)
cat("done\n")
