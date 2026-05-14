suppressPackageStartupMessages({library(readxl); library(dplyr)})
dir <- "data/indicadores"
files <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[!grepl("~\\$", files)]
parts <- strsplit(basename(files), "__")
keys <- sapply(parts, function(p) paste(p[1], p[2], sep = "__"))
reps <- files[!duplicated(keys)]
out <- file("metadata_dump.txt", "w", encoding = "UTF-8")
for (f in reps) {
  cat("\n===== FILE:", basename(f), "=====\n", file = out)
  tryCatch({
    sn <- excel_sheets(f)
    cat("SHEETS:", paste(sn, collapse = " | "), "\n", file = out)
    for (s in setdiff(sn, c("datos"))) {
      d <- suppressMessages(read_excel(f, sheet = s, col_names = FALSE))
      cat("--- Sheet:", s, "---\n", file = out)
      for (i in seq_len(nrow(d))) {
        r <- as.character(unlist(d[i, ]))
        r <- r[!is.na(r) & r != ""]
        if (length(r) > 0) cat(paste(r, collapse = " | "), "\n", file = out)
      }
    }
  }, error = function(e) cat("ERROR:", conditionMessage(e), "\n", file = out))
}
close(out)
cat("DONE\n")
