suppressPackageStartupMessages(library(raster))

tif_path <- "H:/My Drive/Data/SSPs/Gridded/Pop/SSP2/SPP2/SSP2_2020.tif"
r <- raster::raster(tif_path)

NCOLS_R <- raster::ncol(r)  # 43200
NROWS_R <- raster::nrow(r)  # 18720

# Known test points (city, lon, lat, expected high density)
cities <- list(
  list(name="Mexico City",     lon=-99.133, lat=19.433,  expected_high=TRUE),
  list(name="Guadalajara",     lon=-103.35, lat=20.667,  expected_high=TRUE),
  list(name="Monterrey",       lon=-100.32, lat=25.686,  expected_high=TRUE),
  list(name="San Luis Potosi", lon=-100.97, lat=22.15,   expected_high=TRUE),
  list(name="Open Pacific",    lon=-110.0,  lat=20.0,    expected_high=FALSE),
  list(name="Yucatan jungle",  lon=-89.0,   lat=18.0,    expected_high=FALSE)
)

cat("=== TESTING DIFFERENT FORMULA HYPOTHESES ===\n\n")

# Candidate formula: row = floor((lat_top - lat) / px_lat) + 1
#                   col = floor((lon - lon_left) / px_lon) + 1
# For square pixels: px_lat = px_lon = 360/NCOLS = 0.008333
# Hypothesis A: top=90, left=-180, px_lat = 180/18720 = 0.009615 (current code, non-square)
# Hypothesis B: top=90, left=-180, px_lat = 360/43200 = 0.008333 (square, 90 top)
# Hypothesis C: top=84, left=-180, px_lat = 0.008333 (square, 84 top)
# Hypothesis D: top=78, left=-180, px_lat = 0.008333 (square, 78 top, 18720*0.0083=156 deg)
# Hypothesis E: top=72, left=-180, px_lat = 0.008333

hypotheses <- list(
  A = list(name="A: top=90, px_lat=180/18720=0.009615 (non-square)", top=90, left=-180, px_lat=180/18720, px_lon=360/43200),
  B = list(name="B: top=90, px_lat=360/43200=0.008333 (square, 90N top)", top=90, left=-180, px_lat=360/43200, px_lon=360/43200),
  C = list(name="C: top=84, px_lat=0.008333 (square, 84N top -> covers -72 to 84)", top=84, left=-180, px_lat=360/43200, px_lon=360/43200),
  D = list(name="D: top=78, px_lat=0.008333 (square, covers -78 to 78)", top=78, left=-180, px_lat=360/43200, px_lon=360/43200),
  E = list(name="E: top=72, px_lat=0.008333 (square, covers -84 to 72)", top=72, left=-180, px_lat=360/43200, px_lon=360/43200)
)

for (hname in names(hypotheses)) {
  h <- hypotheses[[hname]]
  cat(h$name, "\n")
  cat(sprintf("  %-20s  %6s %6s  %8s\n", "City", "row", "col", "value"))
  
  for (city in cities) {
    col <- floor((city$lon - h$left) / h$px_lon) + 1L
    row <- floor((h$top  - city$lat) / h$px_lat) + 1L
    
    if (row < 1 || row > NROWS_R || col < 1 || col > NCOLS_R) {
      cat(sprintf("  %-20s  %6s %6s  %8s\n", city$name, "OOB", "OOB", "OOB"))
      next
    }
    
    val <- tryCatch(raster::getValues(r, row=row, nrows=1)[col], error=function(e) NA)
    cat(sprintf("  %-20s  %6d %6d  %8.2f  %s\n", city$name, row, col, val,
                if (city$expected_high) ifelse(val > 10, "OK-HIGH", "LOW?") else ""))
  }
  cat("\n")
}

# Extra: show the values in a 3-degree grid around Mexico to find where high values are
cat("=== SCANNING FOR HIGH VALUES IN MEXICO BOUNDING BOX ===\n")
cat("(sampling every 50 rows/cols within Mexico bbox)\n\n")

# Mexico bbox approx: lon -118 to -86, lat 14 to 33
# Under hypothesis A (current code):
h <- hypotheses[["A"]]
row_N <- floor((h$top - 33) / h$px_lat) + 1L  # 33°N
row_S <- floor((h$top - 14) / h$px_lat) + 1L  # 14°N
col_W <- floor((-118 - h$left) / h$px_lon) + 1L
col_E <- floor((-86  - h$left) / h$px_lon) + 1L
cat(sprintf("Hypothesis A rows %d-%d, cols %d-%d\n", row_N, row_S, col_W, col_E))

# Sample the block
block <- raster::getValuesBlock(r, row=row_N, nrows=row_S-row_N+1, col=col_W, ncols=col_E-col_W+1)
mat <- matrix(block, nrow=row_S-row_N+1, ncol=col_E-col_W+1, byrow=TRUE)

cat("Max value in Mexico bbox:", max(mat, na.rm=TRUE), "\n")
cat("Mean value (nonzero):", mean(mat[mat>0], na.rm=TRUE), "\n")
cat("Nonzero count:", sum(mat > 0, na.rm=TRUE), "\n")

# Find top 5 pixel locations
flat_top <- order(as.vector(mat), decreasing=TRUE)[1:5]
for (fi in flat_top) {
  ri <- ((fi-1) %/% ncol(mat)) + 1
  ci <- ((fi-1) %% ncol(mat)) + 1
  g_row <- row_N + ri - 1L
  g_col <- col_W + ci - 1L
  lat_c <- h$top - (g_row - 0.5) * h$px_lat
  lon_c <- h$left + (g_col - 0.5) * h$px_lon
  cat(sprintf("  row=%d col=%d -> lat=%.3f lon=%.3f  val=%.1f\n", g_row, g_col, lat_c, lon_c, mat[ri, ci]))
}
