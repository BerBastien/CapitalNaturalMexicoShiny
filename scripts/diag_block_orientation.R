suppressPackageStartupMessages({
  library(sf)
  library(raster)
})

hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg", layer = "hex_10km", quiet = TRUE)
r <- raster::raster("H:/My Drive/Data/SSPs/Gridded/Pop/SSP2/SPP2/SSP2_2050.tif")

PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 180 / 18720

all_bb <- st_bbox(hex)
g_col_min <- max(1L, as.integer(floor((all_bb[["xmin"]] + 180) / PX_RES_LON)))
g_col_max <- min(43200L, as.integer(ceiling((all_bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((90 - all_bb[["ymax"]]) / PX_RES_LAT)))
g_row_max <- min(18720L, as.integer(ceiling((90 - all_bb[["ymin"]]) / PX_RES_LAT)) + 1L)

n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L

cat(sprintf("Block rows %d-%d cols %d-%d (%d x %d)\n", g_row_min, g_row_max, g_col_min, g_col_max, n_rb, n_cb))

vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb, col = g_col_min, ncols = n_cb)
mat_byrow_true <- matrix(vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
mat_byrow_false <- matrix(vals, nrow = n_rb, ncol = n_cb, byrow = FALSE)

# test coordinates (Mexico City, Guadalajara, Acapulco, Pacific point)
test_pts <- data.frame(
  name = c("Mexico City", "Guadalajara", "Acapulco", "Pacific"),
  lon = c(-99.13, -103.35, -99.90, -99.02),
  lat = c(19.43, 20.66, 16.85, 15.40),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(test_pts))) {
  lon <- test_pts$lon[i]; lat <- test_pts$lat[i]
  gc <- as.integer(floor((lon + 180) / PX_RES_LON)) + 1L
  gr <- as.integer(floor((90 - lat) / PX_RES_LAT)) + 1L

  bc <- gc - g_col_min + 1L
  br <- gr - g_row_min + 1L

  direct <- raster::getValuesBlock(r, row = gr, nrows = 1, col = gc, ncols = 1)
  v_true <- if (bc >= 1 && bc <= n_cb && br >= 1 && br <= n_rb) mat_byrow_true[br, bc] else NA
  v_false <- if (bc >= 1 && bc <= n_cb && br >= 1 && br <= n_rb) mat_byrow_false[br, bc] else NA

  cat(sprintf("\n%s\n", test_pts$name[i]))
  cat(sprintf("  global row=%d col=%d | local br=%d bc=%d\n", gr, gc, br, bc))
  cat(sprintf("  direct=%g | byrow=TRUE %g | byrow=FALSE %g\n", direct, v_true, v_false))
}

# random consistency checks
set.seed(42)
idx <- sample.int(n_rb * n_cb, 20)
err_true <- 0L
err_false <- 0L
for (k in idx) {
  br <- ((k - 1L) %/% n_cb) + 1L
  bc <- ((k - 1L) %% n_cb) + 1L
  gr <- g_row_min + br - 1L
  gc <- g_col_min + bc - 1L
  direct <- raster::getValuesBlock(r, row = gr, nrows = 1, col = gc, ncols = 1)
  a <- mat_byrow_true[br, bc]
  b <- mat_byrow_false[br, bc]
  if (!(is.na(direct) && is.na(a)) && !isTRUE(all.equal(as.numeric(direct), as.numeric(a)))) err_true <- err_true + 1L
  if (!(is.na(direct) && is.na(b)) && !isTRUE(all.equal(as.numeric(direct), as.numeric(b)))) err_false <- err_false + 1L
}
cat(sprintf("\nRandom check mismatches: byrow=TRUE %d /20, byrow=FALSE %d /20\n", err_true, err_false))
