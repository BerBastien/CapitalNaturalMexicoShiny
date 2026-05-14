suppressPackageStartupMessages({
  library(sf)
  library(raster)
  library(terra)
  library(sp)
})

hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg", layer = "hex_10km", quiet = TRUE)
tif <- "H:/My Drive/Data/SSPs/Gridded/Pop/SSP2/SPP2/SSP2_2050.tif"
r <- raster::raster(tif)
rt <- terra::rast(tif)
# set true geographic extent for terra extraction
terra::ext(rt) <- terra::ext(-180, 180, -90, 90)
terra::crs(rt) <- "EPSG:4326"

PX_RES_LON <- 360 / 43200
PX_RES_LAT <- 180 / 18720
NCOLS_R <- 43200L
NROWS_R <- 18720L

all_bb <- sf::st_bbox(hex)
g_col_min <- max(1L, as.integer(floor((all_bb[["xmin"]] + 180) / PX_RES_LON)))
g_col_max <- min(NCOLS_R, as.integer(ceiling((all_bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
g_row_min <- max(1L, as.integer(floor((90 - all_bb[["ymax"]]) / PX_RES_LAT)))
g_row_max <- min(NROWS_R, as.integer(ceiling((90 - all_bb[["ymin"]]) / PX_RES_LAT)) + 1L)

n_rb <- g_row_max - g_row_min + 1L
n_cb <- g_col_max - g_col_min + 1L

vals <- raster::getValuesBlock(r, row = g_row_min, nrows = n_rb, col = g_col_min, ncols = n_cb)
block_mat <- matrix(vals, nrow = n_rb, ncol = n_cb, byrow = TRUE)
block_lons <- -180 + (seq.int(g_col_min, g_col_max) - 0.5) * PX_RES_LON

sum_hex_block <- function(h) {
  coords <- st_coordinates(st_geometry(h)[[1]])
  if ("L1" %in% colnames(coords)) coords <- coords[coords[, "L1"] == 1L, , drop = FALSE]
  x_poly <- as.numeric(coords[, "X"])
  y_poly <- as.numeric(coords[, "Y"])
  bb <- st_bbox(h)

  col_min <- max(1L, as.integer(floor((bb[["xmin"]] + 180) / PX_RES_LON)) + 1L)
  col_max <- min(NCOLS_R, as.integer(ceiling((bb[["xmax"]] + 180) / PX_RES_LON)) + 1L)
  row_min <- max(1L, as.integer(floor((90 - bb[["ymax"]]) / PX_RES_LAT)) + 1L)
  row_max <- min(NROWS_R, as.integer(ceiling((90 - bb[["ymin"]]) / PX_RES_LAT)) + 1L)

  bc_min <- max(1L, col_min - g_col_min + 1L)
  bc_max <- min(n_cb, col_max - g_col_min + 1L)
  br_min <- max(1L, row_min - g_row_min + 1L)
  br_max <- min(n_rb, row_max - g_row_min + 1L)
  if (bc_min > bc_max || br_min > br_max) return(0)

  local_cols <- seq.int(bc_min, bc_max)
  lons <- block_lons[local_cols]
  total <- 0
  for (br in seq.int(br_min, br_max)) {
    lat_c <- 90 - (g_row_min + br - 1L - 0.5) * PX_RES_LAT
    pip <- sp::point.in.polygon(lons, rep(lat_c, length(lons)), x_poly, y_poly)
    inside <- pip > 0L
    if (!any(inside)) next
    total <- total + sum(block_mat[br, local_cols][inside], na.rm = TRUE)
  }
  as.numeric(total)
}

# sample target hexes
# continental around Mexico City
mc_pt <- st_sfc(st_point(c(-99.13, 19.43)), crs = 4326)
mc_hex <- hex[st_contains(hex, mc_pt, sparse = FALSE)[,1], ]

# ocean hex from earlier issue
ocean_hex <- hex[hex$hex_id == "HEX0041123", ]

# top continental hex in current workbook (after marine-zero pass)
ref_hex <- hex[hex$hex_id == "HEX0038423", ]

cands <- rbind(mc_hex, ocean_hex, ref_hex)

cat("Comparing methods for", nrow(cands), "hexes\n")
for (i in seq_len(nrow(cands))) {
  h <- cands[i, ]
  hid <- as.character(h$hex_id)
  tipo <- as.character(h$tipo)

  v_block <- sum_hex_block(h)
  vt <- terra::extract(rt, terra::vect(h), fun = sum, na.rm = TRUE)
  v_terra <- as.numeric(vt[1,2])

  cat(sprintf("%s | tipo=%s | block=%0.2f | terra_sum=%0.2f | diff=%0.2f\n",
              hid, tipo, v_block, v_terra, v_block - v_terra))
}
