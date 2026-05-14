library(sf)
library(dplyr)

hex <- st_read("h:/My Drive/CapitalNaturalMexicoShiny/data/master_hex_10km.gpkg", layer="hex_10km", quiet=TRUE)

test_ids <- c("HEX0041123", "HEX0041210", "HEX0040864", "HEX0040950", "HEX0041037")

for (id in test_ids) {
  h <- hex %>% filter(.data$hex_id == id)
  if (nrow(h) > 0) {
    cat(sprintf("%s: tipo=%s zona=%s estado=%s\n", id, h$tipo[1], h$zona[1], h$estado[1]))
  }
}
