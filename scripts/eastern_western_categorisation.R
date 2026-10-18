# west_east_index.R
# Classifies Greenland plant species by whether their range outside Greenland
# is mainly North American (west) or Eurasian (east).
#
# Method
#   - GBIF occurrences for every species in the list (Greenland excluded)
#   - Records binned into 100 x 100 km equal-area cells (Equal Earth projection)
#   - Side assigned by country: West = Northern + Central America + Caribbean,
#     East = Europe + Asia (incl. Iceland, Svalbard, all of Russia)
#   - Sampling effort: a cell counts as "sampled" if at least `min_target_spp`
#     species from the list were recorded there (target-group background)
#   - prop_W / prop_E = share of sampled cells the species occupies on each side
#     (corrects for both continent area and uneven sampling)
#   - Index = (prop_W - prop_E) / (prop_W + prop_E), from -1 (east) to +1 (west)
#
# Requires GBIF credentials in ~/.Renviron:
#   GBIF_USER=..., GBIF_PWD=..., GBIF_EMAIL=...
#   (open the file with usethis::edit_r_environ(), then restart R)

library(rgbif)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(countrycode)
library(CoordinateCleaner)
library(rnaturalearth)   # also needs rnaturalearthdata installed

# ---- Settings -------------------------------------------------------------
species_file      <- "greenland_species.csv"  # one column named 'species'
output_file       <- "west_east_index.csv"
cache_file        <- "gbif_occurrences.rds"   # delete to force a new download
cell_size_m       <- 100000                   # 100 km
min_target_spp    <- 5      # list species needed for a cell to count as sampled
min_cells         <- 5      # fewer occupied cells than this = insufficient data
threshold         <- 1/3    # |index| above this = western / eastern
max_uncertainty_m <- 50000  # drop records with coordinate uncertainty above this
run_cleaning      <- TRUE   # flag country centroids, capitals, herbaria, zeros

# ---- 1. Match names to the GBIF backbone ----------------------------------
spp <- read_csv(species_file, show_col_types = FALSE)

matched <- name_backbone_checklist(spp$species) |>
  select(input = verbatim_name, speciesKey, gbif_name = species,
         matchType, status) |>
  mutate(speciesKey = as.integer(speciesKey))

problems <- filter(matched, is.na(speciesKey) | matchType != "EXACT")
if (nrow(problems) > 0) {
  message("Check these name matches before trusting the results:")
  print(problems, n = Inf)
}

keys <- unique(na.omit(matched$speciesKey))

# ---- 2. Define West and East by country code ------------------------------
cl <- countrycode::codelist

west_cc <- cl$iso2c[cl$un.regionsub.name %in% "Northern America" |
                      cl$un.regionintermediate.name %in% c("Central America", "Caribbean")]
west_cc <- setdiff(na.omit(west_cc), "GL")   # Greenland excluded

east_cc <- cl$iso2c[cl$un.region.name %in% c("Europe", "Asia")]
east_cc <- union(na.omit(east_cc), c("XK", "TW"))  # Kosovo, Taiwan

# ---- 3. Download occurrences (cached) -------------------------------------
if (file.exists(cache_file)) {
  occ <- readRDS(cache_file)
} else {
  dl <- occ_download(
    pred_in("taxonKey", keys),
    pred_in("country", c(west_cc, east_cc)),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    pred("occurrenceStatus", "PRESENT"),
    pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))),
    pred_or(pred_lt("coordinateUncertaintyInMeters", max_uncertainty_m),
            pred_isnull("coordinateUncertaintyInMeters")),
    format = "SIMPLE_CSV"
  )
  message("GBIF download key (cite this DOI in publications): ", dl)
  occ_download_wait(dl)
  
  occ <- occ_download_get(dl, overwrite = TRUE) |>
    occ_download_import(select = c("speciesKey", "decimalLatitude",
                                   "decimalLongitude", "countryCode")) |>
    filter(!is.na(speciesKey))
  
  saveRDS(occ, cache_file)
}

occ <- mutate(occ, speciesKey = as.integer(speciesKey))

# ---- 4. Optional coordinate cleaning --------------------------------------
if (run_cleaning) {
  occ <- clean_coordinates(
    occ,
    lon = "decimalLongitude", lat = "decimalLatitude",
    species = "speciesKey",
    tests = c("capitals", "centroids", "institutions", "zeros", "equal"),
    value = "clean"
  )
}

# ---- 5. Grid in an equal-area projection ----------------------------------
xy <- sf_project(
  from = "EPSG:4326",
  to   = "+proj=eqearth +datum=WGS84 +units=m",
  pts  = cbind(occ$decimalLongitude, occ$decimalLatitude)
)

occ <- occ |>
  mutate(
    cell = paste(floor(xy[, 1] / cell_size_m), floor(xy[, 2] / cell_size_m)),
    side = if_else(countryCode %in% west_cc, "W", "E")
  )

cells <- distinct(occ, speciesKey, side, cell)

# ---- 6. Area correction: land cells available on each side ---------------
# Builds the same 100 km grid over the land of the West and East countries
# and counts how many cells touch land on each side.
eqearth <- "+proj=eqearth +datum=WGS84 +units=m"

land <- ne_countries(scale = 50, returnclass = "sf") |>
  mutate(side = case_when(iso_a2_eh %in% west_cc ~ "W",
                          iso_a2_eh %in% east_cc ~ "E")) |>
  filter(!is.na(side)) |>
  st_transform(eqearth) |>
  st_make_valid()

bb   <- st_bbox(land)
grid <- st_make_grid(land, cellsize = cell_size_m,
                     offset = floor(bb[c("xmin", "ymin")] / cell_size_m) * cell_size_m)
cent <- st_coordinates(st_centroid(grid))
hits <- st_intersects(grid, land)

land_cells <- tibble(
  cell = paste(floor(cent[, 1] / cell_size_m), floor(cent[, 2] / cell_size_m)),
  side = lapply(hits, function(i) unique(land$side[i]))
) |>
  unnest(side) |>
  distinct()

n_land <- count(land_cells, side, name = "n_land")
print(n_land)

# ---- 7. West-east index: share of land cells occupied on each side --------
result <- cells |>
  count(speciesKey, side, name = "n_occ") |>
  complete(speciesKey = keys, side = c("W", "E"), fill = list(n_occ = 0)) |>
  left_join(n_land, by = "side") |>
  mutate(prop = n_occ / n_land) |>
  pivot_wider(names_from = side, values_from = c(n_occ, n_land, prop)) |>
  mutate(
    index = (prop_W - prop_E) / (prop_W + prop_E),
    category = case_when(
      n_occ_W + n_occ_E < min_cells ~ "insufficient data",
      index >  threshold            ~ "western",
      index < -threshold            ~ "eastern",
      TRUE                          ~ "both"
    )
  )

# ---- Sampling-effort version (needs a list of many species) ---------------
# sampled <- cells |>
#   count(side, cell, name = "n_spp") |>
#   filter(n_spp >= min_target_spp)
# n_sampled <- count(sampled, side, name = "n_sampled")
#
# result <- cells |>
#   semi_join(sampled, by = c("side", "cell")) |>
#   count(speciesKey, side, name = "n_occ") |>
#   complete(speciesKey = keys, side = c("W", "E"), fill = list(n_occ = 0)) |>
#   left_join(n_sampled, by = "side") |>
#   mutate(prop = n_occ / n_sampled) |>
#   pivot_wider(names_from = side, values_from = c(n_occ, n_sampled, prop)) |>
#   mutate(
#     index = (prop_W - prop_E) / (prop_W + prop_E),
#     category = case_when(
#       n_occ_W + n_occ_E < min_cells ~ "insufficient data",
#       index >  threshold            ~ "western",
#       index < -threshold            ~ "eastern",
#       TRUE                          ~ "both"
#     )
#   )

out <- matched |>
  left_join(result, by = "speciesKey") |>
  arrange(index)

write_csv(out, output_file)