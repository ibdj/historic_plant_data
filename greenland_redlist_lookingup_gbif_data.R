#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils, janitor,ggplot2,raster) 

#### reading the data from google sheets ########################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1314126224#gid=1314126224', sheet = 'redlist_import', skip = 1) |>
  clean_names()

names(taxa)

redlist <- taxa |> 
  filter(international_kode %in% c("VU", "NT"))

names(redlist)

taxon_keys <- redlist$gbif_taxon_key |> 
  as.numeric() |> 
  na.omit() |> 
  as.list()

#### reading the data from qgis ################################################


#### setting up gbif ###########################################################
# Read credentials from environment variables
user <- Sys.getenv("GBIF_USER")
pwd <- Sys.getenv("GBIF_PWD")
email <- Sys.getenv("GBIF_EMAIL")

# Optional: Safety check to ensure they aren't empty
if (user == "" || pwd == "") {
  stop("GBIF credentials missing. Please check your .Renviron file.")
}

#### starting the request #########################################################

taxon_keys_vec <- unlist(taxon_keys)

download_key <- occ_download(
  pred_in("taxonKey", taxon_keys_vec),
  pred("country", "GL"),
  format = "SIMPLE_CSV"
)

occ_download_meta(download_key)

#### downloading the data #####
occ_download_get(download_key, path = "data")

# Unzip the file
unzip("data/0053643-260226173443078.zip", exdir = "data")

gbif_data <- read_delim("data/0053643-260226173443078.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

#### checking the data #####
coord_summary <- table(
  has_coords = !is.na(gbif_data$decimalLatitude) & !is.na(gbif_data$decimalLongitude)
)
coord_summary

species_counts <- gbif_data %>%
  group_by(species) %>%
  summarise(n_occurrences = n()) %>%
  arrange(desc(n_occurrences))

species_counts

#### quick map ####
gbif_coords <- gbif_data[!is.na(gbif_data$decimalLatitude) & !is.na(gbif_data$decimalLongitude), ]

# Quick map
ggplot(gbif_coords, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point(alpha = 0.5, color = "blue") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "GBIF Occurrences with Coordinates (Greenland)"
  )

#### refined map ####

gbif_coords <- gbif_coords |>
  dplyr::filter(!is.na(decimalLongitude),
                !is.na(decimalLatitude)) |>
  sf::st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326,         # WGS84
    remove = FALSE
  )

gbif_coords <- sf::st_transform(gbif_coords, 32622)

grl <- sf::st_read("/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/botany/r/r_occurrence/grl_land_only.gpkg")
grl <- sf::st_transform(grl, 32622)

ggplot() +
  #geom_sf(data = grl, fill = "brown", color = NA, alpha = 0.3) +
  geom_sf(data = gbif_coords, aes(color = species), size = 1, show.legend = FALSE) +
  theme_minimal()

#### map loop ####
# Get the unique species names
species_list <- unique(gbif_coords$species)
r <- rasterize(grl, raster(extent(grl), ncol=1200, nrow=1200), field=1)

# Loop over each species
for (sp in species_list) {
  
  # Filter data for this species
  sp_data <- gbif_coords %>% filter(species == sp)
  
  # Create the plot
  p <- ggplot() +
    #geom_sf(data = grl, fill = "brown", color = NA, alpha = 0.3) +
    geom_raster(data = as.data.frame(r, xy=TRUE), aes(x=x, y=y, fill=factor(layer)), show.legend = FALSE) +
    scale_fill_manual(
      values = c("1" = "#B0B0B0"),   # your gray for cells with value 1
      na.value = NA                   # make NA transparent
    ) +
    geom_sf(data = sp_data, color = "blue", size = 1) +
    theme_minimal() +
    labs(title = sp)
  
  # Print the plot
  print(p)}
