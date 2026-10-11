# devold 1933 

#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")

devtools::install_github("inbo/inborutils")

pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

# Devold, J., & Scholander, P. F. (1933). Flowering plants and ferns of southeast Greenland. Skrifter Om Svalbard Og Ishavet, 56.
# fpafosg

visits <- read_sheet('https://docs.google.com/spreadsheets/d/1L5zXo7zEKJzuD0MnxbOfKX_pbBqbWMnRsuvFz2dhe_I/edit?gid=0#gid=0',
                   sheet = 'visits_locations', col_types = "c") |> 
  slice(-(1:3)) |> 
  distinct()

locations <- read_sheet('https://docs.google.com/spreadsheets/d/1L5zXo7zEKJzuD0MnxbOfKX_pbBqbWMnRsuvFz2dhe_I/edit?gid=0#gid=0',
                                sheet = 'locations')

taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1L5zXo7zEKJzuD0MnxbOfKX_pbBqbWMnRsuvFz2dhe_I/edit?gid=0#gid=0',
                   sheet = 'taxa') |> 
  mutate(rank = as.factor(rank))

summary(taxa)

##### processing taxa ##########################################################

taxa_long <- taxa |> 
  pivot_longer(cols = `01`:last_col(),
               names_to = "taxa_number",
               values_to = "occurrence",
               values_drop_na = TRUE)


#### matching to GBIF ###################################################################################################################

summary(taxa)

taxa <- taxa |> 
  mutate(verbatimName = if_else(
    rank %in% c("var.", "subsp.", "f."),
    paste(genus, specificepithet, rank, intraspecifikepithet, coalesce(author_intra, "")),
    paste(genus, specificepithet, coalesce(author, ""))
  ) |> str_squish())

unique <- taxa |> 
  select(verbatimName) |> 
  distinct(verbatimName)
  
gbif_matchedlist <- unique |> 
    name_backbone_checklist("name") |> 
    mutate(matchType = as.factor(matchType))

not_gbif_matchedlist <- gbif_matchedlist |> 
  filter(is.na(scientificName))
