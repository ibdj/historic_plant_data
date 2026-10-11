# devold 1933 

#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")

devtools::install_github("inbo/inborutils")

pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

# Devold, J., & Scholander, P. F. (1933). Flowering plants and ferns of southeast Greenland. Skrifter Om Svalbard Og Ishavet, 56.
# fpafosg

taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1L5zXo7zEKJzuD0MnxbOfKX_pbBqbWMnRsuvFz2dhe_I/edit?gid=0#gid=0',
                   sheet = 'visits_locations', col_types = "c") |> 
  slice(-(1:3)) |> 
  distinct()

summary(taxa)


##### processing taxa ##########################################################
taxa_long <- taxa |> 
  pivot_longer(cols = `01`:last_col(),
               names_to = "taxa_number",
               values_to = "occurrence",
               values_drop_na = TRUE)
