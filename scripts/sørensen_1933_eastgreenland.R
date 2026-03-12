#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 


#### reading the data from google sheets ########################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1S6pmV9dO5dKpk1sDENTa5-XmCY41WaJcFRCJLYRG8xg/edit?gid=768492226#gid=768492226', sheet = 'taxa')

taxa_pivot <- taxa |> 
  pivot_longer(cols = 5:ncol(taxa), names_to = "verbatimLocation", values_to = "presence") |> 
  select(scientificName,verbatimLocation,presence) |> 
  drop_na()

loc <- read_sheet('https://docs.google.com/spreadsheets/d/1S6pmV9dO5dKpk1sDENTa5-XmCY41WaJcFRCJLYRG8xg/edit?gid=768492226#gid=768492226', sheet = 'loc')

taxa_coordinates <- taxa_pivot |> 
  left_join(loc, by = "verbatimLocation")

#### matching with gbif ####
