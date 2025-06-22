#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 


#### reading the data from google sheets########################################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1raYqt1BoLer6AP-bACSlEvHvxYcpnEGdKOXSew6oMSo/edit?gid=0#gid=0', sheet = 'taxa') |> 
  mutate(Latitude = "",
         Longitude = "")

taxa_pivot <- taxa |> 
  pivot_longer(cols = 5:ncol(taxa), names_to = "location", values_to = "obs") |> 
  select(name,location,obs) |> 
  drop_na() |> 
  left_join(locations, by = "location")

taxa_pivot_withdate <- taxa_pivot |> 
  pivot_longer(cols = date1:date7, names_to = "place", values_to = "date") |> 
  drop_na()
