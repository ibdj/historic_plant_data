#### tbu taxon list ####

#### loading packages ####
install.packages("pacman")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools) 
devtools::install_github("inbo/inborutils")
library(inborutils)

#### reading the file ####

xx <- read_sheet('https://docs.google.com/spreadsheets/d/1Zeqozv4IKLRNfixj5c8vwsZQf6BFwduJu7DyWTVSfsw/edit?gid=0#gid=0', sheet = 'taxa')
  
#### matching with gbif backbone ####

gbif_matched <- xx |> 
  gbif_species_name_match(name = "scientificname") 

#### checking fussy taxa ####

fussy <- gbif_matched |> 
  filter(matchType == "FUZZY") |> 
  select(family...1,scientificname, scientificName,pub,page,)

#### writing the file ####

thedate <- strftime(Sys.Date(),"%Y_%m_%d")
#write_csv(ginr_herbarium_ipt, paste0(thedate,"_ginr_herbarium_ipt",".txt"))