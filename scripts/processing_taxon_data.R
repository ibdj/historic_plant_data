#### tbu taxon list ####

#### loading packages ####
if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

library(rgbif)
#### reading the file ####

xx <- read_sheet('https://docs.google.com/spreadsheets/d/1Zeqozv4IKLRNfixj5c8vwsZQf6BFwduJu7DyWTVSfsw/edit?gid=0#gid=0', sheet = 'taxa')
  
#### matching with gbif backbone ####

gbif_matched <- xx |> 
  gbif_species_name_match(name = "scientificname") 

gbif_matched_name_backbone_checklist <- xx |> 
  name_backbone_checklist("scientificname")

#### checking fussy taxa ####

not_matched <- gbif_matched_name_backbone_checklist |> 
  filter(is.na(speciesKey))

fussy <- gbif_matched |> 
  filter(matchType == "FUZZY",
         is.na(taxonomic_note)) |> 
  select(family...1,scientificname, scientificName,pub,page,)

#### organizing data for gbif ####
gbif_file <- gbif_matched |> 
  mutate(
    rank = rank...18 
  ) |> 
  select(
    scientificName,
    order,
    rank,
    family...1,
    family...28
  )

#### checking nas ####

nas <- gbif_matched |> 
  filter(is.na(scientificName))

nas_backbone_checklist <- gbif_matched_name_backbone_checklist |> 
  filter(matchType == "FUZZY")


#### adding geo information #####

add_geo <- function(df){
  df %>% 
    mutate(
      Geodetic_datum = "wgs84",
      continent = "EUROPE",
      higherGeographyID = "http://vocab.getty.edu/page/tgn/1000066",
      higherGeography = "Denmark",
      countryCode = "DA", 
      locationID = "TDWG:DEN-OO"
    )
}

#### adding the id function ####

add_id <- function(df){
  df %>% 
    mutate(
      id1 = "urn:tbu",
      id2 = random_id(nrow(.))
    ) %>% 
    unite("occurrenceID",id1:id2, sep = ":") 
}

#### writing the file ####

thedate <- strftime(Sys.Date(),"%Y_%m_%d")
#write_csv(tbu, paste0(thedate,"_tbu_taxon_list",".txt"))