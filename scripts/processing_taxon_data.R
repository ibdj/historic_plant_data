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