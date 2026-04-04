#### greenlandic names #########################################################

#### packages ##################################################################

library(googlesheets4)
library(tidyverse)
library(rgbif)
library(janitor)

#### importing #################################################################

taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1sHTPbp2SjKqTGQ6SdDBFzdyOZTy5h647GgQnNU1TB98/edit?gid=0#gid=0', sheet = 'taxa') |> 
  clean_names()

names(taxa)

unique <- taxa |> 
  distinct(taxon) |> 
  rename("name" = "taxon")
