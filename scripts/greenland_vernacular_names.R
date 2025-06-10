#### greenlandic names #####

#### packages #####

library(googlesheets4)
library(tidyverse)
library(rgbif)

#### importing #####

Nunatta_naasui <- read_sheet('https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1576015470#gid=1576015470', sheet = 'Nunatta_naasui')

names(Nunatta_naasui)

unique <- Nunatta_naasui |> 
  distinct(taxon) |> 
  rename("name" = "taxon")

gbif_matched <- unique |> 
  name_backbone_checklist("name") |> 
  mutate(matchType = as.factor(matchType),
         kingdom = as.factor(kingdom)
         )

summary(gbif_matched)

#### list of colomns in the extension of vernacular names #####
#https://rs.gbif.org/extension/gbif/1.0/vernacularname.xml
# these are the colomns to be included for the extension of vernacularnames. 
# I have prioritised the list. First section is the ones I think will be included.

# scientificName
# vernacularName
# source
# language
# organismPart
# taxonRemarks

# isPlural

# temporal
# locationID
# locality
# sex
# lifeStage
# isPreferredName
# datasetID
