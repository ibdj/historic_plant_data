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
  mutate(matchType = as.factor(matchType))

#### list of colomns in the extension of vernacular names #####

#https://rs.gbif.org/extension/gbif/1.0/vernacularname.xml

# scientificName
# vernacularName
# source
# language
# temporal
# locationID
# locality
# sex
# lifeStage
# isPlural
# isPreferredName
# organismPart
# taxonRemarks
# datasetID
