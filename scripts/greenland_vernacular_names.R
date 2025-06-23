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

#### seperating to one gl name pr row####

Nunatta_naasui$grl_ref1 <- trimws(Nunatta_naasui$grl_ref1)

max_splits <- max(sapply(strsplit(as.character(Nunatta_naasui$grl_ref1), ","), length))

col_names <- paste0("col", seq_len(max_splits))

seperate <- separate(Nunatta_naasui, grl_ref1 , into = col_names, sep = ",", fill = "right")

Nunatta_naasui_longer <- seperate |> 
  pivot_longer(cols = col1:col3, names_to = "placering", values_to = "grl_name", values_drop_na = TRUE)

Nunatta_naasui_longer$grl_name <- trimws(Nunatta_naasui_longer$grl_name) 

filtered <- Nunatta_naasui_longer |> 
  filter(!is.na("grl_name")) 

#### match to gbif ####

gbif_matched <- unique |> 
  name_backbone_checklist("name") |> 
  mutate(matchType = as.factor(matchType),
         kingdom = as.factor(kingdom)
         )

summary(gbif_matched)

#### joining the gbid match and data file ####
joined <- filtered |> 
  mutate(scientificName = taxon) |> 
  left_join(gbif_matched, by = "scientificName")

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
