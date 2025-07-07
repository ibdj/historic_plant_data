#### porsild hare island #####

#### packages #####

library(googlesheets4)
library(tidyverse)
remotes::install_github("ropensci/rgbif")
library(rgbif)

packageVersion("rgbif")

#### importing #####

porsild1910 <- read_sheet('https://docs.google.com/spreadsheets/d/1raYqt1BoLer6AP-bACSlEvHvxYcpnEGdKOXSew6oMSo/edit?gid=0#gid=0', sheet = 'taxa') |> select("name")

matched_names <- name_backbone_checklist(porsild1910, "taxon")
sapply(matched_names, is.list)

nas <-  matched_names |> 
  filter(is.na(scientificName))

sheet_write(
  data = matched_names,
  ss = "https://docs.google.com/spreadsheets/d/1raYqt1BoLer6AP-bACSlEvHvxYcpnEGdKOXSew6oMSo/edit#gid=0",
  sheet = "matched_names"
)
