#### porsild hare island #####

#### packages #####

library(googlesheets4)
library(tidyverse)
install.packages("rgbif")
library(rgbif)


#### importing #####

porsild1910 <- read_sheet('https://docs.google.com/spreadsheets/d/1raYqt1BoLer6AP-bACSlEvHvxYcpnEGdKOXSew6oMSo/edit?gid=0#gid=0', sheet = 'taxa') 

matched_names <- porsild1910 |> name_backbone_checklist("taxon")
