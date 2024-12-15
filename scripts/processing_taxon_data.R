#### tbu taxon list ####

#### loading packages ####
install.packages("pacman")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, inborutils) 

#### reading the file ####

xx <- read_sheet('https://docs.google.com/spreadsheets/d/1enf76zz9EBApZVkqWaT1F826Y46OJ5NTKqB8nbGrtug/edit#gid=1166476565', sheet = '', skip = 1)

#### matching with gbif backbone ####

#### writing the file ####

thedate <- strftime(Sys.Date(),"%Y_%m_%d")
#write_csv(ginr_herbarium_ipt, paste0(thedate,"_ginr_herbarium_ipt",".txt"))