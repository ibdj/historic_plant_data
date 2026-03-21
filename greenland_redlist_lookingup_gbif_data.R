#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils, janitor) 

#### reading the data from google sheets ########################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1314126224#gid=1314126224', sheet = 'redlist_import', skip = 1) |>
  clean_names()

names(taxa)

redlist <- taxa |> 
  filter(international_kode %in% c("VU", "NT"))
