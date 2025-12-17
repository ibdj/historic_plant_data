#### packages #####

library(googlesheets4)
library(tidyverse)
remotes::install_github("ropensci/rgbif")
library(rgbif)

packageVersion("rgbif")

#### reading the data from google sheets ########################################################################

taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1gjfmwksTCL645VqNmodrJ4PArO51OhijhQCh_dJHLVo/edit?gid=0#gid=0', sheet = 'taxa') |> 
  mutate(across(Ivigtut:Upernavik, as.character))

names(taxa)

taxa_long <- taxa |> 
  pivot_longer(cols = Ivigtut:Upernavik, names_to = "location", values_to = "year") |> 
  mutate(year = as.character(year))

head(taxa)
str(taxa)

max_n_years <- taxa %>%
  mutate(n_years = str_count(year, "," ) + 1) %>%
  summarise(max_n = max(n_years, na.rm = TRUE)) %>%
  pull(max_n)
