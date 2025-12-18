#### packages #####

library(googlesheets4)
library(tidyverse)
remotes::install_github("ropensci/rgbif")
library(rgbif)

packageVersion("rgbif")

#### reading the data from google sheets ########################################################################

taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1gjfmwksTCL645VqNmodrJ4PArO51OhijhQCh_dJHLVo/edit?gid=0#gid=0', sheet = 'taxa') |> 
  mutate(across(Ivigtut:Frederiksdal, as.character))

names(taxa)

taxa_long <- taxa |> 
  pivot_longer(cols = Ivigtut:Frederiksdal, names_to = "location", values_to = "year") |> 
  mutate(year = as.character(year))

head(taxa)
str(taxa)

max_n_years <- taxa_long %>%
  mutate(n_years = str_count(.data$year, ",") + 1) %>%
  summarise(max_n = max(n_years, na.rm = TRUE)) %>%
  pull(max_n)

new_names <- paste0("year_", seq_len(max_n_years))

taxa_long <- taxa_long %>%
  separate(year, into = new_names, sep = ",", fill = "right", extra = "drop")

taxa_long <- taxa_long %>%
  pivot_longer(cols = year_1:year_4, names_to = "pos", values_to = "year")

taxa_long$year <- trimws(taxa_long$year)

taxa_clean <- taxa_long |> 
  filter(!is.na(year), year != "NULL")
