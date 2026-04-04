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

taxa <- taxa |> 
  mutate(
    rank = coalesce(rank, ""),
    intraspecific = coalesce(intraspecific, ""),
    author = coalesce(author, ""),
    
    sci_name = str_squish(paste(
      genus,
      epitet,
      if_else(intraspecific != "" & rank != "",
              paste(rank, intraspecific), ""),
      author
    ))
  )

summary(taxa)

unique <- taxa |> 
  distinct(sci_name) |> 
  rename("name" = "sci_name")
