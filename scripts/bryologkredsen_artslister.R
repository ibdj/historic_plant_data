#### bryologkredsens artslister #########################################################

#### packages ##################################################################

library(googlesheets4)
library(tidyverse)
library(rgbif)
library(janitor)
library(fs)
library(stringdist)

#### importing #################################################################

artslister <- read_sheet('https://docs.google.com/spreadsheets/d/1sOgdCCPdk0qTtko0bapGjsNSxdyQULbShGpGfimuuqw/edit?gid=0#gid=0', sheet = 'taxa') |> 
  drop_na(lokalitet) |> 
  mutate(region = as.factor(region),
         area = as.factor(area),
         observer = as.factor(observer))

new_sheet <- gs4_create("my-sheet-name")
sheet_write(data = all_data, ss = new_sheet, sheet = "Sheet1")

names(artslister)

str(artslister)
summary(artslister)

# Define the directory path
dir_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/bryologkredsen/artslister_txt/"

# Get all CSV files matching your pattern
files <- dir_ls(dir_path, glob = "*.csv")

# Import and process all files
all_data <- files %>%
  set_names() %>%
  map_dfr(~{
    read_delim(.x, delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
      rename(taxon = 1) %>%
      mutate(filnavn = basename(.x) %>% 
               str_remove("\\.csv$"))
  })

all_data

stats <- all_data |> 
  group_by(filnavn) |> 
  reframe(count = n())

nrow(artslister)
mean(stats$count)
mean(stats$count)*nrow(artslister)

all_data_with_notes <- all_data |> 
  filter(!is.na(notes))

taxon_list <- all_data |> 
  distinct(taxon)

check <- all_data |> 
  filter(str_count(taxon, " ") > 1)

gbif_matched <- taxon_list |> 
  name_backbone_checklist("taxon") |> 
  mutate(kingdom = as.factor(kingdom))

summary(gbif_matched)

#### find typos ####
