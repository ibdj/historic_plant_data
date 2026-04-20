#### bryologkredsens artslister #########################################################

#### packages ##################################################################

library(googlesheets4)
library(tidyverse)
library(rgbif)
library(janitor)
library(fs)
library(stringdist) # to find potential typos

#### importing #################################################################

meta_data <- read_sheet('https://docs.google.com/spreadsheets/d/1sOgdCCPdk0qTtko0bapGjsNSxdyQULbShGpGfimuuqw/edit?gid=0#gid=0', sheet = 'meta') |> 
  drop_na(lokalitet) |> 
  mutate(region = as.factor(region),
         area = as.factor(area),
         observer = as.factor(observer))

koordinator <- read_sheet('https://docs.google.com/spreadsheets/d/1sOgdCCPdk0qTtko0bapGjsNSxdyQULbShGpGfimuuqw/edit?gid=0#gid=0', sheet = 'koordinator') |> 
  #drop_na(lokalitet) |> 
  mutate()

names(meta_data)

lokaliteter <- meta_data |> 
  group_by(lokalitetsnavn,lat) |> 
  reframe(count = n())

str(meta_data)
summary(meta_data)

# Define the directory path
dir_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/bryologkredsen/artslister_txt/"

# Get all CSV files matching your pattern
files <- dir_ls(dir_path, glob = "*.csv")

# Import and process all files
all_data <- files %>%
  set_names() %>%
  map_dfr(~{
    read.csv(
      .x, 
      sep = ";", 
      fill = TRUE,           # This works in read.csv()
      stringsAsFactors = FALSE,
      strip.white = TRUE     # Equivalent to trim_ws
    ) %>%
      rename(taxon = 1) %>%
      mutate(tur_id = basename(.x) %>% 
               str_remove("\\.csv$"))
  }) |> 
  mutate(
    # Updated regex pattern to include 'sect' and 'subsect'
    match_result = str_match(
      str_squish(taxon),
      "^([[:alpha:]]+)(?:\\s+([[:alpha:]-]+))?(?:\\s+(spp?\\.?))?(?:\\s+(ssp|subsp|var|f|fo|sect|subsect)\\.?)?(?:\\s+([[:alpha:]]+))?$"
    )
  ) |> 
  mutate(
    genus = match_result[, 2],
    epithet = match_result[, 3],
    rank = case_when(
      !is.na(match_result[, 4]) ~ match_result[, 4],  # sp. or spp.
      !is.na(match_result[, 5]) ~ match_result[, 5],  # ssp, var, f, fo, sect, subsect
      TRUE ~ NA_character_
    ),
    infraspecific_epithet = match_result[, 6]
  ) |> 
  mutate(
    rank = str_to_lower(rank),
    # Fill empty rank cells with "species"
    rank = if_else(is.na(rank), "species", rank)
  ) |> 
  mutate(taxon = paste0(genus, ifelse(is.na(epithet), "", paste0(" ", epithet)))) |>
  mutate(across(everything(), ~ifelse(trimws(.) == "", NA, .))) |> 
  rename(habitat_species = habitat)

all_data

names(all_data)

split <- all_data |> 
  mutate(
    # Updated regex pattern to include 'sect' and 'subsect'
    match_result = str_match(
      str_squish(taxon),
      "^([[:alpha:]]+)(?:\\s+([[:alpha:]-]+))?(?:\\s+(spp?\\.?))?(?:\\s+(ssp|subsp|var|f|fo|sect|subsect)\\.?)?(?:\\s+([[:alpha:]]+))?$"
    )
  ) |> 
  mutate(
    genus = match_result[, 2],
    epithet = match_result[, 3],
    rank = case_when(
      !is.na(match_result[, 4]) ~ match_result[, 4],  # sp. or spp.
      !is.na(match_result[, 5]) ~ match_result[, 5],  # ssp, var, f, fo, sect, subsect
      TRUE ~ NA_character_
    ),
    infraspecific_epithet = match_result[, 6]
  ) |> 
  mutate(
    rank = str_to_lower(rank),
    # Fill empty rank cells with "species"
    rank = if_else(is.na(rank), "species", rank)
  ) |> 
  select(taxon, syn, genus, epithet, rank, notes, infraspecific_epithet, filnavn) |> 
  left_join(meta_data, by = "filnavn") |> 
  left_join(koordinator, by = "lokalitetsnavn")

taxon_list <- all_data |> 
  group_by(taxon) |> 
  reframe(count = n()) |> 
  drop_na()

stats <- all_data |> 
  group_by(filnavn) |> 
  reframe(count = n())

nrow(meta_data)
mean(stats$count)
mean(stats$count)*nrow(meta_data)

all_data_with_notes <- all_data |> 
  filter(!is.na(notes))

check <- all_data |> 
  filter(str_count(taxon, " ") > 1)

# Assuming taxon_list has a column called 'taxon' with scientific names
gbif_matched <- taxon_list |> 
  rowwise() |> 
  mutate(
    backbone_data = list(name_backbone(taxon))
  ) |> 
  unnest_wider(backbone_data, names_sep = "_")

summary(gbif_matched)

#### find typos ################################################################

# Function to find typoes (calculating how many characters it would take to change)
compare_column_typos <- function(df, col_name, threshold = 0.8) {
  # Get the column as character vector
  col_data <- df[[col_name]] |> as.character()
  
  # Generate all unique pairs
  pairs <- expand.grid(
    row_i = seq_along(col_data),
    row_j = seq_along(col_data)
  ) |> 
    filter(row_i < row_j) |>  # Unique pairs, no self-comparison
    rowwise() |> 
    mutate(
      text_i = col_data[row_i],
      text_j = col_data[row_j],
      lev_dist = stringdist(text_i, text_j, method = "lv"),
      similarity = 1 - (lev_dist / max(nchar(text_i), nchar(text_j), 1))
    ) |> 
    ungroup()
  
  # Return pairs above threshold (likely typos)
  pairs |> 
    filter(similarity >= threshold, similarity < 1) |> 
    arrange(desc(similarity))
}

# Find typoes with the function above
typos <- compare_column_typos(taxon_list, "taxon", threshold = 0.8)
typos

matches <- all_data[all_data$taxon == "Chilocsyphus latifolius", ]
matches

matches2 <- all_data[grepl("Syntrichia ruralis/calcicola", all_data$taxon), ]
matches2

matches3 <- all_data[grepl("2002-10-05-Lønborg Hede-Mergelgrav ved Lø", all_data$filnavn), ]
matches3

#### splitting to rank #########################################################
split <- taxon_list |> 
  mutate(
    # Updated regex pattern to include 'sect' and 'subsect'
    match_result = str_match(
      str_squish(taxon),
      "^([[:alpha:]]+)(?:\\s+([[:alpha:]-]+))?(?:\\s+(spp?\\.?))?(?:\\s+(ssp|subsp|var|f|fo|sect|subsect)\\.?)?(?:\\s+([[:alpha:]]+))?$"
    )
  ) |> 
  mutate(
    genus = match_result[, 2],
    epithet = match_result[, 3],
    rank = case_when(
      !is.na(match_result[, 4]) ~ match_result[, 4],  # sp. or spp.
      !is.na(match_result[, 5]) ~ match_result[, 5],  # ssp, var, f, fo, sect, subsect
      TRUE ~ NA_character_
    ),
    infraspecific_epithet = match_result[, 6]
  ) |> 
  mutate(
    rank = str_to_lower(rank),
    # Fill empty rank cells with "species"
    rank = if_else(is.na(rank), "species", rank)
  ) |> 
  select(taxon, genus, epithet, rank, infraspecific_epithet)

taxonlist2 <- split |> 
  mutate(taxon = paste0(genus, ifelse(is.na(epithet), "", paste0(" ", epithet)))) |> 
  select(taxon)

gbif_matched2 <- taxonlist2 |> 
  rowwise() |> 
  mutate(
    backbone_data = list(name_backbone(taxon))
  ) |> 
  unnest_wider(backbone_data, names_sep = "_")

joined_all <- all_data |> 
  left_join(meta_data, by = "filnavn") |> 
  left_join(gbif_matched2, by = "taxon") |> 
  distinct()
