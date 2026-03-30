#### bryologkredsens artslister #########################################################

#### packages ##################################################################

library(googlesheets4)
library(tidyverse)
library(rgbif)
library(janitor)
library(fs)
library(stringdist)

#### importing #################################################################

meta_data <- read_sheet('https://docs.google.com/spreadsheets/d/1sOgdCCPdk0qTtko0bapGjsNSxdyQULbShGpGfimuuqw/edit?gid=0#gid=0', sheet = 'taxa') |> 
  drop_na(lokalitet) |> 
  mutate(region = as.factor(region),
         area = as.factor(area),
         observer = as.factor(observer))

#new_sheet <- gs4_create("my-sheet-name")
#sheet_write(data = all_data, ss = new_sheet, sheet = "Sheet1")

names(meta_data)

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
      mutate(filnavn = basename(.x) %>% 
               str_remove("\\.csv$"))
  })

all_data

taxon_list <- all_data |> 
  group_by(taxon) |> 
  reframe(count = n())

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

gbif_matched <- taxon_list |> 
  name_backbone_checklist("taxon") |> 
  mutate(kingdom = as.factor(kingdom))

summary(gbif_matched)

#### find typos ################################################################

# Assuming your column is called 'text' in dataframe all_data
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

# Run it
typos <- compare_column_typos(taxon_list, "taxon", threshold = 0.8)
typos

matches <- all_data[all_data$taxon == "Ptilidium pulchrerrimum", ]
matches

matches2 <- all_data[grepl("^Amblystegium varium / Hygroa", all_data$taxon), ]
matches2

matches3 <- all_data[grepl("2002-10-05-Lønborg Hede-Mergelgrav ved Lø", all_data$filnavn), ]
matches3
