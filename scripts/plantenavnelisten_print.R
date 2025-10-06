#### ####
library(readxl)
library(tidyverse)
library(writexl)

df <- read_excel('/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/DBF navneliste 03-10-2025.xlsx')


# Read the file
file_path <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/DBF navneliste 03-10-2025.xlsx"
df <- read_excel(file_path) |> 
  mutate(n_filled = rowSums(across(everything(), ~ !is.na(.x))), 
         index = row_number())


df_clean <- df %>%
  # Identify Latin genus names (no space)
  mutate(genus_marker = ifelse(
    !is.na(`Videnskabeligt navn`) & !grepl(" ", `Videnskabeligt navn`),
    `Videnskabeligt navn`,
    NA
  )) %>%
  # Identify Danish genus names (ending with "slægten")
  mutate(dansk_slaegt_marker = ifelse(
    grepl("slægten$", `Accepterede danske navne`),
    `Accepterede danske navne`,
    NA
  )) %>%
  # Fill both downwards
  tidyr::fill(genus_marker, dansk_slaegt_marker, .direction = "down") %>%
  # Rename for clarity
  mutate(
    rank = case_when(
      grepl("slægten", `Accepterede danske navne`, ignore.case = TRUE) ~ "slægt",
      grepl("var\\.", `Videnskabeligt navn`, ignore.case = TRUE) ~ "varitet",
      grepl("subsp\\.", `Videnskabeligt navn`, ignore.case = TRUE) ~ "underart",
      grepl("×", `Videnskabeligt navn`) ~ "hybrid",
      TRUE ~ "art"
    )
  ) |> 
  rename(genus = genus_marker,
         `Dansk slægt` = dansk_slaegt_marker) |> 
  select(`Accepterede danske navne`, `Videnskabeligt navn`,`Dansk slægt`,rank,genus,index,n_filled) |> 
  #filter(!(is.na(`Accepterede danske navne`) & is.na(`Videnskabeligt navn`)))
  filter(!is.na(`Accepterede danske navne`))

# Define an output path
output_path <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/DBF_navneliste_print.xlsx"

# Write to Excel
write_xlsx(df_clean, output_path)

slægter <- df_clean |> 
  filter(rank == "slægt")

hybrider <- df_clean |> 
  filter(rank == "hybrid")

arter <- df_clean |> 
  filter(rank == "art")
