#### ####
library(readxl)
library(tidyverse)
library(writexl)

df <- read_excel('/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/2025-11-06 DBF navneliste 06-11-2025.xlsx') |> 
  mutate(n_filled = rowSums(across(everything(), ~ !is.na(.x))), index = row_number())


# # Read the file
# 
# DBF_navneliste_13_10_2025 <- read_excel("~/Desktop/DBF navneliste 13-10-2025.xlsx")
# df <- DBF_navneliste_13_10_2025
# 
# file_path <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/DBF navneliste 03-10-2025.xlsx"
# # df <- read_excel(file_path) |> 
# #   mutate(n_filled = rowSums(across(everything(), ~ !is.na(.x))), 
# #          index = row_number())
# 
# df <- DBF_navneliste_13_10_2025 |> 
#   mutate(n_filled = rowSums(across(everything(), ~ !is.na(.x))), 
#          index = row_number())

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

print <- df_clean |> 
  filter(rank != "slægt") |> 
  mutate(latex1 = "&\\textit{",
         latex2 = "}&",
         latex3 = "&\\textit{",
         latex4 = "}",
         latex5 = "\\\\") |> 
  select(`Accepterede danske navne`,latex1,`Videnskabeligt navn`,latex2,`Dansk slægt`,latex3,genus,latex4,latex5)

# Define an output path
output_path <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/DBF_navneliste_print06-11-2025.csv"

# Write to Excel
write_delim(print, output_path, delim = " ")

write.table(print, file = output_path, sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE, eol = "\n")

write_xlsx(df_clean, output_path)

slægter <- df_clean |> 
  filter(rank == "slægt")

hybrider <- df_clean |> 
  filter(rank == "hybrid")

arter <- df_clean |> 
  filter(rank == "art")
