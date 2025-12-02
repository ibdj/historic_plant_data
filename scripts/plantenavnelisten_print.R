#### loading packages ####
library(readxl)
library(tidyverse)
library(writexl)
library(googlesheets4)

#### importing data #####
df <- read_excel('/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/2025 11 19 DBF navneliste 5. version 19-11-2025.xlsx') |> 
  mutate(n_filled = rowSums(across(everything(), ~ !is.na(.x))), index = row_number())

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
  filter(!is.na(`Accepterede danske navne`)) |> 
  group_by(`Dansk slægt`) |> 
  mutate(count = n())

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

#### slægt-art mismatch #############

names(df_clean)

slægt_art_mismatch <- df_clean |> 
  mutate(
    slægt_navn = case_when(
      tolower(sub("slægten$", "", `Dansk slægt`)) == "pile" ~ "pil",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "rosen" ~ "rose",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "lærke" ~ "lærk",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "fyrre" ~ "fyr",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "rønne" ~ "røn",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "elme" ~ "elm",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "ege" ~ "eg",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "elle" ~ "el",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "birke" ~ "birk",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "linde" ~ "lind",
      TRUE ~ tolower(sub("slægten$", "", `Dansk slægt`))
    ),
    genus_dk = sub("^[^- ]+[- ]+([^ ]+).*", "\\1", `Accepterede danske navne`),
    epithet = ifelse(rank != "slægt" & grepl("[- ]", `Accepterede danske navne`),sub("([^- ]+[- ]).*", "\\1", `Accepterede danske navne`),""),
    expected = case_when(
      rank != "slægt" & epithet != "" ~ paste0(epithet, slægt_navn),
      rank != "slægt" & epithet == ""  ~ paste0(toupper(substr(slægt_navn, 1, 1)), substr(slægt_navn, 2, nchar(slægt_navn))),
      TRUE ~ ""
    ),
    match    = ifelse(rank != "slægt", `Accepterede danske navne` == expected, ""),
    detail   = ifelse(rank != "slægt", ifelse(match, "", paste0("Kunne være ", expected, ", men er ", `Accepterede danske navne`)
      ),"")
  )

gs4_auth()
gs4_create("slægt_art_mismatch_results", sheets = slægt_art_mismatch)

slægt_art_mismatch_kun <- slægt_art_mismatch |> 
  filter(detail != "", epithet != "")



kun_en_art <- df_clean |> 
  filter(count == 2, rank == "art")

kun_en_art2 <- kun_en_art %>%
  mutate(
    expected = paste0(`Accepterede danske navne`, "slægten"),
    match = `Dansk slægt` == expected,
    detail = ifelse(
      match,
      "",
      paste0("Kunne være ", sub("slægten$", "", `Dansk slægt`), " men er ", `Accepterede danske navne`)
    )
  )

names(kun_en_art2)
write_xlsx(kun_en_art2 |> select(`Accepterede danske navne`,`Videnskabeligt navn`,`Dansk slægt`,detail), "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/2025 11 14 DBF_navneliste_kun_en_art.xlsx")

slægter <- df_clean |> 
  filter(rank == "slægt")

hybrider <- df_clean |> 
  filter(rank == "hybrid")

arter <- df_clean |> 
  filter(rank == "art")
