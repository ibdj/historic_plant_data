### navneudvalgslist ####

#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
library(pacman)
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils, janitor) 
library(readxl)
library(writexl)

### remmeber to convert to native google sheet

DBF_navneliste_22_10_2025 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/DBF navneliste 22-10-2025.xlsx") |> 
clean_names() 

taxa <- DBF_navneliste_22_10_2025 |> 
  mutate(verbatim_name = videnskabeligt_navn) |> 
  filter(!is.na(accepterede_danske_navne)) |> 
  mutate(
    rang = case_when(
      grepl("slægten", accepterede_danske_navne, ignore.case = TRUE) ~ "slægt",
      grepl("var\\.", videnskabeligt_navn , ignore.case = TRUE) ~ "varitet",
      grepl("subsp\\.", videnskabeligt_navn, ignore.case = TRUE) ~ "underart",
      grepl("×", videnskabeligt_navn) ~ "hybrid",
      TRUE ~ "art"
    ),
    rang_engelsk = case_when(
      grepl("slægten", accepterede_danske_navne, ignore.case = TRUE) ~ "GENUS",
      grepl("var\\.", videnskabeligt_navn , ignore.case = TRUE) ~ "VARIETY",
      grepl("subsp\\.", videnskabeligt_navn, ignore.case = TRUE) ~ "SUBSPECIES",
      grepl("×", videnskabeligt_navn) ~ "HYBRID",
      TRUE ~ "SPECIES"
    )
  ) 
  

names(taxa)

match_list <- taxa |>
  select(name = videnskabeligt_navn) |>  # rename in one go
  distinct()

matched_names <- name_backbone_checklist(match_list, "name", kingdom = "Plantae", phylum = "Tracheophyta")
#verbatim_name is the one with the original name

##### making an editable list of colomns#  ####
#column_list <- colnames(joined_list)

#column_string <- paste(column_list, collapse = ",\n")
#cat(column_string)

joined_list <- taxa |> 
  left_join(matched_names, by = "verbatim_name") |> 
  filter(!is.na(videnskabeligt_navn)) |> 
  select(
    #column_1,
    #antal,
    #version,
    rank,
    rang,
    rang_engelsk,
    #dansk_slaegt,
    #videnskalig_slaegt,
    videnskabeligt_navn,
    accepterede_danske_navne,
    danske_synonymer,
    videnskabeligt_synonym,
    #afd,
    #bemaerkninger,
    #afvigende_dk_navn,
    #udvalgets_kommentarer,
    #dansk_feltflora_15,
    #dansk_feltflora_16,
    #dansk_flora_17,
    #dansk_flora_18,
    #den_nye_nordiske_flora_dk,
    #den_nye_nordiske_flora_sci,
    #allearter_21,
    #allearter_22,
    #dyrkede_og_vilde_planter_23,
    #dyrkede_og_vilde_planter_24,
    #atlas_flora_danica,
    #atlas_flora_danica2,
    verbatim_name,
    usageKey,
    scientificName,
    canonicalName,
    rank,
    status,
    confidence,
    matchType,
    kingdom,
    phylum,
    order,
    family,
    genus,
    kingdomKey,
    phylumKey,
    classKey,
    orderKey,
    familyKey,
    genusKey,
    class,
    species,
    speciesKey,
    #synonym,
    acceptedUsageKey,
    #verbatim_rank,
    verbatim_index
    )


names(joined_list)

missing <- joined_list |> 
  filter(is.na(genus))

names(joined_list)

print_missing <- missing |>
  select(rank, rang, videnskabeligt_navn,accepterede_danske_navne, verbatim_name, danske_synonymer, videnskabeligt_synonym)

output_path <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/missing_pnu_liste_2025-10-27.xlsx"

write_xlsx(print_missing, output_path)

joined_list$order <- as.factor(joined_list$order)
joined_list$kingdom <- as.factor(joined_list$kingdom)
joined_list$class <- as.factor(joined_list$class)
joined_list$phylum <- as.factor(joined_list$phylum)
joined_list$matchType <- as.factor(joined_list$matchType)
joined_list$rank <- as.factor(joined_list$rank)
joined_list$rang <- as.factor(joined_list$rang)
joined_list$rang_engelsk <- as.factor(joined_list$rang_engelsk)



summary(joined_list)

mismatch_rank <- joined_list |> 
  filter(rang_engelsk != rank)

mismatch_path <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/mismatch_gbif_pnu_liste_2025-10-27.xlsx"

write_xlsx(mismatch_rank, mismatch_path)

path_joined <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/joined_gbif_pnu_liste_2025-10-27.xlsx"

write_xlsx(joined_list, path_joined)
