### navneudvalgslist ####

#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
library(pacman)
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils, janitor) 


#### reading the data from google sheets########################################################################################
gs4_auth() 

### remmeber to convert to native google sheet

taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1tMYqjWgUHFMlrIp0gA48t39t8aQB8FTMUSSJPdLjlSI/edit?gid=1716451010#gid=1716451010', sheet = 'Ark1') |> 
  clean_names() |> 
  mutate(verbatim_name = videnskabeligt_navn)

names(taxa)

match_list <- taxa |>
  select(name = videnskabeligt_navn) |>  # rename in one go
  distinct()

matched_names <- name_backbone_checklist(match_list, "name")
#verbatim_name is the one with the original name

joined_list <- taxa |> 
  left_join(matched_names, by = "verbatim_name") |> 
  filter(!is.na(videnskabeligt_navn)) |> 
  select(
    #column_1,
    #antal,
    #version,
    rank.x,
    #dansk_slaegt,
    #videnskalig_slaegt,
    videnskabeligt_navn,
    accepterede_danske_navne,
    #danske_synonymer,
    #videnskabeligt_synonym,
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
    rank.y,
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
    synonym,
    acceptedUsageKey,
    verbatim_index,
    verbatim_rank
  )

# making an editable list of colomns# 
column_list <- colnames(joined_list)

column_string <- paste(column_list, collapse = ",\n")
cat(column_string)

names(joined_list)
