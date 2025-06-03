#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

#### reading the data from google sheets########################################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/12SAJkY1w_2K00zcimrtjUWynfprDtKdFRTY03UFPyVM/edit?gid=1761157836#gid=1761157836', sheet = 'taxa')

locations <- read_sheet('https://docs.google.com/spreadsheets/d/12SAJkY1w_2K00zcimrtjUWynfprDtKdFRTY03UFPyVM/edit?gid=1761157836#gid=1761157836', sheet = 'locations')

taxa_pivot <- taxa |> 
  pivot_longer(cols = 5:ncol(taxa), names_to = "location", values_to = "obs") |> 
  select(name,location,obs) |> 
  drop_na() |> 
  left_join(locations, by = "location")

taxa_pivot_withdate <- taxa_pivot |> 
  pivot_longer(cols = date1:date7, names_to = "place", values_to = "date") |> 
  drop_na()


#### matching to GBIF ###################################################################################################################
xy_gbif_matched_name_backbone_checklist <- taxa |> 
  name_backbone_checklist("name") |> 
  rename("name" = "verbatim_name") |> 
  select(usageKey, acceptedUsageKey,scientificName, canonicalName, name,rank,,verbatim_index,verbatim_rank,status,confidence,matchType,kingdom,phylum,order,family,genus,species,kingdomKey,phylumKey,classKey,orderKey,familyKey,genusKey,speciesKey,synonym,class)  

not_matched <- xy_gbif_matched_name_backbone_checklist |> 
  #filter(is.na(speciesKey))
  filter(matchType %in% c("HIGHERRANK","NONE")) 

view(not_matched)


#### generating the final file ##########################################################################################################

add_id <- function(df){
  df  |>  
    mutate(
      id1 = paste("urn:vpferl"),
      id2 = random_id(nrow(df))
    )  |>  
    unite("occurrenceID",id1:id2, sep = ":") 
}

verbatim_names <- taxa |> 
  select(name, verbatim_name)

file_with_ids <- taxa_pivot_withdate |> 
  left_join(xy_gbif_matched_name_backbone_checklist, by = "name") |> 
  add_id()
  
file_with_ids[,"occurrenceID"]

final_file <- file_with_ids |> 
  filter(!grepl("×", name))

final_file <- file_with_ids

# check that all the observation are the correct kingdom

write_rds(final_file,"vaage_1932_eirikraudesland.rds")
vaage_1932_eirikraudesland <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/vaage_1932_eirikraudesland.rds")

not_kingdom <- vaage_1932_eirikraudesland |> filter(kingdom != "Plantae") |> 
  summarise(kingdom)

unique(vaage_1932_eirikraudesland$kingdom)

unique(vaage_1932_eirikraudesland$rank)

#### handling the hybrids  ###################################################################################################################

unique(vaage_1932_eirikraudesland$matchType)
vaage_1932_eirikraudesland$scientificName[is.na(vaage_1932_eirikraudesland$scientificName)] <- vaage_1932_eirikraudesland$name[is.na(vaage_1932_eirikraudesland$scientificName)]

vaage_1932_eirikraudesland$rank[is.na(vaage_1932_eirikraudesland$rank)] <- "HYBRID"
unique(vaage_1932_eirikraudesland$rank)
vaage_1932_eirikraudesland$kingdom[is.na(vaage_1932_eirikraudesland$kingdom)] <- "Plantae"
unique(vaage_1932_eirikraudesland$kingdom)
vaage_1932_eirikraudesland$phylum[is.na(vaage_1932_eirikraudesland$phylum)] <- "Tracheophyta"
unique(vaage_1932_eirikraudesland$phylum)
vaage_1932_eirikraudesland$order[is.na(vaage_1932_eirikraudesland$order) & grepl("Draba", vaage_1932_eirikraudesland$scientificName)] <- "Brassicales"
vaage_1932_eirikraudesland$order[is.na(vaage_1932_eirikraudesland$order) & grepl("Poa ", vaage_1932_eirikraudesland$scientificName)] <- "Poales"
unique(vaage_1932_eirikraudesland$order)
vaage_1932_eirikraudesland$family[is.na(vaage_1932_eirikraudesland$family) & grepl("Draba", vaage_1932_eirikraudesland$scientificName)] <- "Brassicaceae"
vaage_1932_eirikraudesland$family[is.na(vaage_1932_eirikraudesland$family) & grepl("Poa ", vaage_1932_eirikraudesland$scientificName)] <- "Poaceae"
unique(vaage_1932_eirikraudesland$family)
vaage_1932_eirikraudesland$genus[is.na(vaage_1932_eirikraudesland$genus) & grepl("Draba", vaage_1932_eirikraudesland$scientificName)] <- "Draba"
vaage_1932_eirikraudesland$genus[is.na(vaage_1932_eirikraudesland$genus) & grepl("Poa ", vaage_1932_eirikraudesland$scientificName)] <- "Poa"

summary(vaage_1932_eirikraudesland)
str(vaage_1932_eirikraudesland)
vaage_1932_eirikraudesland[sapply(vaage_1932_eirikraudesland, is.character)] <- lapply(vaage_1932_eirikraudesland[sapply(vaage_1932_eirikraudesland, is.character)], as.factor)

unique(vaage_1932_eirikraudesland$rank)
#### writing the file ###################################################################################################################

ipt_file <- vaage_1932_eirikraudesland |> 
  mutate(year = year(date)) |> 
  left_join(verbatim_names, by = "name") |> 
  select(name,
         verbatim_name,
         #location,
         #obs,
         #number,
         verbatimLocality,
         #area,
         decimalLatitude,
         decimalLongitude,
         place,
         date,
         usageKey,
         acceptedUsageKey,
         scientificName,
         #canonicalName,
         rank,
         #verbatim_index,
         #verbatim_rank,
         status,
         confidence,
         matchType,
         kingdom,
         phylum,
         order,
         family,
         genus,
         species,
         kingdomKey,
         phylumKey,
         classKey,
         orderKey,
         familyKey,
         genusKey,
         speciesKey,
         synonym,
         class,
         occurrenceID) |> 
  mutate(
    basisOfRecord = "HumanObservation",
    occurrenceStatus = "presence",
    year = year(date),
    month = month(date),
    day = day(date),
    InstitutionID = "https://ror.org/0342y5q78",
    InstitutionCode = "GINR"
    )

add_geo_gl <- function(df){
  df |>  
    mutate(
      geodeticDatum = "wgs84",
      continent = "NORTH_AMERICA",
      country = "Greenland",
      countryCode = "GL", 
      locationID = "TDWG:GNL-OO"
    )
}

ipt_file <- add_geo_gl(ipt_file)

synonyms <- ipt_file |> 
  filter(status == "SYNONYM") |> 
  distinct(verbatim_name,scientificName)

view(synonyms)

thedate <- strftime(Sys.Date(),"%Y_%m_%d")

write_csv(ipt_file, paste0("outputs/",thedate,"_vaage_1932_eirikaudesland",".csv"))
