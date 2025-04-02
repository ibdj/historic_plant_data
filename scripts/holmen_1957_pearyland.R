#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

#### reading the data from google sheets########################################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'taxa')

locations <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'locations')

identifier <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'identifier')

taxa[] <- lapply(taxa, as.character)

taxa_pivot <- taxa |> 
  pivot_longer(cols = 3:ncol(taxa), names_to = "location", values_to = "observer") |> 
  drop_na() |> 
  filter(observer != "NULL") |> 
  separate_wider_delim(cols = observer, delim = "&", names = c("observer1", "observer2","observer3"), too_few = "align_start") 

taxa_pivot_last <- taxa_pivot |> 
  pivot_longer(cols = observer1:observer3, names_to = "pos", values_to = "observer") |> 
  drop_na() |> 
  filter(!grepl("\\d", observer)) 

dates <- taxa_pivot_last |> 
  left_join(identifier, by = c("observer", "location"))

needs_dates <- dates |> 
  filter(date == "NULL") |> 
  distinct(location, observer)

#### matching to GBIF ###################################################################################################################

#make a unique list of taxon names
unique <- taxa_pivot_last |> 
  distinct(verbatimName)

xy_gbif_matched_name_backbone_checklist <- unique |> 
  name_backbone_checklist("name") |> 
  rename("name" = "verbatim_name") |> 
  mutate(matchType = as.factor(matchType))
#  select(usageKey, acceptedUsageKey,scientificName, canonicalName, name,rank,,verbatim_index,verbatim_rank,status,confidence,matchType,kingdom,phylum,order#,family,genus,species,kingdomKey,phylumKey,classKey,orderKey,familyKey,genusKey,speciesKey,synonym,class)  

summary(xy_gbif_matched_name_backbone_checklist)

not_matched <- xy_gbif_matched_name_backbone_checklist |> 
  #filter(is.na(speciesKey))
  filter(matchType %in% c("HIGHERRANK","NONE")) 

view(not_matched)


#### generating the final file ##########################################################################################################

add_id <- function(df){
  df %>% 
    mutate(
      id1 = paste("urn:vpferl"),
      id2 = random_id(nrow(.))
    ) %>% 
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

write_rds(final_file,"vaage_1932_eirikraudesland.rds")
vaage_1932_eirikraudesland <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/vaage_1932_eirikraudesland.rds")

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
