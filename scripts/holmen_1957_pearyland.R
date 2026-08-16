#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

#### reading the data from google sheets########################################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'taxa') |> 
  distinct() |> 
  lapply(as.character)
names(taxa)

locations <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'locations') |> 
  distinct() |> 
  mutate(location_id = verbatim_location, verbatimLocality = verbatim_location)

identifier <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'identifier') |> 
  distinct()

##### processing taxa ##########################################################

taxa_pivot <- taxa |> 
  pivot_longer(cols = 4:ncol(taxa), names_to = "location", values_to = "observer") |> 
  drop_na() |> 
  filter(observer != "NULL") |> 
  separate_wider_delim(cols = observer, delim = "&", names = c("observer1", "observer2","observer3"), too_few = "align_start") |> 
  filter(!grepl("\\d", observer1)) 

taxa_pivot2 <- taxa_pivot |> 
  pivot_longer(cols = observer1:observer3, names_to = "pos", values_to = "observer") |> 
  drop_na() |> 
  filter(!grepl("\\d", observer)) 

common1 <- intersect(names(taxa_pivot2), names(identifier))
dates <- left_join(taxa_pivot2, identifier, by = common1)

taxa_pivot2 |> count(across(all_of(common1))) |> filter(n > 1)
identifier  |> count(across(all_of(common1))) |> filter(n > 1)

needs_dates <- dates |> 
  filter(date == "NULL") |> 
  distinct(location, observer,verbatimName)

needs_dates2 <- dates |> 
  filter(date == "NULL") |> 
  distinct(location, observer)


#### matching to GBIF ###################################################################################################################
#make a unique list of taxon names

unique <- taxa_pivot2 |> 
  distinct(verbatimName)

gbif_matchedlist <- unique |> 
  name_backbone_checklist("name") |> 
  rename(name = verbatim_name) |> 
  mutate(matchType = as.factor(matchType))
#  select(usageKey, acceptedUsageKey,scientificName, canonicalName, name,rank,,verbatim_index,verbatim_rank,status,confidence,matchType,kingdom,phylum,order#,family,genus,species,kingdomKey,phylumKey,classKey,orderKey,familyKey,genusKey,speciesKey,synonym,class)  

summary(gbif_matchedlist)

not_matched <- gbif_matchedlist |> 
  #filter(is.na(speciesKey))
  filter(matchType %in% c("HIGHERRANK","NONE")) 

view(not_matched)

#### generating the final file ##########################################################################################################

add_id <- function(df){
  df |>  
    mutate(
      id1 = paste("urn:vpferl"),
      id2 = random_id(nrow(.))
    ) |>  
    unite("occurrenceID",id1:id2, sep = ":") 
}

names(gbif_matchedlist)
verbatim_names <- xy_gbif_matched_name_backbone_checklist |> 
  select(scientificName, name)

names(dates)
names(gbif_matchedlist)

common3 <- intersect(names(dates |> mutate(name = verbatimName)), names(xy_gbif_matched_name_backbone_checklist))
common3

file_with_ids <- left_join(dates |> mutate(name = verbatimName), xy_gbif_matched_name_backbone_checklist, by = common3)|> 
  add_id()

file_with_ids[,"occurrenceID"]

common4 <- intersect(names(file_with_ids), names(locations))
common4
final_file <- file_with_ids |> 
  filter(!grepl("×", name)) |> 
  left_join(locations, by = common4)

final_file <- file_with_ids
names(final_file)

write_rds(final_file,"holmen_1957_pearyland.rds")
holmen_1957_pearyland <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/holmen_1957_pearyland.rds")

#### writing the file ###################################################################################################################

names(holmen_1957_pearyland)

common <- intersect(names(holmen_1957_pearyland), names(verbatim_names))
ipt_file <- left_join(holmen_1957_pearyland, verbatim_names, by = common)

names(ipt_file)

ipt_file <- ipt_file |> 
  select(name,
         verbatimName,
         #location,
         #obs,
         #number,
         verbatimLocality,
         #area,
         decimalLatitude,
         decimalLongitude,
  #      place,
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
    occurrenceStatus = "present",
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
