#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")

devtools::install_github("inbo/inborutils")

pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils) 

#### reading the data from google sheets########################################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'taxa') |> 
  distinct() |> 
  mutate(across(everything(), as.character))

class(taxa)
names(taxa)

locations <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'locations') |> 
  distinct() |> 
  mutate(verbatimLocality = locationID)

identifier <- read_sheet('https://docs.google.com/spreadsheets/d/13nuhJEVjqnZ1a1d7jh2M2f8dbjJAl0GL-k6MyihAI0w/edit?gid=199168860#gid=199168860', sheet = 'identifier') |> 
  distinct()

##### processing taxa ##########################################################

taxa_pivot <- taxa |> 
  pivot_longer(cols = 4:ncol(taxa), names_to = "locationID", values_to = "observer") |> 
  drop_na() |> 
  filter(observer != "NULL") |> 
  separate_wider_delim(cols = observer, delim = "&", names = c("observer1", "observer2","observer3"), too_few = "align_start") |> 
  filter(!grepl("\\d", observer1)) 

all_observations <- taxa_pivot |> 
  pivot_longer(cols = observer1:observer3, names_to = "pos", values_to = "observer") |> 
  drop_na() |> 
  filter(!grepl("\\d", observer)) 

common1 <- intersect(names(all_observations), names(identifier))
all_observations_incl_dates <- left_join(all_observations, identifier, by = common1)

all_observations |> count(across(all_of(common1))) |> filter(n > 1)
identifier  |> count(across(all_of(common1))) |> filter(n > 1)

needs_dates <- all_observations_incl_dates |> 
  filter(date == "NULL") |> 
  distinct(locationID, observer,verbatimName)
needs_dates

needs_dates2 <- all_observations_incl_dates |> 
  filter(date == "NULL") |> 
  distinct(locationID, observer)
needs_dates2

#### matching to GBIF ###################################################################################################################

#make a unique list of taxon names
unique <- all_observations_incl_dates |> 
  distinct(verbatimName)

gbif_matchedlist <- unique |> 
  name_backbone_checklist("name") |> 
  rename(name = verbatim_name) |> 
  mutate(matchType = as.factor(matchType), verbatimName = name)

summary(gbif_matchedlist)

not_matched <- gbif_matchedlist |> 
  #filter(is.na(speciesKey))
  filter(matchType %in% c("HIGHERRANK","NONE")) 

not_matched

#### joining all data ##########################################################

common2 <- intersect(names(all_observations_incl_dates), names(gbif_matchedlist))
joined_dates_gbif <- left_join(all_observations_incl_dates, gbif_matchedlist, by = common2)

common3 <- intersect(names(joined_dates_gbif), names(locations))
joined_dates_gbif_coordinates <- left_join(joined_dates_gbif, locations, by = common3)


#### generating the final file ##########################################################################################################
add_id <- function(df){
  df |>
    mutate(occurrenceID = paste("urn:vpferl", random_id(n()), sep = ":"))
}

joined_dates_gbif_coordinates_id <- joined_dates_gbif_coordinates |> 
  add_id()

joined_dates_gbif_coordinates_id[,"occurrenceID"]

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
