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

write_rds(final_file,"vaage_1932_eirikraudesland.rds")
vaage_1932_eirikraudesland <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/vaage_1932_eirikraudesland.rds")

#### writing the file ###################################################################################################################

ipt_fil <- vaage_1932_eirikraudesland |> 
  mutate(year = year(date)) |> 
  left_join(verbatim_names, by = "name") |> 
  select(name,
         verbatim_name,
         #location,
         #obs,
         #number,
         location_name,
         #area,
         lat,
         lon,
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
    verbatimLocality = location_name,
    decimalLatitude = lat,
    decimalLongitude = lon,
    year = year(date),
    month = month(date),
    day = day(date))

add_geo_gl <- function(df){
  df |>  
    mutate(
      Geodetic_datum = "wgs84",
      continent = "NORTH_AMERICA",
      higherGeographyID = "http://vocab.getty.edu/page/tgn/7024573",
      higherGeography = "Greenland",
      countryCode = "GL", 
      locationID = "TDWG:GNL-OO"
    )
}

ipt_fil <- add_geo_gl(ipt_fil)

synonyms <- ipt_fil |> 
  filter(status == "SYNONYM") |> 
  distinct(verbatim_name,scientificName)

view(synonyms)

thedate <- strftime(Sys.Date(),"%Y_%m_%d")
write_csv(ipt_fil, paste0(thedate,"_vaage_1932_eirikaudesland",".csv"))
