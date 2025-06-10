

xy <- read_sheet('https://docs.google.com/spreadsheets/d/1adm-3HKTyeDMY-WPOWWSKN37hGuj5EWNMjJIh3D6tkc/edit?gid=0#gid=0', sheet = 'data')

xy_longer <- xy |> 
  pivot_longer(cols = 4:10, names_to = "location", values_to = "date") |> 
  drop_na()
  
xy_gbif_matched_name_backbone_checklist <- xy |> 
  name_backbone_checklist("name") |> 
  rename(name = verbatim_name)

# to do: change match type on name_backbone_checklist to get better match of Heterotypic synonyms from GBIF?

# reading location ####

xy_locations <- read_sheet('https://docs.google.com/spreadsheets/d/1adm-3HKTyeDMY-WPOWWSKN37hGuj5EWNMjJIh3D6tkc/edit?gid=0#gid=0', sheet = 'locations')

# joining locations and ####

xy_joined <- xy_longer |> 
  left_join(xy_locations, by = 'location', relationship = "many-to-many") |> 
  left_join(xy_gbif_matched_name_backbone_checklist, by = 'name', relationship = "many-to-many")

# checking unmatched ####

xy_joined_not_matched <- xy_joined |> 
  filter(is.na(species))
