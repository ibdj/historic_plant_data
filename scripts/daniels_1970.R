

xy <- read_sheet('https://docs.google.com/spreadsheets/d/1adm-3HKTyeDMY-WPOWWSKN37hGuj5EWNMjJIh3D6tkc/edit?gid=0#gid=0', sheet = 'data')

xy_longer <- xy |> 
  pivot_longer(cols = 4:10, names_to = "location", values_to = "date") |> 
  drop_na()
  
xy_gbif_matched_name_backbone_checklist <- xy |> 
  name_backbone_checklist("name")
