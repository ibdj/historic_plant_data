#### functions to be used #####

add_geo_dk <- function(df){
  df |>  
    mutate(
      Geodetic_datum = "wgs84",
      continent = "EUROPE",
      higherGeographyID = "http://vocab.getty.edu/page/tgn/1000066",
      higherGeography = "Denmark",
      countryCode = "DA", 
      locationID = "TDWG:DEN-OO"
    )
}


add_geo_gl <- function(df){
  df |>  
    mutate(
      Geodetic_datum = "wgs84",
      continent = "",
      higherGeographyID = "http://vocab.getty.edu/page/tgn/1000066",
      higherGeography = "Greenland",
      countryCode = "GL", 
      locationID = "TDWG:DEN-OO"
    )
}

add_id <- function(df,data_name){
  df %>% 
    mutate(
      id1 = paste("urn:"),
      id2 = random_id(nrow(.))
    ) %>% 
    unite("occurrenceID",id1:id2, sep = ":") 
}
