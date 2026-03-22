#### reading packages ##########################################################################################################

if (!require("pacman")) install.packages("pacman")
devtools::install_github("inbo/inborutils")
pacman::p_load(tidyverse,googlesheets4, rgbif, ids, lubridate, devtools, inborutils, janitor) 

#### reading the data from google sheets ########################################################################
taxa <- read_sheet('https://docs.google.com/spreadsheets/d/1-SLBF6wwNXc1z3znjUNZuZQQ8XXx_iplr4sumrnwOn4/edit?gid=1314126224#gid=1314126224', sheet = 'redlist_import', skip = 1) |>
  clean_names()

names(taxa)

redlist <- taxa |> 
  filter(international_kode %in% c("VU", "NT"))

names(redlist)

taxon_keys <- redlist$gbif_taxon_key |> 
  as.numeric() |> 
  na.omit() |> 
  as.list()

#### setting up gbif ###########################################################
# Read credentials from environment variables
user <- Sys.getenv("GBIF_USER")
pwd <- Sys.getenv("GBIF_PWD")
email <- Sys.getenv("GBIF_EMAIL")

# Optional: Safety check to ensure they aren't empty
if (user == "" || pwd == "") {
  stop("GBIF credentials missing. Please check your .Renviron file.")
}

#### starting the request #########################################################

taxon_keys_vec <- unlist(taxon_keys)

download_key <- occ_download(
  pred_in("taxonKey", taxon_keys_vec),
  pred("country", "GL"),
  format = "SIMPLE_CSV"
)

occ_download_meta(download_key)

#### downloading the data #####
occ_download_get(download_key, path = "data")

# Unzip the file
unzip("data/0053643-260226173443078.zip", exdir = "data")

gbif_data <- read_delim("data/0053643-260226173443078.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
