#### bryologkredsens artslister #########################################################

#### packages ##################################################################

library(googlesheets4)
library(tidyverse)
library(rgbif)
library(janitor)

#### importing #################################################################

artslister <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/botany/bryologkredsen/artslister.xlsx")
