# medlemsundersøgelse
# packages
library(tidyverse)
library(writexl)

write_xlsx(data, "medlemsundersøgelse.xlsx")

#data <- readr::read_delim("/Users/ibdj/Desktop/Survey.csv", delim = ";")
data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/data/Survey.csv",
                 sep = ";", fileEncoding = "latin1")

class(data)

write_excel_csv("data", )

write_excel_csv(data, file = "medlemsundersøgelse.excel")

data <- as.data.frame(data)
view(data)
summary(data)


ggplot(data, aes())
hist(data$sp01)
hist(data$sp01, breaks = seq(min(data$sp01), max(data$sp01)))
hist(data$sp03)
hist(data$sp05)
hist(data$sp07)
