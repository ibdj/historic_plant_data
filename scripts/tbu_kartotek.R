#install.packages("tesseract", type = "binary")
#install.packages("pdftools")
#remove.packages("dplyr")
install.packages("dplyr", type = "binary")
library(pdftools)
library(tidyverse)
library(tesseract)
library(magick)
library(rgbif)

# Set your main folder path
main_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/tbu/tartotek/TBU_Kartotek/split"

# Get all PDF files recursively from all subfolders
pdf_files <- list.files(
  path = main_folder,
  pattern = "\\.pdf$",
  recursive = TRUE,
  full.names = TRUE
)

# View the list
print(pdf_files)

pdf_files_df <- as.data.frame(pdf_files) 

pdf_files_df$species <- tools::file_path_sans_ext(basename(pdf_files_df$pdf_files))

split_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/tbu/tartotek/TBU_Kartotek/split" 

# Get all PDF files recursively from all subfolders
split_files <- list.files(
  path = split_folder,
  pattern = "\\.pdf$",
  recursive = TRUE,
  full.names = TRUE
) |> 
  as.data.frame()

# Fix the column name first
names(split_files) <- "full_path"

# Extract species (everything before _p and the page number)
split_files$species <- gsub("_p\\d+$", "", tools::file_path_sans_ext(basename(split_files$full_path)))

nm <- tools::file_path_sans_ext(basename(split_files$full_path))

m <- str_match(nm, "^(.*)_p(\\d+)(?:_tbu(.+))?$")

split_files$species  <- as.factor(m[, 2])
split_files$page     <- as.integer(m[, 3])
split_files$district <- as.factor(m[, 4])

summary(split_files)

nas <- split_files |> 
  filter(is.na(district)) 


# Fix the column name first
names(split_files) <- "full_path"

# Extract species (everything before _p and the page number)
split_files$species <- gsub("_p\\d+$", "", tools::file_path_sans_ext(basename(split_files$full_path)))

# Extract page number
split_files$page <- as.integer(gsub(".*_p(\\d+)$", "\\1", tools::file_path_sans_ext(basename(split_files$full_path))))

head(split_files)
# clean up #####

dir <- "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/botany/tbu/tartotek/TBU_Kartotek/split/med_distrikt"

valid <- c(setdiff(as.character(1:53), c("22", "39", "45")),
           "13a", "13b", "22a", "22b", "39a", "39b", "45a", "45b",
           "Slesvig")

files <- list.files(dir, pattern = "\\.pdf$")
tbu   <- sub(".*_tbu(.*)\\.pdf$", "\\1", files)

bad <- data.frame(file = files, tbu = tbu)[!tbu %in% valid, ]
nrow(bad)
table(bad$tbu)
writeLines(bad$file, "~/Desktop/tbu_invalid.txt")
print(bad)

# basic meta data

split_files <- split_files |>
  mutate(rang = case_when(
    str_detect(species, fixed(" × "))    ~ "hybrid",
    str_detect(species, fixed(" ×"))    ~ "hybrid",
    str_detect(species, fixed("var."))   ~ "varietet",
    str_detect(species, fixed("subsp.")) ~ "underart",
    str_detect(species, fixed("ssp."))    ~ "underart",
    TRUE                                 ~ "art"
  ))

count(split_files, rang)

rang_antal <- split_files |>
  group_by(species, rang) |>
  summarise(antal = n(), .groups = "drop")

# checking the order of the pages and distrikts ###

ord <- c(as.character(1:12), "13a", "13b", as.character(14:21), "22a", "22b",
         as.character(23:38), "39a", "39b", as.character(40:44), "45a", "45b",
         as.character(46:53), "Slesvig")

df <- tibble(file = list.files(dir, pattern = "\\.pdf$")) |>
  mutate(species = sub("_p[0-9]+_tbu.*$", "", file),
         page    = as.integer(sub(".*_p([0-9]+)_tbu.*$", "\\1", file)),
         tbu     = sub(".*_tbu(.*)\\.pdf$", "\\1", file),
         rank    = match(tbu, ord)) |>
  arrange(species, page) |>
  group_by(species) |>
  mutate(prev_tbu = lag(tbu), next_tbu = lead(tbu),
         drop = rank < lag(rank)) |>
  ungroup()

flagged <- filter(df, drop)
nrow(flagged)
flagged |> select(species, page, prev_tbu, tbu, next_tbu) |> print(n = 30)
write.csv(flagged, "~/Desktop/tbu_order_check.csv", row.names = FALSE)

df <- df |>
  group_by(species) |>
  mutate(prev_file = lag(file)) |>
  ungroup()

flagged <- filter(df, drop)
flagged |> select(file, prev_file, prev_tbu, tbu, next_tbu) |> print(n = Inf)


# which ones contrain an x #

split_files |>
  filter(str_detect(species, "\\bx\\b|×")) |>
  distinct(species) |>
  pull(species)

split_files |>
  filter(str_detect(species, "(^| )x[^ ]") | str_detect(species, "×[^ ]|[^ ]×")) |>
  distinct(species) |>
  pull(species)

#match to gbif
