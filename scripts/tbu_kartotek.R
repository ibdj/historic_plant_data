
install.packages("pdftools")
library(pdftools)
library(tidyverse)
install.packages("tesseract")
library(tesseract)

# Set your main folder path
main_folder <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/tbu/tartotek/TBU_Kartotek"

# Get all PDF files recursively from all subfolders
pdf_files <- list.files(
  path = main_folder,
  pattern = "\\.pdf$",
  recursive = TRUE,
  full.names = TRUE
)

# View the list
print(pdf_files)

rm(pdf_files_df)
pdf_files_df <- as.data.frame(pdf_files) 

pdf_files_df$species <- tools::file_path_sans_ext(basename(pdf_files_df$pdf_files))



# Output folder for split pages
output_dir <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/tbu/tartotek/TBU_Kartotek/split"
dir.create(output_dir, showWarnings = FALSE)

# Loop over all PDFs and split into single pages
for (i in seq_len(nrow(pdf_files_df))) {
  
  input_path <- path.expand(pdf_files_df$pdf_files[i])
  species     <- pdf_files_df$species[i]
  n_pages     <- pdf_info(input_path)$pages
  
  if (n_pages == 1) {
    file.copy(input_path, file.path(output_dir, paste0(species, ".pdf")))
  } else {
    for (page in seq_len(n_pages)) {
      out_file <- file.path(output_dir, paste0(species, "_p", page, ".pdf"))
      pdf_subset(input_path, pages = page, output = out_file)
    }
  }
}

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

# Extract page number
split_files$page <- as.integer(gsub(".*_p(\\d+)$", "\\1", tools::file_path_sans_ext(basename(split_files$full_path))))

# Fix the column name first
names(split_files) <- "full_path"

# Extract species (everything before _p and the page number)
split_files$species <- gsub("_p\\d+$", "", tools::file_path_sans_ext(basename(split_files$full_path)))

# Extract page number
split_files$page <- as.integer(gsub(".*_p(\\d+)$", "\\1", tools::file_path_sans_ext(basename(split_files$full_path))))

head(split_files)

stats_split <- split_files |> 
  group_by(species) |> 
  summarise(
    count   = n(),
    max_page = max(page, na.rm = TRUE)
  )

text <- pdf_text(split_files$full_path[1])
cat(text)

tesseract_download("dan")

eng <- tesseract("dan")
img <- pdftools::pdf_convert(split_files$full_path[1], dpi = 300)
text <- ocr(img, engine = eng)
cat(text)
file.remove(img)

split_files$text <- sapply(split_files$full_path, function(path) {
  tryCatch({
    img <- pdftools::pdf_convert(path, dpi = 300, verbose = FALSE)
    text <- ocr(img, engine = tesseract("dan"))
    file.remove(img)
    text
  }, error = function(e) NA)
})
