#### loading packages ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("inbo/inborutils")
pacman::p_load(tidyverse, googlesheets4, rgbif, ids, lubridate,janitor, readxl, writexl, diffobj)

#### importing data ############################################################
#DBF_navneliste_6_version_06_01_2026
#DBF_navneliste_7_version_24_02_2026 
#DBF_navneliste_8_version_21_04_2026 

#DBF_navneliste_9_version_21_05_2026 <- read_excel(file.choose())
DBF_navneliste_8_version_21_04_2026 <- read_excel("~/Google Drive/My Drive/navneudvalget/DBF navneliste 8. version 21-04-2026.xlsx")
#seneste_liste <- read_excel("~/Google Drive/My Drive/navneudvalget/DBF navneliste 8. version 21-04-2026.xlsx")

df_v9 <- DBF_navneliste_9_version_21_04_2026 |>
  mutate(n_filled = rowSums(across(everything(), ~ !is.na(.x))), index = row_number())

df_clean_v9 <- df_v9 |>
  # Identify Latin genus names (no space)
  mutate(genus_marker = ifelse(
    !is.na(`Videnskabeligt navn`) & !grepl(" ", `Videnskabeligt navn`),
    `Videnskabeligt navn`,
    NA
  )) |>
  # Identify Danish genus names (ending with "slægten")
  mutate(dansk_slaegt_marker = ifelse(
    grepl("slægten$", `Accepterede danske navne`),
    `Accepterede danske navne`,
    NA
  )) |>
  # Fill both downwards
  tidyr::fill(genus_marker, dansk_slaegt_marker, .direction = "down") |>
  # Rename for clarity
  mutate(
    rank = case_when(
      grepl("slægten", `Accepterede danske navne`, ignore.case = TRUE) ~ "slægt",
      grepl("var\\.", `Videnskabeligt navn`, ignore.case = TRUE) ~ "varitet",
      grepl("subsp\\.", `Videnskabeligt navn`, ignore.case = TRUE) ~ "underart",
      grepl("×", `Videnskabeligt navn`) ~ "hybrid",
      TRUE ~ "art"
    )
  ) |> 
  rename(genus = genus_marker,
         `Dansk slægt` = dansk_slaegt_marker) |> 
  select(`Accepterede danske navne`, `Videnskabeligt navn`,`Dansk slægt`,rank,genus,index,n_filled) |> 
  #filter(!(is.na(`Accepterede danske navne`) & is.na(`Videnskabeligt navn`)))
  filter(!is.na(`Accepterede danske navne`)) |> 
  group_by(`Dansk slægt`) |> 
  mutate(count = n())


#### lave printliste ##########################################################

print <- df_clean |> 
  filter(rank != "slægt") |> 
  mutate(latex1 = "&\\textit{",
         latex2 = "}&",
         latex3 = "&\\textit{",
         latex4 = "}",
         latex5 = "\\\\") |> 
  select(`Accepterede danske navne`,latex1,`Videnskabeligt navn`,latex2,`Dansk slægt`,latex3,genus,latex4,latex5)

anden_export <- print |> 
  select(`Accepterede danske navne`,`Videnskabeligt navn`,`Dansk slægt`,genus)

unique(df_clean$rank)

rank <- df_clean |> 
  group_by(rank) |> 
  summarise(antal = n())

# Define an output path
output_path <- "~/Google Drive/My Drive/navneudvalget/DBF_navneliste_print_2026_05_21.txt"
output_path_csv <- "~/Google Drive/My Drive/navneudvalget/DBF_navneliste_print_2026_05_21.csv"

# Write to Excel
write_delim(print, output_path, delim = " ")
write_delim(export, output_path_csv, delim = " ")

write.table(print, file = output_path, sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE, eol = "\n")

write_xlsx(anden_export,  "/Users/ibdj/Nextcloud/Botanisk Forening/Hovedbestyrelsen/Udvalg og samarbejde/Navneudvalget/DBF_navneliste_print2026_04_21.xlsx")

#### matching til GBIF ########################################################

taxa <- df |> clean_names() |>  
  mutate(verbatim_name = videnskabeligt_navn) |> 
  filter(!is.na(accepterede_danske_navne)) |> 
  mutate(
    rang = case_when(
      grepl("slægten", accepterede_danske_navne, ignore.case = TRUE) ~ "slægt",
      grepl("var\\.", videnskabeligt_navn , ignore.case = TRUE) ~ "varitet",
      grepl("subsp\\.", videnskabeligt_navn, ignore.case = TRUE) ~ "underart",
      grepl("×", videnskabeligt_navn) ~ "hybrid",
      TRUE ~ "art"
    ),
    rang_engelsk = case_when(
      grepl("slægten", accepterede_danske_navne, ignore.case = TRUE) ~ "GENUS",
      grepl("var\\.", videnskabeligt_navn , ignore.case = TRUE) ~ "VARIETY",
      grepl("subsp\\.", videnskabeligt_navn, ignore.case = TRUE) ~ "SUBSPECIES",
      grepl("×", videnskabeligt_navn) ~ "HYBRID",
      TRUE ~ "SPECIES"
    )
  ) 

names(taxa)

#### slægt-art mismatch ########################################################

slægt_art_mismatch <- taxa |> 
  mutate(
    slægt_navn = case_when(
      tolower(sub("slægten$", "", `Dansk slægt`)) == "pile" ~ "pil",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "rosen" ~ "rose",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "lærke" ~ "lærk",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "fyrre" ~ "fyr",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "rønne" ~ "røn",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "elme" ~ "elm",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "ege" ~ "eg",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "elle" ~ "el",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "birke" ~ "birk",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "aske" ~ "ask",
      tolower(sub("slægten$", "", `Dansk slægt`)) == "linde" ~ "lind",
      TRUE ~ tolower(sub("slægten$", "", `Dansk slægt`))
    ),
    genus_dk = sub("^[^- ]+[- ]+([^ ]+).*", "\\1", `Accepterede danske navne`),
    epitet = ifelse(rank != "slægt" & grepl("[- ]", `Accepterede danske navne`),sub("([^- ]+[- ]).*", "\\1", `Accepterede danske navne`),""),
    expected = case_when(
      rank != "slægt" & epitet != "" ~ paste0(epitet, slægt_navn),
      rank != "slægt" & epitet == ""  ~ paste0(toupper(substr(slægt_navn, 1, 1)), substr(slægt_navn, 2, nchar(slægt_navn))),
      TRUE ~ ""
    ),
    match    = ifelse(rank != "slægt", `Accepterede danske navne` == expected, ""),
    detail   = ifelse(rank != "slægt", ifelse(match, "", paste0("Kunne være ", expected, ", men er ", `Accepterede danske navne`)
      ),"")
  )
names(slægt_art_mismatch )

gs4_auth()
gs4_create("slægt_art_mismatch_results", sheets = slægt_art_mismatch)

slægt_art_mismatch_kun <- slægt_art_mismatch |> 
  filter(detail != "", epithet != "")



kun_en_art <- df_clean |> 
  filter(count == 2, rank == "art")

kun_en_art2 <- kun_en_art |>
  mutate(
    expected = paste0(`Accepterede danske navne`, "slægten"),
    match = `Dansk slægt` == expected,
    detail = ifelse(
      match,
      "",
      paste0("Kunne være ", sub("slægten$", "", `Dansk slægt`), " men er ", `Accepterede danske navne`)
    )
  )

names(kun_en_art2)
write_xlsx(kun_en_art2 |> select(`Accepterede danske navne`,`Videnskabeligt navn`,`Dansk slægt`,detail), "/Users/ibdj/Library/Mobile Documents/com~apple~CloudDocs/dbf/navneudvalget/2025 11 14 DBF_navneliste_kun_en_art.xlsx")

slægter <- df_clean |> 
  filter(rank == "slægt")

hybrider <- df_clean |> 
  filter(rank == "hybrid")

arter <- df_clean |> 
  filter(rank == "art")

#### export af text filer ####

export <- df_clean |>
  filter(rank != "slægt") |>
  select(`Accepterede danske navne`,`Videnskabeligt navn`,`Dansk slægt`,genus) |>
  rename(Accepteret_dansk = `Accepterede danske navne`, Videnskabeligt_navn = `Videnskabeligt navn`, Dansk_slægt = `Dansk slægt`, Videnskabelig_slægt = genus)
#### arter som hedder det samme som slægten ####################################

etleddet_samme_slægt <- slægt_art_mismatch |> 
  filter(!grepl("[[:space:]-]", `Accepterede danske navne`), rank != "slægt") |> 
  filter(match == TRUE, count > 2)

#### navne uden dansk arts epitet (étledet navn) ############################### 

etleddet <- df_clean |> 
  filter(!grepl("[[:space:]-]", `Accepterede danske navne`), rank != "slægt") |> 
  mutate(arter_i_slægten = count-1) |> 
  select(!c(count, n_filled, index))

nrow(etleddet[etleddet$arter_i_slægten == 1, ])

df_etleddet_fleretaxa <- etleddet |> 
  filter(arter_i_slægten > 1)

ss <- gs4_create(
  name  = "2026_01_16_etledede_navne",
  sheets = list(etleddet = etleddet)  # sheet name = "etleddet"
)

#### epitet kategorier ####

epiteter <- slægt_art_mismatch |> 
  group_by(epithet) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

#write_csv(epiteter, "epiteter_kategorier.csv")

epiteter_kategorier <- read_csv("epiteter_kategorier.csv")
names(epiteter_kategorier)  
epiteter_kategorier$Kategori <- as.factor(epiteter_kategorier$Kategori)

summary(epiteter_kategorier)

names(slægt_art_mismatch)

med_kategorier <- slægt_art_mismatch |> 
  left_join(epiteter_kategorier, by = "epitet") |> 
  filter(rank != "slægt") |> 
  select(genus_dk,epitet,Kategori)

summary(med_kategorier)

med_kategorier_stats <- med_kategorier |> 
  group_by(epitet,Kategori) |> 
  summarise(count = n())

med_kategorier_stats

slægt_art_mismatch[slægt_art_mismatch$epithet == "Volga-", ]

#### comparing files ####

library(stringdist)

v8 <- df_clean_v8
v9 <- df_clean_v9
v8 <- ungroup(v8)
v9 <- ungroup(v9)
key <- "Videnskabeligt navn"
common_cols <- intersect(names(v8), names(v9))
threshold   <- 0.10                          # tune after inspecting fuzzy matches
# columns that DECIDE whether a row changed
compare_cols <- c("Accepterede danske navne", "Videnskabeligt navn")

row_str <- function(df) do.call(paste, c(df[compare_cols], sep = "\u0001"))
v8$.row <- row_str(v8)
v9$.row <- row_str(v9)

## Phase 1 — match on scientific name (clears every row whose name is unchanged)
shared <- intersect(v8[[key]], v9[[key]])
i8 <- match(shared, v8[[key]]); i9 <- match(shared, v9[[key]])
unchanged_keys <- shared[v8$.row[i8] == v9$.row[i9]]
changed_keys   <- setdiff(shared, unchanged_keys)

orphan_old_rows <- which(!(v8[[key]] %in% shared))   # v8 rows: removed or name-corrected
orphan_new_rows <- which(!(v9[[key]] %in% shared))   # v9 rows: added or name-corrected
old_orphan <- v8[orphan_old_rows, , drop = FALSE]
new_orphan <- v9[orphan_new_rows, , drop = FALSE]

## Phase 2 — fuzzy match orphans on edit distance
pairs <- data.frame(old = integer(), new = integer(), dist = numeric())
if (nrow(old_orphan) && nrow(new_orphan)) {
  Dn <- stringdistmatrix(old_orphan$.row, new_orphan$.row, method = "lv") /
    outer(nchar(old_orphan$.row), nchar(new_orphan$.row), pmax)
  repeat {
    if (all(is.na(Dn)) || min(Dn, na.rm = TRUE) > threshold) break
    ij <- which(Dn == min(Dn, na.rm = TRUE), arr.ind = TRUE)[1, ]
    pairs <- rbind(pairs, data.frame(old = ij[1], new = ij[2], dist = Dn[ij[1], ij[2]]))
    Dn[ij[1], ] <- NA; Dn[, ij[2]] <- NA
  }
}
matched_new_v9 <- orphan_new_rows[pairs$new]
matched_old_v8 <- orphan_old_rows[pairs$old]

## ---- Build one consolidated table, relative to the NEW version ----
out_new <- v9[, common_cols, drop = FALSE]
out_new$change_type   <- "Unchanged"
out_new$previous_name <- NA_character_
out_new$edit_distance <- NA_real_

out_new$change_type[v9[[key]] %in% changed_keys] <- "Content changed since last version"
out_new$change_type[orphan_new_rows]             <- "Added since last version"
out_new$change_type[matched_new_v9]              <- "Name/spelling changed since last version"
out_new$previous_name[matched_new_v9]            <- v8[[key]][matched_old_v8]
out_new$edit_distance[matched_new_v9]            <- round(pairs$dist, 3)

removed_old_v8 <- setdiff(orphan_old_rows, matched_old_v8)
out_removed <- v8[removed_old_v8, common_cols, drop = FALSE]
out_removed$change_type   <- "Removed since last version"
out_removed$previous_name <- NA_character_
out_removed$edit_distance <- NA_real_


changes <- bind_rows(out_new, out_removed) |>
  relocate(change_type, previous_name, edit_distance) |>
  arrange(change_type, .data[[key]]) |>
  filter(!(change_type %in% c("Unchanged")))

count(changes, change_type)   # overview of how many fell into each category
