## 1) BULLETPROOF FILE READING --------------------------------------------
data_dir <- "~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/data"
file_path <- file.path(data_dir, "anholt_taxa.txt")

# Read as RAW and extract ONLY valid ASCII/UTF-8 bytes
con <- file(file_path, "rb")
raw_bytes <- readBin(con, "raw", file.info(file_path)$size)
close(con)

# Keep only safe printable ASCII + basic UTF-8 (32-127 + common letters)
safe_bytes <- raw_bytes[raw_bytes >= as.raw(32) & 
                          (raw_bytes <= as.raw(127) | 
                             raw_bytes %in% as.raw(c(229,230,248,197,198,216,216)) ) ]  # æøåÆØÅ

raw <- rawToChar(safe_bytes)

# Now safe to clean:
raw <- gsub("[\r\n\t]+", " ", raw)
raw <- gsub("[ ]{2,}", " ", raw)  
raw <- trimws(raw)

cat("SUCCESS! Length:", nchar(raw), "\n")
cat("Preview:", substr(raw, 1, 300), "\n")


## IMPROVED PARSING: every capitalized token OR lowercase epithet after digit starts new line
tokens <- unlist(regmatches(raw, gregexpr("\\S+", raw)))

# Mark likely "starts" more aggressively:
is_capitalized <- grepl("^[A-ZÆØÅ]", tokens)  # any capital start
is_epithet <- grepl("^[a-z]+$", tokens)       # lowercase-only (species names)
has_digit <- grepl("[0-9]", tokens)

phrase_id <- integer(length(tokens))
phrase_id[1] <- 1L
current_phrase <- 1L

for (i in 2:length(tokens)) {
  # New phrase if:
  # 1. Capitalized genus-like token
  # OR 2. Lowercase epithet after a number (continuations like "platanoides 4")
  # OR 3. Any token after a number block
  if (is_capitalized[i] || 
      (has_digit[i-1] && (is_epithet[i] || length(grep("[a-z]+", tokens[i])) > 0)) ||
      has_digit[i-1]) {
    current_phrase <- current_phrase + 1L
  }
  phrase_id[i] <- current_phrase
}

phrases <- tapply(tokens, phrase_id, paste, collapse = " ")
phrases <- unname(phrases)

# Keep only phrases with at least one number reference
valid_phrases <- phrases[grepl("[0-9]", phrases)]
phrases <- valid_phrases

## Simple split: everything before LAST number sequence is name/note
split_phrase <- function(x) {
  # Find position of last pure number sequence
  numbers <- unlist(regmatches(x, gregexpr("([0-9,lo]+)(?=[^0-9]|$)", x, perl=TRUE)))
  if (length(numbers) == 0) return(x)
  
  last_num_pos <- gregexpr("([0-9,lo]+)(?=[^0-9]|$)", x, perl=TRUE)[[1]][length(numbers)]
  name_part <- substr(x, 1, last_num_pos-1)
  refs <- substr(x, last_num_pos, nchar(x))
  
  paste(trimws(name_part), trimws(refs))
}

formatted <- vapply(phrases, split_phrase, character(1))

# Write and preview
writeLines(formatted, "~/Library/Mobile Documents/com~apple~CloudDocs/botany/historic_plant_data/data/anholt_formatted.txt")
cat("DONE!", length(formatted), "lines\n")
writeLines(head(formatted, 15))



## 4) Helper: does a token contain any digit? ----------------------------
has_digit <- grepl("[0-9]", tokens)

## 5) Parse into “phrases” separated by clear breaks ---------------------
## A new phrase is started when we see a capitalized token that
## *follows* a token containing any digit (i.e. after a number block),
## or it's the very first token.

is_capitalized <- grepl("^[A-ZÆØÅ][A-Za-zæøåÆØÅ.'-]*$", tokens)

phrase_id <- integer(length(tokens))
current <- 1L
phrase_id[1] <- current

for (i in 2:length(tokens)) {
  start_new <- FALSE
  
  ## start a new phrase if:
  ##  - previous token had a digit (typically end of references), AND
  ##  - current token is capitalized (a new genus or genus-like start)
  if (has_digit[i-1] && is_capitalized[i]) {
    start_new <- TRUE
  }
  
  if (start_new) current <- current + 1L
  phrase_id[i] <- current
}

length(tokens)
length(phrase_id)
head(tokens, 10)
head(phrase_id, 10)
any(is.na(phrase_id))

phrases <- tapply(tokens, phrase_id, paste, collapse = " ")
phrases <- unname(phrases)

## 6) Within each phrase, separate the “name + note” part from numbers ---
## Strategy:
##  - find the last token(s) that consist only of digits or digits+comma
##    e.g. "1,2,3,9" or "7"
##  - everything before that belongs to name/notes, that token is refs

split_phrase <- function(x) {
  tt <- strsplit(x, " ")[[1]]
  ## identify purely numeric / numeric-comma tokens
  ref_idx <- which(grepl("^[0-9,]+$", tt))
  if (length(ref_idx) == 0L) {
    ## no numeric references at all -> return as-is
    return(x)
  }
  last_ref <- tail(ref_idx, 1)
  name_part <- paste(tt[1:(last_ref - 1)], collapse = " ")
  refs      <- tt[last_ref]
  out <- paste(name_part, refs)
  trimws(out)
}

formatted <- vapply(phrases, split_phrase, character(1))

## 7) Inspect and write to file ------------------------------------------
## Look at the first 50 lines:
cat(paste(head(formatted, 50), collapse = "\n"))

## Write all to a new text file:
writeLines(formatted, "data/anholt_formatted.txt")
