library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(janitor)
library(googlesheets4)
library(stringr)

name <- "west-coast-BRUVs"

# Raw TransectMeasure/CATAMI exports name these columns differently to what
# the rest of the script (and CheckEM) expects. This batch of files uses the
# older BROAD/MORPHOLOGY/TYPE/CODE naming rather than
# level_2/level_3/level_4/scientific - confirmed by inspecting the raw files
# directly (no level_2/scientific columns exist in them at all; they do
# have their own OpCode/Period columns, but no CATAMI-style level_* fields).
standardise_catami_names <- function(df) {
  rename_map <- c(
    level_2   = "broad",
    level_3   = "morphology",
    level_4   = "type",
    caab_code = "code"
  )
  rename_map <- rename_map[rename_map %in% names(df)]
  if (length(rename_map) > 0) {
    df <- dplyr::rename(df, !!!rename_map)
  }
  df
}

schema <- CheckEM::catami %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::select(-qualifiers)

# HABITAT -----
metadata <- read_csv(paste0("data/uploads/", name, "_metadata.csv")) %>%
  dplyr::filter(campaignid %in% "2022-05_PtCloates_stereo-BRUVs") %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim(
  "data/raw/ga upload/BRUVS/habitat/2022-05_PtCloates_stereo-BRUVS_Forwards_Dot Point Measurements.txt",
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  standardise_catami_names()

# read in backwards annotations
backwards <- read.delim(
  "data/raw/ga upload/BRUVS/habitat/2022-05_PtCloates_stereo-BRUVS_Backwards_Dot Point Measurements.txt",
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  standardise_catami_names()

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::mutate(
    # Some exports append a "_N" suffix to duplicated CAAB codes
    # (e.g. "80300000_2"). Strip it before converting to numeric so the
    # code still parses instead of silently becoming NA.
    caab_code = str_remove(caab_code, "_[0-9]+$"),
    caab_code = as.numeric(caab_code),
    caab_code = dplyr::case_when(
      # "Invertebrate Complex" points have no CAAB code of their own in
      # this export - map them to the generic complex code so the join
      # below can still resolve level_1/level_2 etc.
      is.na(caab_code) & level_2 == "Invertebrate Complex" ~ 99900044,  # Mixed sessile invertebrates
      TRUE ~ caab_code
    )
  ) %>%
  dplyr::select(-starts_with("level"), -any_of(c("family", "genus", "species"))) %>%
  dplyr::left_join(CheckEM::catami %>% dplyr::mutate(caab_code = as.numeric(caab_code)), by = "caab_code") %>%
  dplyr::select(-any_of("opcode")) %>%
  separate(filename, into = c("sample", "extra"), sep = "_") %>%
  dplyr::mutate(sample = str_replace_all(sample, c(".JPG" = "", ".jpg" = ""))) %>%
  dplyr::rename(opcode = sample)

missing_caab_code <- habitat_with_schema %>% dplyr::filter(is.na(caab_code)) # should be ~empty

num.points <- 40
wrong_points_habitat <- habitat_with_schema %>%
  dplyr::count(opcode, name = "points_annotated") %>%
  dplyr::full_join(metadata %>% dplyr::select(opcode, campaignid), by = "opcode") %>%
  dplyr::mutate(points_annotated = tidyr::replace_na(points_annotated, 0)) %>%
  dplyr::filter(points_annotated != num.points) %>%
  dplyr::arrange(opcode)

wrong_points_habitat

distinct_hab_types <- habitat_with_schema %>%
  dplyr::select(starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::distinct()

missing_caab_code <- habitat_with_schema %>% dplyr::filter(is.na(caab_code))

unique(habitat_with_schema$opcode)

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("opcode")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat_with_schema, by = c("opcode")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = unique(metadata$campaignid)) %>%
  dplyr::select(campaignid, opcode, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("", "Unscorable", "Open Water", "Unknown", NA)) %>%
  group_by(campaignid, opcode, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::select(campaignid, opcode, level_1, everything()) %>%
  glimpse()

write_csv(tidy_habitat, "data/uploads/2022-05_PtCloates_stereo-BRUVs_benthos-count.csv")

# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim(
  "data/raw/ga upload/BRUVS/habitat/2022-05_PtCloates_stereo-BRUVS_Forwards_Relief_Dot Point Measurements.txt",
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names()

# read in backwards annotations
backwards_relief <- read.delim(
  "data/raw/ga upload/BRUVS/habitat/2022-05_PtCloates_stereo-BRUVS_Backwards_Relief_Dot Point Measurements.txt",
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names()

# The relief annotation text (e.g. ".3. Good relief structure...") is
# already in a column literally called "relief" in this export - no column
# swapping needed here, unlike the habitat file above.
relief_catami <- CheckEM::catami %>%
  dplyr::filter(level_2 == "Relief") %>%
  dplyr::mutate(caab_code = as.numeric(caab_code))

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::select(filename, relief) %>%
  dplyr::mutate(relief_rank = str_extract(relief, "\\d")) %>%   # pulls "0".."5" as character, matches catami$level_5 type
  dplyr::rename(relief_raw = relief) %>%
  dplyr::left_join(relief_catami, by = c("relief_rank" = "level_5")) %>%
  separate(filename, into = c("sample", "extra"), sep = "_") %>%
  dplyr::mutate(sample = str_replace_all(sample, c(".JPG" = "", ".jpg" = ""))) %>%
  dplyr::rename(opcode = sample)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = "opcode") %>% glimpse()

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = "opcode") %>% glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = unique(metadata$campaignid)) %>%
  dplyr::select(campaignid, opcode, number, level_1, level_2, level_3, level_4, relief_rank, caab_code) %>%
  dplyr::filter(!is.na(relief_rank)) %>%
  group_by(campaignid, opcode, level_1, level_2, level_3, level_4, relief_rank, caab_code) %>%
  dplyr::rename(level_5 = relief_rank) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  glimpse()

write_csv(tidy_relief, "data/uploads/2022-05_PtCloates_stereo-BRUVs_benthos-relief.csv")
