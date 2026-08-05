library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

name <- "ningaloo-marine-park"


schema <- CheckEM::catami %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::select(-qualifiers)

# HABITAT -----
metadata <- read_csv(paste0("data/uploads/", name, "_metadata.csv")) %>%
  dplyr::filter(campaignid %in% "2022-05_PtCloates_stereo-BRUVs") %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/raw/2022-05/2022-05_PtCloates_stereo-BRUVS_Forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards <- read.delim("data/raw/2022-05/2022-05_PtCloates_stereo-BRUVS_Backwards_Dot Point Measurements.txt",
                        header = T, skip = 4, stringsAsFactors = FALSE,
                        colClasses = "character", na.strings = "") %>%
  clean_names()


habitat_with_schema <- bind_rows(forwards, backwards) %>%
  clean_names() %>%
  dplyr::rename(caab_code = scientific) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::mutate(caab_code = dplyr::case_when(
    is.na(caab_code) & level_2 == "Invertebrate Complex" ~ 99900044,  # Mixed sessile invertebrates
    TRUE ~ caab_code
  )) %>%
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
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
  group_by(campaignid, opcode, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::select(campaignid, opcode, level_1, everything()) %>%
  glimpse()

write_csv(tidy_habitat, "data/uploads/2022-05_PtCloates_stereo-BRUVs_benthos-count.csv")

# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/raw/2022-05/2022-05_PtCloates_stereo-BRUVS_Forwards_Relief_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/raw/2022-05/2022-05_PtCloates_stereo-BRUVS_Backwards_Relief_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names()


relief_catami <- CheckEM::catami %>%
  dplyr::filter(level_2 == "Relief") %>%
  dplyr::mutate(caab_code = as.numeric(caab_code))

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  clean_names() %>%
  dplyr::select(filename, level_5) %>%
  dplyr::mutate(relief_rank = str_extract(level_5, "\\d")) %>%   # pulls "0".."5" as character, matches catami$level_5 type
  dplyr::rename(level_5_raw = level_5) %>%
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