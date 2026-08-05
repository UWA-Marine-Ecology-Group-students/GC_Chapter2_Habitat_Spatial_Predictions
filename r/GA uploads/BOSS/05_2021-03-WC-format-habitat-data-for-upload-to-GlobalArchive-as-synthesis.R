library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(janitor)
library(stringr)

# Small helper: guarantee these columns exist (as NA) even if the schema
# join doesn't produce every CATAMI level - avoids "column doesn't exist"
# errors later in select() if your data doesn't reach level_6/7/8.
ensure_cols <- function(df, cols) {
  df <- dplyr::ungroup(df)
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    df[missing] <- NA_character_
  }
  df
}

schema <- CheckEM::catami %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::select(-qualifiers)

# HABITAT -----
metadata <- read_csv(here::here("data/uploads/west-coast-BOSS_metadata.csv")) %>%
  dplyr::filter(campaignid == "2021-03_West-Coast_BOSS") %>%
  dplyr::rename(sample = period) %>%
  glimpse()

# read in panoramic annotations
panoramic <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2021-03_West-Coast_BOSS_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$"))


names(panoramic)


habitat_with_schema <- panoramic %>%
  dplyr::mutate(caab_code = as.numeric(code)) %>%   # raw "CODE" column holds the CATAMI code
  dplyr::mutate(
    caab_code = dplyr::case_when(
      caab_code == 54080001 ~ 54079009,  # Ecklonia radiata - old caab_code, schema now uses 54079009
      caab_code == 90300910 ~ 80300910,  # Erect fine branching, Red - old caab_code, schema now uses 80300910
      TRUE ~ caab_code
    )
  ) %>%
  dplyr::left_join(schema, by = "caab_code") %>%
  dplyr::mutate(
    level_1 = dplyr::if_else(caab_code == 2, "Biota", level_1)
  ) %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species"))

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code)

missing_caab_code

names(habitat_with_schema)

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code_raw <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(sample, filename)

unique(habitat_with_schema$sample) %>% sort()

num.points <- 80

wrong_points_habitat <- habitat_with_schema %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata, by = "sample") %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample)) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = "2021-03_West-Coast_BOSS") %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species")) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::rename(period = sample) %>%
  dplyr::select(campaignid, period,
                level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8,
                family, genus, species, caab_code, count) %>%
  glimpse()

metadata.missing.habitat <- anti_join(
  metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes") %>%
    dplyr::rename(period = sample),
  tidy_habitat,
  by = c("campaignid", "period")
) %>%
  glimpse()

write_csv(tidy_habitat, here::here("data/uploads/2021-03_West-Coast_BOSS_benthos-count.csv"))



#RELIEF
relief_file <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2021-03_West-Coast_BOSS_Relief_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = "", quote = ""
) %>%
  clean_names() %>%
  glimpse()

# NOTE: `period` and `qualifiers` are blank for every row in this export.
# The sample identifier has to be pulled from the second underscore-delimited
# field in `filename` instead (e.g. "20230309-1_12_02_..." -> "12"), and the

# lookup table: one row per relief score 0-4 (caves/5 excluded - not present in this dataset)
relief_lookup <- catami %>%
  dplyr::filter(level_2 == "Relief", !is.na(level_5)) %>%
  dplyr::mutate(level_5 = as.integer(level_5)) %>%
  dplyr::distinct(level_1, level_2, level_3, level_4, level_5, caab_code)

relief_with_schema <- relief_file %>%
  dplyr::select(filename, relief) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(
    sample = stringr::str_remove(filename, "\\.jpg$|\\.JPG$"),
    relief_clean = stringr::str_remove_all(relief, '^"|"$'),      # strip stray wrapping quotes
    level_5 = as.integer(stringr::str_match(relief_clean, "^\\.(\\d)\\.")[, 2])
  ) %>%
  dplyr::filter(!is.na(level_5)) %>%
  dplyr::left_join(relief_lookup, by = "level_5") %>%
  glimpse()

# sanity check: any score that didn't find a match in the lookup (should be empty)
relief_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  dplyr::distinct(relief_clean)

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("sample")) %>%
  glimpse()

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(campaignid = "2021-03_West-Coast_BOSS", number = 1) %>%
  dplyr::group_by(campaignid, sample, level_1, level_2, level_3, level_4, level_5, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  dplyr::ungroup() %>%
  dplyr::rename(period = sample) %>%
  dplyr::select(campaignid, period, level_1, level_2, level_3, level_4, level_5, caab_code, count) %>%
  glimpse()

write_csv(tidy_relief, "data/uploads/2021-03_West-Coast_BOSS-benthos-relief.csv")

relief_samples <- tidy_relief %>%
  distinct(campaignid, period) %>%
  group_by(campaignid) %>%
  summarise(relief_sample = n(), .groups = "drop")

benthos_samples <- tidy_habitat %>%
  distinct(campaignid, period) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")

metadata_samples <- metadata %>%
  distinct(campaignid, sample) %>%
  group_by(campaignid) %>%
  summarise(metadata_sample = n(), .groups = "drop")

sample_summary <- relief_samples %>%
  full_join(benthos_samples, by = "campaignid") %>%
  full_join(metadata_samples, by = "campaignid")
sample_summary

missing_from_relief <- metadata %>%
  dplyr::rename(period = sample) %>%
  dplyr::anti_join(tidy_relief, by = c("campaignid", "period")) %>%
  dplyr::select(campaignid, period, location, site, depth_m,
                successful_count, successful_length, successful_habitat_panoramic,
                observer_habitat_panoramic)

missing_from_benthos <- metadata %>%
  dplyr::rename(period = sample) %>%
  dplyr::anti_join(tidy_habitat, by = c("campaignid", "period")) %>%
  dplyr::select(campaignid, period, location, site, depth_m,
                successful_count, successful_length, successful_habitat_panoramic,
                observer_habitat_panoramic)

missing_from_relief
missing_from_benthos
metadata.missing.relief <- anti_join(
  metadata %>% dplyr::filter(successful_habitat_panoramic == 'Yes') %>%
    dplyr::rename(period = sample),
  tidy_relief,
  by = c("campaignid", "period")
) %>%
  glimpse()

catami %>% dplyr::filter(level_2 == "Relief") %>%
  dplyr::distinct(level_3, level_4, level_5, caab_code)





