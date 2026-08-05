library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(janitor)
library(stringr)

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

physical_categories <- c("Substrate", "Relief", "Bedform")
num.points <- 80

# HABITAT -----

metadata <- read_metadata(here::here("data/raw"), method = "BOSS") %>%
  dplyr::filter(campaignid == "2022-05_PtCloates_Naked-BOSS") %>%
  dplyr::select(campaignid, sample, emob, longitude_dd, latitude_dd, date_time,
                location, site, depth_m, successful_count, successful_length, successful_habitat_panoramic, observer_habitat_panoramic) %>%
  glimpse()

# read in panoramic annotations
panoramic <- read.delim(
  here::here("data/raw/2022-05 Naked/2022-05_PtCloates_Naked-BOSS_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_match(filename, "NAKED_(.*?)_[0-9]+_")[, 2]) %>% 
  tidyr::separate(filename, into = c("emob", "extra"), sep = "_") %>%
  dplyr::select(-extra) %>%
  # dplyr::mutate(sample = if_else((sample %in% "PCFB15" & emob %in% "20210826-3-FLASH"), "PCFB15.1", sample)) %>%
  # dplyr::mutate(sample = if_else((sample %in% "PCFB15" & emob %in% "20210908-6-FLASH"), "PCFB15.2", sample)) %>%
  # dplyr::mutate(sample = if_else((sample %in% "PCFB1" & emob %in% "20210908-3-FLASH"), "PCFB1.1", sample)) %>%
  # dplyr::mutate(sample = if_else((sample %in% "PCFB1" & emob %in% "20210908-4-FLASH"), "PCFB1.2", sample)) %>%
  glimpse

head(panoramic)

habitat_with_schema <- panoramic %>%
  rename(caab_code = scientific) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  # TransectMeasure doesn't auto-populate a caab_code for every valid
  # category. Patch the known cases here before we drop the raw text
  # columns and join on caab_code.
  dplyr::mutate(
    caab_code = dplyr::case_when(
      !is.na(caab_code) ~ caab_code,
      level_2 == "Invertebrate Complex" & level_3 == "Complex 1" ~ 99900044,
      level_2 == "Substrate > Unconsolidated (soft)" &
        level_3 == "Sand / mud (<2mm)" &
        level_4 == "Coarse sand (with shell fragments)" ~ 82001014,
      TRUE ~ caab_code
    )
  ) %>%
  dplyr::select(-level_2, -level_3, -level_4, -level_5) %>%
  dplyr::left_join(schema) %>%
  dplyr::mutate(
    level_1 = dplyr::if_else(caab_code == 2, "Biota", level_1)
  )

unmapped_categories <- panoramic %>%
  dplyr::mutate(caab_code = as.numeric(scientific)) %>%
  dplyr::filter(is.na(caab_code)) %>%
  dplyr::distinct(level_2, level_3, level_4, level_5) %>%
  glimpse()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code)

missing_caab_code

names(habitat_with_schema)

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code, level_2, level_3)

missing_caab_code_raw <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(sample)

unique(habitat_with_schema$sample) %>% sort()

wrong_points_habitat <- habitat_with_schema %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::filter(points.annotated != num.points) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2022-05_PtCloates_Naked-BOSS") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

metadata.missing.habitat <- anti_join(
  metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes"),
  tidy_habitat,
  by = c("campaignid", "sample")
) %>%
  glimpse()

write_csv(tidy_habitat %>%
            dplyr::rename(period = sample),"data/uploads/2022-05_PtCloates_Naked-BOSS_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
relief <- read.delim("data/raw/2022-05 Naked/2022-05_PtCloates_Naked-BOSS_Relief_Dot Point Measurements.txt", 
                     header = T, skip = 4, stringsAsFactors = FALSE, 
                     colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  glimpse

relief_with_schema <- relief %>%
  dplyr::rename(relief = level_5) %>%
  dplyr::select(filename, relief) %>%
  dplyr::mutate(sample = str_match(filename, "NAKED_(.*?)_[0-9]+_")[, 2]) %>%
  tidyr::separate(filename, into = c("emob", "extra"), sep = "_") %>%
  dplyr::select(-extra) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(level_5 = str_sub(relief, 2, 2)) %>%
  dplyr::filter(!level_5 %in% "n") %>%
  dplyr::left_join(catami) %>%
  glimpse

unique(relief_with_schema$level_5)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(sample = str_trim(sample)) %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2022-05_PtCloates_Naked-BOSS") %>%
  dplyr::mutate(level_1 = "Physical") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes"),
                                     tidy_relief,
                                     by = c("campaignid", "sample")
) %>% 
  glimpse()

write_csv(tidy_relief %>%
            dplyr::rename(period = sample), "data/uploads/2022-05_PtCloates_Naked-BOSS_benthos-relief.csv")


