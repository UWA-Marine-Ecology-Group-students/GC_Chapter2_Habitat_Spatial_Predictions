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

metadata <- read_csv(here::here("data/uploads/west-coast-BOSS_metadata.csv")) %>%
  dplyr::filter(campaignid == "2021-05_PtCloates_BOSS") %>%
  dplyr::rename(sample = period) %>%
  glimpse()

unique(metadata$campaignid)

# read in panoramic annotations
panoramic <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2021-05_PtCloates_BOSS_Dot Point Measurements.txt"), # update path for old file
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::rename(level_2 = broad, level_3 = morphology, level_4 = type, 
                level_5 = field_of_view, scientific = relief) %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$"))

habitat_with_schema <- panoramic %>%
  rename(caab_code = code) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::mutate(
    caab_code = dplyr::case_when(
      !is.na(caab_code) ~ caab_code,
      level_2 == "Invertebrate Complex" & level_3 == "Complex 1" ~ 99900044,
      level_2 == "Sponges" & level_3 == "Hollow formed" ~ 10000000,
      TRUE ~ caab_code
    )
  ) %>%
  dplyr::select(-level_2, -level_3, -level_4, -level_5, -scientific) %>%
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
  distinct(sample, filename)

unique(habitat_with_schema$sample) %>% sort()

wrong_points_habitat <- habitat_with_schema %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%   # works now once metadata has `sample` from the rename
  dplyr::mutate(expected = dplyr::case_when(
    successful_habitat_panoramic %in% "Yes" ~ num.points,
    successful_habitat_panoramic %in% "No"  ~ 0
  )) %>%
  dplyr::filter(points.annotated != expected) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2021-05_PtCloates_BOSS") %>%
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
            dplyr::rename(period = sample),"data/uploads/2021-05_PtCloates_BOSS_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
relief <- read.delim("data/raw/ga upload/BOSS/habitat/2021-05_PtCloates_BOSS_Relief_Dot Point Measurements.txt", 
                     header = T, skip = 4, stringsAsFactors = FALSE, 
                     colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  glimpse

relief_with_schema <- relief %>%
  # dplyr::rename(relief = scientific) %>%   <- remove for old-format file
  dplyr::select(filename, relief) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = ""))) %>%
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
  dplyr::mutate(campaignid = "2021-05_PtCloates_BOSS") %>%
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
            dplyr::rename(period = sample), "data/uploads/2021-05_PtCloates_BOSS_benthos-relief.csv")
