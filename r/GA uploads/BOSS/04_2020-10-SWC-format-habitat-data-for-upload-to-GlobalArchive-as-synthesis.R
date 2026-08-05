library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(janitor)
library(stringr)

# install.packages("remotes")   # skip if already installed
# remotes::install_github("GlobalArchiveManual/CheckEM")
# remotes::install_github("GlobalArchiveManual/CheckEM", dependencies = TRUE, upgrade = "always")
# library(CheckEM)
packageVersion("CheckEM")   # should now read 1.0.2
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
  dplyr::filter(campaignid == "2020-10_south-west_BOSS") %>%
  dplyr::rename(sample = period) %>%
  glimpse()


# read in north annotations
north <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_north_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, regex("\\.jpg$", ignore_case = TRUE))) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

unique(north$sample)

# read in south annotations
south <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_south_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$")) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

unique(south$sample)

# read in west annotations
west <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_west_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$")) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# read in east annotations
east <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_east_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$")) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# read in 50 point j annotations
multibeamed <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-11_south-west_BOSS_multibeamed_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, "\\.jpg$")) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))


flat_codes <- schema %>%
  dplyr::filter(is.na(level_5), is.na(family), is.na(genus), is.na(species)) %>%
  dplyr::distinct(level_2, level_3, level_4, .keep_all = TRUE)

dupes <- schema %>%
  dplyr::count(level_2, level_3, level_4) %>%
  dplyr::filter(n > 1)
dupes

# check that every one of the 9 ambiguous combos now has exactly 1 match
flat_codes %>%
  dplyr::semi_join(dupes, by = c("level_2", "level_3", "level_4")) %>%
  dplyr::count(level_2, level_3, level_4) %>%
  dplyr::filter(n != 1)

# genus/species lookup for macroalgae species annotated as full binomials
macroalgae_species_lookup <- schema %>%
  dplyr::filter(level_2 == "Macroalgae", level_3 == "Large canopy-forming", !is.na(genus)) %>%
  dplyr::mutate(binomial = paste(genus, species))

# genus lookup for seagrasses annotated at genus level (e.g. "Thalassodendron sp.")
seagrass_genus_lookup <- schema %>%
  dplyr::filter(level_2 == "Seagrasses", !is.na(genus)) %>%
  dplyr::distinct(genus, .keep_all = TRUE)

# one-level-deep schema: keeps level_5 detail where it exists (corals, pebble/sand),
# but only where there's no genus/species/level_6+ complexity our raw data can't capture
level5_lookup <- schema %>%
  dplyr::filter(is.na(level_6), is.na(family), is.na(genus), is.na(species)) %>%
  dplyr::distinct(level_1, level_2, level_3, level_4, level_5, .keep_all = TRUE)

habitat_raw <- bind_rows(north, south, west, east, multibeamed) %>%
  dplyr::mutate(
    # fix known annotator typo before anything else touches `type`
    type = dplyr::if_else(
      broad == "Seagrasses",
      str_replace(type, "^Thalassodendrum", "Thalassodendron"),
      type
    ),
    seagrass_genus = dplyr::if_else(
      broad == "Seagrasses",
      str_extract(type, "^[A-Za-z]+"),   # first word, e.g. "Thalassodendron"
      NA_character_
    ),
    level_2 = dplyr::case_when(
      broad %in% c("Consolidated", "Unconsolidated") ~ "Substrate",
      broad %in% c("Stony corals", "Octocoral/Black")  ~ "Cnidaria",
      broad == "Hydroids"                              ~ "Cnidaria",   # <- new
      broad == "Invertebrate Complex" & morphology == "Complex 1" ~ "Sessile invertebrates",
      TRUE ~ broad
    ),
    level_3 = dplyr::case_when(
      broad == "Consolidated"   ~ "Consolidated (hard)",
      broad == "Unconsolidated" ~ "Unconsolidated (soft)",
      broad %in% c("Stony corals", "Octocoral/Black") ~ "Corals",
      broad == "Hydroids"                              ~ "Hydroids",   # <- new
      broad == "Invertebrate Complex" & morphology == "Complex 1" ~ NA_character_,
      morphology == "Erect course branching"    ~ "Erect coarse branching",
      morphology == "Filamentous and filiform"  ~ "Filamentous / filiform",
      morphology == "Sheet-like membranous"     ~ "Sheet-like / membraneous",
      TRUE ~ morphology
    ),
    level_4 = dplyr::case_when(
      broad == "Stony corals"     ~ "Stony corals",
      broad == "Octocoral/Black"  ~ "Black & Octocorals",
      broad == "Consolidated"     ~ NA_character_,   # <- always use the flat Consolidated (hard) code
      broad == "Unconsolidated" & morphology == "Sand"   ~ "Sand / mud (<2mm)",
      broad == "Unconsolidated" & morphology == "Pebble" ~ "Pebble / gravel",
      type == "Unknown"           ~ NA_character_,   # <- annotator left type blank/unknown, fall back to level_3 flat code
      TRUE ~ type
    ),
    level_5 = dplyr::case_when(
      broad %in% c("Stony corals", "Octocoral/Black") ~ morphology,
      morphology == "Pebble" & type == "gravel (gravel 2-10mm)"  ~ "Gravel (2-10mm)",
      morphology == "Pebble" & type == "gravel (pebble 10-64mm)" ~ "Pebble (10-64mm)",
      morphology == "Pebble" & type == "gravel (biogenic)"       ~ "Biologenic",
      TRUE ~ NA_character_
    ),
    # capture the macroalgae binomial before it gets lost
    binomial = dplyr::if_else(broad == "Macroalgae" & morphology == "Large canopy-forming", type, NA_character_)
  ) %>%
  dplyr::filter(!level_2 %in% c("", "Unknown", "Open Water", NA))

# split: rows with a known species binomial join on genus/species; everything else joins on level_2:5
habitat_species <- habitat_raw %>%
  dplyr::filter(binomial %in% macroalgae_species_lookup$binomial) %>%
  dplyr::left_join(macroalgae_species_lookup %>% dplyr::select(binomial, level_1, level_4, family, genus, species, caab_code),
                   by = "binomial") %>%
  dplyr::mutate(level_2 = "Macroalgae", level_3 = "Large canopy-forming")

habitat_seagrass <- habitat_raw %>%
  dplyr::filter(seagrass_genus %in% seagrass_genus_lookup$genus) %>%
  dplyr::left_join(
    seagrass_genus_lookup %>%
      dplyr::select(genus, level_1, level_4, family, species, caab_code),
    by = c("seagrass_genus" = "genus")
  ) %>%
  dplyr::mutate(level_2 = "Seagrasses", level_3 = "Strap-like leaves", genus = seagrass_genus)

habitat_generic <- habitat_raw %>%
  dplyr::filter(!(broad == "Seagrasses" & seagrass_genus %in% seagrass_genus_lookup$genus)) %>%
  dplyr::filter(!binomial %in% macroalgae_species_lookup$binomial) %>%
  dplyr::left_join(level5_lookup, by = c("level_2", "level_3", "level_4", "level_5"))


habitat_with_schema <- bind_rows(habitat_species, habitat_generic, habitat_seagrass) %>%
  ensure_cols(c("level_1", "level_2", "level_3", "level_4", "level_5",
                "level_6", "level_7", "level_8", "family", "genus", "species"))

habitat_with_schema_1 <- habitat_with_schema %>%
  dplyr::mutate(
    caab_code = dplyr::case_when(
      level_2 == "Sponges" & level_3 %in% c("Small mixed", "Hollow forms") ~ 10000000,
      level_2 == "Macroalgae" & level_3 == "Small mixed"                   ~ 80300000,
      TRUE ~ caab_code    # <- drop the `level_2 == "Hydroids" ~ 11001000` line
    ),
    level_1 = dplyr::case_when(
      level_2 %in% c("Sponges", "Macroalgae") & is.na(level_1) ~ "Biota",   # <- drop "Hydroids" from this list
      TRUE ~ level_1
    )
  )

habitat_with_schema_2 <- habitat_with_schema_1 %>%
  dplyr::mutate(
    level_1   = dplyr::if_else(level_2 == "Crinoids", "Biota", level_1),
    level_3   = dplyr::if_else(level_2 == "Crinoids", "Feather stars", level_3),
    caab_code = dplyr::if_else(level_2 == "Crinoids", 25001000, caab_code),   # <- updated to 25001000
    level_2   = dplyr::if_else(level_2 == "Crinoids", "Echinoderms", level_2)
  )

missing_caab_code <- habitat_with_schema_2 %>%
  dplyr::filter(is.na(level_1)) %>%
  distinct(caab_code)

missing_caab_code

names(habitat_with_schema_2)

distinct_hab_types <- habitat_with_schema_2 %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code_raw <- habitat_with_schema_2 %>%
  dplyr::filter(is.na(caab_code)) %>%
  distinct(sample, filename)

unique(habitat_with_schema$sample) %>% sort()

num.points <- 80

wrong_points_habitat <- habitat_with_schema_2 %>%
  dplyr::filter(!level_2 %in% c("", "Unscorable", NA)) %>%
  group_by(sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata, by = "sample") %>%
  dplyr::filter(points.annotated != 80) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat_with_schema_2, metadata, by = c("sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema_2 %>%
  dplyr::mutate(sample = str_trim(sample)) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_BOSS") %>%
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

tidy_habitat_final <- tidy_habitat %>%
  group_by(campaignid, period, level_1, level_2, family, genus, species, caab_code) %>%
  dplyr::summarise(count = sum(count), .groups = "drop") %>%
  dplyr::mutate(
    level_3 = NA_character_,
    level_4 = NA_character_,
    level_5 = NA_character_,
    level_6 = NA_character_,
    level_7 = NA_character_,
    level_8 = NA_character_
  ) %>%
  dplyr::select(campaignid, period,
                level_1, level_2, level_3, level_4, level_5, level_6, level_7, level_8,
                family, genus, species, caab_code, count)

metadata.missing.habitat <- anti_join(
  metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes") %>%
    dplyr::rename(period = sample),
  tidy_habitat,
  by = c("campaignid", "period")
) %>%
  glimpse()

write_csv(tidy_habitat_final, here::here("data/uploads/2020-10_south-west_BOSS_benthos-count.csv"))

distinct_hab_types <- habitat_with_schema %>%
  select(starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

distinct_hab_types %>% dplyr::filter(is.na(caab_code))

habitat_with_schema %>%
  dplyr::filter(caab_code %in% c(90300910, 54080001)) %>%
  dplyr::distinct(sample, filename, code)

# check what CATAMI category number sits either side of it in the schema,
# for a clue as to what family of codes it belongs to
schema %>% dplyr::filter(caab_code %in% c(90300909, 90300911, 54079999, 54080002))

#RELIEF
# read in north relief annotations
north_relief <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_north_relief_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, regex("\\.jpg$", ignore_case = TRUE))) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# read in south relief annotations
south_relief <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_south_relief_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, regex("\\.jpg$", ignore_case = TRUE))) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# read in east relief annotations
east_relief <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_east_relief_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, regex("\\.jpg$", ignore_case = TRUE))) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# read in west relief annotations
west_relief <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/2020-10_south-west_BOSS_west_relief_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, regex("\\.jpg$", ignore_case = TRUE))) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# read in multibeamed relief annotations
multibeamed_relief <- read.delim(
  here::here("data/raw/ga upload/BOSS/habitat/20201119_Multibeamed_BRUVstyle_Dot Point Measurements.txt"),
  header = TRUE, skip = 4, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = ""
) %>%
  clean_names() %>%
  dplyr::mutate(sample = str_remove(filename, regex("\\.jpg$", ignore_case = TRUE))) %>%
  dplyr::mutate(sample = str_remove(sample, "[NSEWB](?=REDO$|$)"))

# CATAMI relief categories: level_1/2/3/4 + caab_code, keyed by the raw
# numeric score (0-4), which sits in level_5 for this category
relief_schema_lookup <- schema %>%
  dplyr::filter(level_2 == "Relief") %>%
  dplyr::distinct(level_1, level_2, level_3, level_4, level_5, caab_code) %>%
  dplyr::mutate(level_5 = as.character(level_5))

relief_with_schema <- bind_rows(north_relief, south_relief, east_relief, west_relief, multibeamed_relief) %>%
  dplyr::rename(relief_score = relief) %>%
  dplyr::filter(!is.na(relief_score)) %>%
  dplyr::mutate(relief_score = str_extract(relief_score, "(?<=\\.)\\d")) %>%   # <- raw text is like ".3. Good relief structure..."; pull out the leading digit so it matches schema level_5 ("0"-"5")
  dplyr::left_join(relief_schema_lookup, by = c("relief_score" = "level_5")) %>%
  dplyr::rename(level_5 = relief_score) %>%
  glimpse()

# any raw scores that didn't match a schema category (should be empty)
relief_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>%
  dplyr::distinct(level_5)

num.points <- 80

# flag any samples with an unexpected number of annotated points
relief_with_schema %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(points.annotated = n()) %>%
  dplyr::filter(points.annotated != num.points)

# samples present in relief but missing from metadata (should be empty)
relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample")) %>%
  glimpse()

# samples in metadata with no relief annotations
# (SS-09, SS-11, SS-14 expected here - they have no successful_count/length in metadata)


tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%
  dplyr::mutate(campaignid = "2020-10_south-west_BOSS") %>%
  dplyr::select(campaignid, sample, level_1, level_2, level_3, level_4, level_5, caab_code, number) %>%
  group_by(campaignid, sample, level_1, level_2, level_3, level_4, level_5, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%
  dplyr::rename(period = sample) %>%
  glimpse()

metadata.missing.relief <- anti_join(
  metadata %>% dplyr::filter(successful_habitat_panoramic == 'Yes') %>%
    dplyr::rename(period = sample),
  tidy_relief,
  by = c("campaignid", "period")
) %>%
  glimpse()

write_csv(tidy_relief, "data/uploads/2020-10_south-west_BOSS-benthos-relief.csv")

# SUMMARY ----

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
  full_join(metadata_samples, by = "campaignid") %>%
  dplyr::mutate(
    metadata_vs_relief  = metadata_sample - relief_sample,
    metadata_vs_benthos = metadata_sample - benthos_sample
  )

sample_summary

# which sample(s) are missing, and were they even expected to have habitat/relief?
samples_missing_annotations <- metadata %>%
  dplyr::rename(period = sample) %>%
  dplyr::mutate(
    in_relief  = period %in% tidy_relief$period,
    in_benthos = period %in% tidy_habitat$period
  ) %>%
  dplyr::filter(!in_relief | !in_benthos) %>%
  dplyr::select(campaignid, period, location, site, depth_m,
                successful_count, successful_length, successful_habitat_panoramic,
                observer_habitat_panoramic, in_relief, in_benthos)

samples_missing_annotations

# samples in relief but not in habitat
setdiff(unique(tidy_relief$period), unique(tidy_habitat$period))

# samples in habitat but not in relief
setdiff(unique(tidy_habitat$period), unique(tidy_relief$period))

