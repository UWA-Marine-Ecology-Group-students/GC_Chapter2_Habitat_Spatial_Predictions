library(dplyr)
library(tidyr)
library(readr)
library(CheckEM)
library(googlesheets4)
library(stringr)

schema <- CheckEM::catami%>%
  dplyr::mutate(caab_code = as.numeric(caab_code))%>%
  select(-qualifiers)

name <- "west-coast-BRUVs"

head(schema)

# HABITAT -----
metadata <- read_csv(paste0("data/uploads/", name, "_metadata.csv")) %>%
  dplyr::filter(campaignid %in% "2020-06_south-west_stereoBRUVs") %>%
  glimpse()
# read in forwards annotations
forwards <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-06_south-west_stereo-BRUVs_random-points_forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-06_south-west_stereo-BRUVs_random-points_backwards_Dot Point Measurements.txt", 
                        header = T, skip = 4, stringsAsFactors = FALSE, 
                        colClasses = "character", na.strings = "") %>%
  clean_names()

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::rename(caab_code = code) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::mutate(caab_code = case_when(
    broad %in% c("Unknown", "Open Water") ~ 1,
    broad %in% "Invertebrate Complex" ~ 99900044,   # was 2 - now matches schema's "Sessile invertebrates"
    
    type %in% "Thalassodendrum sp." ~ 63618905, # fix incorrect caab code
    type %in% "Thalassodendrum sp. with epiphytes algae" ~ 63618905, # fix incorrect caab code
    type %in% "Ecklonia radiata" ~ 54079009, # fix incorrect caab code
    
    caab_code %in% 90300910 ~ 80300910, # fix incorrect caab code
    
    .default = caab_code
  )) %>%
  dplyr::left_join(schema) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = "")))

missing_in_schema <- anti_join(habitat_with_schema, schema)

distinct_hab_types <- habitat_with_schema %>%
  select(broad, morphology, type, starts_with("level"), family, genus, species, caab_code) %>%
  distinct()

missing_caab_code <- habitat_with_schema %>%
  dplyr::filter(is.na(caab_code)) %>% 
  distinct(broad, morphology, type) # good

unique(habitat_with_schema$sample) %>% sort()

num.points <- 40
wrong_points_habitat <- habitat_with_schema %>%
  dplyr::count(sample, name = "points_annotated") %>%
  dplyr::full_join(metadata %>% dplyr::select(opcode, campaignid), by = c("sample" = "opcode")) %>%
  dplyr::mutate(points_annotated = tidyr::replace_na(points_annotated, 0)) %>%
  dplyr::filter(points_annotated != num.points) %>%
  dplyr::arrange(sample)

wrong_points_habitat

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample" = "opcode")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat_with_schema, by = c("opcode" = "sample")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, caab_code, everything()) %>%
  glimpse()

write_csv(tidy_habitat %>%
            dplyr::rename(opcode = sample), "data/uploads/2020-06_south-west_stereo-BRUVs_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-06_south-west_stereo-BRUVs_grid_forwards_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()

# read in forwards annotations
backwards_relief <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-06_south-west_stereo-BRUVs_grid_backwards_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  separate(filename, into = c("filename", "extra"), sep = ".1.JPG") %>%
  dplyr::mutate(filename = str_pad(filename, side = "left", pad = "0", width = 2))

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::select(filename, relief) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = ""))) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(level_5 = str_sub(relief, 2, 2)) %>%
  dplyr::filter(!level_5 %in% "n") %>%
  dplyr::left_join(schema)   # <-- was catami

unique(relief_with_schema$level_5)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample" = "opcode")) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata, relief_with_schema, by = c("opcode" = "sample")) %>%
  glimpse()

tidy_relief <- relief_with_schema %>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = "2020-06_south-west_stereo-BRUVs") %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, caab_code, everything()) %>%
  glimpse()

write_csv(tidy_relief %>%
            dplyr::rename(opcode = sample), "data/uploads/2020-06_south-west_stereo-BRUVs_benthos-relief.csv")


relief_clean <- tidy_relief %>%
  group_by(campaignid, sample) %>%
  summarise(relief_sample = n(), .groups = "drop")

benthos_clean <- tidy_habitat %>%
  group_by(campaignid, sample) %>%
  summarise(benthos_sample = n(), .groups = "drop")

sample_summary <- full_join(
  relief_clean,
  benthos_clean,
  by = c("campaignid", "sample")
)

relief_samples <- tidy_relief %>% 
  distinct(campaignid, sample) %>%
  group_by(campaignid) %>%
  summarise(relief_sample = n(), .groups = "drop")

benthos_samples <- tidy_habitat %>%
  distinct(campaignid, sample) %>%
  group_by(campaignid) %>%
  summarise(benthos_sample = n(), .groups = "drop")

sample_summary <- full_join(
  relief_samples,
  benthos_samples,
  by = c("campaignid")
)

