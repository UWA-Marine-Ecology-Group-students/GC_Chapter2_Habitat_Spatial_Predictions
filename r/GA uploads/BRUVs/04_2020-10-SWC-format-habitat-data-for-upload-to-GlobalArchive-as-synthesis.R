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

# HABITAT -----
metadata <- read_csv(paste0("data/uploads/", name, "_metadata.csv")) %>%
  dplyr::filter(campaignid %in% "2020-10_south-west_stereoBRUVs") %>%
  glimpse()

# read in forwards annotations
forwards <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-10_south-west_stereo-BRUVs_random-points_forwards_Dot Point Measurements.txt", 
                       header = T, skip = 4, stringsAsFactors = FALSE, 
                       colClasses = "character", na.strings = "") %>%
  clean_names() %>%
  dplyr::filter(!filename %in% "IO333.jpg") %>%
  dplyr::mutate(filename = str_replace_all(filename, "take 2", ""))

# read in forwards annotations
backwards <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-10_south-west_stereo-BRUVs_random-points_backwards_Dot Point Measurements.txt", 
                        header = T, skip = 4, stringsAsFactors = FALSE, 
                        colClasses = "character", na.strings = "") %>%
  clean_names()

habitat_with_schema <- bind_rows(forwards, backwards) %>%
  dplyr::rename(caab_code = code) %>%
  dplyr::mutate(caab_code = as.numeric(caab_code)) %>%
  dplyr::mutate(caab_code = case_when(
    broad %in% c("Unknown", "Open Water") ~ 1,
    broad %in% "Invertebrate Complex" ~ 99900044,   # was 2 - now matches schema's "Sessile invertebrates"
    
    type %in% "Thalassodendrum sp." ~ 63618905,
    type %in% "Ecklonia radiata" ~ 54079009,
    
    caab_code %in% 90300910 ~ 80300910,
    
    .default = caab_code
  )) %>%
  dplyr::left_join(schema) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = "")) %>% str_trim())

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

habitat.missing.metadata <- anti_join(habitat_with_schema, metadata, by = c("sample" = "opcode")) %>%
  glimpse()

tidy_habitat <- habitat_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = unique(metadata$campaignid)) %>%
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
  by = c("campaignid", "opcode" = "sample")
) %>%
  glimpse()

write_csv(tidy_habitat %>%
            dplyr::rename(opcode = sample),"data/uploads/2020-10_south-west_stereo-BRUVs_benthos-count.csv")


# RELIEF ----
# read in forwards annotations
forwards_relief <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-10_south-west_stereo_BRUVs_Habitat_grid_forwards_Dot Point Measurements.txt", 
                              header = T, skip = 4, stringsAsFactors = FALSE, 
                              colClasses = "character", na.strings = "") %>%
  clean_names()%>%
  dplyr::filter(!filename %in% "IO333.jpg") %>%
  dplyr::mutate(filename = str_replace_all(filename, "take 2", ""))

# read in forwards annotations
backwards_relief <- read.delim("data/raw/ga upload/BRUVS/habitat/2020-10_south-west_stereo_BRUVs_Habitat_grid_backwards_Dot Point Measurements.txt", 
                               header = T, skip = 4, stringsAsFactors = FALSE, 
                               colClasses = "character", na.strings = "") %>%
  clean_names() 

relief_with_schema <- bind_rows(forwards_relief, backwards_relief) %>%
  dplyr::select(filename, relief) %>%
  dplyr::mutate(sample = str_replace_all(filename, c(".JPG"= "", ".jpg" = "")) %>% str_trim()) %>%
  dplyr::filter(!is.na(relief)) %>%
  dplyr::mutate(level_5 = str_sub(relief, 2, 2)) %>%
  dplyr::filter(!level_5 %in% "n") %>%
  dplyr::left_join(catami) 

unique(relief_with_schema$level_5)

relief.missing.metadata <- anti_join(relief_with_schema, metadata, by = c("sample" = "opcode")) %>%
  glimpse()



tidy_relief <- relief_with_schema %>%
  dplyr::mutate(sample = str_trim(sample))%>%
  dplyr::mutate(number = 1) %>%                                     
  dplyr::mutate(campaignid = unique(metadata$campaignid)) %>%
  dplyr::select(campaignid, sample, number, starts_with("level"), family, genus, species, caab_code) %>%
  dplyr::filter(!level_2 %in% c("","Unscorable", NA)) %>%  
  group_by(campaignid, sample, across(starts_with("level")), family, genus, species, caab_code) %>%
  dplyr::tally(number, name = "count") %>%
  ungroup() %>%                                                     
  dplyr::select(campaignid, sample, level_1, everything()) %>%
  glimpse()

metadata.missing.relief <- anti_join(metadata %>% dplyr::filter(successful_count == "Yes" | successful_length == "Yes"),
                                     tidy_relief,
                                     by = c("opcode" = "sample")
) %>% 
  glimpse()

write_csv(tidy_relief %>%
            dplyr::rename(opcode = sample), "data/uploads/2020-10_south-west_stereo-BRUVs_benthos-relief.csv")

names(tidy_habitat)
names(metadata)

unique(tidy_habitat$campaignid)
unique(metadata$campaignid)
setdiff(unique(tidy_habitat$campaignid), unique(metadata$campaignid))

anti_join(
  tidy_habitat %>% distinct(campaignid, sample),
  metadata %>% distinct(campaignid, opcode),
  by = c("sample" = "opcode")
)

class(tidy_habitat$sample)
class(metadata$opcode)

dplyr::full_join(
  tidy_habitat %>% distinct(campaignid, sample) %>% dplyr::mutate(in_habitat = TRUE),
  metadata %>% distinct(campaignid, opcode) %>% dplyr::mutate(in_metadata = TRUE),
  by = c("sample" = "opcode")
) %>%
  dplyr::filter(is.na(in_habitat) | is.na(in_metadata))

metadata %>% dplyr::filter(str_detect(opcode, "IO254"))
tidy_habitat %>% dplyr::filter(str_detect(sample, "IO254"))

metadata %>% dplyr::filter(opcode == "IO282")

