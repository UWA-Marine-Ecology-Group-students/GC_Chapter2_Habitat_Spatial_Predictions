# Before you use this script please read the below instructions!!
# This workflow assumes all of your metadata is correctly formatted, and has been cleaned by using CheckEM. CheckEM available here: https://marine-ecology.shinyapps.io/CheckEM/
# Please see the CheckEM usermanual for the correct format(https://globalarchivemanual.github.io/CheckEM/articles/manuals/CheckEM_user_guide.html)

# Load libraries -----
library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM") # Run this if you do not have CheckEM installed.
library(CheckEM)
library(tidyverse)
library(here)

# Set name for synthesis, will be used as prefix for your files to upload
name <- "west-coast-BOSS"

# All data needs to be saved in a folder structure "data/raw/" for the script to work, or change the direcory when reading in files.

# Read in metadata ----
# Note check if you have used opcode, opcode or period and change the below code accordingly.


metadata <- read_metadata(here::here("data/raw/ga upload/BOSS/metadata/"), method = "BOSS") %>% # Change here to "DOVs"
  dplyr::select(campaignid, sample, 
                status, 
                longitude_dd, latitude_dd, 
                observer_count, observer_length,
                date_time, 
                location, site, 
                depth_m, 
                successful_count, 
                successful_length, 
                successful_habitat_panoramic, 
                successful_habitat_downward,
                observer_habitat_panoramic, 
                observer_habitat_downward) %>%
  rename(period = sample) %>% # use this line if you need to rename opcode to opcode
  glimpse()

metadata_clean <- metadata %>%
  dplyr::filter(!(period == "287" & campaignid == "2020-10_south-west_BOSS"))

#checks for duplicates of campaign IDs and opcodes
duplicates <- metadata_clean %>%
  dplyr::group_by(campaignid, period) %>%
  dplyr::filter(n() > 1) 

#checks for duplicates in coordinates
metadata_clean %>%
  dplyr::group_by(latitude_dd, longitude_dd) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::arrange(latitude_dd, longitude_dd)

unique(metadata_clean$campaignid)

write_csv(metadata_clean, paste0("data/uploads/", name, "_metadata.csv"))


