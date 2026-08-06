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
name <- "west-coast-BRUVs"

# All data needs to be saved in a folder structure "data/raw/" for the script to work, or change the direcory when reading in files.

# Read in metadata ----
# Note check if you have used opcode, opcode or period and change the below code accordingly.


metadata <- read_metadata(here::here("data/raw/ga upload/BRUVS/metadata/"), method = "BRUVs") %>% # Change here to "DOVs"
  dplyr::select(campaignid, opcode, 
                status, 
                longitude_dd, latitude_dd, 
                observer_count, observer_length,
                date_time, 
                location, site, 
                depth_m, 
                successful_count, 
                successful_length, 
                successful_habitat_forward, 
                successful_habitat_backward, 
                observer_habitat_forward,
                observer_habitat_backward) %>%
  # rename(opcode = opcode) %>% # use this line if you need to rename opcode to opcode
  glimpse()

#checks for duplicates of campaign IDs and opcodes
duplicates <- metadata %>%
  dplyr::group_by(campaignid, opcode) %>%
  dplyr::filter(n() > 1) 

#checks for duplicates in coordinates
metadata %>%
  dplyr::group_by(latitude_dd, longitude_dd) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::arrange(latitude_dd, longitude_dd)

unique(metadata$campaignid)

# Filter out only rows that failed on ALL FOUR criteria ----
metadata_successful <- metadata %>%
  dplyr::filter(
    !(successful_count == "No" &
        successful_length == "No" &
        successful_habitat_forward == "No" &
        successful_habitat_backward == "No")
  ) %>%
  glimpse()

# Optional: see exactly what got dropped
metadata_dropped <- metadata %>%
  dplyr::filter(
    successful_count == "No" &
      successful_length == "No" &
      successful_habitat_forward == "No" &
      successful_habitat_backward == "No"
  ) %>%
  dplyr::select(campaignid, opcode, successful_count, successful_length,
                successful_habitat_forward, successful_habitat_backward)

metadata_dropped

write_csv(metadata, paste0("data/uploads/", name, "_metadata.csv"))

