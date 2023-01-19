
#CH2 GABBY's PhD HABITAT
#PtCloatesFORMATTING BRUVS 2021-05 & 2022-05
#Date created: JAN 2023

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study<-"PtCloates_BRUVS" 

## Set your working directory ----
working.dir <- getwd() # this only works through github projects

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/") 
raw.dir <- paste(data.dir,"raw",sep="/") 
tidy.dir <- paste(data.dir,"tidy",sep="/")
tm.export.dir <- paste(raw.dir,"tm_export",sep="/") 
# em.export.dir <- paste(raw.dir, "em_export", sep = "/")
error.dir <- paste(data.dir,"errors to check",sep="/") 

# Read in the metadata----
setwd(tm.export.dir)
dir()

# Read in metadata1----
metadata1 <- read_csv("2021-05_PtCloates_stereo-BRUVS_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function                                                                                                                                                        
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>%
  dplyr::mutate(campaignid = "2021-05_PtCloates_stereo-BRUVS") %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>%                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata1)

# Read in metadata2----
metadata2 <- read_csv("2022-05_PtCloates_stereo-BRUVS_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% 
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata2)

#combine metadata to get PT Cloates Metadata
metadata <- bind_rows(metadata1, metadata2)

length(unique(metadata$sample))

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the forwards points1 annotations ----
forwards.points1 <- read.delim("2021-05_PtCloates_stereo-BRUVS_Forwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type) %>% 
  dplyr::mutate(campaignid = "2021-05_PtCloates_stereo-BRUVS") %>%# select only these columns to keep
  glimpse() # preview

length(unique(forwards.points1$sample))

# read in the forwards points2 annotations ----
forwards.points2 <- read.delim("2022-05_PtCloates_stereo-BRUVS_Forwards_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  #mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(opcode)) %>% 
  select(sample,image.row,image.col,broad,morphology,type) %>% 
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%# select only these columns to keep
  glimpse() # preview

length(unique(forwards.points2$sample))

#combine metadata to get Pt Cloates Forwards Points Data
forwards.points <- bind_rows(forwards.points1, forwards.points2)

test <- forwards.points %>%
  group_by(campaignid, sample)  %>%
  summarise(n = n())

#test2
test2 <- metadata%>% anti_join(forwards.points)

#read in the backwards points1 annotations ---
backwards.points1 <- read.delim("2021-05_PtCloates_stereo-BRUVS_Backwards_Dot Point Measurements.txt", header=T, skip=4, stringsAsFactors = FALSE) %>% #read in the file
  ga.clean.names() %>% # tidy the column names using Global Archive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(campaignid = "2021-05_PtCloates_stereo-BRUVS") %>%
  glimpse() # preview

#read in the backwards points2 annotations ---
backwards.points2 <- read.delim("2022-05_PtCloates_stereo-BRUVS_Backwards_Dot Point Measurements.txt", header=T, skip=4, stringsAsFactors = FALSE) %>% #read in the file
  ga.clean.names() %>% # yidy the column names using Global Archive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type) %>% # select only these columns to keep
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
  glimpse() # preview

#combine metadata to get Pt Cloates Backwards Points Data
backwards.points <- bind_rows(backwards.points1, backwards.points2)

length(unique(backwards.points$sample)) # 64 samples

#combine all points
points <- rbind(forwards.points, backwards.points)

no.annotations <- points%>% #number of annotations
  group_by(sample)%>%
  summarise(points.annotated=n()) # 1 have 81


# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(points,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,points, by = c("sample"))

# read in forwards relief Pt cloates 2021
forwards.relief1 <- read.delim("2021-05_PtCloates_stereo-BRUVS_Forwards_Relief_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,relief) %>% # select only these columns to keep
  dplyr::mutate(campaignid = "2021-05_PtCloates_stereo-BRUVS") %>%
  glimpse() # preview

#read in forwards relief Pt Cloates 2022
forwards.relief2 <- read.delim("2022-05_PtCloates_stereo-BRUVS_Forwards_Relief_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  #mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(opcode)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,relief) %>% # select only these columns to keep
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
  glimpse() # preview

#combine metadata to get Pt Cloates BRUVS Forwards Relief Data
forwards.relief <- bind_rows(forwards.relief1, forwards.relief2)

length(unique(forwards.relief$sample)) # 88 samples

#read in backwards relief Pt Cloates 2021
backwards.relief1 <- read.delim("2021-05_PtCloates_stereo-BRUVS_Backwards_Relief_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,relief) %>% # select only these columns to keep
  dplyr::mutate(campaignid = "2021-05_PtCloates_stereo-BRUVS") %>%
  glimpse() # preview

#read in backwards relief Pt Cloates 2022
backwards.relief2 <- read.delim("2022-05_PtCloates_stereo-BRUVS_Backwards_Relief_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,relief) %>% # select only these columns to keep
  dplyr::mutate(campaignid = "2022-05_PtCloates_stereo-BRUVS") %>%
  glimpse() # preview

#combine metadata to get Pt Cloates BRUVS Backwards Relief Data
backwards.relief <- bind_rows(backwards.relief1, backwards.relief2)

length(unique(backwards.relief$sample)) # 64 samples

relief <- rbind(forwards.relief, backwards.relief)

no.annotations <- relief%>%
  group_by(sample)%>%
  summarise(relief.annotated=n()) # all have 80

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(relief,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,relief, by = c("sample"))

habitat <- bind_rows(points, relief)

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat

# # Create %fov----
# fov.points <- habitat%>%
#   dplyr::select(-c(broad,morphology,type,relief))%>%
#   dplyr::filter(!fieldofview=="")%>%
#   dplyr::filter(!is.na(fieldofview))%>%
#   dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
#   dplyr::mutate(count=1)%>%
#   spread(key=fieldofview,value=count, fill=0)%>%
#   dplyr::select(-c(image.row,image.col))%>%
#   dplyr::group_by(sample)%>%
#   dplyr::summarise_all(funs(sum))%>%
#   dplyr::mutate(fov.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
#   ga.clean.names()
# 
# fov.percent.cover<-fov.points %>%
#   group_by(sample)%>%
#   mutate_at(vars(starts_with("fov")),funs(./fov.total.points.annotated*100))%>%
#   dplyr::select(-c(fov.total.points.annotated))%>%
#   glimpse()


# CREATE catami_broad------
broad.points <- habitat%>%
  dplyr::select(-c(morphology,type,relief))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse

broad.percent.cover<-broad.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("broad")),funs(./broad.total.points.annotated*100))%>%
  dplyr::select(-c(broad.total.points.annotated))%>%
  glimpse()


# CREATE catami_morphology------
detailed.points <- habitat%>%
  dplyr::select(-c(relief))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm.")))%>%
  dplyr::select(-c(broad,type))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse()

detailed.percent.cover<-detailed.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("detailed")),funs(./detailed.total.points.annotated*100))%>%
  dplyr::select(-c(detailed.total.points.annotated))%>%
  glimpse()

# Create relief----
relief.grid<-habitat%>%
  dplyr::filter(!broad%in%c("Open Water","Unknown"))%>%
  dplyr::filter(!relief%in%c("",NA))%>%
  dplyr::select(-c(broad,morphology,type,image.row,image.col))%>%
  dplyr::mutate(relief.rank=ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,
                                   ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,
                                          ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,
                                                 ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,
                                                        ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,
                                                               ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  dplyr::select(-c(relief))%>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()


# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad.points <- metadata%>%
  #left_join(fov.points, by = "sample")%>%
  left_join(broad.points)%>%
  left_join(relief.grid)

habitat.detailed.points <- metadata%>%
  #left_join(fov.points, by = "sample")%>%
  left_join(detailed.points)%>%
  left_join(relief.grid)

habitat.broad.percent <- metadata%>%
  #left_join(fov.percent.cover, by = "sample")%>%
  left_join(broad.percent.cover)%>%
  left_join(relief.grid)

habitat.detailed.percent <- metadata%>%
  #left_join(fov.percent.cover, by = "sample")%>%
  left_join(detailed.percent.cover)%>%
  left_join(relief.grid)

write.csv(habitat.broad.points,file=paste(study,"random-points_broad.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.detailed.points,file=paste(study,"random-points_detailed.habitat.csv",sep = "_"), row.names=FALSE)


write.csv(habitat.broad.percent,file=paste(study,"random-points_percent-cover_broad.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.detailed.percent,file=paste(study,"random-points_percent-cover_detailed.habitat.csv",sep = "_"), row.names=FALSE)

setwd(working.dir)
