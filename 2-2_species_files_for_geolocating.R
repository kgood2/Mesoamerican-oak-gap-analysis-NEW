## 2-1_compile_exsitu_data.R
### Author: Emily Beckman ### Date: 12/13/2019, Edits by Kate Good ## Date: 10/5/2022

### DESCRIPTION:
# This script takes the standardized and compiled ex situ file created in 2-1 script, adds and 
# combines columns so it can be used for geolocating, and creates one new file per species

### DATA IN:
# ExSitu_Compiled_Standardized file from 2-1_compile_exsitu_data script

### DATA OUT:
# To_Geolocate_CR-EN-VU-NT_.csv file and one .csv file per species

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'textclean', 'spatialEco',
                 'maps', 'measurements', 'CoordinateCleaner', 'rnaturalearth', 'raster', 'dplyr')
select <- dplyr::select
rename <- dplyr::rename
arrange <- dplyr::arrange
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
# install.packages("naniar") # when running above script you may get an error that naniar cant be loaded. Load separately
# library(nanier)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

main_dir <- "/Volumes/GoogleDrive/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"

# Read in ExSitu_Compiled_Standardized file from 2-1_compile_exsitu_data script and name all_data13
all_data13 <- read.csv(file.path(main_dir, "outputs",
                                 "ExSitu_Compiled_Standardized_KG_2022-09-27.csv"),
                       header = T, na.strings = c("","NA"),colClasses = "character")

## RENAME FOR GEOLOCATE AND (optionally.. SPLIT BY SPECIES)
##

# FIRST CHECK TO BE SURE THIS IS ZERO !!
all_data13[which(grepl("\\|",all_data13$acc_num)),]

# add GEOLocate standard columns
all_data13$correction.status <- NA
all_data13$precision <- NA
all_data13$error.polygon <- NA
all_data13$multiple.results <- NA
all_data13$uncertainty <- NA

all_data14 <- all_data13 %>%
  # filter to remove cultivated records and those without locality info
  #filter(rl_category == "CR" | rl_category == "EN" |
  #       rl_category == "VU" | rl_category == "NT") %>%
  #filter(prov_type != "H") %>%
  #filter(!is.na(all_locality)) %>%
  # rename to GEOLocate standard columns
  rename(locality.string = all_locality) %>%
  #rename latitude and longitude
  rename(latitude = lat_dd, longitude = long_dd) %>%
  # order with NA lat-long records on top
  arrange(locality.string) %>%
  arrange(!is.na(latitude),latitude) %>%
  # replace NA with "" to make simpler to view in GEOLocate
  replace(., is.na(.), "") %>%
  # group by all non-ID fields
  group_by(
    locality.string,country,state,county,latitude,longitude,
    flag,gps_det,prov_type,lin_num,coll_num,coll_name,coll_year,
    inst_short,filename,inst_lat,inst_long,
    list,species_name_acc,taxon_full_name) %>%
  # concatenate values in ID fields
  mutate(
    UID = paste(UID, collapse="|"),
    acc_num = paste(acc_num, collapse="|")) %>%
  ungroup() %>%
  # remove duplicates
  distinct(
    locality.string,country,state,county,latitude,longitude,
    flag,gps_det,prov_type,lin_num,coll_num,coll_name,coll_year,
    inst_short,filename,inst_lat,inst_long,
    list,species_name_acc,taxon_full_name,
    .keep_all=T) %>%
  # reorder columns
  select(
    ## GeoLocate
    locality.string,country,state,county,latitude,longitude,
    correction.status,precision,error.polygon,multiple.results,uncertainty,
    ## record metadata
    flag,gps_det,prov_type,acc_num,lin_num,coll_num,coll_name,coll_year,
    ## institituion metadata
    inst_short,filename,inst_lat,inst_long,
    ## taxon name & record ID
    list,species_name_acc,taxon_full_name,UID) %>%
  # rename concatenated fields to make that clear
  rename(acc_num_CAT = acc_num, UID_CAT = UID)

# remove dot in column names (replace with space) for GEOLocate
names(all_data14) <- gsub(x = names(all_data14),pattern = "\\.",
                          replacement = " ")
str(all_data14)
head(as.data.frame(all_data14),n=30)

# write file
write.csv(all_data14, file.path(main_dir,"outputs","to_geolocate",
                                paste0("To_Geolocate_CR-EN-VU-NT_", Sys.Date(), ".csv")),row.names = F)


# create one CSV for each target species
sp_split <- split(all_data14, as.factor(all_data14$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(!dir.exists(file.path(main_dir,"outputs","to_geolocate")))
  dir.create(file.path(main_dir,"outputs","to_geolocate"),
             recursive=T)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
                                                  file.path(main_dir,"outputs","to_geolocate",
                                                            paste0(names(sp_split)[[i]], ".csv")),row.names = F))
