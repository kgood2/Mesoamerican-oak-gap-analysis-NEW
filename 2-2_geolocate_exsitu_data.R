## 2-2_geolocate_exsitu_data.R
### Author: Emily Beckman ### Date: 12/13/2019, Edits by Kate Good ## Date: 10/5/2022

### DESCRIPTION:
# This script takes the standardized and compiled ex situ file created in 2-1 script, adds and
# combines columns so it can be used for geolocating, and creates one new file per species

### DATA IN:
# ExSitu_Compiled_Standardized file from 2-1_compile_exsitu_data script

### DATA OUT:
# To_Geolocate_.csv file and one .csv file per species

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

##
## SELECT RECORDS THAT HAVE POTENTIAL WILD-ORIGIN INFO RENAME FOR GEOLOCATE
##

# FIRST CHECK TO BE SURE THIS IS ZERO !!
all_data13[which(grepl("\\|",all_data13$acc_num)),]

# add GEOLocate standard columns
all_data13$correction.status <- NA
all_data13$precision <- NA
all_data13$error.polygon <- NA
all_data13$multiple.results <- NA
all_data13$uncertainty <- NA

# create prov_type_edit column to record our assumed provenance type
all_data13$prov_type_edit <- all_data13$prov_type

all_data14 <- all_data13 %>%
  # filter to remove cultivated records and those without locality info;
  # we're leaving records with lat-long in case they are helpful refereces
  filter(prov_type != "H") %>%
  filter(!is.na(all_locality) &
         all_locality != "| | | | | | | | | | | |" &
         all_locality != "") %>%
  # rename to GEOLocate standard columns
  rename(locality.string = all_locality,
         latitude = lat_dd,
         longitude = long_dd) %>%
  # replace NA with "" to make simpler to view in GEOLocate
  replace(., is.na(.), "") %>%
  # select only the columns we need for geolocating
  select(
    ## GEOLocate
    locality.string,country,state,county,latitude,longitude,
    correction.status,precision,error.polygon,multiple.results,uncertainty,
    ## record metadata for reference
    species_name_acc,flag,gps_det,prov_type_edit,
    inst_short,acc_num,lin_num,coll_num,coll_name,coll_year,
    ## some institituion metadata
    submission_year,inst_lat,inst_long,inst_country,
    ## UID
    UID) %>%
  # combine locality duplicates so we don't need to geolocate them mult. times
  group_by(locality.string) %>%
    # concatenate unique values in other columns so we can still see those
  mutate(
    species_name_acc = paste(unique(species_name_acc),collapse=" | "),
    flag = paste(unique(flag),collapse=" | "),
    gps_det = paste(unique(gps_det),collapse=" | "),
    prov_type_edit = paste(unique(prov_type_edit),collapse=" | "),
    inst_short = paste(unique(inst_short),collapse=" | "),
    acc_num = paste(unique(acc_num),collapse=" | "),
    lin_num = paste(unique(lin_num),collapse=" | "),
    coll_num = paste(unique(coll_num),collapse=" | "),
    coll_name = paste(unique(coll_name),collapse=" | "),
    coll_year = paste(unique(coll_year),collapse=" | "),
    submission_year = paste(unique(submission_year),collapse=" | "),
    inst_lat = paste(unique(inst_lat),collapse=" | "),
    inst_long = paste(unique(inst_long),collapse=" | "),
    inst_country = paste(unique(inst_country),collapse=" | "),
    UID = paste(unique(UID),collapse=" | ")) %>%
  ungroup() %>%
  # remove duplicates
  distinct(locality.string,.keep_all=T) %>%
  # order by locality string and lat/long pts at bottom
  arrange(locality.string) %>%
  arrange(!is.na(latitude),latitude)
# 1630 without removing locality duplicates, 472 after!
nrow(all_data14)

# remove dot in column names (replace with space) for GEOLocate
names(all_data14) <- gsub(x = names(all_data14),pattern = "\\.",
                          replacement = " ")
str(all_data14)
head(as.data.frame(all_data14),n=30)

# write file
write.csv(all_data14, file.path(main_dir,"outputs","to_geolocate",
                                paste0("To_Geolocate_", Sys.Date(), ".csv")),row.names = F)


## !! MANUAL STEP IN EXCEL !!
## Now follow PART 1 in the how to geolocate doc:
##   https://docs.google.com/document/d/16EjD39JI0416c0MEbKcHF2qGJ0ZAZ8Nqpkv54N5HQkM/edit?usp=sharing
## When you finish that section, save your file as "To_Geolocate_Checked.csv"
##   and come back to this script for the final step:


##
## SPLIT BY SPECIES
##

# This step makes it easier to use the GEOLocate tool because it doesn't
#   handle lots of records very well

# read in checked data
all_data15 <- read.csv(file.path(main_dir, "outputs","To_Geolocate_Checked.csv"),
                       header = T, na.strings = c("","NA"),colClasses = "character")

# remove records with anything in the gps_det column
XXXXXXXX

# see if any records are for multiple species
XXXXXXXX

# remove dot in column names (replace with space) for GEOLocate
names(all_data15) <- gsub(x = names(all_data15),pattern = "\\.",
                          replacement = " ")
str(all_data15)
head(as.data.frame(all_data15),n=30)

# create one CSV for each target species
sp_split <- split(all_data15, as.factor(all_data15$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(!dir.exists(file.path(main_dir,"outputs","to_geolocate")))
  dir.create(file.path(main_dir,"outputs","to_geolocate"),
             recursive=T)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
                                                  file.path(main_dir,"outputs","to_geolocate",
                                                            paste0(names(sp_split)[[i]], ".csv")),row.names = F))
