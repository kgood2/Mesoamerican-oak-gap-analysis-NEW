### Authors: Emily Beckman Bruns, Shannon M Still, Kate Good
# Date: January 17, 2023

### Description 
# Takes compiled ex situ file from Quercus ex situ processing script 
# (compile_exsitu_data-Quercus2022.R) and identifies points for georeferencing. 

## INPUTS:
# 1) compiled ex situ file
# 2) target taxa list

## OUTPUTS:
# 1) Table with...
#   species name
#   num wild acc
#   num non-H acc w/ coords
#   num non-H acc with no coords & yes locality info

#2) ExSitu_Need_Geolocation file



################################################################################
# Load libraries
################################################################################

my.packages <- c('plyr', 'tidyverse', 'textclean', 'spatialEco',
                 'maps', 'measurements', 'CoordinateCleaner', 'raster',
                 'data.table', 'terra')

# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
group_by <- dplyr::group_by
mutate <- dplyr::mutate
distinct <- dplyr::distinct

################################################################################
# Set working directory
################################################################################

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

# set up file structure within your main working directory
data <- "occurrence_data"
raw <- "raw_occurrence_data"
standard <- "standardized_occurrence_data"

################################################################################
# Load target taxa list
################################################################################

taxon_list <- read.csv(file.path(main_dir,"inputs", "taxa_list", "target_taxa_with_synonyms.csv"),
                       header = T, colClasses="character")

################################################################################
# Explore geoerferencing needs
################################################################################

# create folder for all georeferencing data
if(!dir.exists(file.path(main_dir,data, "georeferencing")))
  dir.create(file.path(main_dir, data, "georeferencing"), recursive=T)

# upload compiled ex situ file from script 2-0
data_sel <- read.csv(file.path(main_dir,data,standard,"Ex_situ.csv"), colClasses = "character",
                     na.strings=c("", "NA"), strip.white=T, fileEncoding="UTF-8")

# create all_locality column
data_sel$latitude <- round(as.numeric(data_sel$lat_dd,digits=4))
data_sel$longitude <- round(as.numeric(data_sel$long_dd,digits=4))
data_sel <- unite(data_sel, "all_locality",
                   c(locality,municipality,county,state,country,orig_source,
                     lin_num,coll_num,coll_name,coll_year,
                     latitude,longitude,notes),sep = " | ",remove = F)
# remove NA in concatenated locality column
data_sel$all_locality <- gsub("NA","",data_sel$all_locality)
# if no locality info at all, make it NA
data_sel$all_locality[which(data_sel$all_locality ==
                               " |  |  |  |  |  |  |  |  |  |  |  | ")] <- NA


### explore georeferencing needs
# table with...
#   species name
#   num wild acc
#   num non-H acc w/ coords
#   num non-H acc with no coords & yes locality info
geo_needs <- data_sel %>%
  group_by(taxon_name_accepted) %>%
  summarize(
    num_acc = sum(!is.na(taxon_name_accepted)),
    num_wild = sum(prov_type == "W"),
    NotH_YesCoords = sum(!is.na(lat_dd) & prov_type != "H"),
    NotH_NoCoords_YesLocality = sum(is.na(lat_dd) & 
                                      !is.na(all_locality) & 
                                      prov_type != "H"),
    Percent_NonH_NeedGeo = (sum(is.na(lat_dd) & 
                                  !is.na(all_locality) & 
                                  prov_type != "H") 
                            / sum(prov_type != "H")*100)
  )
head(geo_needs,n=20)
# write file
write.csv(geo_needs, file.path(main_dir, data, "georeferencing",
                               paste0("ExSitu_Geolocation_Needs_Summary_", Sys.Date(), ".csv")),
          row.names = F)

# records that may need geolocation
#   (no lat-long, yes locality, prov type not H)
#   (also add flagged records: water or at institution)
need_geo <- data_sel %>%
  filter((is.na(lat_dd) & prov_type != "H" &
            !is.na(all_locality) & all_locality != "NA") |
           flag!="")
nrow(need_geo) #4506
# add a couple more columns for keeping notes while geolocating
need_geo$geolocated_by <- NA
need_geo$gps_notes <- NA
# condense all_locality duplicates
need_geo <- need_geo %>%
  group_by(prov_type,lat_dd,long_dd,gps_det,all_locality) %>%
  mutate(UID = paste0(UID,collapse=" | "),
         inst_short = paste0(unique(inst_short),collapse=" | "),
         taxon_name_accepted = paste0(unique(taxon_name_accepted),
                                      collapse=" | ")) %>%
  ungroup() %>%
  distinct(UID,inst_short,taxon_name_accepted,prov_type,lat_dd,long_dd,
           uncertainty,gps_det,geolocated_by,gps_notes,all_locality,county,
           state,country,flag) %>%
  select(UID,inst_short,taxon_name_accepted,prov_type,lat_dd,long_dd,
         uncertainty,gps_det,geolocated_by,gps_notes,all_locality,county,
         state,country,flag)
nrow(need_geo) #2907
head(need_geo)


# write file
write.csv(need_geo, file.path(main_dir, data, "georeferencing",
                              paste0("ExSitu_Need_Geolocation_", Sys.Date(), ".csv")),row.names = F)

### NOW MANUALLY GEOLOCATE !
### INSTRUCTIONS FOR GEOLOCATING:
### https://docs.google.com/document/d/1RBUD6-ogLc7PRVkDJSIEKzkgvC6xekj9Q_kl1vzxhCs/edit?usp=sharing

################################################################################
# 9. Add geolocated data, after manual geolocation
################################################################################

# read in all compiled ex situ data (exported above)
exsitu <- read.csv(file.path(exsitu_dir,data_out,
                             "All_ExSitu_Compiled_2022-12-07.csv"), header = T, colClasses="character")

# read in geolocated dataset
geo_raw <- read.csv(file.path(exsitu_dir,data_out,
                              "ExSitu_Need_Geolocation_2022-12-07_Geolocated.csv"),
                    header = T, colClasses="character")
head(geo_raw)
# check this is just NA and no "priority" records that are not geolocated
unique(geo_raw[which(is.na(geo_raw$gps_det)),"priority"])

# add geolocated coordinates to ex situ data
# separate UID row
geolocated <- separate_rows(geo_raw, UID, sep=" \\| ")
#OLD: geolocated <- separate_rows(geo_raw, UID, sep="\\|;\\|")
#OLD: geolocated$UID <- gsub("UCalifornia BGerkeley","UCaliforniaBGBerkeley",geolocated$UID)
# keep only edited rows (lat, long, gps_det) and
#   records that have gps_det filled in
geolocated <- geolocated %>%
  select(UID,lat_dd,long_dd,gps_det,uncertainty,geolocated_by,gps_notes,
         county,state) %>%
  filter(!is.na(gps_det))
head(geolocated)
table(geolocated$gps_det)
#   L     C    G    X
#   289   62   259   1331
# select geolocated rows in full dataset and remove cols we want to add
exsitu_geo <- exsitu %>%
  filter(UID %in% geolocated$UID) %>%
  select(-lat_dd,-long_dd,-gps_det,-uncertainty,-county,-state)
# these two values should be the same:
nrow(exsitu_geo) #1941
nrow(geolocated) #1941
# add geolocation data
exsitu_geo <- full_join(exsitu_geo,geolocated)
# join geolocated rows with rest of ex situ rows
exsitu_no_geo <- exsitu %>%
  filter(!(UID %in% exsitu_geo$UID))
nrow(exsitu_no_geo)
exsitu_all <- rbind.fill(exsitu_no_geo,exsitu_geo)
nrow(exsitu_all)
table(exsitu_all$gps_det)
#   L     C    G     X     H
#   289   62   1261  1331  2095

# write new file
write.csv(exsitu_all, file.path(exsitu_dir,data_out,
                                paste0("All_ExSitu_Compiled_Post-Geolocation_", Sys.Date(), ".csv")), 
          row.names = F)
# write to in situ folder also
write.csv(exsitu_all, file.path(main_dir,"occurrence_data",
                                "raw_occurrence_data","Ex-situ",
                                paste0("ExSitu_Compiled_Post-Geolocation_", Sys.Date(), ".csv")), 
          row.names = F)

