################################################################################
# Explore geoerferencing needs
################################################################################

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
write.csv(geo_needs, file.path(exsitu_dir,data_out,
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
# flag records for species that are high priority...
#   threatened and/or have less than 15 wild accessions
#   (you can choose whatever threshold you want)
# thresholds
rl_threat <- c("CR (Critically Endangered)","EN (Endangered)","VU (Vulnerable)")
ns_threat <- c("G1 (Critically Imperiled)","G2 (Imperiled)","G3 (Vulerable)")
few_wild <- geo_needs[geo_needs$num_wild<15,]$taxon_name_accepted
# flag priority taxa
priority_taxa <- taxon_list %>%
  filter(iucnredlist_category %in% rl_threat |
           natureserve_rank %in% ns_threat |
           taxon_name %in% few_wild) %>%
  select(taxon_name_accepted)
priority_taxa$priority <- "Priority"
need_geo <- left_join(need_geo,priority_taxa)
table(need_geo$priority) #456

# write file
write.csv(need_geo, file.path(exsitu_dir,data_out,
                              paste0("ExSitu_Need_Geolocation_", Sys.Date(), ".csv")),row.names = F)

### NOW MANUALLY GEOLOCATE !
### INSTRUCTIONS FOR GEOLOCATING:
### https://docs.google.com/document/d/1RBUD6-ogLc7PRVkDJSIEKzkgvC6xekj9Q_kl1vzxhCs/edit?usp=sharing

################################################################################
# Add geolocated data, after manual geolocation
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
