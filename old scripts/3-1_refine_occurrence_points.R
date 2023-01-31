################################################################################

## 3-1_refine_occurrence_points.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/10/2020

### DESCRIPTION:
# Flags suspect points by adding a column for each type of flag, where
#   FALSE = flagged. Most of the flagging is done through the
#   'CoordinateCleaner' package, which was created for "geographic cleaning
#   of coordinates from biologic collections."

### DATA IN:
# output from 3_compile_raw_occurrence_points.R
# tabular data:
# - target_taxa_with_syn.csv
# - globaltreesearch_country_distribution.csv
# - spatialpolygon data ...
#

### DATA OUT:
# spp_edited_points folder with CSV of occurrence points for each target
#   species (e.g., Quercus_lobata.csv)
# Summary table with one row for each target species, listing number of
#   points and number of flagged records in each flag column
#   (flag_summary_by_sp.csv)

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("raster", "sp", "tools", "spatialEco", "rgdal", "geosphere",
                 "readxl", "writexl", "dplyr", "tidyr", "tidyverse", "housingData", "maps",
                 "data.table", "textclean", "CoordinateCleaner", "countrycode", "usmap",
                 "RColorBrewer", "leaflet")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

main_dir <- "/Volumes/GoogleDrive/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"

################################################################################
################################################################################
# 1. Read in data
################################################################################

# bring in polygon (load from saved .RData file)
load(file.path(main_dir, "inputs", "gis_data", "admin_shapefiles.RData"))
# define projection
wgs_proj <- sp::CRS(SRS_string="EPSG:4326")
urban.poly <- spTransform(urban.poly,wgs_proj)

# read in country-level native distribution data
native_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                  "target_taxa_with_native_dist.csv"), header = T, na.strings = c("","NA"),
                        colClasses = "character")

# create new folder for revised points, if not already existing
out.fld.nm <- "spp_edited_points"
if(dir.exists(file.path(main_dir, "outputs", out.fld.nm)))
  print("directory already created") else dir.create(file.path(main_dir,
                                                               "outputs", out.fld.nm), recursive=TRUE)

################################################################################
# 2. Iterate through species files and flag suspect points
################################################################################

# list of species files to iterate
all.spp.files <- list.files(path=file.path(main_dir, "outputs",
                                           "spp_raw_points"), ignore.case=FALSE, full.names=FALSE, recursive=TRUE)
#all.spp.files <- all.spp.files[1:20]
spp_list <- file_path_sans_ext(all.spp.files)
# or can choose specific species
#spp_list <- c("Magnolia_brasiliensis","Magnolia_boliviana","Magnolia_arcabucoana","Magnolia_angustioblonga")#"Magnolia_calimaensis")
spp_list

# start a table to add summary of results for each species
summary_tbl <- data.frame(species_name_acc = "start", total_pts = "start",
                          unflagged_pts = "start", .cen = "start", #.urb = "start",
                          .inst = "start",
                          .con = "start", .outl = "start", .gtsnative = "start", .rlnative = "start",
                          .rlintroduced = "start", .yr1950 = "start", .yr1980 = "start",
                          .yrna = "start", stringsAsFactors=F)

# header/column name order and selection
c.nms <- c("species_name_acc", "taxon_name", "scientificName",
           "taxonIdentificationNotes", "database", "all_source_databases", "year",
           "basisOfRecord", "establishmentMeans","decimalLatitude", "decimalLongitude",
           "coordinateUncertaintyInMeters", "geolocationNotes", "localityDescription",
           "county", "stateProvince", "countryCode_standard",
           "datasetName", "publisher", "nativeDatabaseID", "references",
           "informationWithheld", "issue", "taxon_name_full", "list", "UID",
           "country.name", "country.iso_a2", "country.iso_a3", "country.continent",
           ".cen",#".urb",
           ".inst",".con",".outl",".gtsnative",".rlnative",
           ".rlintroduced",".yr1950",".yr1980",".yrna")

# iterate through each species file to flag suspect points
cat("Starting ", "target ", "taxa (", length(spp_list), " total)", ".\n\n",
    sep="")

for (i in 1:length(spp_list)){
  #i <- 1 #turn on to test loop for one species (and skip above line)
  f.nm <- spp_list[i]
  
  # bring in records
  eo.df <- read.csv(file.path(main_dir, "outputs", "spp_raw_points",
                              paste0(f.nm, ".csv")))
  
  # create SpatialPointsDataFrame for species
  eo.spdf <- SpatialPointsDataFrame(eo.df[,c("decimalLongitude",
                                             "decimalLatitude")], eo.df, proj4string = wgs_proj)
  ## add country polygon data to each point based on lat-long location
  eo.post <- point.in.poly(eo.spdf, adm0.poly, sp=TRUE)@data
  
  ## CHECK POINT LOCATION AGAINST "ACCEPTED" COUNTRY DISTRUBUTION
  ## GlobalTreeSearch
  # species native country distribution list from GTS
  s.nd.gts.l <- unique(unlist(strsplit(native_dist$gts_native_dist_iso2c[
    native_dist$species_name_acc==gsub("_"," ", f.nm)], "; ")))
  if(!is.na(s.nd.gts.l[[1]])){
    ## flag records where GTS country doesn't match record's coordinate location
    eo.post <- eo.post %>% mutate(.gtsnative=(ifelse(
      country.iso_a2 %in% s.nd.gts.l, TRUE, FALSE)))
  } else {
    eo.post$.gtsnative <- NA
  }
  ## IUCN Red List native
  # species native country distribution list from RL
  s.nd.rln.l <- unique(unlist(strsplit(native_dist$rl_native_dist_iso2c[
    native_dist$species_name_acc==gsub("_"," ", f.nm)], "; ")))
  if(!is.na(s.nd.rln.l[[1]])){
    ## flag records where RL country doesn't match record's coordinate location
    eo.post <- eo.post %>% mutate(.rlnative=(ifelse(
      country.iso_a2 %in% s.nd.rln.l, TRUE, FALSE)))
  } else {
    eo.post$.rlnative <- NA
  }
  ## IUCN Red List introduced
  # species introduced country distribution list from RL
  s.nd.rli.l <- unique(unlist(strsplit(native_dist$rl_introduced_dist_iso2c[
    native_dist$species_name_acc==gsub("_"," ", f.nm)], "; ")))
  if(!is.na(s.nd.rli.l[[1]])){
    ## flag records where RL introduced country does match record's coord location
    eo.post <- eo.post %>% mutate(.rlintroduced=(ifelse(
      country.iso_a2 %in% s.nd.rli.l, FALSE, TRUE)))
  } else {
    eo.post$.rlintroduced <- NA
  }
  
  ## SERIES OF VETTED TESTS FROM CoordinateCleaner PACKAGE
  # Geographic Cleaning of Coordinates from Biologic Collections
  # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152
  #   Cleaning geographic coordinates by multiple empirical tests to flag
  #     potentially erroneous coordinates, addressing issues common in
  #     biological collection databases.
  ## tests included:
  # cc_cen -> Identify Coordinates in Vicinity of Country and Province Centroids
  # cc_inst -> Identify Records in the Vicinity of Biodiversity Institutions
  ## other test not included but could add:
  # cc_iucn -> Identify Records Outside Natural Ranges
  eo.post2 <- clean_coordinates(eo.post,
                                lon = "decimalLongitude", lat = "decimalLatitude",
                                species = "species_name_acc",
                                centroids_rad = 500, # radius around capital coords (meters); default=1000
                                inst_rad = 100, # radius around biodiversity institutions coord (meters)
                                tests = c("centroids","institutions")
  )
  # adding urban area test separately because won't work for just 1 point
  #if(nrow(eo.df)<2){
  #  eo.post2$.urb <- NA
  #  print("Speices with fewer than 2 records will not be tested.")
  #} else {
  #  eo.post2 <- as.data.frame(eo.post2)
  #  flag_urb <- cc_urb(eo.post2,
  #    lon = "decimalLongitude",lat = "decimalLatitude",
  #    ref = urban.poly, value = "flagged")
  #  eo.post2$.urb <- flag_urb
  #}
  # for some reason the "sea" flag isn't working in the above function...
  #    adding here separately
  # actually, found it flags a lot on islands, etc. Skipping for now.
  #   flag_sea <- cc_sea(eo.post2,
  #     lon = "decimalLongitude", lat = "decimalLatitude", value = "flagged")
  #   eo.post2$.sea <- flag_sea
  # for some reason the outlier section won't work when part of
  #   "clean_coordinates" function above so adding it here
  eo.post2 <- as.data.frame(eo.post2)
  flag_outl <- cc_outl(eo.post2,
                       lon = "decimalLongitude",lat = "decimalLatitude",
                       species = "species_name_acc", method = "quantile",
                       mltpl = 4, value = "flagged")
  eo.post2$.outl <- flag_outl
  
  ## OTHER CHECKS
  ## Given country vs. lat-long country
  # check if given country matches lat-long country (CoordinateCleaner
  #   has something like this but also flags when NA? Didn't love that)
  eo.post2 <- eo.post2 %>% mutate(.con=(ifelse(
    (as.character(country.iso_a3) == as.character(countryCode_standard) &
       !is.na(country.iso_a3) & !is.na(countryCode_standard)) |
      is.na(country.iso_a3) | is.na(countryCode_standard), TRUE, FALSE)))
  ## Year
  eo.post2 <- eo.post2 %>% mutate(.yr1950=(ifelse(
    (as.numeric(year)>1950 | is.na(year)), TRUE, FALSE)))
  eo.post2 <- eo.post2 %>% mutate(.yr1980=(ifelse(
    (as.numeric(year)>1980 | is.na(year)), TRUE, FALSE)))
  eo.post2 <- eo.post2 %>% mutate(.yrna=(ifelse(
    !is.na(year), TRUE, FALSE)))
  
  # set column order and remove a few unnecessary columns
  eo.post3 <- eo.post2 %>% dplyr::select(all_of(c.nms))
  # df of unflagged points
  unflagged <- eo.post3 %>%
    filter(.cen & #.urb &
             .inst & .con & .outl & .yr1950 & .yr1980 & .yrna &
             (.gtsnative | is.na(.gtsnative)) &
             (.rlnative  | is.na(.rlnative)) &
             (.rlintroduced | is.na(.rlintroduced)) &
             basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
             establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
             establishmentMeans != "INVASIVE")
  # add to summary table
  summary_add <- data.frame(
    species_name_acc = spp_list[i],
    total_pts = nrow(eo.post3),
    unflagged_pts = nrow(unflagged),
    .cen = sum(!eo.post3$.cen),
    #.urb = sum(!eo.post3$.urb),
    .inst = sum(!eo.post3$.inst),
    .con = sum(!eo.post3$.con),
    .outl = sum(!eo.post3$.outl),
    .gtsnative = sum(!eo.post3$.gtsnative),
    .rlnative = sum(!eo.post3$.rlnative),
    .rlintroduced = sum(!eo.post3$.rlintroduced),
    .yr1950 = sum(!eo.post3$.yr1950),
    .yr1980 = sum(!eo.post3$.yr1980),
    .yrna = sum(!eo.post3$.yrna),
    stringsAsFactors=F)
  summary_tbl[i,] <- summary_add
  
  # WRITE NEW FILE
  write.csv(eo.post3, file.path(main_dir, "outputs", out.fld.nm,
                                paste0(f.nm, ".csv")), row.names=FALSE)
  
  cat("Ending ", f.nm, ", ", i, " of ", length(spp_list), ".\n\n", sep="")
}

# write summary table
write.csv(summary_tbl, file.path(main_dir,"outputs",
                                 paste0("summary_of_flagged_points_", Sys.Date(), ".csv")),row.names = F)
