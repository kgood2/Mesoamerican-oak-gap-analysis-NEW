# script to map species distribution in relation to Key Biodiversity Areas. Occurrence points
# that occur within KBAs are highlighted blue and occurrence points that occur outside KBAs
# are highlighted green. 

# Inputs: taxo_edited_points_removed csv
# KBA shapefile 

# Outputs: htmp and static map 

# Kate Good
# March 14, 2023

################################################################################
#load libraries and main directory 

library(sf)
library(dplyr)
library(leaflet)

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

################################################################################

# read in data with lat/long coordinates. Update depending on species you 
# are interested in 

acutifolia <- read.csv(file.path(main_dir, "occurrence_data","standardized_occurrence_data","taxon_edited_points_removed", "Quercus_acutifolia_points_removed.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character") 
acutifolia_sf <- st_as_sf(acutifolia, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

#read in KBA shapefile
KBA <- st_read(file.path(main_dir,"gis_data",
                         "protected_areas","North_Central_America_KBA","North_Central_America_KBA.shp"))

# Create a spatial join between the occurrence points and the KBA boundary
locations_in_boundary <- st_intersection(acutifolia_sf, KBA)

#Create a new column to indicate if the location falls within the boundary
#locations_in_boundary$within_boundary <- ifelse(!is.na(locations_in_boundary), "Yes", "No")

# Create the leaflet map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = KBA, color = "red") %>%
  addCircleMarkers(data=acutifolia_sf,radius = 2,
                   fillOpacity = 0.8, color = "green",
                   fillColor = "green") %>%
  addCircleMarkers(data = locations_in_boundary, radius = 2,
                   fillOpacity = 0.8, color = "blue",
                   fillColor = "blue")



