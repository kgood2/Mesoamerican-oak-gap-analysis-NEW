# script to map species distribution in relation to Key Biodiversity Areas. Occurrence points
# that occur within KBAs are highlighted blue and occurrence points that occur outside KBAs
# are highlighted green. 

# Inputs: taxo_edited_points_removed csv
# KBA shapefile 

# Outputs: htmp and static map 

# Kate Good
# March 14, 2023

# script to map species distribution in relation to Protected Areas. Occurrence points
# that occur within Protected Areas are highlighted blue and occurrence points that occur outside 
# Protected Areas are highlighted green. 
# This script may need to be modified to exclude ex situ - ask Emily 
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

## PA directory
pa_dir <- file.path(main_dir, "gis_data","protected_areas")

## for polygon data (ecoregions, states, countries)
poly_dir <- file.path(main_dir,"gis_data")

################################################################################
# read in occurrence data and protected area shapefiles Update depending on 
# species you are interested in 
################################################################################

acherdophylla <- read.csv(file.path(main_dir, "occurrence_data","standardized_occurrence_data","taxon_edited_points_removed", "Quercus_acherdophylla_points_removed.csv"), 
                          header = T, na.strings=c("","NA"),colClasses="character") 
acherdophylla_sf <- st_as_sf(acherdophylla, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

#read in KBA shapefile
KBA <- st_read(file.path(main_dir,"gis_data",
                         "protected_areas","North_Central_America_KBA","North_Central_America_KBA.shp"))

#check validity of KBA file and fix if needed
st_is_valid(KBA)
KBA_fixed <- st_make_valid(KBA)

# read in world boundaries 
world_countries <- st_read(file.path(poly_dir,"UIA_World_Countries_Boundaries", "World_Countries__Generalized_.shp"))

# Create a spatial join between the occurrence points and the KBA boundary
locations_in_boundary <- st_intersection(acherdophylla_sf, KBA_fixed)

# Create a spatial join between the occurrence points and the KBA boundary
#locations_in_boundary <- st_join(acherdophylla_sf, KBA_fixed, join = st_within)

# count number of points inside protected areas 
points_in_polygons <- st_join(locations_in_boundary, KBA_fixed, join=st_within)
points_per_polygon <- points_in_polygons %>%
  summarize(n_points = n()) 
points_per_polygon 

###############################################################################
# Create the leaflet map
###############################################################################
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = world_countries, fillOpacity = 0, color = "#969696", weight = 1.2, opacity = 1) %>%
  addPolygons(data = KBA_fixed, color = "#3d8c40", opacity = 1, weight = 2) %>%
  addCircleMarkers(data=acherdophylla_sf,radius = 1,
                   color = "red", 
                   opacity = 1, 
                   fillOpacity = 1) %>%
  addCircleMarkers(data = locations_in_boundary, radius = 1,
                   color = "blue", 
                   opacity = 1,
                   fillOpacity = 1) %>%
  addScaleBar(position = "bottomright",
              options = scaleBarOptions(maxWidth = 150)) %>%
  addControl(html = "<img src='https://i.ibb.co/WWfzSyw/square-png-25129.png'
  		                                style='width:20px;height:20px;'> Key Biodiversity Area (KBA) <br/>
  		                                <img src='https://i.ibb.co/8M5jS4r/circle-png-25316.png'
  		                                style='width:20px;height:20px;'> Species occurence within KBA <br/>
  		                                <img src='https://i.ibb.co/ykQSPYH/circle-icon-16060.png'
                                      style='width:20px;height:20px;'> Species occurence outside KBA",
             position = "bottomleft") %>%
  setView(-99, 19, zoom = 4)