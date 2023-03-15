# script to map species distribution in relation to Protected Areas. Occurrence points
# that occur within Protected Areas are highlighted blue and occurrence points that occur outside 
# Protected Areas are highlighted green. 

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

acutifolia <- read.csv(file.path(main_dir, "occurrence_data","standardized_occurrence_data","taxon_edited_points_removed", "Quercus_acutifolia_points_removed.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character") 
acutifolia_sf <- st_as_sf(acutifolia, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

#read in KBA shapefiles
# Mexico
MXprotected_areas0 <- st_read(file.path(pa_dir,"Mexico_PAs","WDPA_WDOECM_Jan2023_Public_MEX_shp_0",
                                        "WDPA_WDOECM_Jan2023_Public_MEX_shp-polygons.shp"))
MXprotected_areas1 <- st_read(file.path(pa_dir, "Mexico_PAs","WDPA_WDOECM_Jan2023_Public_MEX_shp_1",
                                        "WDPA_WDOECM_Jan2023_Public_MEX_shp-polygons.shp"))
MXprotected_areas2 <- st_read(file.path(pa_dir, "Mexico_PAs","WDPA_WDOECM_Jan2023_Public_MEX_shp_2",
                                        "WDPA_WDOECM_Jan2023_Public_MEX_shp-polygons.shp"))
MX_all <- bind_rows(MXprotected_areas0,MXprotected_areas1,MXprotected_areas2)

# Costa Rica 
CRprotected_areas0 <-st_read(file.path(pa_dir,"Costa_Rica_PAs","WDPA_WDOECM_Jan2023_Public_CRI_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_CRI_shp-polygons.shp"))
CRprotected_areas1 <-st_read(file.path(pa_dir,"Costa_Rica_PAs","WDPA_WDOECM_Jan2023_Public_CRI_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_CRI_shp-polygons.shp"))
CRprotected_areas2 <-st_read(file.path(pa_dir,"Costa_Rica_PAs","WDPA_WDOECM_Jan2023_Public_CRI_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_CRI_shp-polygons.shp"))
CR_all <- bind_rows(CRprotected_areas0,CRprotected_areas1,CRprotected_areas2)

# Guatemala
GTprotected_areas0 <-st_read(file.path(pa_dir,"Guatemala_PAs","WDPA_WDOECM_Jan2023_Public_GTM_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_GTM_shp-polygons.shp"))
GTprotected_areas1 <-st_read(file.path(pa_dir,"Guatemala_PAs","WDPA_WDOECM_Jan2023_Public_GTM_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_GTM_shp-polygons.shp"))
GTprotected_areas2 <-st_read(file.path(pa_dir,"Guatemala_PAs","WDPA_WDOECM_Jan2023_Public_GTM_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_GTM_shp-polygons.shp"))
GT_all <- bind_rows(GTprotected_areas0,GTprotected_areas1,GTprotected_areas2)

# Panama
PAprotected_areas0 <-st_read(file.path(pa_dir,"Panama_PAs","WDPA_WDOECM_Jan2023_Public_PAN_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_PAN_shp-polygons.shp"))
PAprotected_areas1 <-st_read(file.path(pa_dir,"Panama_PAs","WDPA_WDOECM_Jan2023_Public_PAN_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_PAN_shp-polygons.shp"))
PAprotected_areas2 <-st_read(file.path(pa_dir,"Panama_PAs","WDPA_WDOECM_Jan2023_Public_PAN_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_PAN_shp-polygons.shp"))
PA_all <- bind_rows(PAprotected_areas0,PAprotected_areas1,PAprotected_areas2)

#El Salvador
SVprotected_areas0 <-st_read(file.path(pa_dir,"El_Salvador_PAs","WDPA_WDOECM_Jan2023_Public_SLV_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_SLV_shp-polygons.shp"))
SVprotected_areas1 <-st_read(file.path(pa_dir,"El_Salvador_PAs","WDPA_WDOECM_Jan2023_Public_SLV_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_SLV_shp-polygons.shp"))
SVprotected_areas2 <-st_read(file.path(pa_dir,"El_Salvador_PAs","WDPA_WDOECM_Jan2023_Public_SLV_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_SLV_shp-polygons.shp"))
SV_all <- bind_rows(SVprotected_areas0,SVprotected_areas1,SVprotected_areas2)

# Honduras
HNprotected_areas0 <-st_read(file.path(pa_dir,"Honduras_PAs","WDPA_WDOECM_Jan2023_Public_HND_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_HND_shp-polygons.shp"))
HNprotected_areas1 <-st_read(file.path(pa_dir,"Honduras_PAs","WDPA_WDOECM_Jan2023_Public_HND_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_HND_shp-polygons.shp"))
HNprotected_areas2 <-st_read(file.path(pa_dir,"Honduras_PAs","WDPA_WDOECM_Jan2023_Public_HND_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_HND_shp-polygons.shp"))
HN_all <- bind_rows(HNprotected_areas0,HNprotected_areas1,HNprotected_areas2)

# Nicaragua
NIprotected_areas0 <-st_read(file.path(pa_dir,"Nicaragua_PAs","WDPA_WDOECM_Jan2023_Public_NIC_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_NIC_shp-polygons.shp"))
NIprotected_areas1 <-st_read(file.path(pa_dir,"Nicaragua_PAs","WDPA_WDOECM_Jan2023_Public_NIC_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_NIC_shp-polygons.shp"))
NIprotected_areas2 <-st_read(file.path(pa_dir,"Nicaragua_PAs","WDPA_WDOECM_Jan2023_Public_NIC_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_NIC_shp-polygons.shp"))
NI_all <- bind_rows(NIprotected_areas0,NIprotected_areas1,NIprotected_areas2)

# Belize
BLZprotected_areas0 <- st_read(file.path(pa_dir,"Belize_PAs","WDPA_WDOECM_Mar2023_Public_BLZ_shp_0",
                                         "WDPA_WDOECM_Mar2023_Public_BLZ_shp-polygons.shp"))
BLZprotected_areas1 <- st_read(file.path(pa_dir,"Belize_PAs","WDPA_WDOECM_Mar2023_Public_BLZ_shp_1",
                                         "WDPA_WDOECM_Mar2023_Public_BLZ_shp-polygons.shp"))
BLZprotected_areas2 <- st_read(file.path(pa_dir,"Belize_PAs","WDPA_WDOECM_Mar2023_Public_BLZ_shp_2",
                                         "WDPA_WDOECM_Mar2023_Public_BLZ_shp-polygons.shp"))
BLZ_all <- bind_rows(BLZprotected_areas0,BLZprotected_areas1,BLZprotected_areas2)

# read in country shapefiles and crop KBAs to country boundaries (excludes marine PAs)
Mexico <- rnaturalearth::ne_countries(country = "Mexico")  %>%
  sf::st_as_sf()
Mexico_cropped <- st_intersection(MX_all, Mexico)

Costa_Rica <- Mexico <- rnaturalearth::ne_countries(country = "Costa Rica")  %>%
  sf::st_as_sf()
Costa_Rica_cropped <- st_intersection(CR_all, Costa_Rica)

Guatemala <- rnaturalearth::ne_countries(country = "Guatemala")  %>%
  sf::st_as_sf()
Guatemala_cropped <- st_intersection(GT_all, Guatemala)

Panama <- rnaturalearth::ne_countries(country = "Panama")  %>%
  sf::st_as_sf()
Panama_cropped <- st_intersection(PA_all, Panama)

El_Salvador <- rnaturalearth::ne_countries(country = "El Salvador")  %>%
  sf::st_as_sf()
El_Salvador_cropped <- st_intersection(SV_all, El_Salvador)

Honduras <- rnaturalearth::ne_countries(country = "Honduras")  %>%
  sf::st_as_sf()
Honduras_cropped <- st_intersection(HN_all, Honduras)

Nicaragua <- rnaturalearth::ne_countries(country = "Nicaragua")  %>%
  sf::st_as_sf()
Nicaragua_cropped <- st_intersection(NI_all, Nicaragua)

Belize <- rnaturalearth::ne_countries(country = "Belize")  %>%
  sf::st_as_sf()
Belize_cropped <- st_intersection(BLZ_all, Belize)



# combine 3 shapefiles for each country into one
Protected_areasAll <- bind_rows(Mexico_cropped, Costa_Rica_cropped, Guatemala_cropped,
                                Panama_cropped, El_Salvador_cropped,Honduras_cropped,
                                Nicaragua_cropped, Belize_cropped)

# read in world boundaries 
world_countries <- st_read(file.path(poly_dir,"UIA_World_Countries_Boundaries", "World_Countries__Generalized_.shp"))

# Create a spatial join between the occurrence points and the KBA boundary
locations_in_boundary <- st_intersection(acutifolia_sf, Protected_areasAll)

###############################################################################
# Create the leaflet map
###############################################################################
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = world_countries, fillOpacity = 0, color = "#969696", weight = 1.2, opacity = 1) %>%
  addPolygons(data = Protected_areasAll, color = "#99cc99", fillOpacity = 1) %>%
  addCircleMarkers(data=acutifolia_sf,radius = 2,
                   fillOpacity = 0.8, color = "red",
                   fillColor = "red") %>%
  addCircleMarkers(data = locations_in_boundary, radius = 2,
                   fillOpacity = 0.8, color = "blue",
                   fillColor = "blue")