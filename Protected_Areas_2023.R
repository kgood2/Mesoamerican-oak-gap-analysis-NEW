###NEED TO WORK ON CLIPPING BOUNDARIES TO LAND
###NEED TO EXCLUDE EX SITU POINTS FROM DATABASE
### THIS IS JUST FOR Q. BRANDEGEEI, NOT ALL SPECIES
### need to edit key


### Author: Jean Linsky  ###  Date: 04/7/2021
### Adapted by Kate Good ### Date: January 27, 2023
### Adapted From:
# Emily Beckman et al. global_insitu_versus_exsitu_buffers script

### DESCRIPTION: Create a map of Protected areas and in situ points of threatened mesoamerican Oaks

### INPUTS:
## Protected Areas
# 	Protected areas via ProtectedPlanet, UNEP-WCMC and IUCN (2021)
#			https://www.protectedplanet.net/en
## Global country boundaries
#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via ArcGIS Hub
# 		Shapefile
#			https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0
## In situ occurrence points (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R
# 		https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
# 		Each file has data for only one species and is named "Genus_species.csv"
# 		You can read in data for mult. species in one file but need to edit the
#			code to split after reading in

### OUTPUTS:
## Interactive map with protected areas and insitu points visualized

################################################################################

#################
### LIBRARIES ###
#################

rm(list=ls())
my.packages <- c("leaflet","raster","sp","rgeos","plyr","dplyr","rgdal",
                 "Polychrome","cleangeo","RColorBrewer","smoothr","rnaturalearth","polylabelr",
                 "sf", "rnaturalearthhires")

#install.packages(my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)

select <- dplyr::select

#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

#install.packages('cmdstanr')
#install.packages("rnaturalearth")
#devtools::install_github("ropensci/rnaturalearth")

#########################
### WORKING DIRECTORY ###
#########################

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

## for point data (in situ)
pts_dir <- file.path(main_dir, "occurrence_data","standardized_occurrence_data","taxon_edited_points_removed")

## for polygon data (ecoregions, states, countries)
poly_dir <- file.path(main_dir,"gis_data")

## PA directory
pa_dir <- file.path(main_dir, "gis_data","protected_areas")

## for outputs
output.maps <- file.path(main_dir, "outputs", "protected_areas")

#################
### FUNCTIONS ###
#################

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.by.boundary <- function(pts,pt_proj,boundary){
  # select coordinate columns
  latlong <- pts %>% select(decimalLongitude,decimalLatitude)
  # turn occurrence point data into a SpatialPointsDataFrame
  spatial_pts <- SpatialPointsDataFrame(latlong, pts, proj4string = pt_proj)
  # clip by boundary created earlier
  spatial_pts <- spatial_pts[boundary, ]
  # keep just the data (not the spatial info you added)
  pts_new <- spatial_pts@data
  return(pts_new)
}

################################################################################
################################################################################
## Set things up
################################################################################

### DEFINE PROJECTIONS / COORDINATE REFERENCE SYSTEM (CRS)

## define initial projection of points (usually WGS 84); also used when creating
##	leaflet map
wgs.proj <- sp::CRS(SRS_string="EPSG:4326")
##CRS arguments: +proj=longlat +datum=WGS84 +no_defs

### READ IN POLYGON DATA

## Countries
world_countries <- readOGR(file.path(poly_dir,"UIA_World_Countries_Boundaries", "World_Countries__Generalized_.shp"))

## filter to only target countries; speeds things up and prevents errors.
##	there is a self-intersection error when trying the aggregate function for
##	the aea projection using all countries; tried clgeo_Clean and did not fix
sort(unique(world_countries@data$ISO))

## Look up country codes at website below, using "Alpha 2" column:
##	https://www.nationsonline.org/oneworld/country_code_list.htm
# "united states of america","mexico","belize","guatemala",
# "honduras","panama","costa rica","nicaragua","el salvador"
target_iso <- c("US","MX","BZ","GT","HN","PA","CR","NI","SV")
target_countries <- world_countries[world_countries@data$ISO %in% target_iso,]

## create polygon for clipping buffers later, one in each projection
target_countries.wgs <- spTransform(target_countries,wgs.proj)
boundary.wgs <- aggregate(target_countries.wgs,dissolve = TRUE)


##Protected Areas (Mexico)
MXprotected_areas0 <- readOGR(file.path(pa_dir,"Mexico_PAs","WDPA_WDOECM_Jan2023_Public_MEX_shp_0",
                                      "WDPA_WDOECM_Jan2023_Public_MEX_shp-polygons.shp"))
MXprotected_areas0_clip.wgs <- raster::intersect(MXprotected_areas0,boundary.wgs)

MXprotected_areas1 <- readOGR(file.path(pa_dir, "Mexico_PAs","WDPA_WDOECM_Jan2023_Public_MEX_shp_1",
                                      "WDPA_WDOECM_Jan2023_Public_MEX_shp-polygons.shp"))
MXprotected_areas1_clip.wgs <- raster::intersect(MXprotected_areas1,boundary.wgs)

MXprotected_areas2 <- readOGR(file.path(pa_dir, "Mexico_PAs","WDPA_WDOECM_Jan2023_Public_MEX_shp_2",
                                      "WDPA_WDOECM_Jan2023_Public_MEX_shp-polygons.shp"))
MXprotected_areas2_clip.wgs <- raster::intersect(MXprotected_areas2,boundary.wgs)

##Protected Areas (Costa Rica)
CRprotected_areas0 <-readOGR(file.path(pa_dir,"Costa_Rica_PAs","WDPA_WDOECM_Jan2023_Public_CRI_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_CRI_shp-polygons.shp"))
CRprotected_areas0_clip.wgs <- raster::intersect(CRprotected_areas0,boundary.wgs)

CRprotected_areas1 <-readOGR(file.path(pa_dir,"Costa_Rica_PAs","WDPA_WDOECM_Jan2023_Public_CRI_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_CRI_shp-polygons.shp"))
CRprotected_areas1_clip.wgs <- raster::intersect(CRprotected_areas1,boundary.wgs)

CRprotected_areas2 <-readOGR(file.path(pa_dir,"Costa_Rica_PAs","WDPA_WDOECM_Jan2023_Public_CRI_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_CRI_shp-polygons.shp"))
CRprotected_areas2_clip.wgs <- raster::intersect(CRprotected_areas2,boundary.wgs)

##Protected Areas (Guatemala)
GTprotected_areas0 <-readOGR(file.path(pa_dir,"Guatemala_PAs","WDPA_WDOECM_Jan2023_Public_GTM_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_GTM_shp-polygons.shp"))
GTprotected_areas0_clip.wgs <- raster::intersect(GTprotected_areas0,boundary.wgs)

GTprotected_areas1 <-readOGR(file.path(pa_dir,"Guatemala_PAs","WDPA_WDOECM_Jan2023_Public_GTM_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_GTM_shp-polygons.shp"))
GTprotected_areas1_clip.wgs <- raster::intersect(GTprotected_areas1,boundary.wgs)

GTprotected_areas2 <-readOGR(file.path(pa_dir,"Guatemala_PAs","WDPA_WDOECM_Jan2023_Public_GTM_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_GTM_shp-polygons.shp"))
GTprotected_areas2_clip.wgs <- raster::intersect(GTprotected_areas2,boundary.wgs)

##Protected Areas (Panama)
PAprotected_areas0 <-readOGR(file.path(pa_dir,"Panama_PAs","WDPA_WDOECM_Jan2023_Public_PAN_shp_0",
                                       "WDPA_WDOECM_Jan2023_Public_PAN_shp-polygons.shp"))
PAprotected_areas0_clip.wgs <- raster::intersect(PAprotected_areas0,boundary.wgs)

PAprotected_areas1 <-readOGR(file.path(pa_dir,"Panama_PAs","WDPA_WDOECM_Jan2023_Public_PAN_shp_1",
                                       "WDPA_WDOECM_Jan2023_Public_PAN_shp-polygons.shp"))
PAprotected_areas1_clip.wgs <- raster::intersect(PAprotected_areas1,boundary.wgs)

PAprotected_areas2 <-readOGR(file.path(pa_dir,"Panama_PAs","WDPA_WDOECM_Jan2023_Public_PAN_shp_2",
                                       "WDPA_WDOECM_Jan2023_Public_PAN_shp-polygons.shp"))
PAprotected_areas2_clip.wgs <- raster::intersect(PAprotected_areas2,boundary.wgs)


## countries
## read in country polygons using rnaturalearth package (or can read in other shapefile
##		you have downloaded online)

country_bound <- ne_countries(scale = 110, type="countries")
#country_bound <- readOGR(file.path(poly_dir,"UIA_World_Countries_Boundaries","World_Countries__Generalized_.shp"))

## project to WGS84
country_bound.wgs <- spTransform(country_bound,wgs.proj)



################################################################################
## Create interactive leaflet map
################################################################################

### CREATE LIST OF TARGET SPECIES

target_sp <- c("Quercus_brandegeei")
## select species to work with now
sp <- 1

### READ IN AND PREP POINT DATA

## read in wild in situ occurrence points
insitu <- read.csv(file.path(pts_dir,paste0(target_sp[sp], "_points_removed",
                                            ".csv")),na.strings=c("","NA"),
                   stringsAsFactors = F)
str(insitu)
## change column names or remove columns as needed; need at least
##	"decimalLatitude" and "decimalLongitude"
#insitu <- insitu %>%
  #dplyr::rename(decimalLatidude = Latitude, decimalLongitude = Longitude)
  #filter(Category == "in_situ")
## if desired, can clip points by boundary so only in target area
## (helpful if focusing on one country/region)
#insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
#str(insitu)


### CREATE MAP


## map everything!
## can turn layers on or off, or switch them for other polygons, as desired
map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
  ## Base layer
  ##	 explore other base layer options here:
  ##	 http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  ## Species name label
  addControl(paste0("<b>",gsub("_"," ",target_sp[sp])),
             position = "topright") %>%
  ## ProtectedAreas
  addPolygons(
    data = MXprotected_areas0_clip.wgs, label = "Quercus_brangegeei",
    fillOpacity = 0.8, color = "#038f28", weight = 1.5, opacity = 0.8) %>%
  addPolygons(
    data = MXprotected_areas1_clip.wgs, label = "Quercus_brangegeei",
    fillOpacity = 0.8, color = "#038f28", weight = 1.5, opacity = 0.8) %>%
  addPolygons(
    data = MXprotected_areas2_clip.wgs, label = "Quercus_brangegeei",
    fillOpacity = 0.8, color = "#038f28", weight = 1.5, opacity = 0.8) %>%
 
  ## (optional) In situ points
  addCircleMarkers(data = insitu,
                   lng = ~decimalLongitude, lat = ~decimalLatitude,
                   color = "black", radius = 3, fillOpacity = 1, stroke = F) %>%
  ## Add scale bar
  addScaleBar(position = "bottomright",
              options = scaleBarOptions(maxWidth = 150)) %>%
  

  ## Add legend
  ##	Used https://imgbb.com to host the buffer PNG images
  addControl(
    html = "<img src='https://i.ibb.co/j855sx6/black-circle.png'
    style='width:40px;height:40px;'> Geolocated in situ occurrence points<br/>
        <img src='https://i.ibb.co/Gt6kL1Q/green-square.png' 
    style='width:40px;height:40px;'> Protected areas from Protected Planet<br/>
        (https://www.protectedplanet.net/en)",
		position = "bottomleft") %>%

		
		## Set view (long and lat) and zoom level, for when map initially opens
  setView(-100, 20, zoom = 5) 
map

## save map as html file, so you can embed on a webpage or share with others
# looks like some of these maps are too big to save? works best to view
#		one-by-one in browser straight from R and take screenshot
htmlwidgets::saveWidget(map, file.path(output.maps, paste0(target_sp[sp],"_leaflet_map.html")))
