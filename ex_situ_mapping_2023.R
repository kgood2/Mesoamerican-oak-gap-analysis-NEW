### 8-calc_map_exsitu_coverage.R
### Author: Emily Beckman Bruns & Kate Good
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Funding: 
#   -- USDA Forest Service (Cooperative Agreement 16-CA-11132546-045 with The
#        Morton Arboretum)
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: June 2023
### R version 4.3.0

### DESCRIPTION:
## This script creates circular buffers around in situ points (wild occurrence
#		records) and ex situ points (wild locations where seeds were collected for
#		cultivation in a botanic garden or storage in a genebank) to calculate the 
#		geographic (area within buffers) and ecological (number of ecoregions 
#		within buffers) representation of ex situ living collections.
## Optionally, an interactive map can be created to view ecoregions, buffers, 
#   and locations where exsitu germplasm was collected.

### INPUTS:
## target taxa list 
#   Can use target_taxa_with_synonyms.csv from 1-get_taxa_metadata, or can
#   create own list of accepted taxon names
## ecoregions shapefile
#   For calculations, there are three options. Script 1-prep_gis_layer.R 
#   describes each and instructions for download; options include:
#     - Global terrestrial ecoregions from The Nature Conservancy
#     - Ecoregions of North America (Canada, US, Mexico), Level III, from EPA
#     - Ecoregions of the Continental US, Level IV (finest level), from EPA
#   Right now everything is set up to use the global ecoregions layer. To
#   use a different layer for calculations, there are appropriate functions
#   already created and noted in that section of the script. For mapping 
#   (functions and palette prep), further edits would be needed to use one of 
#   the other ecoregion layers.
## world_countries_10m.shp
#   Shapefile created in 1-prep_gis_layers.R script. It's the Natural Earth 
#   10m countries layer with the lakes cut out and some ISO_2A issues fixed.
## occurrence point data, including ex situ wild collection locations
#   The script currently reads in occurrence point data from 
#   7-filter_occurrence_points.R (taxon_points_final folder), for calculations
#   and mapping; you could also use occurrence points in a different format,
#   as long as the following fields are present: decimalLatitude,
#   decimalLongitude, database (in situ points can have any value here, but 
#   ex situ points need to have "Ex_situ" in the database column)

### OUTPUTS:
## Table of geographic and ecological coverage (%) based on three buffer sizes
#	  (user defined, but defaults are 20, 50, and 100 km), plus Extent of 
#   Occurrence (EOO; convex hull around occurrence points) in km2
## (optional) Maps with occurrence points and ex situ wild collection 
#   locations - plus buffer layers around each point layer - and ecoregions
#   and, optionally, state borders. The map is in an interactive HTML format
#   but has not been fully developed to be highly interactive but rather
#   for a screenshot to be used in a static report.

################################################################################
# Load libraries
################################################################################

# load packages
my.packages <- c('tidyverse','textclean','terra','leaflet','rnaturalearth',
                 'Polychrome','sf')
# versions I used (in the order listed above): 2.0.0, 0.9.3, 1.7-29, 2.1.2, 0.3.3, 1.5.1, 1.0-13
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Create functions
################################################################################

# format text in cell for output table (to be used in a static report)
format.cell <- function(ex_result,in_result,final_result){
  cell <- paste0(round(final_result,2),"%","\n",
                 "(",format(round(ex_result,0),format="d",big.mark=",")," / ",
                 format(round(in_result,0),format="d",big.mark=","),")")
  return(cell)
}

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj,boundary){
  # turn occurrence point data into a SpatVector
  spat_pts <- vect(df, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt_proj)
  # reproject to specified projection
  proj_df <- project(spat_pts,buff_proj)
  # place buffer around each point, then dissolve into one polygon
  buffers <- buffer(proj_df,width=radius)
  buffers <- aggregate(buffers,dissolve = TRUE)
  # clip by boundary so they don't extend into the water
  boundary <- project(boundary,buff_proj)
  buffers_clip <- crop(buffers,boundary)
  # return buffer polygons
  return(buffers_clip)
}

# create buffers around in situ and ex situ spatial points, calculate areas,
#		then compare to calculate percent coverage
compare.buff.area <- function(insitu,exsitu,radius,pt_proj,buff_proj,boundary){
  # create buffers
  buffer_insitu <- create.buffers(insitu,radius,pt_proj,buff_proj,boundary)
  buffer_exsitu <- create.buffers(exsitu,radius,pt_proj,buff_proj,boundary)
  # calculate buffer area
  print(paste("Based on ",radius/1000," km radius..."))
  area_exsitu <- expanse(buffer_exsitu)/1000000
  print(paste("Area covered by ex situ buffers:", round(area_exsitu,0),"km²"))
  area_insitu <- expanse(buffer_insitu)/1000000
  print(paste("Area covered by in situ buffers:", round(area_insitu,0),"km²"))
  # calculate difference between in situ and ex situ buffer areas (% coverage)
  area_diff_percent <- (area_exsitu/area_insitu)*100
  print(paste0("Percent geographic coverage: ", round(area_diff_percent,2), "%"))
  txt <- format.cell(area_exsitu,area_insitu,area_diff_percent)
  return(txt)
}

# create data frame with ecoregion data extracted for area covered by buffers
intersect.eco.buff <- function(df,radius,pt_proj,buff_proj,eco,boundary){
  # create buffers
  buffers <- create.buffers(df,radius,pt_proj,buff_proj,boundary)
  # make sure ecoregions are in same projection as buffers
  eco_proj <- project(eco,buff_proj)
  # intersect buffers with ecoregions
  buff_join_eco <- intersect(buffers,eco_proj)
  return(buff_join_eco)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions.
#   uses the *HOLDRIDGE LIFE ZONES* layer
compare.ecoGlobal.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$DN))
  print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$DN))
  print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# Function to list DN under in situ and ex situ buffers
#   uses the *HOLDRIDGE LIFE ZONES* layer
compare.ecoGlobal.NAME <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  name_exsitu <- unique(eco_exsitu$DN)
  name_insitu <- unique(eco_insitu$DN)
  
  exsitu_message <- paste("Name of ecoregions under ex situ buffers:", paste(name_exsitu, collapse = ", "))
  insitu_message <- paste("Name of ecoregions under in situ buffers:", paste(name_insitu, collapse = ", "))
  
  print(exsitu_message)
  print(insitu_message)
}

# Function to list DN under in situ buffers ONLY
#   uses the *HOLDRIDGE LIFE ZONES* layer
compare.ecoGlobal.NAME.insitu <- function(insitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  name_insitu <- unique(eco_insitu$DN)
  
  insitu_message <- paste("Name of ecoregions under in situ buffers:", paste(name_insitu, collapse = ", "))
  
  print(insitu_message)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions.
#   uses the *North American Level III EPA* ecoregions layer
compare.ecoNorthAm.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$NA_L3CODE))
  print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$NA_L3CODE))
  print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions.
#   uses the *United States Level IV EPA* ecoregions layer
compare.ecoUS.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$US_L4CODE))
  print(paste0("Number of US ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$US_L4CODE))
  print(paste0("Number of US ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent US ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.pt.by.boundary <- function(pts,pt_proj,boundary){
  # turn occurrence point data into a SpatVector
  spat_pts <- vect(pts, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt_proj)
  # clip by boundary created earlier
  spat_pts <- crop(spat_pts,boundary)
  # keep just the data (not the spatial info you added)
  pts_new <- as.data.frame(spat_pts)
  return(pts_new)
}

# function to create ex situ coverage map, with ecoregions, buffers, and points
map.exsitu <- function(taxon,eco_now,states,in_buff,exsitu_buff,exsitu_pt,insitu_pts){
  # you can set the "maxZoom" to the level you'd like, or remove; used to 
  #   protect locations of rare wild plants
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## background
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    ## taxon name bolded at top right
    addControl(paste0("<b>",taxon), position = "topright") %>%
    ## global ecoregions
    # if you want to label the ecoregions, I would suggest creating a map
    #   with no points, buffers, or state borders (but with ecoregions labeled)
    #   and using that as a reference guide for the taxon maps; labeling 
    #   ecoregions on the taxon maps is really difficult in leaflet
    # if you want to use other ecoregion layer, you just need to change the 
    #   ECO_ID column to the equivalent ecoregion ID column in your layer
    addPolygons(
      data = eco_now, fillColor = ~eco_pal(eco_now$DN),
      fillOpacity = 0.8, color = "#757575", weight = 0.8, opacity = 1) %>%
    ## state boundaries
    addPolygons(
      data = states, fillColor = "transparent",
      weight = 0, opacity = 0.3, color = "#939799") %>%
    ## in situ buffers
    addPolygons(
      data = in_buff,
      fillColor = "#a3a3a3", fillOpacity = 0.45,
      weight = 2.3, opacity = 0.9, color = "black",
      smoothFactor = 0) %>%
    ## ex situ buffers
    addPolygons(
      data = exsitu_buff,
      fillColor = "white", fillOpacity = 0.52,
      weight = 1.3, color = "white", opacity = 0,
      smoothFactor = 0) %>%
    ## ex situ points
    addMarkers(data = exsitu_pt,
               lng = ~decimalLongitude, lat = ~decimalLatitude, icon = triangle_sm) %>%
    ## in situ points
    # can remove if you don't want these! remember to remove from legend too below
    addCircleMarkers(
      data = insitu_pts, lng = ~decimalLongitude, lat = ~decimalLatitude,
      color = "black", fillColor = "white", stroke = T,  fillOpacity = 1, weight = 1.5,
      # you may want to change the radius
      radius = 4) %>%
    ## add scale bar
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 150)) %>%
    ## add legend
    ##	not perfect, but something! Used https://imgbb.com to host the buffer
    ##	PNG images! So you could do that for any shape you'd like
    # in situ and ex situ buffers
    addControl(
      html = "<img src='https://i.ibb.co/pLbq1n4/In-situ-buffer.png'
      		style='width:40px;height:40px;'> Species' estimated native distribution<br/>
      		(20 km buffer around wild occurrence points)<br/>
      		<img src='https://i.ibb.co/SR71N6k/Exsitu-buffer.png'
      		style='width:40px;height:40px;'> Estimated representation of ex situ collections<br/>
      		(20 km buffer around wild provenance localities)",
      position = "bottomleft") %>%
    # ex situ triangles
    addControl(
      html = "<img src='https://www.freeiconspng.com/uploads/triangle-png-28.png'
      style='width:8px;height:8px;'> Source locality of wild provenance<br/>
      individuals in ex situ collections",
      position = "bottomleft") %>%
    # in situ occurrence points
    addControl(
      html = "<img src='https://www.freeiconspng.com/uploads/grey-circle-icon-8.png'
        		style='width:9px;height:9px;'> Wild occurrence point",
      position = "bottomleft") %>%
    ## set view (long and lat) and zoom level, for when map initially opens
    setView(-96, 40, zoom = 5)
  
  return(map)
}

# function to create map for taxa with no ex situ points
map.no.exsitu <- function(taxon,eco_now,states,in_buff,insitu_pts){
  # you can set the "maxZoom" to the level you'd like, or remove; used to 
  #   protect locations of rare wild plants
  map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
    ## background
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    ## taxon name bolded at top right
    addControl(paste0("<b>",taxon), position = "topright") %>%
    ## global ecoregions
    addPolygons(
      data = eco_now,
      fillColor = ~eco_pal(eco_now$DN),
      fillOpacity = 0.8, color = "#757575", weight = 0.8, opacity = 1) %>%
    ## state boundaries
    addPolygons(
      data = states,
      fillColor = "transparent",
      weight = 0, opacity = 0.3, color = "#939799") %>%
    ## in situ buffers
    addPolygons(
      data = in_buff,
      fillColor = "#a3a3a3", fillOpacity = 0.45,
      weight = 2.3, opacity = 0.9, color = "black",
      smoothFactor = 0) %>%
    ## in situ points
    # can remove if you don't want these!
    addCircleMarkers(data = insitu_pts,
                     lng = ~decimalLongitude, lat = ~decimalLatitude,
                     color = "black", fillColor = "white", weight = 1.5,
                     # you may want to change the radius
                     radius = 4, fillOpacity = 1, stroke = T) %>%
    ## add scale bar
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(maxWidth = 150)) %>%
    ## add legend
    ##	not perfect, but something! Used https://imgbb.com to host the buffer
    ##	PNG images! So you could do that for any shape you'd like
    # in situ buffers
    addControl(
      html = "<img src='https://i.ibb.co/pLbq1n4/In-situ-buffer.png'
      		style='width:40px;height:40px;'> Species' estimated native distribution<br/>
      		(20 km buffer around wild occurrence points)",
      position = "bottomleft") %>%
    # in situ occurrence points
    addControl(
      html = "<img src='https://www.freeiconspng.com/uploads/grey-circle-icon-8.png'
          		style='width:9px;height:9px;'> Wild occurrence point",
      position = "bottomleft") %>%
    ## set view (long and lat) and zoom level, for when map initially opens
    setView(-96, 40, zoom = 4)
  
  return(map)
}

################################################################################
# Set working directory
################################################################################

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

# assign folder where you have input occurrence point data
data_in <- "taxon_edited_points_removed"

# set up file structure within your main working directory
data <- "occurrence_data"
standardized_occ <- "standardized_occurrence_data"
occ_dir <- "occurrence_data"
analysis_dir <- "analysis"


################################################################################
# Set up standards to use throughout
################################################################################

# choose buffer sizes (in meters!) to use in calculations;
#   we use the medium size for mapping
# note you could also make this into a list and update the calc section further 
#   down (note added) so it loops through each element, making the number of 
#   different-sized buffers flexible
large_buff <- 50000  # 50 km
med_buff <-  20000    # 20 km
small_buff <- 10000   # 10 km

## DECIDE if you'd like to make maps, or run calculations only
#   no maps = FALSE
#   yes maps = TRUE
make_maps <- TRUE

if(make_maps){
  
  # CHOOSE cutoffs used for grouping ex situ data by number of individuals in maps;
  #   three categories will be created: 
  #   1) x < few_indiv   2) x >= few_indiv & x < many_indiv   3) x >= many_indiv
  #few_indiv <- 10
  #many_indiv <- 30
  
  # get icons used to mark number of ex situ individuals on maps 
  triangle_sm <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                          iconWidth = 11, iconHeight = 11)
  #triangle_md <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
  #iconWidth = 15, iconHeight = 15)
  #triangle_lg <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
  #iconWidth = 22, iconHeight = 22)
  
  # SELECT target countries (countries with taxa you're mapping); these are
  #   for getting state boundaries from rnaturalearth package
  target_countries <- c("United States of America","Mexico","Belize","Guatemala",
                        "Honduras","Panama","Costa Rica","Nicaragua","El Salvador")
  
  # create folder for output maps
  maps_out <- "exsitu_coverage_maps"
  if(!dir.exists(file.path(main_dir,analysis_dir,maps_out)))
    dir.create(file.path(main_dir,analysis_dir,maps_out), 
               recursive=T)
  
}

# define projections
#	points will be WGS84
pt.proj <- "+proj=longlat +datum=WGS84"
# for calculations, we need something with units in meters and equal area
calc.proj <- "+proj=eqearth +datum=WGS84"

# read in target taxa list
# you can also simply create the "target_taxa" and "target_files" lists
#   manually, if desired
taxon_list <- read.csv(file.path(main_dir,"inputs", "taxa_list","target_taxa_with_synonyms.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))
# list of accepted taxa to cycle through
target_taxa <- unique(taxon_list$taxon_name_acc)
# make taxon list with underscores added where spaces, to format for reading/
#   writing when we cycle through in our loop below
target_files <- unique(mgsub(taxon_list$taxon_name_acc, 
                             c(" ","var.","subsp."), c("_","var","subsp")))

################################################################################
# Read in and prep polygon data
################################################################################

# depending on the scale/region of your analysis, choose the ecoregion layer
#   you'd like to use (download instructions in 1-prep_gis_layers.R)...
## Global terrestrial ecoregions from The Nature Conservancy
# read in shapefile of global ecoregions
#this shapefile has what used to be HLZID 28 merged with 18. They should be the same name. 
ecoregions <- vect(file.path("/Volumes/GoogleDrive/My Drive/Holdridge Life Zones/Final_30s/HLZ_30s.shp"))

# read in world countries layer created in 1-prep_gis_layers.R
# this will be used to clip buffers so they're not in the water
# read in shapefile of country boundaries
world_poly_clip <- vect(file.path(main_dir,"gis_data",
                                  "world_countries_10m","world_countries_10m.shp"))


if(make_maps){
  
  # read in world countries layer from rnaturalearth package
  # this is for adding as a map layer
  world_poly_map <- ne_countries(scale = 50, type = "countries", 
                                 returnclass = "sf")
  
  # optionally, if you'd like to have state borders on your map:
  # read in state polygons for target countries using rnaturalearth package
  state_ls <- list()
  for(i in 1:length(target_countries)){
    state_ls[[i]] <- ne_states(country=target_countries[i],returnclass = "sf")
    print(target_countries[i])
  }
  # merge all state polygons
  state_boundaries <- Reduce(rbind,state_ls)
  
  # prep ecoregions for mapping
  # transform ecoregion polygons to point projection, for mapping
  eco_map <- terra::project(ecoregions,pt.proj)
  # select realm of interest; keep only ecoregions in that realm; this helps
  # with the mapping palette and size of output map
  #   OPTIONS:
  #     "Australasia" "Antarctic"   "Afrotropic"  "Indo-Malay"  "Nearctic"   
  #     "Neotropic"   "Oceania"     "Palearctic" 
  #eco_map <- eco_map[eco_map$WWF_REALM2 == "Nearctic" | 
  #eco_map$WWF_REALM2 == "Neotropic" ,]
  # the global ecoregions layer does not have major lakes cut out, so we'll do 
  #   that; takes a little while; you can skip if needed
  #eco_map <- crop(eco_map,world_poly_clip)
  # if you're using more than one realm and/or have concerns about the maps
  #   being too large, crop ecoregions by state layer as well
  #eco_map <- crop(eco_map,state_boundaries)
  # make sf object instead of terra, for input into leaflet
  eco_map <- st_as_sf(eco_map)
  
  # create ecoregion color palette, based on manually-selected 'seed colors'
  # every time you run this section it creates a new palette; if you
  #   want the same color ecoregions for every taxon, run them all in one go;
  #   if you don't like the ecoregion colors, run this again or edit it
  #eco_pal_colors <- createPalette(length(unique(eco_map$DN)),
  #seedcolors = c("#ba3c3c","#ba7d3c","#baab3c",
  #"#3ca7ba","#3c6aba","#573cba",
  #"#943cba","#ba3ca1","#ba3c55"),
  #range = c(5,45), target = "normal", M=50000)
  eco_pal_colors <- c("#ffb6c1","#7b68ee","#ff1493","#90ee90","#7f7d9c",
                               "#fa8072","#1e90ff","#ff00ff","#d37da5","#247777",
                               "#4f7942","#dc143c","#00ff7f","#884070","#deb887",
                               "#228b22","#e4d00a","#d2691e","#b03060","#800080",
                               "#8fbc8f","#799943","#9acd32","#ebad56","#4682b4",
                               "#097969","#483d8b","#808000","#bf0a30","#964e02",
                               "#6a77ec","#964754","#ce5f77","#8a927f")
  swatch(eco_pal_colors)
  eco_pal_colors <- as.vector(eco_pal_colors)
  eco_map <- eco_map[order(eco_map$DN),]
  eco_pal <- colorFactor(eco_pal_colors,eco_map$DN)
  
}

################################################################################
## Calculate & map geographic and ecological coverage of ex situ collections
################################################################################

# start summary table for analysis results
# we add each target taxon as we go along
# note that the formatting of this table was created as an easy input for a
#   table in a final report; you may want to format differently (split out
#   cell contents that are concatenated) if using for different output
summary_tbl <- data.frame(
  taxon = "start",
  geo_sm = "start", geo_md = "start",	geo_lg = "start",
  eco_sm = "start",eco_sm_nm = "start", eco_md = "start",eco_md_nm = "start", eco_lg = "start", eco_lg_nm = "start",
  EOO = "start",
  stringsAsFactors=F)

### CYCLE THROUGH TARGET TAXA TO CALCULATE EX SITU COVERAGE

for(i in 1:length(target_taxa)){
  
  ## can test with one taxon first if you'd like - skip loop line above and
  ##  uncomment next line
  #i <- 14
  
  # print progress
  cat("\nStarting", target_taxa[i], "\n")
  
  ### READ IN AND PREP POINT DATA
  
  ## read in occurrence points (includes ex situ)
  insitu_pt <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
                                  paste0(target_files[i],"_points_removed",".csv")), 
                        na.strings=c("","NA"), stringsAsFactors = F)
  nrow(insitu_pt)
  
  ### CALCULATE EOO (Extent of Occurrence on the IUCN Red List; convex hull)
  
  # make points into vector spatial object
  spat_pts <- vect(insitu_pt, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt.proj, keepgeom=FALSE)
  spat_pts.calc <- project(spat_pts,calc.proj)
  # calculate area of convex hull in km2
  hull_insitu <- convHull(spat_pts.calc)
  hull_area <- expanse(hull_insitu)/1000000
  print(paste("EOO:",round(hull_area,0),"km²"))
  
  ### CREATE DATA SUBSET WITH EX SITU ONLY
  
  # create subset with just ex situ points
  exsitu_pt <- insitu_pt %>% filter(database == "Ex_situ")
  # print number of individuals ex situ with lat-long data
  print(paste("Number of ex situ individuals:",
              sum(as.numeric(exsitu_pt$individualCount))))
  
  # if no ex situ points, skip calculations and create map without ex situ elements
  if(nrow(exsitu_pt) == 0){
    print("No ex situ points; skipping buffer calculations")
    
    #name ecoregions under large buffers
    eco_coverage_lg_name <- compare.ecoGlobal.NAME.insitu(insitu_pt,large_buff,
                                                          pt.proj,calc.proj,ecoregions,
                                                          world_poly_clip)
    
    #name ecoregions under medium buffers
    eco_coverage_md_name <- compare.ecoGlobal.NAME.insitu(insitu_pt,med_buff,
                                                          pt.proj,calc.proj,ecoregions,
                                                          world_poly_clip)
    
    #name ecoregions under small buffers
    eco_coverage_sm_name <- compare.ecoGlobal.NAME.insitu(insitu_pt,small_buff,
                                                          pt.proj,calc.proj,ecoregions,
                                                          world_poly_clip)
    
    # add text results to summary table
    summary_add <- data.frame(
      taxon = target_taxa[i],
      geo_sm = "0%", geo_md = "0%",	geo_lg = "0%",
      eco_sm = "0%", eco_sm_nm = eco_coverage_sm_name, eco_md = "0%", eco_md_nm = eco_coverage_md_name, eco_lg = "0%", eco_lg_nm = eco_coverage_lg_name,
      EOO = round(hull_area,0),
      stringsAsFactors=F)
    summary_tbl[i,] <- summary_add
    
    if(make_maps){
      # create buffers around in situ points, for mapping
      insitu_buff <- sf::st_as_sf(create.buffers(insitu_pt,med_buff,pt.proj,
                                                 pt.proj,world_poly_clip))
      # create map
      #map <- map.no.exsitu(target_taxa[i],eco_map,state_boundaries,insitu_buff,
      #insitu_pt); map
      # save map
      #htmlwidgets::saveWidget(map,file.path(main_dir,analysis_dir,maps_out,
      #paste0(target_files[i],
      #"__exsitu_coverage_map",
      #".html")))
    }
  } else {
    
    ### CALCULATE EX SITU COVERAGE
    
    # Note you could also make this section into a loop based on the number
    #   of different-sized buffers you'd like to test
    
    ## Geographic Coverage
    cat("GEOGRAPHIC COVERAGE\n")
    # calculate area based on large buffers
    geo_coverage_lg <- compare.buff.area(insitu_pt,exsitu_pt,large_buff,
                                         pt.proj,calc.proj,world_poly_clip)
    # calculate area based on medium buffers
    geo_coverage_md <- compare.buff.area(insitu_pt,exsitu_pt,med_buff,
                                         pt.proj,calc.proj,world_poly_clip)
    # calculate area based on small buffers
    geo_coverage_sm <- compare.buff.area(insitu_pt,exsitu_pt,small_buff,
                                         pt.proj,calc.proj,world_poly_clip)
    
    ## Ecological Coverage
    cat("ECOLOGICAL COVERAGE\n")
    ##  change the function based on the ecoregions you want to use:
    ##    compare.ecoGlobal.count
    ##    compare.ecoNorthAm.count
    ##    compare.ecoUS.count
    # count ecoregions under large buffers
    eco_coverage_lg <- compare.ecoGlobal.count(insitu_pt,exsitu_pt,large_buff,
                                               pt.proj,calc.proj,ecoregions,
                                               world_poly_clip)
    #name ecoregions under large buffers
    eco_coverage_lg_name <- compare.ecoGlobal.NAME(insitu_pt,exsitu_pt,large_buff,
                                                   pt.proj,calc.proj,ecoregions,
                                                   world_poly_clip)
    
    # count ecoregions under medium buffers
    eco_coverage_md <- compare.ecoGlobal.count(insitu_pt,exsitu_pt,med_buff,
                                               pt.proj,calc.proj,ecoregions,
                                               world_poly_clip)
    
    #name ecoregions under medium buffers
    eco_coverage_md_name <- compare.ecoGlobal.NAME(insitu_pt,exsitu_pt,med_buff,
                                                   pt.proj,calc.proj,ecoregions,
                                                   world_poly_clip)
    
    # count ecoregions under small buffers
    eco_coverage_sm <- compare.ecoGlobal.count(insitu_pt,exsitu_pt,small_buff,
                                               pt.proj,calc.proj,ecoregions,
                                               world_poly_clip)
    
    #name ecoregions under small buffers
    eco_coverage_sm_name <- compare.ecoGlobal.NAME(insitu_pt,exsitu_pt,small_buff,
                                                   pt.proj,calc.proj,ecoregions,
                                                   world_poly_clip)
    
    
    ## Summary Table
    # add text results to summary table
    summary_add <- data.frame(
      taxon = target_taxa[i],
      geo_sm = geo_coverage_sm,
      geo_md = geo_coverage_md,
      geo_lg = geo_coverage_lg,
      eco_sm = eco_coverage_sm,
      eco_sm_nm = eco_coverage_sm_name,
      eco_md = eco_coverage_md,
      eco_md_nm = eco_coverage_md_name,
      eco_lg = eco_coverage_lg,
      eco_lg_nm = eco_coverage_lg_name,
      EOO = round(hull_area,0),
      stringsAsFactors=F)
    summary_tbl[i,] <- summary_add
    
    ### CREATE MAP
    
    if(make_maps){
      
      # prep buffer layers for mapping
      insitu_buff <- st_as_sf(create.buffers(insitu_pt,med_buff,
                                             pt.proj,pt.proj,world_poly_clip))
      exsitu_buff <- st_as_sf(create.buffers(exsitu_pt,med_buff,
                                             pt.proj,pt.proj,world_poly_clip))
      
      # split ex situ data by number of individuals, to use different symbols
      #exsitu1 <- exsitu_pt %>% arrange(individualCount) %>%
      #filter(individualCount < few_indiv)
      #exsitu2 <- exsitu_pt %>% arrange(individualCount) %>%
      #filter(individualCount >= few_indiv & individualCount < many_indiv)
      #exsitu3 <- exsitu_pt %>% arrange(individualCount) %>%
      #33filter(individualCount >= many_indiv)
      
      # create buffers around in situ points, for mapping
      insitu_buff <- sf::st_as_sf(create.buffers(insitu_pt,med_buff,pt.proj,
                                                 pt.proj,world_poly_clip))
      # create map
      #map <- map.exsitu(target_taxa[i],eco_map,state_boundaries,insitu_buff,
      #exsitu_buff,exsitu_pt,insitu_pt); map
      # save map
      #htmlwidgets::saveWidget(map,file.path(main_dir,analysis_dir,maps_out,
      #paste0(target_files[i],
      #"__exsitu_coverage_map",
      #".html")))
    } 
  }
}
## write summary table
summary_tbl
write.csv(summary_tbl, file.path(main_dir,analysis_dir,
                                 paste0("exsitu_coverage_",Sys.Date(),".csv")),
          row.names = F)  


################################################################################
#Run this part of script to read in CSV file with HLZ ID's, and replace ID numbers
# with HLZ names. Save a new CSV file. 
################################################################################

HLZ <- read.csv(file.path(main_dir, analysis_dir, "HLZ_coverage.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))

head(HLZ)

#create a new column that identifies the life zones that are in the in situ 
#column but not in the ex situ column. Also makes columns numeric and trims trailing white space
find_non_conserved <- function(hlz_insitu, hlz_exsitu) {
  insitu_numbers <- strsplit(trimws(hlz_insitu), ",")[[1]]
  exsitu_numbers <- strsplit(trimws(hlz_exsitu), ",")[[1]]
  insitu_numbers <- as.numeric(insitu_numbers)
  exsitu_numbers <- as.numeric(exsitu_numbers)
  non_conserved <- setdiff(insitu_numbers, exsitu_numbers)
  return(paste(non_conserved, collapse = ","))
}

HLZ <- HLZ %>%
  mutate(NotConserved = mapply(find_non_conserved, hlz_insitu, hlz_exsitu))

#add a space back in between commas and numbers in NonConserved column 
HLZ$NotConserved <- gsub(",", ", ", HLZ$NotConserved)


# sub HLZ id numbers with their life zone name in ex_situ column. 
HLZ$hlz_exsitu <- mgsub(HLZ$hlz_exsitu,
                        c("117","216","238","315","337",
                          "348","359","425","436","447",
                          "458","469","513","524","535",
                          "546","557","568","579","613",
                          "624","635","646","657","668",
                          "679","712","723","734","745",
                          "756","767","778","789"),
                        c("Polar desert","Subpolar dry tundra","Subpolar wet tundra","Boreal desert","Boreal moist forest",
                          "Boreal wet forest","Boreal rain forest","Cool temperate desert scrub","Cool temperate steppe","Cool temperate moist forest",
                          "Cool temperate wet forest","Cool temperate rain forest","Warm temperate desert","Warm temperate desert scrub","Warm temperate thorn scrub",
                          "Warm temperate dry forest","Warm temperate moist forest","Warm temperate wet forest","Warm temperate rain forest","Subtropical desert",
                          "Subtropical desert scrub","Subtropical thorn woodland","Subtropical dry forest","Subtropical moist forest","Subtropical wet forest",
                          "Subtropical rain forest","Tropical desert","Tropical desert scrub","Tropical thorn woodland","Tropical very dry forest",
                          "Tropical dry forest","Tropical moist forest","Tropical wet forest","Tropical wet forest"))
                        
# sub HLZ id numbers with their life zone name in in_situ column. 
HLZ$hlz_insitu <- mgsub(HLZ$hlz_insitu,
                        c("117","216","238","315","337",
                          "348","359","425","436","447",
                          "458","469","513","524","535",
                          "546","557","568","579","613",
                          "624","635","646","657","668",
                          "679","712","723","734","745",
                          "756","767","778","789"),
                        c("Polar desert","Subpolar dry tundra","Subpolar wet tundra","Boreal desert","Boreal moist forest",
                          "Boreal wet forest","Boreal rain forest","Cool temperate desert scrub","Cool temperate steppe","Cool temperate moist forest",
                          "Cool temperate wet forest","Cool temperate rain forest","Warm temperate desert","Warm temperate desert scrub","Warm temperate thorn scrub",
                          "Warm temperate dry forest","Warm temperate moist forest","Warm temperate wet forest","Warm temperate rain forest","Subtropical desert",
                          "Subtropical desert scrub","Subtropical thorn woodland","Subtropical dry forest","Subtropical moist forest","Subtropical wet forest",
                          "Subtropical rain forest","Tropical desert","Tropical desert scrub","Tropical thorn woodland","Tropical very dry forest",
                          "Tropical dry forest","Tropical moist forest","Tropical wet forest","Tropical wet forest"))

# sub HLZ id numbers with their life zone name in NotConserved column. 
HLZ$NotConserved <- mgsub(HLZ$NotConserved,
                          c("117","216","238","315","337",
                            "348","359","425","436","447",
                            "458","469","513","524","535",
                            "546","557","568","579","613",
                            "624","635","646","657","668",
                            "679","712","723","734","745",
                            "756","767","778","789"),
                          c("Polar desert","Subpolar dry tundra","Subpolar wet tundra","Boreal desert","Boreal moist forest",
                            "Boreal wet forest","Boreal rain forest","Cool temperate desert scrub","Cool temperate steppe","Cool temperate moist forest",
                            "Cool temperate wet forest","Cool temperate rain forest","Warm temperate desert","Warm temperate desert scrub","Warm temperate thorn scrub",
                            "Warm temperate dry forest","Warm temperate moist forest","Warm temperate wet forest","Warm temperate rain forest","Subtropical desert",
                            "Subtropical desert scrub","Subtropical thorn woodland","Subtropical dry forest","Subtropical moist forest","Subtropical wet forest",
                            "Subtropical rain forest","Tropical desert","Tropical desert scrub","Tropical thorn woodland","Tropical very dry forest",
                            "Tropical dry forest","Tropical moist forest","Tropical wet forest","Tropical wet forest"))

#write csv
write.csv(HLZ, file.path(main_dir,analysis_dir,
                                 paste0("HLZ_with_names",Sys.Date(),".csv")),
          row.names = F)  
