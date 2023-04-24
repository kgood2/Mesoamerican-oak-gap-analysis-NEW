################################################################################

## 7-0_calculate_map_exsitu_coverage_2023.R
### Authors: Emily Beckman Bruns
### Date: September 23, 2022

### DESCRIPTION:
# This script creates circular buffers around in situ points (wild occurrence
#		records) and ex situ points (wild locations where seeds were collected for
#		cultivation in botanic gardens or arboreta) to calculate the geographic
#		(area within buffers) and ecological (number of ecoregions within buffers)
#		diversity conserved in ex situ living collections.
# An interactive map is also created to view buffers and ecoregions.

### DATA IN:
## Global country boundaries
#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via
#			ArcGIS Hub Shapefile
#			https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0
## In situ occurrence points (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R
# 		https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
# 		Each file has data for only one species and is named "Genus_species.csv"
# 		You can read in data for mult. species in one file but need to edit the
#			code to split after reading in
## Ex situ wild localities (latitude and longitude in decimal degrees)
# 	Can use the output from 3-1_refine_occurrence_points.R, which has a
#			"database" column that has "Ex_situ" to distinguish the ex situ records
#			from the rest of the in situ records
## Global Ecoregions
# 	Terrestrial Ecoregions of the World, via WWF (Olson et al., 2001)
#			https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
## U.S. EPA Level III and Level IV Ecoregions
#		Level III (us_eco_l3) -> https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip
#		Level IV (us_eco_l4)-> https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip

### DATA OUT:
## Table of geographic and ecological coverage based on three buffer sizes
#		(10km, 50km, 100km) and two ecoregion levels (US IV, global)

################################################################################
# Load libraries
################################################################################

my.packages <- c(
  # for mapping
  "leaflet","rnaturalearth","Polychrome","polylabelr","RColorBrewer",
  # for calculations
  "dplyr","terra"
)
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

remotes::install_github('rstudio/rmarkdown')

################################################################################
# Set working directory
################################################################################

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

################################################################################
# Load functions
################################################################################

# format text in cell for output table
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
#		for both in situ and ex situ points, then compare count of ecoregions
compare.eco.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$ECO_ID))
  print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$ECO_ID))
  print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# create data frame with Level III ecoregion data extracted for area covered by
#		buffers, for both in situ and ex situ points, then compare ecoregion counts
compare.ecol3.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
  # create data frame of ecoregion-buffer intersection
  eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco,boundary)
  eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco,boundary)
  # count number of ecoregions under buffers
  print(paste("Based on ",radius/1000," km radius..."))
  count_exsitu <- length(unique(eco_exsitu$US_L3CODE))
  print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
  count_insitu <- length(unique(eco_insitu$US_L3CODE))
  print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
  # calculate difference in number of ecoregions
  eco_diff_percent <- (count_exsitu/count_insitu)*100
  print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
  txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
  return(txt)
}

# create data frame with Level IV ecoregion data extracted for area covered by
#		buffers, for both in situ and ex situ points, then compare ecoregion counts
compare.ecol4.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco,boundary){
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

################################################################################
################################################################################
# Set up workspace
################################################################################

# define projections we'll use throughout
#		points will be WGS84
pt.proj <- "+proj=longlat +datum=WGS84"
#   for calculations we need something in meters, like Equal Earth Projection
# calc.proj <- "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# calc.proj <- "+proj=aea + lat_1=29.5 + lat_2=45.5 + lat_0=37.5 + lon_0=-96 +x_0=0 +y_0=0 + ellps =GRS80 +datum=NAD83 + units=m +no_defs"

# Mexico Albers Equal Area Conic 
calc.proj <- "+proj=aea + lat_1=14.5 + lat_2=32.5 + lat_0=24.0 + lon_0=-105 +x_0=0 +y_0=0 + ellps =GRS80 +datum=NAD83 + units=m +no_defs"


# choose buffer sizes to use
# buffer size in kilometers = value/1000
large_buff <- 100000
med_buff <- 50000
small_buff <- 20000

# select target countries (countries with species you're mapping)
target_iso <- c("US","MX","BZ","GT","HN","PA","CR","NI","SV")
target_countries <- c("united states of america","mexico","belize","guatemala",
                      "honduras","panama","costa rica","nicaragua","el salvador")

# get outside icons used in maps
# triangle markers for ex situ point data
triangle_black <- makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                        iconWidth = 8, iconHeight = 8)
triangle_white <- makeIcon(iconUrl = "https://i.ibb.co/jyBBBdg/pngaaa-com-1992709.png",
                        iconWidth = 8, iconHeight = 8)

# cutoffs used for groupping exsitu data by number of individuals, for mapping
#   three categories will be used:
#   <=few_indiv | >few_indiv & <many_indiv | >=many_indiv
few_indiv <- 10
many_indiv <- 30

# create folder for maps, if not yet created
if(!dir.exists(file.path(main_dir,"outputs","exsitu_coverage")))
  dir.create(file.path(main_dir,"outputs","exsitu_coverage"), recursive=T)

################################################################################
# Choose target species
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list",
                                 "target_taxa_with_synonyms.csv"),
                       header = T, na.strings = c("","NA"),colClasses = "character")
head(taxon_list)

## Add species here that have no occurrence points and skipping
## Q. gulielmi-treleasei for now. Is throwing an error. 
#no_occ <- c("Quercus centenaria","Quercus mexiae","Quercus gulielmi-treleasei")
#taxon_list <- subset(taxon_list,!(taxon_name_acc %in% no_occ))


target_sp <- unique(taxon_list$taxon_name_acc)

# added line to replace _ with a space in species name when searching for file
# to read in occurrence points
target_sp <- gsub(" ","_",target_sp)


# read in native dist information to see if using RL (default) or GTS
native_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                  "target_taxa_with_native_dist.csv"), header = T, na.strings = c("","NA"),
                        colClasses = "character")
native_dist <- native_dist %>% dplyr::select(taxon_name_acc,rl_native_dist,
                                             gts_native_dist)

################################################################################
# Read in and prep polygon data
################################################################################

# read in shapefile of global ecoregions
ecoregions <- vect(file.path(main_dir,"gis_data",
                             "official","wwf_terr_ecos.shp"))

# read in shapefile of U.S. EPA Level III ecoregions
ecol3 <- vect(file.path(main_dir,"gis_data",
                        "us_eco_l3/us_eco_l3.shp"))

# read in shapefile of U.S. EPA Level IV ecoregions
ecol4 <- vect(file.path(main_dir,"gis_data",
                        "us_eco_l4_state_boundaries/us_eco_l4.shp"))

# read in shapefile of country boundaries
world_countries <- vect(file.path(main_dir,"gis_data",
                                  "UIA_World_Countries_Boundaries/World_Countries__Generalized_.shp"))
# create subset with only target countries
target_countries_shp <- subset(world_countries,
                               world_countries$ISO %in% target_iso,)

# create polygon for clipping points later (project to pt projection)
target_countries.pt <- project(target_countries_shp,pt.proj)
boundary.pt <- aggregate(target_countries.pt,dissolve = TRUE)

# create polygon for clipping buffers later
ecoregions_proj <- project(ecoregions,pt.proj)
boundary.poly <- aggregate(ecoregions_proj,dissolve = TRUE)

# optionally, if you'd like to have state borders on your map:
# read in state polygons for target countries using rnaturalearth package
state_bound_ls <- list()
for(i in 1:length(target_countries)){
  state_bound_ls[[i]] <- ne_states(country=target_countries[i])
  print(target_countries[i])
}
# merge all state polygons
state_bound <- Reduce(rbind,state_bound_ls)

################################################################################
## Calculate geographic and ecological coverage of ex situ collections; map
################################################################################

### START SUMMARY TABLE

# we add each target species as we go along
summary_tbl <- data.frame(
  species = "start",
  geo_sm = "start", geo_md = "start",	geo_lg = "start",
  eco_sm = "start", eco_md = "start", eco_lg = "start",
  eco_usl4_sm = "start", eco_usl4_md = "start", eco_usl4_lg = "start",
  EOO = "start",
  dist_filter = "start",
  #pa_coverage = "start",
  stringsAsFactors=F)


### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

for(sp in 1:length(target_sp)){
  
  ## can test with one species first if you'd like (skip loop line above)
  # turn on line below and turn off line above
  # sp <- 1
  
  # print progress
  cat("\nStarting ", target_sp[sp], "\n")
  
  ### READ IN AND PREP POINT DATA
  
  ## read in occurrence points (includes ex situ)
  insitu_raw <- read.csv(file.path(main_dir,"occurrence_data","standardized_occurrence_data",
                                   "taxon_edited_points_removed",
                                   paste0(target_sp[sp],"_points_removed",".csv")), na.strings=c("","NA"), stringsAsFactors = F)
  nrow(insitu_raw)
  spp.rl.dist <- native_dist[which(
    native_dist$taxon_name_acc == gsub("_"," ",target_sp[sp])),]
  insitu <- insitu_raw
  
  ### CALCULATE EOO (convex hull)
  
  # make points into vector spatial object
  spat_pts <- vect(insitu, geom=c("decimalLongitude", "decimalLatitude"),
                   crs=pt.proj, keepgeom=FALSE)
  spat_pts.calc <- project(spat_pts,calc.proj)
  # calculate area of convex hull in km2 (called EOO on the IUCN Red List)
  hull_insitu <- convHull(spat_pts.calc)
  hull_area <- expanse(hull_insitu)/1000000
  print(paste("EOO:",hull_area,"km²"))
  
  
  ### CREATE DATA SUBSET WITH EX SITU ONLY
  
  # create df with just ex situ points
  exsitu <- insitu %>% filter(database == "Ex_situ")
  # print number of individuals ex situ with lat-long data
  print(paste("Number of ex situ individuals:",
              sum(as.numeric(exsitu$individualCount))))
  
  # check there are ex situ points, if not skip calculations and create Map 1 only without ex situ points
  if(nrow(exsitu) == 0){
    # add text results to summary table
    summary_add <- data.frame(
      species = gsub("_"," ",target_sp[sp]),
      geo_sm = NA, geo_md = NA,	geo_lg = NA, geo_vlg = NA,
      eco_sm = NA, eco_md = NA, eco_lg = NA,
      eco_usl4_sm = NA, eco_usl4_md = NA, eco_usl4_lg = NA,
      EOO = hull_area,
      #dist_filter = dist_filter_val,
      stringsAsFactors=F)
    print("No ex situ points; skipping buffer calculations")
    
    summary_tbl[sp,] <- summary_add
    
    ### CREATE MAP
    
    # create maps to visualze species distribution and ecoregions
    # 	note that you can comment out any section to remove that element
    
    cat("\tMapping ", target_sp[sp])
    
    # prep ecoregions for mapping
    # transform ecoregion polygons to point projection, for mapping
    eco_ptproj <- project(ecoregions,pt.proj)
    # select only ecoregions that are within the buffers; otherwise map takes a
    #		long time to load in browser and colors are not distinct enough
    inter <- intersect.eco.buff(insitu,50000,pt.proj,pt.proj,eco_ptproj,boundary.poly)
    codes <- unique(inter$ECO_ID)
    eco_sel <- eco_ptproj[eco_ptproj$ECO_ID %in% codes,]
    eco_sel <- sf::st_as_sf(eco_sel)
    ### !!! other option is to clip ecoregions to countries of interest !!!
    
    # prep state labels, if using
    # find the visual center point of each state (still not perfect), for
    #		labeling purposes
    state_bound_sf <- sf::st_as_sf(state_bound)
    state_centers <- as.data.frame(do.call(rbind, poi(state_bound_sf, precision=0.01)))
    state_centers$label <- state_bound@data$name_en
    state_centers$x <- as.numeric(state_centers$x)
    state_centers$y <- as.numeric(state_centers$y)
    state_centers
    
    # can edit state/province names to better format for map labels
    state_centers$label <- gsub(" Prefecture","",state_centers$label)
    state_centers$label <- gsub(" Autonomous Region","",state_centers$label)
    state_centers$label <- gsub(" Department","",state_centers$label)
    state_centers$label <- gsub(" Province","",state_centers$label)
    state_centers$label <- gsub(" District","",state_centers$label)
    state_centers
    
    # prep buffer layers for mapping
    insitu_buff <- sf::st_as_sf(create.buffers(insitu,med_buff,pt.proj,pt.proj,boundary.poly))
    
    ## MAP
    ## No exsitu points or buffers with this map
    eco_pal_colors <- createPalette(length(unique(eco_sel$ECO_ID)),
                      seedcolors = c("#d58a8a","#d5b18a","#dcd59d","#b1dbe3","#b1c3e3","#bbb1e3",
                                              "#d4b1e3","#dc9dd0","#f8ebee"),range = c(5,42), target = "normal", M=50000)
                                    swatch(eco_pal_colors)
                                    eco_pal_colors <- as.vector(eco_pal_colors)
                                    eco_pal <- colorFactor(eco_pal_colors,eco_sel$ECO_ID)
                                                            
                                                            
                                    # create map
                                    coverage_map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
                                    ## background
                                    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
                                    ## species name label
                                    addControl(paste0("<b>",gsub("_"," ",target_sp[sp])),
                                          position = "topright") %>%
                                    ## global ecoregions
                                    addPolygons(
                                        data = eco_sel,
                                        label = ~ECO_NAME, fillColor = ~eco_pal(eco_sel$ECO_ID),
                                        fillOpacity = 0.8, color = "#757575", weight = 1.5, opacity = 0.8) %>%
                                    ## state boundaries
                                    addPolygons(
                                        data = state_bound, label = ~name_en, fillColor = "transparent",
                                        weight = 1.5, opacity = 0.3, color = "black") %>%
                                                              
                                    ## in situ buffers
                                      addPolygons(data = insitu_buff,
                                                  smoothFactor = 0.5,	weight = 1.2, opacity = 1, fillColor = "#e8f4f8",
                                                  color = "black",fillOpacity = 0.3) %>%
                                                              
                                    ## in situ points
                                      addMarkers(data = insitu,
                                                 lng = ~decimalLongitude, lat = ~decimalLatitude,
                                                 icon = triangle_white ) %>%
                                                              
                                      ## add scale bar
                                      addScaleBar(position = "bottomright",
                                          options = scaleBarOptions(maxWidth = 150)) %>%
                                                              
                                      ## add legend
                                      ##	not perfect, but something! Used https://imgbb.com to host the buffer
                                      ##	PNG images! So you could do that for any shape you'd like
                                      addControl(
                                      html = "<img src='https://upload.wikimedia.org/wikipedia/commons/a/a0/Circle_-_black_simple.svg'
  		                                style='width:40px;height:40px;'> Inferred native range<br/>
  		                                (50 km buffer around in situ occurrence points)",
  		                                position = "bottomleft") %>%
                                                              
                                      ## Set view (long and lat) and zoom level, for when map initially opens
                                      setView(-99, 19, zoom = 4)
                                                            
                                      # view map
                                      coverage_map
                                                            
                                      # save map
                                      htmlwidgets::saveWidget(coverage_map,
                                      file = file.path(main_dir,"outputs","exsitu_coverage",
                                      paste0(target_sp[sp],"-exsitu_coverage_interactive_map.html")))
  } else {
    
    ### CALCULATE EX SITU COVERAGE
    
    ## For species that have both in situ and ex situ points, first goes through calculations and then 
    ## creates Maps 1-3
    
    ## Geographic Coverage
    
    # calculate area based on large buffers
    geo_coverage_lg <- compare.buff.area(insitu,exsitu,large_buff,pt.proj,calc.proj,boundary.poly)
    # calculate area based on medium buffers
    geo_coverage_md <- compare.buff.area(insitu,exsitu,med_buff,pt.proj,calc.proj,boundary.poly)
    # calculate area based on small buffers
    geo_coverage_sm <- compare.buff.area(insitu,exsitu,small_buff,pt.proj,calc.proj,boundary.poly)
    
    ## Ecological Coverage
    
    ## Global ecoregions
    # count ecoregions under large buffers
    eco_coverage_lg <- compare.eco.count(insitu,exsitu,large_buff,pt.proj,calc.proj,ecoregions,boundary.poly)
    # count ecoregions under medium buffers
    eco_coverage_md <- compare.eco.count(insitu,exsitu,med_buff,pt.proj,calc.proj,ecoregions,boundary.poly)
    # count ecoregions under small buffers
    eco_coverage_sm <- compare.eco.count(insitu,exsitu,small_buff,pt.proj,calc.proj,ecoregions,boundary.poly)
    
    ## U.S. Level 4 (most specific) ecoregions
    # get just points that are in the U.S.
    us_insitu <- clip.pt.by.boundary(insitu,pt.proj,boundary.pt)
    us_exsitu <- clip.pt.by.boundary(exsitu,pt.proj,boundary.pt)
    # if there are in situ and ex situ points in the U.S., then calculate coverage
    if(nrow(us_exsitu) > 0 & nrow(us_insitu) > 0){
      # count ecoregions under large buffers
      ecol4_coverage_lg <- compare.ecol4.count(insitu,exsitu,large_buff,pt.proj,calc.proj,ecol4,boundary.poly)
      # count ecoregions under medium buffers
      ecol4_coverage_md <- compare.ecol4.count(insitu,exsitu,med_buff,pt.proj,calc.proj,ecol4,boundary.poly)
      # count ecoregions under small buffers
      ecol4_coverage_sm <- compare.ecol4.count(insitu,exsitu,small_buff,pt.proj,calc.proj,ecol4,boundary.poly)
      # if there's distribution in the U.S. but no ex situ points, assign 0%
    } else if(nrow(us_exsitu) == 0 & nrow(us_insitu) > 0){
      ecol4_coverage_lg <- "0%"
      ecol4_coverage_md <- "0%"
      ecol4_coverage_sm <- "0%"
      # if not in U.S. then NA
    } else {
      ecol4_coverage_lg <- NA
      ecol4_coverage_md <- NA
      ecol4_coverage_sm <- NA
    }
    
    ## Summary Table
    
    ## Add text results to summary table
    summary_add <- data.frame(
      species = gsub("_"," ",target_sp[sp]),
      geo_sm = geo_coverage_sm,
      geo_md = geo_coverage_md,
      geo_lg = geo_coverage_lg,
      eco_sm = eco_coverage_sm,
      eco_md = eco_coverage_md,
      eco_lg = eco_coverage_lg,
      eco_usl4_sm = ecol4_coverage_sm,
      eco_usl4_md = ecol4_coverage_md,
      eco_usl4_lg = ecol4_coverage_lg,
      EOO = round(hull_area,2),
      #dist_filter = dist_filter_val,
      stringsAsFactors=F)
    
    summary_tbl[sp,] <- summary_add
    
    ### CREATE MAP
    
    # create maps to visualze species distribution and ecoregions
    # 	note that you can comment out any section to remove that element
    
    cat("\tMapping ", target_sp[sp])
    
    # prep ecoregions for mapping
    # transform ecoregion polygons to point projection, for mapping
    eco_ptproj <- project(ecoregions,pt.proj)
    # select only ecoregions that are within the buffers; otherwise map takes a
    #		long time to load in browser and colors are not distinct enough
    inter <- intersect.eco.buff(insitu,50000,pt.proj,pt.proj,eco_ptproj,boundary.poly)
    codes <- unique(inter$ECO_ID)
    eco_sel <- eco_ptproj[eco_ptproj$ECO_ID %in% codes,]
    eco_sel <- sf::st_as_sf(eco_sel)
    ### !!! other option is to clip ecoregions to countries of interest !!!
    
    # prep state labels, if using
    # find the visual center point of each state (still not perfect), for
    #		labeling purposes
    state_bound_sf <- sf::st_as_sf(state_bound)
    state_centers <- as.data.frame(do.call(rbind, poi(state_bound_sf, precision=0.01)))
    state_centers$label <- state_bound@data$name_en
    state_centers$x <- as.numeric(state_centers$x)
    state_centers$y <- as.numeric(state_centers$y)
    state_centers
    # can edit state/province names to better format for map labels
    state_centers$label <- gsub(" Prefecture","",state_centers$label)
    state_centers$label <- gsub(" Autonomous Region","",state_centers$label)
    state_centers$label <- gsub(" Department","",state_centers$label)
    state_centers$label <- gsub(" Province","",state_centers$label)
    state_centers$label <- gsub(" District","",state_centers$label)
    state_centers
    
    # prep buffer layers for mapping
    insitu_buff <- sf::st_as_sf(create.buffers(insitu,med_buff,pt.proj,pt.proj,boundary.poly))
    exsitu_buff <- sf::st_as_sf(create.buffers(exsitu,med_buff,pt.proj,pt.proj,boundary.poly))
    
                  
      ##
      ## MAP
      ## This map has both in situ and ex situ points
                                                            
    eco_pal_colors <- createPalette(length(unique(eco_sel$ECO_ID)),
                                    seedcolors = c("#d58a8a","#d5b18a","#dcd59d","#b1dbe3","#b1c3e3","#bbb1e3",
                                                            "#d4b1e3","#dc9dd0","#f8ebee"),range = c(5,42), target = "normal", M=50000)
                                                            swatch(eco_pal_colors)
                                                            eco_pal_colors <- as.vector(eco_pal_colors)
                                                            eco_pal <- colorFactor(eco_pal_colors,eco_sel$ECO_ID)
                                                            
      # create map
      coverage_map <- leaflet(options = leafletOptions(maxZoom = 9)) %>%
      ## background
      addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
      ## species name label
      addControl(paste0("<b>",gsub("_"," ",target_sp[sp])),
            position = "topright") %>%
      ## global ecoregions
      addPolygons(
            data = eco_sel,
            label = ~ECO_NAME, fillColor = ~eco_pal(eco_sel$ECO_ID),
            fillOpacity = 0.8, color = "#757575", weight = 1.5, opacity = 0.8) %>%
      ## state boundaries
        addPolygons(data = state_bound,
                    fillOpacity = 0, color = "#969696", weight = 1.2, opacity = 1) %>%
        ## in situ buffers
        addPolygons(data = insitu_buff,
                    smoothFactor = 0.5,	weight = 1.2, opacity = 1, fillColor = "#e8f4f8",
                    color = "black",fillOpacity = 0.3) %>%
        ## ex situ buffers
        addPolygons(data = exsitu_buff,
                    smoothFactor = 0.5,	weight = 1.2, opacity = 1, color = "#1c1c1b",
                    fillOpacity = 0.3) %>%
        ## in situ points
        addMarkers(data = insitu,
                         lng = ~decimalLongitude, lat = ~decimalLatitude,
                         icon = triangle_white ) %>%
                        
        ## ex situ points
        addMarkers(data = exsitu,
                   lng = ~decimalLongitude, lat = ~decimalLatitude,
                   icon = triangle_black) %>%
          
        ## add scale bar
          addScaleBar(position = "bottomright",
              options = scaleBarOptions(maxWidth = 150)) %>%
          ## add legend
          ##	not perfect, but something! Used https://imgbb.com to host the buffer
          ##	PNG images! So you could do that for any shape you'd like
        
        addControl(
          html = "<img src='https://upload.wikimedia.org/wikipedia/commons/a/a0/Circle_-_black_simple.svg'
  		                                style='width:40px;height:40px;'> Inferred native range<br/>
  		                                (50 km buffer around in situ occurrence points)<br/>
  		                                <img src='https://upload.wikimedia.org/wikipedia/commons/f/fe/Wikisource_proofread_grey_circle.svg'
  		                                style='width:40px;height:40px;'> Estimated capture of ex situ collections<br/>
  		                                (50 km buffer around wild provenance localities)<br/>
                                      <img src='https://i.ibb.co/jyBBBdg/pngaaa-com-1992709.png'
                                      style='width:20px;height:20px;'> Geolocated in situ occurrence point<br/>
                                      <img src='https://www.freeiconspng.com/uploads/triangle-png-28.png'
                                     style='width:20px;height:20px;'> Wild provenance source of ex situ living specimen(s)",
          position = "bottomleft") %>%
    
        ## Set view (long and lat) and zoom level, for when map initially opens
          setView(-99, 19, zoom = 4)
                                                            
        # view map
        coverage_map
                                                            
        # save map
        htmlwidgets::saveWidget(coverage_map,
              file = file.path(main_dir,"outputs","exsitu_coverage",
              paste0(target_sp[sp],"-exsitu_coverage_interactive_map.html")))
                                                            
  }
  
  ## write summary table
  summary_tbl
  write.csv(summary_tbl, file.path(main_dir,"outputs","exsitu_coverage",
                                   paste0("ExSitu_Coverage_Table_", Sys.Date(), ".csv")),row.names = F)  
  
  ## Summary Table
  
  ## Add text results to summary table
  summary_add <- data.frame(
    species = gsub("_"," ",target_sp[sp]),
    geo_sm = geo_coverage_sm,
    geo_md = geo_coverage_md,
    geo_lg = geo_coverage_lg,
    eco_sm = eco_coverage_sm,
    eco_md = eco_coverage_md,
    eco_lg = eco_coverage_lg,
    eco_usl4_sm = ecol4_coverage_sm,
    eco_usl4_md = ecol4_coverage_md,
    eco_usl4_lg = ecol4_coverage_lg,
    EOO = round(hull_area,2),
    #dist_filter = dist_filter_val,
    stringsAsFactors=F)
  
  summary_tbl[sp,] <- summary_add
  
  
}  


## write summary table
summary_tbl
write.csv(summary_tbl, file.path(main_dir,"outputs","exsitu_coverage",
                                 paste0("ExSitu_Coverage_Table_", Sys.Date(), ".csv")),row.names = F)