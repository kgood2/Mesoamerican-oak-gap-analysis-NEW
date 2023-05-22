# map species based on IUCN list Category 

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("ggplot2","maps","leaflet","RColorBrewer","dplyr","raster",
                 "terra")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################


main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

# set up file paths
output.maps <- file.path(main_dir, "outputs", "interactive_maps","IUCN_Red_List_Category")
path.gis <- file.path(main_dir,"gis_data")
path.pts <- file.path(main_dir,"occurrence_data",
                      "standardized_occurrence_data","taxon_edited_points_removed")
path.rasters <- file.path(main_dir,"sdm_rasters")

# select target taxa
taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
                                 "target_taxa_with_IUCN.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character")

# add country distribution data
taxon_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                 "target_taxa_with_native_dist.csv"), header = T, na.strings=c("","NA"),
                       colClasses="character")
taxon_list <- left_join(taxon_list,taxon_dist)

# create taxa list with only CR and EN species
#taxon_list_new <- taxon_list[taxon_list$IUCN_Category %in% c("EN","CR"),] 

### UPDATE THIS AS NEEDED:
no_sdm <- c("Quercus centenaria","Quercus mexiae")
# select accepted taxa and remove one that has no occurrence points
target_taxa <- taxon_list %>%
  dplyr::filter(taxon_name_status == "Accepted"
                # optionally, remove species with no SDM (list created manually above)
                & !(taxon_name_acc %in% no_sdm)
  )
nrow(target_taxa) #87 // 96
spp.all <- unique(gsub(" ","_",target_taxa$taxon_name_acc))
spp.all




# list of native countries for each target species
countries <- target_taxa$all_native_dist_iso2
# read in country boundaries shapefile
world_polygons <- vect(file.path(path.gis,
                                 "UIA_World_Countries_Boundaries","World_Countries__Generalized_.shp"))                             


### Combine individual edited points files for each species into one file 
# read in raw occurrence data
file_list <- list.files(file.path(main_dir,"occurrence_data",
                                  "standardized_occurrence_data","taxon_edited_points_removed"), pattern = ".csv", 
                        full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings = c("","NA"),
                   colClasses = "character")
length(file_dfs) 

# stack all datasets using bind_rows, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous.
# this may take a few minutes if you have lots of data
all_spp_combined <- Reduce(bind_rows, file_dfs)

# add column for IUCN Red List Category and create new dataframe with 
# only CR and EN species
all_spp_combined2 <- merge(all_spp_combined,taxon_list,by="taxon_name_acc")

all_spp_combined3 <- all_spp_combined2[all_spp_combined2$IUCN_Category %in% c("EN","CR"),]

# read in shapefile of KBAs
KBA <- vect(file.path(main_dir,"gis_data",
                                  "protected_areas","North_Central_America_KBA","North_Central_America_KBA.shp"))
KBA <- sf::st_as_sf(KBA)

### cycle through all species combined file and create map

  
  # read in records
  spp.now <- all_spp_combined3
  
  spp.now$decimalLatitude <- as.numeric(spp.now$decimalLatitude)
  spp.now$decimalLongitude <- as.numeric(spp.now$decimalLongitude)
  
  ## palette based on species 
  # set species name as factor and order appropriately
  spp.now$taxon_name_acc <- factor(spp.now$taxon_name_acc,
                             levels = c("Quercus graciliformis", "Quercus hinckleyi", "Quercus mulleri", 
                                         "Quercus brandegeei", "Quercus carmenensis", "Quercus cualensis", 
                                         "Quercus cupreata", "Quercus delgadoana", "Quercus devia", 
                                         "Quercus diversifolia", "Quercus dumosa", "Quercus engelmannii", 
                                         "Quercus flocculenta", "Quercus galeanensis", "Quercus hintonii", 
                                         "Quercus hirtifolia", "Quercus insignis", "Quercus macdougallii", 
                                         "Quercus miquihuanensis", "Quercus nixoniana", "Quercus radiata", 
                                         "Quercus runcinatifolia", "Quercus tomentella"))
  
spp.now <- spp.now %>% arrange(desc(taxon_name_acc))
  
# create color palette
  # https://color-hex.org/palettes/popular

colors <- c("#adbb3f","#819756","#5fbb9a","#6a9ebd","#7b83cc","#7264de",
                       "#3c2c7a","#e0bfb8","#c4c4c4","#ccdcf2","#3475cc","#0152bf",
                     "#FFA500","#CC8400","#8E5C00", "#E0B566", "#FFC04D",
                     "#FFCCCC","#ff9999","#ff4c4c","#ff0000",
                     "#deedd8","#bedbb1")
                       species.pal <- colorFactor(palette=colors,
                                                   levels = c("Quercus graciliformis", "Quercus hinckleyi", "Quercus mulleri", 
                                                              "Quercus brandegeei", "Quercus carmenensis", "Quercus cualensis", 
                                                              "Quercus cupreata", "Quercus delgadoana", "Quercus devia", 
                                                              "Quercus diversifolia", "Quercus dumosa", "Quercus engelmannii", 
                                                              "Quercus flocculenta", "Quercus galeanensis", "Quercus hintonii", 
                                                              "Quercus hirtifolia", "Quercus insignis", "Quercus macdougallii", 
                                                              "Quercus miquihuanensis", "Quercus nixoniana", "Quercus radiata", 
                                                              "Quercus runcinatifolia", "Quercus tomentella"))
                       
final_map <- leaflet() %>%
                             # Base layer groups
                             #addProviderTiles(providers$CartoDB.PositronNoLabels,
                             #  group = "CartoDB.PositronNoLabels") %>%
                             addProviderTiles(providers$CartoDB.Positron,
                                              group = "CartoDB.Positron") %>%
        addCircleMarkers(data = spp.now, ~decimalLongitude, ~decimalLatitude,
                         popup = ~paste0(
                           "<b>Accepted species name:</b> ",taxon_name_acc,"<br/>",
                           "<b>IUCN category:</b> ",IUCN_Category,"<br/>",
                           "<b>Source database:</b> ",database,"<br/>",
                           "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                           "<b>Year:</b> ",year,"<br/>",
                           "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                           "<b>Dataset name:</b> ",datasetName,"<br/>",
                           "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                           "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                           "<b>ID:</b> ",UID),
                         color = ~species.pal(taxon_name_acc),radius = 4,
                         fillOpacity = 0.9, stroke = T) %>%
        addPolygons(data = KBA,
                     fillOpacity = 0, color = "#969696", weight = 1.2, opacity = 1)

        
                       final_map
                       
                       #save image of map (png, pdf, or jpeg)
                       mapview::mapshot(final_map, 
                                        file = paste0(main_dir, "/outputs", "/static_maps/", 
                                                      "CR_and_EN_occurrence_mapp.png"),
                                        remove_controls = c("zoomControl", "layersControl"),
                                        zoom=6)
                       
                       # save map
                       (htmlwidgets::saveWidget(final_map, file.path(output.maps,
                                                                        paste0("CR_and_EN_occurrence_map.html"))))
                       
