##############################################################################

## 6_2-visualize_occurrence_data_after_point_editing.R

### Authors: Emily Beckman Bruns and Kate Good 
### Funding: Base script was funded by the Institute of Museum and Library 
# Services (IMLS MFA program grant MA-30-18-0273-18 to The Morton Arboretum).
# Moderate edits were added with funding from a cooperative agreement
# between the United States Botanic Garden and San Diego Botanic Garden
# (subcontracted to The Morton Arboretum), with support from
# Botanic Gardens Conservation International U.S.

### Last updated: 3 January 2023
### R version 4.2.2

### DESCRIPTION:
# Creates interactive (HTML) occurrence point map for each target species,
#   for exploring after points have been reviewed and removed. Creates maps
# without key or points highlighted in red that can be shared with expert reviewers. 

### INPUTS:
# target_taxa_with_synonyms.csv
# Edited Occurrence points from remove points.R script

### OUTPUTS:
# interactive_maps/visualize_occurrence_data folder with HTML map for each 
#   target taxa (e.g., Quercus_lobata_occurrence_map.html), which can be 
#   downloaded and opened in your browser for exploring

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("ggplot2","maps","leaflet","RColorBrewer","dplyr","raster",
                 "terra", "mapview")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

webshot::install_phantomjs()

################################################################################
# Set working directory
################################################################################

#Apple
main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

#Windows
#main_dir <- "G:/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"


################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

# set up file paths
output.maps <- file.path(main_dir, "outputs", "interactive_maps","visualize_occurrence_data_edited_maps")
path.gis <- file.path(main_dir,"gis_data")
path.pts <- file.path(main_dir,"occurrence_data",
                      "standardized_occurrence_data","taxon_edited_points_removed")
path.rasters <- file.path(main_dir,"sdm_rasters")

# select target taxa
taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
                                 "target_taxa_with_synonyms.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character")
# add country distribution data
taxon_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                 "target_taxa_with_native_dist.csv"), header = T, na.strings=c("","NA"),
                       colClasses="character")
taxon_list <- left_join(taxon_list,taxon_dist)

### UPDATE THIS AS NEEDED:
## Add species here that have no occurrence points (Q. centenaria and Q. mexiae)
## adding Q. gulielmi-treleasei here as well because was giving an error
#no_sdm <- c("Quercus centenaria","Quercus mexiae", "Quercus gulielmi-treleasei")
# select accepted taxa and remove one that has no occurrence points
target_taxa <- taxon_list %>%
  dplyr::filter(taxon_name_status == "Accepted"
                # optionally, remove species with no SDM (list created manually above)
                #& !(taxon_name_acc %in% no_sdm)
  )
nrow(target_taxa) #65
spp.all <- unique(gsub(" ","_",target_taxa$taxon_name_acc))
spp.all
# list of native countries for each target species
countries <- target_taxa$all_native_dist_iso2
# read in country boundaries shapefile
world_polygons <- vect(file.path(path.gis,
                                 "UIA_World_Countries_Boundaries","World_Countries__Generalized_.shp"))                             

# create folder for output maps, if not yet created
if(!dir.exists(output.maps)) dir.create(output.maps, recursive=T)

### cycle through each species file and create map

# if you just want to make map for one species, find index of species in spp.all (for example,Quercus acutifolia 
# is 2. Add i < - index number below, then run everything inside loop)
i <- 56

for(i in 1:length(spp.all)){
  
  # read in records
  spp.now <- read.csv(file.path(path.pts, paste0(gsub("\\.","",spp.all[i]), "_points_removed", ".csv")))
  
  #target.iso <- unlist(strsplit(countries[i],split="; "))
  #target_countries <- world_polygons[world_polygons$IOS %in% target.iso,]
  
  ## palette based on database
  # set database as factor and order appropriately
  spp.now$database <- factor(spp.now$database,
                             levels = c("NorthAm_herbaria","GBIF","BIEN","CONABIO","CR","Ex_situ","Expert_Comm",
                                        "FIA","iDigBio","IUCN_RedList","PMA",
                                        "TEFH","Tropicos"))
  spp.now <- spp.now %>% arrange(desc(database))
  # create color palette
  # https://color-hex.org/palettes/popular
  colors <- c("#adbb3f","#5fbb9a","#6a9ebd","#7b83cc",
                       "#3c2c7a","green","#FFBF00","#3475cc","#0152bf","#CC8400","#454B1B","#ffc0cb","#e04ce0")
                       database.pal <- colorFactor(palette=colors,
                                                   levels = c("NorthAm_herbaria","GBIF","BIEN","CONABIO","CR","Ex_situ","Expert_Comm",
                                                              "FIA","iDigBio","IUCN_RedList","PMA",
                                                              "TEFH","Tropicos"))
      
                
                       
                       # create map
                       try(final_map <- leaflet() %>%
                             addProviderTiles(providers$CartoDB.Positron,
                                              group = "CartoDB.Positron") %>%
                             addControl(paste0("<b>",spp.all[i]), position = "topright") %>%
                             
        addCircleMarkers(data = spp.now, ~decimalLongitude, ~decimalLatitude,
                         popup = ~paste0(
                           "<b>Accepted species name:</b> ",taxon_name_acc,"<br/>",
                           "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
                           "<b>Source database:</b> ",database,"<br/>",
                           "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                           "<b>Year:</b> ",year,"<br/>",
                           "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                           "<b>Dataset name:</b> ",datasetName,"<br/>",
                           "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                           "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                           "<b>References:</b> ",references,"<br/>",
                           "<b>Native Database ID:</b> ",nativeDatabaseID,"<br/>",
                           "<b>ID:</b> ",UID),
                         color = ~database.pal(database),radius = 4,
                         fillOpacity = 0.9, stroke = T) %>%
        
  
        addLegend(pal = database.pal, values = unique(spp.now$database),
                  #title = "Occurrence point</br>source database", position = "bottomright", opacity = 0.8)) 
                  title = "Puntos de ocurrencia</br>por base de datos", position = "bottomright", opacity = 0.8))
                       final_map
                       
                       #save image of map (png, pdf, or jpeg)
                       mapview::mapshot(final_map, 
                                        file = paste0(main_dir, "/outputs", "/static_maps/", spp.all[i], 
                                                      "-edited_points_static_map.png"),
                                        remove_controls = c("zoomControl", "layersControl"),
                                        zoom=3)
                       # save map
                       try(htmlwidgets::saveWidget(final_map, file.path(output.maps,
                                                                        paste0(spp.all[i], "_occurrence_map.html")))) 
                         
                         
                         
                        
                       
                       cat("\tEnding ", spp.all[i], ", ", i, " of ", length(spp.all), ".\n\n", sep="")
}