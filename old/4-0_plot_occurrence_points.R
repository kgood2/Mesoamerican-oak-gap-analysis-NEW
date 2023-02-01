################################################################################

## 4-0_plot_occurrence_points.R
### Authors: Emily Beckman & Christy Rollinson ### Date: 09/11/2020

### DESCRIPTION:
# Creates interactive (HTML) occurrence point map for each target species,
#   for exploring. Includes toggles that show points flagged in
#   3-1_refine_occurrence_points.R
#   Also creates two fixed basic (PNG) maps for each target species: one with
#   all valid occurrence points (output from 3-0_compile_occurrence_points.R)
#   and another with all flagged points removed (output from
#   3-1_refine_occurrence_points.R)

### DATA IN:
# Occurrence points from 3-1_refine_occurrence_points.R

### DATA OUT:
# spp_interactive_maps folder with HTML map for each target species
#   (e.g., Quercus_lobata_leafet_map.html), which can be downloaded and opened
#   in your browser for exploring
# spp_basic_maps folder with PNG maps for each target species, one with
#   all valid points (e.g., Quercus_lobata_raw.png) and one with unflagged
#   points only (e.g., Quercus_lobata_filtered.png)

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("ggplot2", "maps", "leaflet", "RColorBrewer", "dplyr", "mapview")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
webshot::install_phantomjs()
rm(my.packages)

################################################################################
# Set working directory
################################################################################

main_dir <- "/Volumes/GoogleDrive/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"


################################################################################
################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

# set up file paths
output <- file.path(main_dir, "outputs")
path.pts <- file.path(output, "spp_edited_points")
path.figs <- file.path(output, "spp_interactive_maps")
# either run for all species...
spp.all <- tools::file_path_sans_ext(dir(path.pts, ".csv"))
# ...or select target species only:
#taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
#"target_taxa_with_syn.csv"), header = T, na.strings = c("","NA"),
#colClasses = "character")
#taxon_list$num_latlong_records <- as.numeric(taxon_list$num_latlong_records)
#target_spp <- taxon_list %>% filter(grepl("^MAP",map_flag) &
#num_latlong_records > 2)
#spp.all <- gsub(" ","_",target_spp$species_name_acc)
spp.all
target_spp <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                 "target_taxa_with_native_dist.csv"), header = T, na.strings = c("","NA"),
                       colClasses = "character")
countries <- target_spp$all_native_dist_iso2
load(file.path(main_dir, "inputs", "gis_data", "admin_shapefiles.RData"))

# create folder for maps, if not yet created
if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

# cycle through each species file and create map
for(i in 1:length(spp.all)){
  
  # read in records
  spp.now <- read.csv(file.path(path.pts, paste0(spp.all[i], ".csv")))
  
  target.iso <- unlist(strsplit(countries[i],split="; "))
  target_countries <- adm0.poly[adm0.poly@data$country.iso_a2 %in% target.iso,]
  
  ## palette based on database
  # set database as factor and order appropriately
  spp.now$database <- factor(spp.now$database,
                             levels = c("BIEN","Maricela","PMA","GBIF","US_Herbaria","iDigBio","GT_USCG","Herbario_TEFH_Hondouras","IUCN_RedList","El_Salvador","Base_Quercus","Ex_situ"))
  spp.now <- spp.now %>% arrange(desc(database))
  # create color palette
  # slate gray, pink, light green, orange, bright green, blue, yellow, purple, dusty green, teal, dark blue-purple, brown
  colors <- c("#708090","#ffc0cb","#a8ee90", "#c9a467","#48b06c","#2f68d4","#c1c70c","#9279a6","#7d9e77","#27abb0","#432194","#a5682a")
  database.pal <- colorFactor(palette=colors,
                              levels=c("BIEN","Maricela","PMA","GBIF","US_Herbaria","iDigBio","GT_USCG","Herbario_TEFH_Hondouras","IUCN_RedList","El_Savador","Base_Quercus","Ex_situ"))
  
  # create map
  map <- leaflet() %>%
    # Base layer groups
    #addProviderTiles(providers$CartoDB.PositronNoLabels,
    #  group = "CartoDB.PositronNoLabels") %>%
    addProviderTiles(providers$CartoDB.Positron,
                     group = "CartoDB.Positron") %>%
    addControl(paste0("<b>",spp.all[i]), position = "topright") %>%
    addControl(
      "Toggle the checkboxes below on/off to view flagged points (colored red) in each category.</br>
      If no points turn red when box is checked, there are no points flagged in that category.</br>
      Click each point for more information about the record.",
      position = "topright") %>%
    # Native country outlines
    addPolygons(data = target_countries, fillColor = "transparent",
                weight = 3, opacity = 0.8, color = "#126e52") %>%
    # Color by database
    addCircleMarkers(data = spp.now, ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     color = ~database.pal(database),radius = 5,
                     fillOpacity = 0.9, stroke = T) %>%
    # Overlay groups (can toggle)
    addCircleMarkers(data = spp.now %>% filter(!.cen & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Within 500m of country/state centroid (.cen)") %>%
    addCircleMarkers(data = spp.now %>% filter(database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "In urban area (.urb)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.inst & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Within 100m of biodiversity institution (.inst)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.con & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Not in reported country (.con)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.outl & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Geographic outlier (.outl)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.gtsnative),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Outside GTS native country (.gtsnative)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.rlnative),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Outside IUCN RL native country (.rlnative)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.rlintroduced),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "In IUCN RL introduced country (.rlintroduced)") %>%
    addCircleMarkers(data = spp.now %>%
                       filter(basisOfRecord == "FOSSIL_SPECIMEN" |
                                basisOfRecord == "LIVING_SPECIMEN"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "FOSSIL_SPECIMEN or LIVING_SPECIMEN (basisOfRecord)") %>%
    addCircleMarkers(data = spp.now %>%
                       filter(establishmentMeans == "INTRODUCED" |
                                establishmentMeans == "MANAGED" |
                                establishmentMeans == "INVASIVE"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "INTRODUCED, MANAGED, or INVASIVE (establishmentMeans)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.yr1950 & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Recorded prior to 1950 (.yr1950)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.yr1980 & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Recorded prior to 1980 (.yr1980)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.yrna & database!="Ex_situ"),
                     ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
                       "<b>Source database:</b> ",database,"<br/>",
                       "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
                       "<b>Year:</b> ",year,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>ID:</b> ",UID),
                     radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
                     group = "Year unknown (.yrna)") %>%
    # Layers control
    addLayersControl(
      #baseGroups = c("CartoDB.PositronNoLabels",
      #               "CartoDB.Positron",
      #               "Esri.WorldTopoMap",
      #               "Stamen.Watercolor"),
      overlayGroups = c("Within 500m of country/state centroid (.cen)",
                        "In urban area (.urb)",
                        "Within 100m of biodiversity institution (.inst)",
                        "Not in reported country (.con)",
                        "Geographic outlier (.outl)",
                        "Outside GTS native country (.gtsnative)",
                        "Outside IUCN RL native country (.rlnative)",
                        "In IUCN RL introduced country (.rlintroduced)",
                        "FOSSIL_SPECIMEN or LIVING_SPECIMEN (basisOfRecord)",
                        "INTRODUCED, MANAGED, or INVASIVE (establishmentMeans)",
                        "Recorded prior to 1950 (.yr1950)",
                        "Recorded prior to 1980 (.yr1980)",
                        "Year unknown (.yrna)"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    #hideGroup("Within 500m of country/state centroid (.cen)") %>%
    hideGroup("In urban area (.urb)") %>%
    #hideGroup("Within 100m of biodiversity institution (.inst)") %>%
    #hideGroup("Not in reported country (.con)") %>%
    #hideGroup("Geographic outlier (.outl)") %>%
    #hideGroup("Outside GTS native country (.gtsnative)") %>%
    #hideGroup("Outside IUCN RL native country (.rlnative)") %>%
    hideGroup("In IUCN RL introduced country (.rlintroduced)") %>%
    #hideGroup("FOSSIL_SPECIMEN or LIVING_SPECIMEN (basisOfRecord)") %>%
    #hideGroup("INTRODUCED, MANAGED, or INVASIVE (establishmentMeans)") %>%
    hideGroup("Recorded prior to 1950 (.yr1950)") %>%
    hideGroup("Recorded prior to 1980 (.yr1980)") %>%
    hideGroup("Year unknown (.yrna)") %>%
    addLegend(pal = database.pal, values = unique(spp.now$database),
              title = "Source database", position = "bottomright", opacity = 0.6) %>%
    addControl(
      "See https://github.com/MortonArb-CollectionsValue/OccurrencePoints
      for information about data sources and flagging methodology.",
      position = "bottomleft")
  map
  
  # save map
  htmlwidgets::saveWidget(map, file.path(path.figs,
                                         paste0(spp.all[i], "_leaflet_map.html")))
  
  cat("\tEnding ", spp.all[i], ", ", i, " of ", length(spp.all), ".\n\n", sep="")
}

################################################################################
# Basic fixed maps
################################################################################

path.figs <- file.path(output, "spp_basic_maps")
#spp.all <- tools::file_path_sans_ext(dir(path.pts, ".csv"))

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

map.world <- map_data("world")

native_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                  "target_taxa_with_native_dist.csv"), header = T, na.strings = c("","NA"),
                        colClasses = "character")
native_dist <- native_dist %>% dplyr::select(species_name_acc,rl_native_dist)

for(i in 1:length(spp.all)){
  # get data
  spp.now <- spp.all[i]
  dat.now <- read.csv(file.path(path.pts, paste0(spp.all[i], ".csv")))
  dat.now$decimalLatitude <- as.numeric(dat.now$decimalLatitude)
  dat.now$decimalLongitude <- as.numeric(dat.now$decimalLongitude)
  spp.rl.dist <- native_dist %>% filter(species_name_acc == dat.now$species_name_acc[i])
  summary(dat.now)
  
  if(nrow(dat.now[!is.na(dat.now$decimalLatitude),])==0) next
  
  # map of raw data
  png(file.path(path.figs, paste0(spp.now, "_raw.png")), height=6,
      width=10, units="in", res=180)
  print(
    ggplot() +
      coord_equal() +
      ggtitle(sub("_", " ", spp.now)) +
      geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
      geom_point(data=dat.now, aes(x=decimalLongitude, y=decimalLatitude),
                 color="red", size=2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_minimal()
  )
  dev.off()
  
  # map with all flagged points removed
  
  dat.now2 <- dat.now %>%
    filter(database == "Ex_situ" |
             (.cen & .inst & .con & .outl & #.urb & .yr1950 & .yr1980 & .yrna &
                #(.gtsnative | is.na(.gtsnative)) &
                #(.rlnative  | is.na(.rlnative)) &
                #(.rlintroduced | is.na(.rlintroduced)) &
                basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
                establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
                establishmentMeans != "INVASIVE"))
  if(is.na(spp.rl.dist[1,2])){
    dat.now2 <- dat.now2 %>%
      filter(.gtsnative | is.na(.gtsnative))
  } else {
    dat.now2 <- dat.now2 %>%
      filter(.rlnative | is.na(.rlnative))
  }
  
  png(file.path(path.figs, paste0(spp.now, "_filtered.png")), height=6,
      width=10, units="in", res=180)
  print(
    ggplot() +
      coord_equal() +
      ggtitle(sub("_", " ", spp.now)) +
      geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
      geom_point(data=dat.now2, aes(x=decimalLongitude, y=decimalLatitude),
                 color="green", size=2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_minimal()
  )
  dev.off()
  
  cat("\tEnding ", spp.now, ", ", i, " of ", length(spp.all), ".\n\n", sep="")
}

## SAVE STATIC IMAGE OF LEAFLET MAP ##

# save image of map (png, pdf, or jpeg)
mapview::mapshot(coverage_map, 
                 file = paste0(main_dir, "/outputs”, "/exsitu_coverage/", target_sp[sp], "-exsitu_coverage_static_map1.png"),
                 remove_controls = c("zoomControl","layersControl"))
