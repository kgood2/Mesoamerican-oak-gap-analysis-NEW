## Working on script to map species richness by State


################################################################################
#load libraries and main directory 

library(sf)
library(dplyr)
library(leaflet)

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

## for polygon data (ecoregions, states, countries)
poly_dir <- file.path(main_dir,"gis_data")

## select target taxa
taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
                                 "target_taxa_with_IUCN.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character")

# add country distribution data
taxon_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
                                 "target_taxa_with_native_dist.csv"), header = T, na.strings=c("","NA"),
                       colClasses="character")
taxon_list <- left_join(taxon_list,taxon_dist)

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
# only CR, EN, VU species
all_spp_combined2 <- merge(all_spp_combined,taxon_list,by="taxon_name_acc")

all_spp_combined3 <- all_spp_combined2[all_spp_combined2$IUCN_Category %in% c("EN","CR"),]


#read in country shapefile with states
Mexico_states <- rnaturalearth::ne_states(country="Mexico") %>%
  sf::st_as_sf()

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
                       addPolygons(data = Mexico_states,
                                   fillOpacity = 0, color = "black", weight = 2)
                     
                     
                     final_map                     
                     

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state
                     
points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, Mexico_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count

# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

#join this new dataframe with Mexico_states file 
new <- st_join(Mexico_states,df_new)

#create color palette based on species richness
palette <-colorNumeric(palette = "OrRd", domain = new$species_richness)
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~palette(species_richness), stroke = FALSE, fillOpacity = 0.5) %>%
  addPolygons(data = Mexico_states,
              fillOpacity = 0, color = "black", weight = 2)



