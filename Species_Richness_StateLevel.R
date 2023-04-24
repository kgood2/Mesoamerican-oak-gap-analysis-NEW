## Working on script to map species richness by State


################################################################################
#load libraries and main directory 

library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)

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
#no_sdm <- c("Quercus centenaria","Quercus mexiae")
# select accepted taxa and remove one that has no occurrence points
target_taxa <- taxon_list %>%
  dplyr::filter(taxon_name_status == "Accepted"
                # optionally, remove species with no SDM (list created manually above)
                #& !(taxon_name_acc %in% no_sdm)
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

all_spp_combined3 <- all_spp_combined2[all_spp_combined2$IUCN_Category %in% c("EN","CR","VU"),]

spp.now <- all_spp_combined3

#change latitude and longitude to numeric values
spp.now$decimalLatitude <- as.numeric(spp.now$decimalLatitude)
spp.now$decimalLongitude <- as.numeric(spp.now$decimalLongitude)


# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state
                     
###############################################################################
# Mexico
################################################################################
#read in country shapefile with states
Mexico_states <- rnaturalearth::ne_states(country="Mexico") %>%
              sf::st_as_sf()                     

                     
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

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(Mexico_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,4,7,10,Inf)
label <- c("0","1-3","4-6","7-9","10+")
binpal <- colorBin(c("#e5e5e5","#b2d8b2","#66b266","#008000","#004c00"), new$species_richness, bins = bins, na.color = "white")

# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0



#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = Mexico_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2","#66b266","#008000","#004c00"),
            title = "Species richness",
            labels = c("0","1-3","4-6","7-9","10+"),
            position = "bottomright")

###############################################################################
# Guatemala 
###############################################################################

#read in country shapefile with states

Guatemala_states <- rnaturalearth::ne_states(country="Guatemala") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, Guatemala_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(Guatemala_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2,3,4,5)
label <- c("0","1","2","3","4")
binpal <- colorBin(c("#e5e5e5","#b2d8b2","#66b266","#008000","#004c00"), new$species_richness, bins = bins, na.color = "white")

# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0



#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = Guatemala_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2","#66b266","#008000","#004c00"),
            title = "Species richness",
            labels = c("0","1","2","3","4"),
            position = "bottomright")

###############################################################################
# Honduras 
###############################################################################

#read in country shapefile with states

Honduras_states <- rnaturalearth::ne_states(country="Honduras") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, Honduras_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

#join this new dataframe with Mexico_states file 
new <- st_join(Honduras_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2,3)
label <- c("0","1","2")
binpal <- colorBin(c("#e5e5e5","#66b266","#004c00"), new$species_richness, bins = bins, na.color = "white")


# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0

#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = Honduras_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#66b266","#004c00"),
            title = "Species richness",
            labels = c("0","1","2"),
            position = "bottomright")

###############################################################################
# Costa Rica
###############################################################################

#read in country shapefile with states

CR_states <- rnaturalearth::ne_states(country="Costa Rica") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, CR_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(CR_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2,3,4)
label <- c("0","1","2","3")
binpal <- colorBin(c("#e5e5e5","#b2d8b2","#66b266","#004c00"), new$species_richness, bins = bins, na.color = "white")


# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0

#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = CR_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2","#66b266","#004c00"),
            title = "Species richness",
            labels = c("0","1","2","3"),
            position = "bottomright")

###############################################################################
#El Salvador
###############################################################################

#read in country shapefile with states

ES_states <- rnaturalearth::ne_states(country="El Salvador") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, ES_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(ES_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2,3)
label <- c("0","1","2")
binpal <- colorBin(c("#e5e5e5","#b2d8b2","#66b266"), new$species_richness, bins = bins, na.color = "white")


# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0

#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = ES_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2","#66b266"),
            title = "Species richness",
            labels = c("0","1","2"),
            position = "bottomright")

###############################################################################
# Nicaragua 
###############################################################################

#read in country shapefile with states

Nicaragua_states <- rnaturalearth::ne_states(country="Nicaragua") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, Nicaragua_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(Nicaragua_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2)
label <- c("0","1")
binpal <- colorBin(c("#e5e5e5","#b2d8b2"), new$species_richness, bins = bins, na.color = "white")


# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0

#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = Nicaragua_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2"),
            title = "Species richness",
            labels = c("0","1"),
            position = "bottomright")

###############################################################################
#Panama
###############################################################################

#read in country shapefile with states

Panama_states <- rnaturalearth::ne_states(country="Panama") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, Panama_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(Panama_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2,3,4)
label <- c("0","1","2","3")
binpal <- colorBin(c("#e5e5e5","#b2d8b2","#66b266","#004c00"), new$species_richness, bins = bins, na.color = "white")


# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0

#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = Panama_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2","#66b266","#004c00"),
            title = "Species richness",
            labels = c("0","1","2","3"),
            position = "bottomright")

###############################################################################
# Belize
###############################################################################
#read in country shapefile with states

Belize_states <- rnaturalearth::ne_states(country="Belize") %>%
  sf::st_as_sf()

# The script below creates a table grouped by state name and species name. You can use 
# the table to determine the number of unique species per state

points_sf <- st_as_sf(spp.now, coords = c("decimalLongitude","decimalLatitude"),crs = 4326)

points_new <- st_join(points_sf, Belize_states)


points_count <- points_new %>%
  group_by(name,taxon_name_acc) %>%
  count()


points_count


# create a new dataframe that counts the number of unique species per state
df_new <- points_count %>%
  group_by(name) %>%
  summarize(species_richness = n_distinct(taxon_name_acc))

df_new

#join this new dataframe with Mexico_states file 
new <- st_join(Belize_states,df_new)

# create bins for color-coding species richness
bins <- c(0,1,2,3,4)
label <- c("0","1","2")
binpal <- colorBin(c("#e5e5e5","#b2d8b2","#66b266"), new$species_richness, bins = bins, na.color = "white")


# Replace any NA values with 0
new$species_richness[is.na(new$species_richness)] <- 0

#Map it
#Need to figure out why gray states (Sonora etc. are coming up as NA) Looks like there
# may be zero and those are being replaced by NA
leaflet(data = new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~binpal(species_richness), stroke = FALSE, fillOpacity = 1) %>%
  addPolygons(data = Belize_states,
              fillOpacity = 0, color = "black", weight = 2) %>%
  addLabelOnlyMarkers(lng = ~longitude, lat = ~latitude, label = ~paste(species_richness),
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = "10px", style = list("font-weight" = "bold"))) %>%
  addLegend(values = ~species_richness,
            colors = c("#e5e5e5","#b2d8b2","#66b266"),
            title = "Species richness",
            labels = c("0","1","2"),
            position = "bottomright")
