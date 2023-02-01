### Author: Emily Beckman Bruns, Kate Good
### Date: 08/16/2022
### Funding: Institude of Museum and Library Services
#   IMLS MFA program grant MA-30-18-0273-18 to The Morton Arboretum

### DESCRIPTION:
# Create species richness maps based on geopolitical boundaries

### INPUTS:
# shapefile (SpatialPolygonsDataFrame) from Natural Earth
# Species-List_Mesoamerica_for-R.csv

### OUTPUTS:
# MesoAm-Quercus-Richness_leaflet_map.html
# MesoAm-Threatened-Quercus-Richness_leaflet_map.html
# MesoAm-Endemic-Quercus-Richness_leaflet_map.html


################################################################################
# Load libraries
################################################################################

my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal",
                 "RColorBrewer","tidyverse","rnaturalearth", "rnaturalearthdata", "devtools")

install.packages(my.packages) # turn on to install current versions
devtools::install_github("ropenscilabs/rnaturalearth")
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# edit this based on the path on your computer. Changed all backslash to forward slash. Backslash was returning an error
main_dir <- "G:/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis"


################################################################################
# Read in data
################################################################################

### READ IN WORLD COUNTRY BOUNDARIES POLYGON FILE

# shapefile (SpatialPolygonsDataFrame) from Natural Earth
#		(https://www.naturalearthdata.com), read in using the rnaturalearth function
#		for easy access
country_shp <- ne_countries(type = "countries", scale = "large")
# look at data fields associated with each polygon
head(country_shp@data)

### READ IN SPECIES LIST WITH COUNTRY OCCURRENCE DATA

# read in target species list, which includes countries of occurrence
sp <- read.csv(file.path(main_dir,"Species-List_Mesoamerica_for-R.csv"),
               colClasses="character")
# look at data format
str(sp)

# remove any extra spaces before/after and between country abbreviations
sp$Country <- str_squish(sp$Country)
sp$Country <- gsub(", ",",",sp$Country)
# check it out
unique(sp$Country)


################################################################################
# Calculate species richness
################################################################################

# function to create table of species richness and join to polygon data
richness.poly.countries <- function(df,polygons){
  # see max number of country codes for one species
  count_codes <- sapply(df$Country,function(x) str_count(x, pattern = ","))
  # create an array of separated country codes
  iso_a2 <- str_split_fixed(df$Country, ",", n = (max(count_codes)+1))
  # sum to calculate richness
  richness <- as.data.frame(table(iso_a2))
  #richness <- richness[-1,]
  print(richness)
  # merge polygons with species richness data
  merged <- merge(polygons,richness)
  # make richness zero for unmatched countries
  merged@data$Freq[which(is.na(merged@data$Freq))] <- 0
  # remove unmatched countries from shapefile
  merged <- merged[merged@data$Freq > 0,]
  # return shapefile with species richness added
  return(merged)
}

# country-level richness for ALL species
ctry_richness <- richness.poly.countries(sp,country_shp)

# country-level richness for THREATENED species
sp_th <- sp %>%
  filter(sp$IUCN.Red.List.threat.category == "CR" |
           sp$IUCN.Red.List.threat.category == "EN" |
           sp$IUCN.Red.List.threat.category == "VU")
ctry_richness_th <- richness.poly.countries(sp_th,country_shp)

# country-level richness for ENDEMIC species
sp_en <- sp %>%
  filter(nchar(sp$Country) == 2)
ctry_richness_en <- richness.poly.countries(sp_en,country_shp)


################################################################################
# Create maps
################################################################################

# maping function
map.countries <- function(countries,pal,legend_text,legend_labels,title){
  map <- leaflet() %>%
    # this is the backgound for the map; see more options here:
    #		https://leaflet-extras.github.io/leaflet-providers/preview/
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    #
    #addPolygons(data = country_shp,
    #	color = "grey", weight = 0.6, opacity = 1,
    #	fillColor = "white",fillOpacity = 1) %>%
    # country polygons with colors based on species richness
    addPolygons(data = countries,
                color = "grey", weight = 1, opacity = 1,
                fillColor = ~pal(countries@data$Freq),
                fillOpacity = 1) %>%
    # legend with number of species represented by each color
    addLegend(values = countries@data$Freq,
              pal = pal, opacity = 1,
              title = legend_text,
              labFormat = function(type, cuts, p) {paste0(legend_labels)},
              position = "bottomright") %>%
    # add title
    addControl(title, position = "topright") %>%
    return(map)
}

# to view available color palettes, run the next line:
#display.brewer.all()


### MAP < ALL > SPECIES

# create title text for map
title_text <- "Mesoamerica oak species richness"

# create color bins and labels
# look at distribution of data
hist(ctry_richness@data$Freq,breaks=90,xlim=c(0,200),ylim=c(0,25))
# assign bin breaks and labels
bins <- c(0,1,5,10,15,20,30,40,50,Inf)
labels <- c("0","1-4","5-9","10-14","15-19","20-29","30-39","40-49","50+")
# create color palette
palette_country <- colorBin(palette = "PuRd", bins = bins,
                            domain = ctry_richness@data$Freq, reverse = F, na.color = "white")
# create text for legend
legend <- paste0("Number of native","<br/>","oak species")

# create map
map_richness <- map.countries(ctry_richness,palette_country,legend,labels,title_text)
# view map
map_richness
# optionally, save map
htmlwidgets::saveWidget(map_richness, file.path(main_dir,"Species Richness Maps",
                                                "MesoAm-Quercus-Richness_leaflet_map.html"))


### MAP < THREATENED > SPECIES

hist(ctry_richness_th@data$Freq,breaks=31,xlim=c(0,40),ylim=c(0,25))
## the scale still needs to be optimized!
bins <- c(0,1,2,4,5,6,15,20,30,Inf)
labels <- c("0","1","2-3","4","5","6-14","15-19","20-29","30+")
palette_country <- colorBin(palette = "PuRd", bins = bins,
                            domain = ctry_richness_th@data$Freq, reverse = F, na.color = "white")

legend <- paste0("Number of native,","<br/>","threatened oak species")
map_richness_th <- map.countries(ctry_richness_th,palette_country,legend,labels,title_text)
map_richness_th
htmlwidgets::saveWidget(map_richness, file.path(main_dir,"Species Richness Maps",
                                                "MesoAm-Threatened-Quercus-Richness_leaflet_map.html"))


### MAP < ENDEMIC > SPECIES

hist(ctry_richness_en@data$Freq,breaks=19,xlim=c(0,120),ylim=c(0,15))
## the scale still needs to be optimized!
bins <- c(0,1,2,3,5,7,10,15,50,Inf)
labels <- c("0","1","2","3-4","5-6","7-9","10-14","15-49","50+")
palette_country <- colorBin(palette = "PuRd", bins = bins,
                            domain = ctry_richness_en@data$Freq, reverse = F, na.color = "white")

legend <- paste0("Number of endemic","<br/>","oak species")
map_richness_en <- map.countries(ctry_richness_en,palette_country,legend,labels,title_text)
map_richness_en
htmlwidgets::saveWidget(map_richness, file.path(main_dir,"Species Richness Maps",
                                                "MesoAm-Endemic-Quercus-Richness_leaflet_map.html"))


## STOP HERE ##





################################################################################
#		!! not yet developed !!

# Potential alternate way to map species richness at the state level, based on
#		occurrence points from the IUCN Red List
################################################################################

# First download data from https://www.iucnredlist.org by...
# 1) Creating an account and signing in
# 2) Searching for "Quercus" in the top search bar
# 3) Clicking the "Download" dropdown on the left and selecting
#		 "Range data - Points (CSV)" from the dropdown menu
# 4) Go to your account (click your name at the top) and click "Download"
#			next to the download you just initiated
#	5) Move the downloaded folder to your working directory for this project

# next we would see which state each point is in (use rnaturalearth state-level
#		polygons?) and then summarize data at the species level, then at the state
#   level
