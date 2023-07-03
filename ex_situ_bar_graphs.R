##################################################################
# Description 
#################################################################
# script to create stacked bar graphs in individual species profiles
# graphs show number and origin of species in ex situ collections. 
# Provenance Types: W = wild, Z  = indirect wild, H = horticultural, 
# U = unknown 

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
# Read in files
################################################################################

ex_situ <- read.csv(file.path(main_dir,"occurrence_data","georeferencing","ExSitu_Compiled_Post-Geolocation_2023-05-18.csv"),
                    header = T, colClasses="character")

################################################################################
# create bar  graphs
# https://r-charts.com/part-whole/stacked-bar-chart-ggplot2/
################################################################################

# create new dataframe with only necessary columns
ex_situ2 <- ex_situ[,c(3,5,6,10)]

#select target species. Edit this line for each species you are interested in 
ex_situ3 <- ex_situ2[which(ex_situ2$taxon_name_accepted == "Quercus hintoniorum"),]

# rename all "not given" provenance types as "unknown"
ex_situ3["prov_type"][ex_situ3["prov_type"]=="NG"] <- "U"

# rename all "Given" gps_det as "G"
ex_situ3["gps_det"][ex_situ3["gps_det"]=="Given"] <- "G"

# rename all "S" gps_det as "X"
ex_situ3["gps_det"][ex_situ3["gps_det"]=="S"] <- "X"

# combine W and Z into one category 
ex_situ3["prov_type"][ex_situ3["prov_type"]=="W"] <- "W/Z"
ex_situ3["prov_type"][ex_situ3["prov_type"]=="Z"] <- "W/Z"

# for provenance types that are H and U, change gps_det to NA
ex_situ3$gps_det[ex_situ3$prov_type == "H"] <- "NA"
ex_situ3$gps_det[ex_situ3$prov_type == "U"] <- "NA"

#change num_indiv to numeric
ex_situ3$num_indiv <-as.numeric(ex_situ3$num_indiv)



# final rename of all gps_det
ex_situ3$gps_det[ex_situ3$gps_det == "G"] <- "Coordinates provided"
ex_situ3$gps_det[ex_situ3$gps_det == "L"] <- "Geolocated with locality notes"
ex_situ3$gps_det[ex_situ3$gps_det == "X"] <- "Location data unknown"



# graph it
graph <- ggplot(ex_situ3, aes(x=prov_type, y=num_indiv)) +
  geom_col(aes(fill = gps_det)) +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  scale_y_continuous(breaks = seq(0, 15, 2)) +
  scale_fill_manual(values=c("yellowgreen","skyblue2","lightsalmon","gray"))
graph
graph.labs <- graph + labs(x = "Provenance type", y = "Number of plants")
graph.labs  