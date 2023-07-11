##################################################################
# Description 
#################################################################
# script to create stacked bar graphs in individual species profiles.
# Graphs show number and origin of species in ex situ collections. 
# Provenance Types: W = wild, Z  = indirect wild, H = horticultural, 
# U = unknown 

## Per discussion with Emily (0621 office hours), when accession's provenance
## type is NG or U but it is geolocated, recode provenance type to W/Z. 

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
ex_situ3 <- ex_situ2[which(ex_situ2$taxon_name_accepted == "Quercus ajoensis"),]

# rename all "not given" provenance types as "unknown"
ex_situ3["prov_type"][ex_situ3["prov_type"]=="NG"] <- "U"

# rename all "Given" gps_det as "G"
ex_situ3["gps_det"][ex_situ3["gps_det"]=="Given"] <- "G"

# rename all "S" gps_det as "X"
ex_situ3["gps_det"][ex_situ3["gps_det"]=="S"] <- "X"

# combine W and Z into one category 
ex_situ3["prov_type"][ex_situ3["prov_type"]=="W"] <- "W/Z"
ex_situ3["prov_type"][ex_situ3["prov_type"]=="Z"] <- "W/Z"

# if provenance type is U but gps_det is G or L, change provenance
# type to W/Z
ex_situ3$prov_type[ex_situ3$prov_type =='U'& ex_situ3$gps_det == 'G'] <-"W/Z"
ex_situ3$prov_type[ex_situ3$prov_type =='U'& ex_situ3$gps_det == 'L'] <-"W/Z"


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
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_fill_manual(values=c("yellowgreen","skyblue2","lightsalmon","gray"))
graph
graph.labs <- graph + labs(x = "Provenance type", y = "Number of plants")
graph.labs  


###############################################################################
# stacked bar graph of number of individuals in ex situ collections 
# categorized by provenance type (Wild, Unknown, Horticultural). When provenance
# type was listed as NA or U, but coordinates were given or it was geolocated, 
# change to W. 
###############################################################################

ex_situ <- read.csv(file.path(main_dir,"occurrence_data","georeferencing","ExSitu_Compiled_Post-Geolocation_2023-05-18.csv"),
                    header = T, colClasses="character")

taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
                                 "target_taxa_with_synonyms.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character")

# filter to only include target taxa. First, rename columns to match in both files
colnames(ex_situ)[colnames(ex_situ) == "taxon_name_accepted"] ="taxon_name_acc"

target_only <- dplyr::semi_join(ex_situ, taxon_list, by = "taxon_name_acc")

#change num_indiv to numeric
target_only$num_indiv <-as.numeric(target_only$num_indiv)

#combine H? and H
target_only["prov_type"][target_only["prov_type"]=="H?"] <- "H"

#combine W and Z
target_only["prov_type"][target_only["prov_type"]=="Z"] <- "W"

#combine U and NG
target_only["prov_type"][target_only["prov_type"]=="U"] <- "NG"

# final rename for key
target_only$prov_type[target_only$prov_type == "H"] <- "Horticultural"
target_only$prov_type[target_only$prov_type == "NG"] <- "Unknown"
target_only$prov_type[target_only$prov_type == "W"] <- "Wild"

# if provenance type is U but gps_det is G or L, change provenance
# type to W
target_only$prov_type[target_only$prov_type =='U'& target_only$gps_det == 'G'] <-"W"
target_only$prov_type[target_only$prov_type =='U'& target_only$gps_det == 'L'] <-"W"

# Sort data frame in descending order based on number of individuals 
target_only <- target_only[order(-target_only$num_indiv), ]


# split data into two groups to make different graphs (big = >75, small = <75)
species_sum <- aggregate(num_indiv ~ taxon_name_acc, target_only, sum)
big <- subset(species_sum, num_indiv >=75)
size <- "big"
big$size <- size
big

small <- subset(species_sum, num_indiv <= 74)
size <- "small"
small$size <- size
small

#join dataframes together and add back into target_only
combined <- rbind(big,small)
target_only2 <- merge(target_only,combined,by = "taxon_name_acc", all = TRUE)

# graph "big"
big_only <- subset(target_only2, size == "big")
graph_big <- ggplot(big_only, aes(x = taxon_name_acc, y=num_indiv.x)) +
  geom_col(aes(fill = prov_type)) +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
graph_big
graph.labs_big <- graph_big + labs(x = "Target mesoamerican taxa", y = "Number of plants")
graph.labs_big  


# graph "small"
small_only <- subset(target_only2, size == "small")
graph_small <- ggplot(small_only, aes(x = taxon_name_acc, y=num_indiv.x)) +
  geom_col(aes(fill = prov_type)) +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
graph_small
graph.labs_small <- graph_small + labs(x = "Target mesoamerican taxa", y = "Number of plants")
graph.labs_small  

###############################################################################
# bar graph of number of ex situ institutions stacked by region
###############################################################################

ex_situ <- read.csv(file.path(main_dir,"occurrence_data","georeferencing","ExSitu_Compiled_Post-Geolocation_2023-05-18.csv"),
                    header = T, colClasses="character")

taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
                                 "target_taxa_with_synonyms.csv"), 
                       header = T, na.strings=c("","NA"),colClasses="character")

# filter to only include target taxa. First, rename columns to match in both files
colnames(ex_situ)[colnames(ex_situ) == "taxon_name_accepted"] ="taxon_name_acc"

target_only <- dplyr::semi_join(ex_situ, taxon_list, by = "taxon_name_acc")



# rename countries to the region they are found 

unique(target_only$inst_country)

target_only$inst_country <-mgsub(target_only$inst_country,
                                 c("United States", "South Korea", "Spain", "Mexico", "France",
                                   "Israel", "Australia", "Poland", "England", "Wales", 
                                   "New Zealand", "Argentina", "NA", "Switzerland","Netherlands",
                                   "Belgium","Germany"),
                                 c("South America","Oceania"))