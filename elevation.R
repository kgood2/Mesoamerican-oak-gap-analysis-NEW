# script to take taxon_edited_points_removed file per species
# and all a column for elevation. Can use this information for 
# point editing. 

################################################################################
# Set working directory
################################################################################

#Apple
main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

#Windows
#main_dir <- "G:/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

# set up file structure within your main working directory
data <- "occurrence_data"
standard <- "standardized_occurrence_data"

install.packages(c("raster", "rgdal", "elevatr"))
library(raster)
library(rgdal)
library(elevatr)


#read in edited points removed file for species of interest
insignis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points_removed",
                                         "Quercus_insignis_points_removed.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

# create an sf object of your coordinates
sf_points <- st_as_sf(insignis, 
                      coords = c("decimalLongitude","decimalLatitude"),
                      crs = 4326)

# add elevation column to the points
#   this can take a couple minutes or longer if you have many points; if it 
#   takes too long, you can move this step to script #5, inside the 
#   taxon-by-taxon loop
add_elev <- get_elev_point(locations = sf_points,
                           proj = "+proj=longlat +datum=WGS84",
                           src = "aws")

# add elevation column to our main data frame
insignis$elevationInMeters <- add_elev$elevation

# save file 
write.csv(insignis, file.path(main_dir,data, standard, "elevation",
                                             paste0("Quercus_insignis_elevation", ".csv")),row.names = F)





