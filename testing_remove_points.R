################################################################################
# Set working directory
################################################################################


main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

# set up file structure within your main working directory
data <- "occurrence_data"
standard <- "standardized_occurrence_data"


###############################################################################
# Read in species files and remove points based on UID
###############################################################################

Quercus_acherdophylla <- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                            "Quercus_acherdophylla.csv"), header = T, na.strings=c("","NA"),
                                  colClasses="character")
new_Quercus_acherdophylla = subset(Quercus_acherdophylla,Quercus_acherdophylla$UID!="id00015578" & UID!="id00009468")
write.csv(new_Quercus_acherdophylla, file.path(main_dir,data,
                                 paste0("testing_Quercus_acherdophylla", Sys.Date(), ".csv")),row.names = F)

