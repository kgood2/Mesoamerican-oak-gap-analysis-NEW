# K. Good
# 3 March 2023

###############################################################################
# This script is used to complete the ex situ table in each species profile. 
# it allows you to calculate 
        # 1) Number of ex situ collections reporting this species
        # 2)  Number of plants in ex situ collections
        # 3) Average number of plants per institution
        # 4) Percent of ex situ plants of wild origin
        # 5) Percent of wild origin plants with known locality
###############################################################################

main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points/occurrence_data/georeferencing"

# read in ex situ file
ex_situ <- read.csv(file.path(main_dir,"ExSitu_Compiled_Post-Geolocation_2023-01-19.csv"), 
                    header = T, na.strings=c("","NA"),colClasses="character")

# identify and count number of ex situ institutions with species 
# update script with species name you are interested in 
new <- ex_situ[which(ex_situ$taxon_name_accepted == "Quercus acherdophylla"),]
unique(new[c("inst_short")])
nrow(unique(new[c("inst_short")]))

# institution countries holding species. Re-names countries based on region and then 
# count frequency of regions represented in collections
#new$inst_country <-mgsub(new$inst_country,
                  #c("United States","Spain","Honduras","Mexico","England","Belgium","France",
                   #"Germany","Sweden","Australia","Canada","Scotland","NA","Argentina",
                    #"Wales","Norway","Czech Republic","Netherlands","Finland","Denmark",
                    #"Switzerland","South Korea","New Zealand","Israel","Poland","Georgia","Estonia"),
                  #c("United States and Canada","Europe","Mesoamerica","Mesoamerica","Europe","Europe","Europe",
                   #"Europe","Europe","Oceania","United States and Canada","Europe","NA","South America",
                    #"Europe","Europe","Europe","Europe","Europe","Europe",
                    #"Europe","Asia","Oceania","Asia","Europe","Europe","Europe"))
Country <- unique(new[c("inst_short","inst_country")])
Country %>%
  count(inst_country, name = "Country_Frequency")

# count number of individuals
new$num_indiv <- as.numeric(new$num_indiv)
sum(new$num_indiv)


#average number of individuals per institution 
avg <-aggregate(new$num_indiv,list(new$inst_short),sum)
avg <- aggregate(num_indiv ~ inst_short, data = new, sum)
avg
mean(avg$num_indiv)

#if prov_type is NG or U but gps_det is G or L, change prov_type to W
new$prov_type <- ifelse((new$prov_type == "NG" | new$prov_type == "U") & (new$gps_det == "G" | new$gps_det == "L"), "W", new$prov_type)


# number of plants of wild origin 
wild <-aggregate(new$num_indiv,list(new$prov_type),sum)
wild <- aggregate(num_indiv ~ prov_type, data = new, sum)
wild


# percent of wild origin plants with known locality
# First, sort so only including prov type = W or Z
locality <- new[which(new$prov_type == "W" | new$prov_type == "Z"),]

#number of wild origin plants recorded as G
G_only <- locality[which(locality$gps_det == "G"),]
sum(G_only$num_indiv)

#number of wild origin plants recorded as L
L_only <- locality[which(locality$gps_det == "L"),]
sum(L_only$num_indiv)

#number of wild origin plants recorded as X
X_only <- locality[which(locality$gps_det == "X"),]
sum(X_only$num_indiv)



