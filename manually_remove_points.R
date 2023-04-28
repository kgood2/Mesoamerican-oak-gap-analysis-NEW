###############################################################################
# January 25, 2023
# udated with new UIDs removed March 30, 2023
# Kate Good

# This script goes through each species and removes UIDs from the the species'
# taxon edited points file. It creates a new file for each species with points
# removed. I manually enter the UIDs to remove for each species from the 
# Manual Point Edits file. I get these points from the Interactive Maps 
# generated in script 6-visuaize_occurrence_data. If there are no points that 
# need to be removed, I read in the file and then write the file, renaming it 
# with the "points removed" file name and putting it in the "taxon_edited_points_removed"
# folder



################################################################################
# Set working directory
################################################################################

#Apple
#main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

#Windows
main_dir <- "G:/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

# set up file structure within your main working directory
data <- "occurrence_data"
standard <- "standardized_occurrence_data"

# create new folder for edited points removed, if not already existing
if(!dir.exists(file.path(main_dir,data,standard,"taxon_edited_points_removed")))
  dir.create(file.path(main_dir,data,standard,"taxon_edited_points_removed"), 
             recursive=T)


###############################################################################
# Read in species files and remove points based on UID
# Multiple pound signs before number indicate a species where no points were removed
###############################################################################

#1
  Quercus_acherdophylla <- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                            "Quercus_acherdophylla.csv"), header = T, na.strings=c("","NA"),
                                  colClasses="character")

  new_Quercus_acherdophylla = subset(Quercus_acherdophylla,!(UID %in% "id00038888"))

  write.csv(new_Quercus_acherdophylla, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                 paste0("Quercus_acherdophylla_points_removed", ".csv")),row.names = F)


#2
  Quercus_acutifolia <- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                            "Quercus_acutifolia.csv"), header = T, na.strings=c("","NA"),
                                  colClasses="character")

  new_Quercus_acutifolia = subset(Quercus_acutifolia,!(UID %in% c("id00036392", "id00037699", "id00031914")))

  write.csv(new_Quercus_acutifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_acutifolia_points_removed", ".csv")),row.names = F)

#3
  Quercus_aerea<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                   "Quercus_aerea.csv"), header = T, na.strings=c("","NA"),
                         colClasses="character")

  write.csv(Quercus_aerea, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_aerea_points_removed", ".csv")),row.names = F)

#4
  Quercus_ajoensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_ajoensis.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

  new_Quercus_ajoensis = subset(Quercus_ajoensis,!(UID %in% c("id00076613", "id00076618", "id00034626")))

  write.csv(new_Quercus_ajoensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ajoensis_points_removed", ".csv")),row.names = F)

#5
  Quercus_alpescens<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_alpescens.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  write.csv(Quercus_alpescens, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                            paste0("Quercus_alpescens_points_removed", ".csv")),row.names = F)

#6
  Quercus_barrancana<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_barrancana.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  write.csv(Quercus_barrancana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_barrancana_points_removed", ".csv")),row.names = F)

#7
  Quercus_brandegeei<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_brandegeei.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_brandegeei = subset(Quercus_brandegeei,!(UID %in% c("id00099165", "id00036240", "id00036757", 
                                                                  "id00037145")))

  write.csv(new_Quercus_brandegeei, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_brandegeei_points_removed", ".csv")),row.names = F)

#8
  Quercus_breedloveana<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_breedloveana.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")

  write.csv(Quercus_breedloveana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_breedloveana_points_removed", ".csv")),row.names = F)

#9
  Quercus_carmenensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_carmenensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  write.csv(Quercus_carmenensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_carmenensis_points_removed", ".csv")),row.names = F)

#10
  Quercus_cedrosensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_cedrosensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  new_Quercus_cedrosensis = subset(Quercus_cedrosensis,!(UID %in% c("id00000337", "id00025050", "id00156319")))

  write.csv(new_Quercus_cedrosensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_cedrosensis_points_removed", ".csv")),row.names = F)

#11
  Quercus_centenaria<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                           "Quercus_centenaria.csv"), header = T, na.strings=c("","NA"),
                                 colClasses="character")
  
  write.csv(Quercus_centenaria, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_centenaria_points_removed", ".csv")),row.names = F)
  
  
#12
  Quercus_coahuilensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_coahuilensis.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

  new_Quercus_coahuilensis = subset(Quercus_coahuilensis,!(UID %in% "id00000339"))

  write.csv(new_Quercus_coahuilensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_coahuilensis_points_removed", ".csv")),row.names = F)

#13
  Quercus_coffeicolor<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_coffeicolor.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

  new_Quercus_coffeicolor = subset(Quercus_coffeicolor,!(UID %in% "id00000340"))

  write.csv(new_Quercus_coffeicolor, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_coffeicolor_points_removed", ".csv")),row.names = F)

#14
  Quercus_costaricensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_costaricensis.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_costaricensis = subset(Quercus_costaricensis,!(UID %in% c("id00038190", "id00035189", "id00000507",
                                                                        "id00032049", "id00034133", "id00042438", 
                                                                        "id00000507")))

  write.csv(new_Quercus_costaricensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_costaricensis_points_removed", ".csv")),row.names = F)
#15
  Quercus_cualensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_cualensis.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")

  new_Quercus_cualensis = subset(Quercus_cualensis,!(UID %in% c("id00038898", "id00000615")))

  write.csv(new_Quercus_cualensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_cualensis_points_removed", ".csv")),row.names = F)
#16
  Quercus_cupreata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                   "Quercus_cupreata.csv"), header = T, na.strings=c("","NA"),
                         colClasses="character")
  
  new_Quercus_cupreata = subset(Quercus_cupreata,!(UID %in% "id00243782"))

  write.csv(new_Quercus_cupreata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_cupreata_points_removed", ".csv")),row.names = F)
#17
  Quercus_delgadoana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                     "Quercus_delgadoana.csv"), header = T, na.strings=c("","NA"),
                           colClasses="character")

  new_Quercus_delgadoana = subset(Quercus_delgadoana,!(UID %in% c("id00248672", "id00248676", "id00024434",
                                                                  "id00024890", "id00221007", "id00248678",
                                                                  "id00248682", "id00000616","id00000616")))

  write.csv(new_Quercus_delgadoana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_delgadoana_points_removed", ".csv")),row.names = F)
#18
  Quercus_deliquescens<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_deliquescens.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  write.csv(Quercus_deliquescens, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_deliquescens_points_removed", ".csv")),row.names = F)
#19
  Quercus_devia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_devia.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")

  new_Quercus_devia = subset(Quercus_devia,!(UID %in% c("id00243795", "id00000620", "id00096198", "id00035588")))
  

  write.csv(new_Quercus_devia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_devia_points_removed", ".csv")),row.names = F)
#20
  Quercus_diversifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_diversifolia.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  new_Quercus_diversifolia = subset(Quercus_diversifolia,!(UID %in% c("id00118182", "id00032051")))
  
  write.csv(new_Quercus_diversifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_diversifolia_points_removed", ".csv")),row.names = F)
#21
  Quercus_dumosa<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_dumosa.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")

  new_Quercus_dumosa = subset(Quercus_dumosa,!(UID %in% c("id00057558", "id00039667", "id00037621")))

  write.csv(new_Quercus_dumosa, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_dumosa_points_removed", ".csv")),row.names = F)

#22
  Quercus_edwardsiae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_edwardsiae.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

  write.csv(Quercus_edwardsiae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_edwardsiae_points_removed", ".csv")),row.names = F)

#23
  Quercus_engelmannii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_engelmannii.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")
  
  new_Quercus_engelmannii = subset(Quercus_engelmannii,!(UID %in% c("id00000660", "id00038895")))

  write.csv(new_Quercus_engelmannii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_engelmannii_points_removed", ".csv")),row.names = F)
#24
  Quercus_flocculenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_flocculenta.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_flocculenta = subset(Quercus_flocculenta,!(UID %in% "id00000698"))

  write.csv(new_Quercus_flocculenta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_flocculenta_points_removed", ".csv")),row.names = F)
#25
  Quercus_furfuracea<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_furfuracea.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

  new_Quercus_furfuracea = subset(Quercus_furfuracea,!(UID %in% c("id00036395", "id00000700")))

  write.csv(new_Quercus_furfuracea, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_furfuracea_points_removed", ".csv")),row.names = F)
#26
  Quercus_galeanensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_galeanensis.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_galeanensis = subset(Quercus_galeanensis,!(UID %in% "id00038371"))

  write.csv(new_Quercus_galeanensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_galeanensis_points_removed", ".csv")),row.names = F)
#27
  Quercus_ghiesbreghtii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_ghiesbreghtii.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")
  
  new_Quercus_ghiesbreghtii = subset(Quercus_ghiesbreghtii,!(UID %in% "id00000707"))

  write.csv(new_Quercus_ghiesbreghtii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ghiesbreghtii_points_removed", ".csv")),row.names = F)
#28
  Quercus_graciliformis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_graciliformis.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")

  new_Quercus_graciliformis = subset(Quercus_graciliformis,!(UID %in% c("id00038374", "id00024852", "id00024850")))

  write.csv(new_Quercus_graciliformis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_graciliformis_points_removed", ".csv")),row.names = F)
#29
  Quercus_gracilior<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_gracilior.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

  write.csv(Quercus_gracilior, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_gracilior_points_removed", ".csv")),row.names = F)
#30
  Quercus_grahamii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                     "Quercus_grahamii.csv"), header = T, na.strings=c("","NA"),
                           colClasses="character")

  new_Quercus_grahamii = subset(Quercus_grahamii,!(UID %in% c("id00380018", "id00043696")))

  write.csv(new_Quercus_grahamii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_grahamii_points_removed", ".csv")),row.names = F)
#31
  Quercus_gulielmi_treleasei<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_gulielmi-treleasei.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")
 
   new_Quercus_gulielmi_treleasei = subset(Quercus_gulielmi_treleasei,!(UID %in% "id00042439"))

  write.csv(new_Quercus_gulielmi_treleasei, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_gulielmi-treleasei_points_removed", ".csv")),row.names = F)

#32
  Quercus_hinckleyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hinckleyi.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hinckleyi = subset(Quercus_hinckleyi,!(UID %in% "id00038379"))

  write.csv(new_Quercus_hinckleyi, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hinckleyi_points_removed", ".csv")),row.names = F)
#33
  Quercus_hintonii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintonii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hintonii = subset(Quercus_hintonii,!(UID %in% c("id00036129", "id00040578")))

  write.csv(new_Quercus_hintonii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hintonii_points_removed", ".csv")),row.names = F)
#34
  Quercus_hintoniorum<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintoniorum.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hintoniorum = subset(Quercus_hintoniorum,!(UID %in% c("id00000757","id00036813")))

  write.csv(new_Quercus_hintoniorum, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hintoniorum_points_removed", ".csv")),row.names = F)
#35
  Quercus_hirtifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hirtifolia.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hirtifolia = subset(Quercus_hirtifolia,!(UID %in% c("id00250429", "id00000764", "id00041079")))

  write.csv(new_Quercus_hirtifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hirtifolia_points_removed", ".csv")),row.names = F)
#36
  Quercus_ignaciensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_ignaciensis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_ignaciensis = subset(Quercus_ignaciensis,!(UID %in% "id00000765"))
  
  write.csv(new_Quercus_ignaciensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ignaciensis_points_removed", ".csv")),row.names = F)
#37
  Quercus_insignis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_insignis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
  
  new_Quercus_insignis = subset(Quercus_insignis,!(UID %in% c("id00036391", "id00039104", "id00036388", 
                                                              "id00038897", "id00031913", "id00261590", 
                                                              "id00043698", "id00000810")))

  write.csv(new_Quercus_insignis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_insignis_points_removed", ".csv")),row.names = F)

#38
  Quercus_macdougallii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_macdougallii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_macdougallii = subset(Quercus_macdougallii,!(UID %in% c("id00038892", "id00000818")))

  write.csv(new_Quercus_macdougallii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_macdougallii_points_removed", ".csv")),row.names = F)
#39
  Quercus_meavei<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_meavei.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
  
  new_Quercus_meavei = subset(Quercus_meavei,!(UID %in% "id00000820"))

  write.csv(new_Quercus_meavei, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_meavei_points_removed", ".csv")),row.names = F)
#40
  Quercus_melissae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_melissae.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  write.csv(Quercus_melissae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_melissae_points_removed", ".csv")),row.names = F)

#41
  Quercus_mexiae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_mexiae.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")
  
  write.csv(Quercus_mexiae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                        paste0("Quercus_mexiae_points_removed", ".csv")),row.names = F)
  
  
#42
  Quercus_miquihuanensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_miquihuanensis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  write.csv(Quercus_miquihuanensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_miquihuanensis_points_removed", ".csv")),row.names = F)

#43
  Quercus_mulleri<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                           "Quercus_mulleri.csv"), header = T, na.strings=c("","NA"),
                                 colClasses="character")

  new_Quercus_mulleri = subset(Quercus_mulleri,!(UID %in% "id00038889"))

  write.csv(new_Quercus_mulleri, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_mulleri_points_removed", ".csv")),row.names = F)
#44
  Quercus_nixoniana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_nixoniana.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_nixoniana = subset(Quercus_nixoniana,!(UID %in% "id00000826"))

  write.csv(new_Quercus_nixoniana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_nixoniana_points_removed", ".csv")),row.names = F)
#45
  Quercus_opaca<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_opaca.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_opaca, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_opaca_points_removed", ".csv")),row.names = F)
#46
  Quercus_paxtalensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_paxtalensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_paxtalensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_paxtalensis_points_removed", ".csv")),row.names = F)
#47
  Quercus_perpallida<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_perpallida.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_perpallida, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_perpallida_points_removed", ".csv")),row.names = F)
#48
  Quercus_porphyrogenita<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_porphyrogenita.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_porphyrogenita = subset(Quercus_porphyrogenita,!(UID %in% "id00035011"))

  write.csv(new_Quercus_porphyrogenita, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_porphyrogenita_points_removed", ".csv")),row.names = F)
#49
  Quercus_radiata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_radiata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_radiata = subset(Quercus_radiata,!(UID %in% "id00000934"))

  write.csv(new_Quercus_radiata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_radiata_points_removed", ".csv")),row.names = F)
#50
  Quercus_rekonis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_rekonis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_rekonis = subset(Quercus_rekonis,!(UID %in% "id00000946"))

  write.csv(new_Quercus_rekonis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_rekonis_points_removed", ".csv")),row.names = F)
#51
  Quercus_robusta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_robusta.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_robusta = subset(Quercus_robusta,!(UID %in% c("id00039487", "id00041913", "id00042484", 
                                                            "id00038166", "id00038899")))

  write.csv(new_Quercus_robusta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_robusta_points_removed", ".csv")),row.names = F)

#52
  Quercus_rubramenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_rubramenta.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")

  new_Quercus_rubramenta = subset(Quercus_rubramenta,!(UID %in% "id00038922"))

  write.csv(new_Quercus_rubramenta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_rubramenta_points_removed", ".csv")),row.names = F)
#53
  Quercus_runcinatifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_runcinatifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_runcinatifolia = subset(Quercus_runcinatifolia,!(UID %in% c("id00000951", "id00228637", "id00041072")))

  write.csv(new_Quercus_runcinatifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_runcinatifolia_points_removed", ".csv")),row.names = F)
#54
  Quercus_sarahmariae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_sarahmariae.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_sarahmariae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_sarahmariae_points_removed", ".csv")),row.names = F)
#55
  Quercus_supranitida<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_supranitida.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_supranitida, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_supranitida_points_removed", ".csv")),row.names = F)
#56
  Quercus_tardifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tardifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_tardifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tardifolia_points_removed", ".csv")),row.names = F)
#57
  Quercus_tinkhamii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tinkhamii.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tinkhamii = subset(Quercus_tinkhamii,!(UID %in% c("id00223045", "id00043289")))

  write.csv(new_Quercus_tinkhamii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tinkhamii_points_removed", ".csv")),row.names = F)
#58
  Quercus_tomentella<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tomentella.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tomentella = subset(Quercus_tomentella,!(UID %in% c("id00000953", "id00241591", "id00035215", 
                                                                  "id00037400", "id00037376", "id00036593")))

  write.csv(new_Quercus_tomentella, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tomentella_points_removed", ".csv")),row.names = F)
#59
  Quercus_toumeyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toumeyi.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  write.csv(Quercus_toumeyi, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_toumeyi_points_removed", ".csv")),row.names = F)
#60
  Quercus_toxicodendrifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toxicodendrifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_toxicodendrifolia = subset(Quercus_toxicodendrifolia,!(UID %in% c("id00037873", "id00024933")))

  write.csv(new_Quercus_toxicodendrifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_toxicodendrifolia_points_removed", ".csv")),row.names = F)
#61
  Quercus_trinitatis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_trinitatis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_trinitatis = subset(Quercus_trinitatis,!(UID %in% "id00045369"))

  write.csv(new_Quercus_trinitatis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_trinitatis_points_removed", ".csv")),row.names = F)
#62
  Quercus_tuitensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tuitensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tuitensis = subset(Quercus_tuitensis,!(UID %in% c("id00001008", "id00038373", "id00245870")))

  write.csv(new_Quercus_tuitensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tuitensis_points_removed", ".csv")),row.names = F)
#63
  Quercus_undata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_undata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_undata = subset(Quercus_undata,!(UID %in% "id00001010"))

  write.csv(new_Quercus_undata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_undata_points_removed", ".csv")),row.names = F)
#64
  Quercus_verde<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_verde.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_verde = subset(Quercus_verde,!(UID %in% "id00001011"))

  write.csv(new_Quercus_verde, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_verde_points_removed", ".csv")),row.names = F)
#65
  Quercus_vicentensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_vicentensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_vicentensis = subset(Quercus_vicentensis,!(UID %in% "id00001016"))

  write.csv(new_Quercus_vicentensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_vicentensis_points_removed", ".csv")),row.names = F)
  
  #################################################################################################
  # run below script if you want to combine all species files created above
  # into one combined file 
  #################################################################################################
  # read in raw occurrence data
  file_list <- list.files(file.path(main_dir,data,standard,"taxon_edited_points_removed"), pattern = ".csv", 
                          full.names = T)
  file_dfs <- lapply(file_list, read.csv, header = T, na.strings = c("","NA"),
                     colClasses = "character")
  length(file_dfs)
  
  #stack all datasets using bind_rows, which keeps non-matching columns
  #   and fills with NA; 'Reduce' iterates through list and merges with previous.
  # this may take a few minutes if you have lots of data
  combined_taxon_edited_points <- Reduce(bind_rows, file_dfs)
  
  #create new .csv file and save
  write.csv(combined_taxon_edited_points, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Combined_taxon_edited_points", ".csv")),row.names = F)
  
  
    
                                              