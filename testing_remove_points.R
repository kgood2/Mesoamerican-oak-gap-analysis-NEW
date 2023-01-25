################################################################################
# Set working directory
################################################################################


main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

# set up file structure within your main working directory
data <- "occurrence_data"
standard <- "standardized_occurrence_data"

# create new folder for edited points removed, if not already existing
if(!dir.exists(file.path(main_dir,data,standard,"taxon_edited_points_removed")))
  dir.create(file.path(main_dir,data,standard,"taxon_edited_points_removed"), 
             recursive=T)


###############################################################################
# Read in species files and remove points based on UID
###############################################################################

#1
Quercus_acherdophylla <- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                            "Quercus_acherdophylla.csv"), header = T, na.strings=c("","NA"),
                                  colClasses="character")

new_Quercus_acherdophylla = subset(Quercus_acherdophylla,Quercus_acherdophylla$UID!="id00015578")

write.csv(new_Quercus_acherdophylla, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                 paste0("Quercus_acherdophylla_points_removed", Sys.Date(), ".csv")),row.names = F)


#2
Quercus_acutifolia <- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                            "Quercus_acutifolia.csv"), header = T, na.strings=c("","NA"),
                                  colClasses="character")

new_Quercus_acutifolia = subset(Quercus_acutifolia,Quercus_acutifolia$UID!="id00008604" & UID!="id00215352")

write.csv(new_Quercus_acherdophylla, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_acutifolia_points_removed", Sys.Date(), ".csv")),row.names = F)

#3
Quercus_aerea<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                   "Quercus_aerea.csv"), header = T, na.strings=c("","NA"),
                         colClasses="character")

#4
Quercus_ajoensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_ajoensis.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

#5
Quercus_alpenscens<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_alpenscensa.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

#6
Quercus_barrancana<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_barrancana.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

#7
Quercus_brandegeei<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_brandegeei.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

#8
Quercus_breedloveana<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_breedloveana.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")

#9
Quercus_carmenensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_carmenensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

#10
Quercus_cedrosensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_cedrosensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

#11
Quercus_coahuilensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_coahuilensis.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character") 

#12
Quercus_coffeicolor<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_coffeicolor.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")
#13
Quercus_costaricensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_costaricensis.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")
#14
Quercus_cualensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_cualensis.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")
#15
Quercus_cupreata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                   "Quercus_cupreata.csv"), header = T, na.strings=c("","NA"),
                         colClasses="character")
#16
Quercus_delgadoana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                     "Quercus_delgadoana.csv"), header = T, na.strings=c("","NA"),
                           colClasses="character")
#17
Quercus_deliquescens<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_deliquescens.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")
#18
Quercus_devia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_devia.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")
#19
Quercus_diversifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_diversifolia.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")
#20
Quercus_dumosa<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_dumosa.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")

#21
Quercus_edwardsiae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_edwardsiae.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

#22
Quercus_engelmannii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_engelmannii.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")
#23
Quercus_flocculenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_flocculenta.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")
#24
Quercus_furfuracea<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_furfuracea.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")
#25
Quercus_galeanensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_galeanensis.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")
#26
Quercus_ghiesbreghtii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_ghiesbreghtii.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")
#27
Quercus_graciliformis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_graciliformis.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")
#28
Quercus_gracilior<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_gracilior.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")
#29
Quercus_grahamii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                     "Quercus_grahamii.csv"), header = T, na.strings=c("","NA"),
                           colClasses="character")
#30
Quercus_gulielmi-treleasei<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_gulielmi-treleasei.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

#31
Quercus_hinckleyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hinckleyi.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#32
Quercus_hintonii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintonii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#33
Quercus_hintoniorum<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintoniorum.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#34
Quercus_hirtifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hirtifolia.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#35
Quercus_ignaciensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_ignaciensis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#36
Quercus_insignis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_insignis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#37
Quercus_macdougallii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_macdougallii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#38
Quercus_meavei<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_meavei.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#39
Quercus_melissae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_melissae.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")
#40
Quercus_miquihuanensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_miquihuanensis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

#41
Quercus_mulleri<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                           "Quercus_mulleri.csv"), header = T, na.strings=c("","NA"),
                                 colClasses="character")
#42
Quercus_nixoniana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_nixoniana.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#43
Quercus_opaca<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_opaca.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#45
Quercus_paxtalensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_paxtalensiss.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#46
Quercus_perpallida<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_perpallida.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#47
Quercus_porphyrogenita<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_porphyrogenitas.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#48
Quercus_radiata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_radiata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#49
Quercus_rekonis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_rekonis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#50
Quercus_robusta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_robusta.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

#51
Quercus_rubramenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_rubramenta.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")
#52
Quercus_runcinatifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_runcinatifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#53
Quercus_sarahmariae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_sarahmariaea.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#54
Quercus_supranitida<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_supranitida.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#55
Quercus_tardifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tardifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#56
Quercus_tinkhamii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tinkhamii.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#57
Quercus_tomentella<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tomentella.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#58
Quercus_toumeyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toumeyi.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#59
Quercus_toxicodendrifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toxicodendrifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#60
Quercus_trinitatis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_trinitatis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#61
Quercus_tuitensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tuitensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#62
Quercus_undata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_undata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#63
Quercus_verde<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_verde.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
#64
Quercus_vicentensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_vicentensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
