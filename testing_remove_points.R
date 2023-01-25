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

  new_Quercus_aerea = subset(Quercus_aerea,Quercus_aerea$UID!="id00015578")

  write.csv(new_Quercus_aerea, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_aerea_points_removed", Sys.Date(), ".csv")),row.names = F)

#4
  Quercus_ajoensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_ajoensis.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

  new_Quercus_ajoensis = subset(Quercus_ajoensis,Quercus_ajoensis$UID!="id00015578")

  write.csv(new_Quercus_ajoensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ajoensis_points_removed", Sys.Date(), ".csv")),row.names = F)

#5
  Quercus_alpenscens<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_alpenscensa.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_alpenscens = subset(Quercus_alpenscens,Quercus_alpenscens$UID!="id00015578")

  write.csv(new_Quercus_alpenscens, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_alpenscens_points_removed", Sys.Date(), ".csv")),row.names = F)

#6
  Quercus_barrancana<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_barrancana.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_barrancana = subset(Quercus_barrancana,Quercus_barrancana$UID!="id00015578")

  write.csv(new_Quercus_barrancana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_barrancana_points_removed", Sys.Date(), ".csv")),row.names = F)

#7
  Quercus_brandegeei<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_brandegeei.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_brandegeei = subset(Quercus_brandegeei,Quercus_brandegeei$UID!="id00015578")

  write.csv(new_Quercus_brandegeei, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_brandegeei_points_removed", Sys.Date(), ".csv")),row.names = F)

#8
  Quercus_breedloveana<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_breedloveana.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")

  new_Quercus_breedloveana = subset(Quercus_breedloveana,Quercus_breedloveana$UID!="id00015578")

  write.csv(new_Quercus_breedloveana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_breedloveana_points_removed", Sys.Date(), ".csv")),row.names = F)

#9
  Quercus_carmenensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_carmenensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  new_Quercus_carmenensis = subset(Quercus_carmenensis,Quercus_carmenensis$UID!="id00015578")

  write.csv(new_Quercus_carmenensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_carmenensis_points_removed", Sys.Date(), ".csv")),row.names = F)

#10
  Quercus_cedrosensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_cedrosensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  new_Quercus_cedrosensis = subset(Quercus_cedrosensis,Quercus_cedrosensis$UID!="id00015578")

  write.csv(new_Quercus_cedrosensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_cedrosensis_points_removed", Sys.Date(), ".csv")),row.names = F)

#11
  Quercus_coahuilensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_coahuilensis.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

  new_Quercus_coahuilensis = subset(Quercus_coahuilensis,Quercus_coahuilensis$UID!="id00015578")

  write.csv(new_Quercus_coahuilensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_coahuilensis_points_removed", Sys.Date(), ".csv")),row.names = F)

#12
  Quercus_coffeicolor<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_coffeicolor.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

  new_Quercus_coffeicolora = subset(Quercus_coffeicolor,Quercus_coffeicolor$UID!="id00015578")

  write.csv(new_Quercus_coffeicolor, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_coffeicolora_points_removed", Sys.Date(), ".csv")),row.names = F)

#13
  Quercus_costaricensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_costaricensis.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_costaricensis = subset(Quercus_costaricensis,Quercus_costaricensis$UID!="id00015578")

  write.csv(new_Quercus_costaricensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_costaricensis_points_removed", Sys.Date(), ".csv")),row.names = F)
#14
  Quercus_cualensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_cualensis.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")

  new_Quercus_cualensis = subset(Quercus_cualensis,Quercus_cualensis$UID!="id00015578")

  write.csv(new_Quercus_cualensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_cualensis_points_removed", Sys.Date(), ".csv")),row.names = F)
#15
  Quercus_cupreata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                   "Quercus_cupreata.csv"), header = T, na.strings=c("","NA"),
                         colClasses="character")

  new_Quercus_cupreata = subset(Quercus_cupreata,Quercus_cupreata$UID!="id00015578")

  write.csv(new_Quercus_cupreata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_cupreata_points_removed", Sys.Date(), ".csv")),row.names = F)
#16
  Quercus_delgadoana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                     "Quercus_delgadoana.csv"), header = T, na.strings=c("","NA"),
                           colClasses="character")

  new_Quercus_delgadoana = subset(Quercus_delgadoana,Quercus_delgadoana$UID!="id00015578")

  write.csv(new_Quercus_delgadoana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_delgadoana_points_removed", Sys.Date(), ".csv")),row.names = F)
#17
  Quercus_deliquescens<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_deliquescens.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  new_Quercus_deliquescens = subset(Quercus_deliquescens,Quercus_deliquescensa$UID!="id00015578")

  write.csv(new_Quercus_deliquescensa, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_deliquescens_points_removed", Sys.Date(), ".csv")),row.names = F)
#18
  Quercus_devia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_devia.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")

  new_Quercus_devia = subset(Quercus_devia,Quercus_deviaa$UID!="id00015578")

  write.csv(new_Quercus_devia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_devia_points_removed", Sys.Date(), ".csv")),row.names = F)
#19
  Quercus_diversifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_diversifolia.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

  new_Quercus_diversifolia = subset(Quercus_diversifoli,Quercus_diversifoli$UID!="id00015578")

  write.csv(new_Quercus_diversifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_diversifolia_points_removed", Sys.Date(), ".csv")),row.names = F)
#20
  Quercus_dumosa<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_dumosa.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")

  new_Quercus_dumosa = subset(Quercus_dumosa,Quercus_dumosa$UID!="id00015578")

  write.csv(new_Quercus_dumosa, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_dumosa_points_removed", Sys.Date(), ".csv")),row.names = F)

#21
  Quercus_edwardsiae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_edwardsiae.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

  new_Quercus_edwardsiae = subset(Quercus_edwardsiae,Quercus_edwardsiae$UID!="id00015578")

  write.csv(new_Quercus_edwardsiae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("QQuercus_edwardsiae_points_removed", Sys.Date(), ".csv")),row.names = F)

#22
  Quercus_engelmannii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_engelmannii.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_acherdophylla = subset(Quercus_acherdophylla,Quercus_acherdophylla$UID!="id00015578")

  write.csv(new_Quercus_acherdophylla, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_acherdophylla_points_removed", Sys.Date(), ".csv")),row.names = F)
#23
  Quercus_flocculenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_flocculenta.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_flocculenta = subset(Quercus_flocculenta,Quercus_flocculenta$UID!="id00015578")

  write.csv(new_Quercus_flocculenta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_flocculenta_points_removed", Sys.Date(), ".csv")),row.names = F)
#24
  Quercus_furfuracea<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                       "Quercus_furfuracea.csv"), header = T, na.strings=c("","NA"),
                             colClasses="character")

  new_Quercus_furfuracea = subset(Quercus_furfuracea,Quercus_furfuracea$UID!="id00015578")

  write.csv(new_Quercus_furfuracea, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_furfuracea_points_removed", Sys.Date(), ".csv")),row.names = F)
#25
  Quercus_galeanensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_galeanensis.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_galeanensis = subset(Quercus_galeanensis,Quercus_galeanensis$UID!="id00015578")

  write.csv(new_Quercus_galeanensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("QQuercus_galeanensis_points_removed", Sys.Date(), ".csv")),row.names = F)
#26
  Quercus_ghiesbreghtii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_ghiesbreghtii.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")

  new_Quercus_ghiesbreghtii = subset(Quercus_ghiesbreghtii,Quercus_ghiesbreghtii$UID!="id00015578")

  write.csv(new_Quercus_ghiesbreghtii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ghiesbreghtii_points_removed", Sys.Date(), ".csv")),row.names = F)
#27
  Quercus_graciliformis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_graciliformis.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")

  new_Quercus_graciliformis = subset(Quercus_graciliformis,Quercus_graciliformis$UID!="id00015578")

  write.csv(new_Quercus_graciliformis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_graciliformisa_points_removed", Sys.Date(), ".csv")),row.names = F)
#28
  Quercus_gracilior<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                      "Quercus_gracilior.csv"), header = T, na.strings=c("","NA"),
                            colClasses="character")

  new_Quercus_gracilior = subset(Quercus_gracilior,Quercus_gracilior$UID!="id00015578")

  write.csv(new_Quercus_gracilior, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_gracilior_points_removed", Sys.Date(), ".csv")),row.names = F)
#29
  Quercus_grahamii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                     "Quercus_grahamii.csv"), header = T, na.strings=c("","NA"),
                           colClasses="character")

  new_Quercus_grahamii = subset(Quercus_grahamii,Quercus_grahamii$UID!="id00015578")

  write.csv(new_Quercus_grahamii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_grahamii_points_removed", Sys.Date(), ".csv")),row.names = F)
#30
  Quercus_gulielmi-treleasei<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_gulielmi-treleasei.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_gulielmi-treleasei = subset(Quercus_gulielmi-treleaseia,Quercus_gulielmi-treleasei$UID!="id00015578")

  write.csv(new_Quercus_gulielmi-treleasei, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_gulielmi-treleasei_points_removed", Sys.Date(), ".csv")),row.names = F)

#31
  Quercus_hinckleyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hinckleyi.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hinckleyi = subset(Quercus_hinckleyi,Quercus_hinckleyi$UID!="id00015578")

  write.csv(new_Quercus_hinckleyi, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hinckleyi_points_removed", Sys.Date(), ".csv")),row.names = F)
#32
  Quercus_hintonii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintonii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hintonii = subset(Quercus_hintonii,Quercus_hintonii$UID!="id00015578")

  write.csv(new_Quercus_hintonii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hintonii_points_removed", Sys.Date(), ".csv")),row.names = F)
#33
  Quercus_hintoniorum<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintoniorum.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hintoniorum = subset(Quercus_hintoniorum,Quercus_hintoniorum$UID!="id00015578")

  write.csv(new_Quercus_hintoniorum, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hintoniorum_points_removed", Sys.Date(), ".csv")),row.names = F)
#34
  Quercus_hirtifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hirtifolia.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hirtifolia = subset(Quercus_hirtifolia,Quercus_hirtifolia$UID!="id00015578")

  write.csv(new_Quercus_hirtifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hirtifolia_points_removed", Sys.Date(), ".csv")),row.names = F)
#35
  Quercus_ignaciensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_ignaciensis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_ignaciensis = subset(Quercus_ignaciensis,Quercus_ignaciensis$UID!="id00015578")

  write.csv(new_Quercus_ignaciensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ignaciensis_points_removed", Sys.Date(), ".csv")),row.names = F)
#36
  Quercus_insignis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_insignis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_insignis = subset(Quercus_insignis,Quercus_insignis$UID!="id00015578")

  write.csv(new_Quercus_insignis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_insignis_points_removed", Sys.Date(), ".csv")),row.names = F)

#37
  Quercus_macdougallii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_macdougallii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_macdougallii = subset(Quercus_macdougallii,Quercus_macdougallii$UID!="id00015578")

  write.csv(new_Quercus_macdougallii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_macdougallii_points_removed", Sys.Date(), ".csv")),row.names = F)
#38
  Quercus_meavei<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_meavei.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_meavei = subset(Quercus_meavei,Quercus_meavei$UID!="id00015578")

  write.csv(new_Quercus_meavei, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_meavei_points_removed", Sys.Date(), ".csv")),row.names = F)
#39
  Quercus_melissae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_melissae.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_melissae = subset(Quercus_melissae,Quercus_melissae$UID!="id00015578")

  write.csv(new_Quercus_melissae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_melissae_points_removed", Sys.Date(), ".csv")),row.names = F)
#40
  Quercus_miquihuanensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_miquihuanensis.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_miquihuanensis = subset(Quercus_miquihuanensis,Quercus_miquihuanensis$UID!="id00015578")

  write.csv(new_Quercus_miquihuanensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_miquihuanensis_points_removed", Sys.Date(), ".csv")),row.names = F)

#41
  Quercus_mulleri<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                           "Quercus_mulleri.csv"), header = T, na.strings=c("","NA"),
                                 colClasses="character")

  new_Quercus_mulleri = subset(Quercus_mulleri,Quercus_mulleri$UID!="id00015578")

  write.csv(new_Quercus_mulleri, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_mulleri_points_removed", Sys.Date(), ".csv")),row.names = F)
#42
  Quercus_nixoniana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_nixoniana.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_nixoniana = subset(Quercus_nixoniana,Quercus_nixoniana$UID!="id00015578")

  write.csv(new_Quercus_nixoniana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_nixoniana_points_removed", Sys.Date(), ".csv")),row.names = F)
#43
  Quercus_opaca<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_opaca.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_opaca = subset(Quercus_opaca,Quercus_opaca$UID!="id00015578")

  write.csv(Quercus_opaca, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_opaca_points_removed", Sys.Date(), ".csv")),row.names = F)
#45
  Quercus_paxtalensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_paxtalensiss.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_paxtalensis = subset(Quercus_paxtalensis,Quercus_paxtalensis$UID!="id00015578")

  write.csv(new_Quercus_paxtalensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_paxtalensis_points_removed", Sys.Date(), ".csv")),row.names = F)
#46
  Quercus_perpallida<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_perpallida.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_perpallida = subset(Quercus_perpallida,Quercus_perpallida$UID!="id00015578")

  write.csv(new_Quercus_perpallida, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_perpallida_points_removed", Sys.Date(), ".csv")),row.names = F)
#47
  Quercus_porphyrogenita<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_porphyrogenitas.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_porphyrogenita = subset(Quercus_porphyrogenita,Quercus_porphyrogenita$UID!="id00015578")

  write.csv(new_Quercus_porphyrogenita, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_porphyrogenita_points_removed", Sys.Date(), ".csv")),row.names = F)
#48
  Quercus_radiata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_radiata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_radiata = subset(Quercus_radiata,Quercus_radiata$UID!="id00015578")

  write.csv(new_Quercus_radiata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_radiata_points_removed", Sys.Date(), ".csv")),row.names = F)
#49
  Quercus_rekonis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_rekonis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_rekonis = subset(Quercus_rekonis,Quercus_rekonis$UID!="id00015578")

  write.csv(new_Quercus_rekonis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_rekonis_points_removed", Sys.Date(), ".csv")),row.names = F)
#50
  Quercus_robusta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_robusta.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_robusta = subset(Quercus_robusta,Quercus_robusta$UID!="id00015578")

  write.csv(new_Quercus_robusta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_robusta_points_removed", Sys.Date(), ".csv")),row.names = F)

#51
  Quercus_rubramenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_rubramenta.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")

  new_Quercus_rubramenta = subset(Quercus_rubramenta,Quercus_rubramenta$UID!="id00015578")

  write.csv(new_Quercus_rubramenta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_rubramenta_points_removed", Sys.Date(), ".csv")),row.names = F)
#52
  Quercus_runcinatifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_runcinatifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_runcinatifolia = subset(Quercus_runcinatifolia,Quercus_runcinatifolia$UID!="id00015578")

  write.csv(new_Quercus_runcinatifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_runcinatifolia_points_removed", Sys.Date(), ".csv")),row.names = F)
#53
  Quercus_sarahmariae<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_sarahmariaea.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_sarahmariae = subset(Quercus_sarahmariae,Quercus_sarahmariae$UID!="id00015578")

  write.csv(new_Quercus_sarahmariae, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_sarahmariae_points_removed", Sys.Date(), ".csv")),row.names = F)
#54
  Quercus_supranitida<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_supranitida.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_supranitida = subset(Quercus_supranitida,Quercus_supranitida$UID!="id00015578")

  write.csv(new_Quercus_supranitida, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_supranitida_points_removed", Sys.Date(), ".csv")),row.names = F)
#55
  Quercus_tardifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tardifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tardifolia = subset(Quercus_tardifolia,Quercus_tardifolia$UID!="id00015578")

  write.csv(new_Quercus_tardifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tardifolia_points_removed", Sys.Date(), ".csv")),row.names = F)
#56
  Quercus_tinkhamii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tinkhamii.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tinkhamii = subset(Quercus_tinkhamii,Quercus_tinkhamii$UID!="id00015578")

  write.csv(new_Quercus_tinkhamii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tinkhamii_points_removed", Sys.Date(), ".csv")),row.names = F)
#57
  Quercus_tomentella<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tomentella.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tomentella = subset(Quercus_tomentella,Quercus_tomentella$UID!="id00015578")

  write.csv(new_Quercus_tomentella, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tomentella_points_removed", Sys.Date(), ".csv")),row.names = F)
#58
  Quercus_toumeyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toumeyi.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_toumeyi = subset(Quercus_toumeyi,Quercus_toumeyi$UID!="id00015578")

  write.csv(new_Quercus_toumeyi, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_toumeyi_points_removed", Sys.Date(), ".csv")),row.names = F)
#59
  Quercus_toxicodendrifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toxicodendrifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_toxicodendrifolia = subset( Quercus_toxicodendrifolia, Quercus_toxicodendrifolia$UID!="id00015578")

  write.csv(new_Quercus_toxicodendrifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0(" Quercus_toxicodendrifolia_points_removed", Sys.Date(), ".csv")),row.names = F)
#60
  Quercus_trinitatis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_trinitatis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_trinitatis = subset(Quercus_trinitatis,Quercus_trinitatis$UID!="id00015578")

  write.csv(new_Quercus_trinitatis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_trinitatis_points_removed", Sys.Date(), ".csv")),row.names = F)
#61
  Quercus_tuitensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tuitensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tuitensis = subset(Quercus_tuitensis,Quercus_tuitensis$UID!="id00015578")

  write.csv(new_Quercus_tuitensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tuitensis_points_removed", Sys.Date(), ".csv")),row.names = F)
#62
  Quercus_undata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_undata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_undata = subset(Quercus_undata,Quercus_undata$UID!="id00015578")

  write.csv(new_Quercus_undata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_undata_points_removed", Sys.Date(), ".csv")),row.names = F)
#63
  Quercus_verde<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_verde.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_verde = subset(Quercus_verde,Quercus_verde$UID!="id00015578")

  write.csv(new_Quercus_verde, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_verde_points_removed", Sys.Date(), ".csv")),row.names = F)
#64
  Quercus_vicentensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_vicentensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_vicentensis = subset(Quercus_vicentensis,Quercus_vicentensis$UID!="id00015578")

  write.csv(new_Quercus_vicentensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_vicentensis_points_removed", Sys.Date(), ".csv")),row.names = F)
