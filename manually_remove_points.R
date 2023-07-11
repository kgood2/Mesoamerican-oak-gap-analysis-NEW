###############################################################################
# January 25, 2023
# udated with new UIDs removed May 22, 2023
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
main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

#Windows
#main_dir <- "G:/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points"

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

  new_Quercus_acherdophylla = subset(Quercus_acherdophylla,!(UID %in% c("id00241594", "id00038888")))

  write.csv(new_Quercus_acherdophylla, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                 paste0("Quercus_acherdophylla_points_removed", ".csv")),row.names = F)


#2
  Quercus_acutifolia <- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                            "Quercus_acutifolia.csv"), header = T, na.strings=c("","NA"),
                                  colClasses="character")

  write.csv(Quercus_acutifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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

  new_Quercus_ajoensis <- subset(Quercus_ajoensis,!(UID %in% c("id00036637", "id00034626", "id00076618",
                                                              "id00000321", "id00000323", "id00076613",
                                                              "id00052052", "id00376582", "id00000329",
                                                              "id00076636", "id00076641", "id00076632",
                                                              "id00040570", "id00076588", "id00000332",
                                                              "id00052055", "id00046035", "id00076568",
                                                              "id00035590", "id00376581", "id00076603",
                                                              "id00076583", "id00238092", "id00376576",
                                                              "id00001669", "id00376574", "id00024561",
                                                              "id00046037", "id00376587", "id00376588",
                                                              "id00376592", "id00376589", "id00376591",
                                                              "id00076600")))

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
  
  new_Quercus_barrancana <- subset(Quercus_barrancana,!(UID %in% c("id00224546", "id00380473")))

  write.csv(new_Quercus_barrancana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_barrancana_points_removed", ".csv")),row.names = F)

#7
  Quercus_brandegeei<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                        "Quercus_brandegeei.csv"), header = T, na.strings=c("","NA"),
                              colClasses="character")

  new_Quercus_brandegeei = subset(Quercus_brandegeei,!(UID %in% c("id00099165", "id00036240", " id00036757",
                                                                  " id00037145", "id00037145", "id00036757")))

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

  new_Quercus_carmenensis = subset(Quercus_carmenensis,!(UID %in% c("id00238471", "id00033795")))
  
  write.csv(new_Quercus_carmenensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_carmenensis_points_removed", ".csv")),row.names = F)

#10
  Quercus_cedrosensis<- read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_cedrosensis.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")


  new_Quercus_cedrosensis = subset(Quercus_cedrosensis,!(UID %in% c("id00035830", "id00025050", "id00000337",
                                                                    "id00156319", "id00270443", "id00033363",
                                                                    "id00042635", "id00241187", "id00270358",
                                                                    "id00025046", "id00156399", "id00034187",
                                                                    "id00038997", "id00270446")))

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

  new_Quercus_costaricensis = subset(Quercus_costaricensis,!(UID %in% c("id00038190", "id00042438", "id00035189",
                                                                        "id00000507", "id00034133", "id00042412",
                                                                        "id00032049")))

  write.csv(new_Quercus_costaricensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_costaricensis_points_removed", ".csv")),row.names = F)
#15
  Quercus_cualensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_cualensis.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")

  new_Quercus_cualensis = subset(Quercus_cualensis,!(UID %in% c("id00038898", "id00035210", "id00000615")))

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

  new_Quercus_delgadoana = subset(Quercus_delgadoana,!(UID %in% c("id00024890", "id00221007", "id00248678",
                                                                  "id00248682", "id00024434", "id00248676",
                                                                  "id00248672", "id00248674", "id00248677",
                                                                  "id00248681", "id00248679", "id00000616")))

  write.csv(new_Quercus_delgadoana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_delgadoana_points_removed", ".csv")),row.names = F)
#18
  Quercus_deliquescens<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_deliquescens.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")
  
  new_Quercus_deliquescens = subset(Quercus_deliquescens,!(UID %in% c("id00270463", "id00270465", "id00270471",
                                                                      "id00270472", "id00270466", "id00270467",
                                                                      "id00270475", "id00270469", "id00270473",
                                                                      "id00270460", "id00270470", "id00270476",
                                                                      "id00043349", "id00037637", "id00270462",
                                                                      "id00270464", "id00228397", "id00270461")))
  
  write.csv(new_Quercus_deliquescens, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_deliquescens_points_removed", ".csv")),row.names = F)
#19
  Quercus_devia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_devia.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")
  
  new_Quercus_devia = subset(Quercus_devia,!(datasetName %in%  "iNaturalist research-grade observations"))

  new_Quercus_devia2 = subset(new_Quercus_devia,!(UID %in% c("id00000620", "id00243795", "id00096198",
                                                             "id00036865", "id00038041", "id00034605",
                                                             "id00043186", "id00034843", "id00043342",
                                                             "id00035588", "id00096207", "id00037482",
                                                             "id00270542", "id00037184", "id00006982")))
  

  write.csv(new_Quercus_devia2, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_devia_points_removed", ".csv")),row.names = F)
#20
  Quercus_diversifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                         "Quercus_diversifolia.csv"), header = T, na.strings=c("","NA"),
                               colClasses="character")

 write.csv(Quercus_diversifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_diversifolia_points_removed", ".csv")),row.names = F)
#21
  Quercus_dumosa<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                  "Quercus_dumosa.csv"), header = T, na.strings=c("","NA"),
                        colClasses="character")

  new_Quercus_dumosa = subset(Quercus_dumosa,!(database %in% "IUCN_RedList"))
  
  new_Quercus_dumosa2 = subset(new_Quercus_dumosa,!(datasetName %in% c("Hernando_Rodriguez_Correa", "iNaturalist research-grade observations")))
  
  new_Quercus_dumosa3 = subset(new_Quercus_dumosa2,!(UID %in% c("id00039667", "id00031856", "id00056936",
                                                                "id00033320", "id00007102", "id00037621",
                                                                "id00056934", "id00057558", "id00057615",
                                                                "id00032739", "id00238795", "id00238794",
                                                                "id00057564", "id00057611", "id00057612",
                                                                "id00057628", "id00057609", "id00057293",
                                                                "id00057012", "id00057751", "id00057232",
                                                                "id00057318", "id00052700", "id00057689",
                                                                "id00034171", "id00057750", "id00040825",
                                                                "id00042645", "id00057208", "id00034172",
                                                                "id00032595", "id00037104", "id00057666",
                                                                "id00057319", "id00034173", "id00057302",
                                                                "id00057015", "id00034170", "id00007094",
                                                                "id00056993", "id00038912", "id00033236",
                                                                "id00057135", "id00040765", "id00057641",
                                                                "id00057789", "id00057253", "id00037589",
                                                                "id00057610", "id00056957", "id00056946",
                                                                "id00056945", "id00057142", "id00038498",
                                                                "id00040597", "id00042572", "id00057145",
                                                                "id00056947", "id00057063", "id00057023",
                                                                "id00057786", "id00040595", "id00057213",
                                                                "id00057787", "id00042575", "id00057157",
                                                                "id00035509", "id00040766", "id00034164",
                                                                "id00040764", "id00057148", "id00057681",
                                                                "id00057081", "id00033130", "id00033160",
                                                                "id00038133", "id00057200", "id00057614",
                                                                "id00057793", "id00040763", "id00057169",
                                                                "id00057683", "id00057682", "id00056956",
                                                                "id00057169", "id00057024", "id00037612",
                                                                "id00057019", "id00042450", "id00057757",
                                                                "id00057261", "id00056933", "id00032731",
                                                                "id00057312", "id00057262", "id00056948",
                                                                "id00057310", "id00035728", "id00036679",
                                                                "id00056938", "id00033330", "id00056920",
                                                                "id00033400", "id00056919", "id00032338",
                                                                "id00031862", "id00056991", "id00056995",
                                                                "id00033349", "id00033404", "id00057263",
                                                                "id00057311", "id00036736", "id00056992",
                                                                "id00036698", "id00037850", "id00036721",
                                                                "id00036766", "id00057316", "id00057321",
                                                                "id00057788", "id00057288", "id00041541",
                                                                "id00057276", "id00057640", "id00057191",
                                                                "id00040617", "id00056944", "id00032254",
                                                                "id00057076", "id00033542", "id00041462",
                                                                "id00037682", "id00057743", "id00032738",
                                                                "id00057230", "id00042649",
                                                                "id00057133", "id00057660", "id00056997",
                                                                "id00057099", "id00057642", "id00057317",
                                                                "id00057565", "id00057665", "id00057794",
                                                                "id00037598", "id00032727", "id00057668",
                                                                "id00056974", "id00057001", "id00057078",
                                                                "id00057075", "id00057243", "id00057006",
                                                                "id00056943", "id00247197", "id00057556",
                                                                "id00037596", "id00037597", "id00040594",
                                                                "id00034163", "id00057297", "id00057566",
                                                                "id00056954", "id00040624")))
  
  new_Quercus_dumosa4 = subset(new_Quercus_dumosa3,decimalLatitude <=35)

  write.csv(new_Quercus_dumosa4, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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
  
  new_Quercus_engelmannii = subset(Quercus_engelmannii,!(UID %in% c("id00038895", "id00000660", "id00000678",
                                                                    "id00000677", "id00033384", "id00000641",
                                                                    "id00037343", "id00035567", "id00033133",
                                                                    "id00043898", "id00038794", "id00238887",
                                                                    "id00038749", "id00031753", "id00238926",
                                                                    "id00238927", "id00040590", "id00035802",
                                                                    "id00035846", "id00035830", "id00038044",
                                                                    "id00028839", "id00238898", "id00032058",
                                                                    "id00036142", "id00033478", "id00044186",
                                                                    "id00238888", "id00238889", "id00028838", 
                                                                    "id00028580", "id00273696", "id00038944", 
                                                                    "id00238890", "id00238886")))

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

  write.csv(Quercus_furfuracea, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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
  
  new_Quercus_ghiesbreghtii = subset(Quercus_ghiesbreghtii,!(UID %in% c("id00039327", "id00035121", "id00010825",
                                                                        "id00010826", "id00039374", "id00042364",
                                                                        "id00037887", "id00000707", "id00034298", 
                                                                        "id00037644")))

  write.csv(new_Quercus_ghiesbreghtii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_ghiesbreghtii_points_removed", ".csv")),row.names = F)
#28
  Quercus_graciliformis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                          "Quercus_graciliformis.csv"), header = T, na.strings=c("","NA"),
                                colClasses="character")


  new_Quercus_graciliformis = subset(Quercus_graciliformis,!(UID %in% c("id00043920", "id00043276", "id00038374",
                                                                        "id00024852", "id00024850", "id00024852",
                                                                        "id00031803")))

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

  new_Quercus_grahamii = subset(Quercus_grahamii,!(UID %in% c("id00380018", "id00032163", "id00039241",
                                                              "id00034890", "id00039376", "id00032231",
                                                              "id00043696", "id00038558", "id00032434",
                                                              "id00037545")))

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

  new_Quercus_hinckleyi = subset(Quercus_hinckleyi,!(UID %in% c("id00239299", "id00024569",  "id00035592", 
                                                                "id00038379")))

  write.csv(new_Quercus_hinckleyi, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hinckleyi_points_removed", ".csv")),row.names = F)
#33
  Quercus_hintonii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintonii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hintonii = subset(Quercus_hintonii,!(UID %in% c("id00044047", "id00044046", "id00036129",
                                                              "id00040578", "id00037334", "id00037874",
                                                              "id00244376")))

  write.csv(new_Quercus_hintonii, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hintonii_points_removed", ".csv")),row.names = F)
#34
  Quercus_hintoniorum<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hintoniorum.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hintoniorum = subset(Quercus_hintoniorum,!(UID %in% c("id00000757", "id00036813", " id00012485",
                                                                   "id00012485", "id00275737")))

  write.csv(new_Quercus_hintoniorum, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_hintoniorum_points_removed", ".csv")),row.names = F)
#35
  Quercus_hirtifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_hirtifolia.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_hirtifolia = subset(Quercus_hirtifolia,!(UID %in% c("id00250429", "id00041079", "id00012496",
                                                                  "id00000764")))

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
                                                              "id00031913", "id00038897", "id00037626",
                                                              "id00037709", "id00261603", "id00012900",
                                                              "id00037702", "id00043315", "id00043315",
                                                              "id00018209", "id00261590", "id00043698",
                                                              "id00000810", "id00000772", "id00037778",
                                                              "id00037354", "id00037933", "id00040611",
                                                              "id00041992", "id00037781", "id00033420",
                                                              "id00043694", "id00031948", "id00031978",
                                                              "id00261599", "id00028368", "id00261617",
                                                              "id00000785", "id00225129", "id00038377", 
                                                              "id00037878", "id00041981", "id00037347",
                                                              "id00261596", "id00032136", "id00249059",
                                                              "id00037541", "id00040039", "id00040051",
                                                              "id00041184", "id00038113", "id00038323",
                                                              "id00032195", "id00038891", "id00000782",
                                                              "id00043873", "id00000882", "id00041236",
                                                              "id00225130", "id00032171", "id00033054",
                                                              "id00038110", "id00378405", "id00000862",
                                                              "id00038254", "id00041242", "id00035612",
                                                              "id00041575")))

  write.csv(new_Quercus_insignis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_insignis_points_removed", ".csv")),row.names = F)

#38
  Quercus_macdougallii<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                               "Quercus_macdougallii.csv"), header = T, na.strings=c("","NA"),
                                     colClasses="character")

  new_Quercus_macdougallii = subset(Quercus_macdougallii,!(UID %in% c("id00038892", " id00221569", "id00272581",
                                                                      "id00037687", "id00000818", "id00038047",
                                                                      "id00034379", "id00380189","id00221569",
                                                                      "id00380187")))

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
  
  new_Quercus_miquihuanensis = subset(Quercus_miquihuanensis,!(UID %in% "id00000134"))
  
  write.csv(new_Quercus_miquihuanensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_miquihuanensis_points_removed", ".csv")),row.names = F)

#43
  Quercus_mulleri<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                           "Quercus_mulleri.csv"), header = T, na.strings=c("","NA"),
                                 colClasses="character")

  new_Quercus_mulleri = subset(Quercus_mulleri,!(UID %in% c("id00038889", "id00041844")))

  write.csv(new_Quercus_mulleri, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_mulleri_points_removed", ".csv")),row.names = F)
#44
  Quercus_nixoniana<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_nixoniana.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_nixoniana = subset(Quercus_nixoniana,!(UID %in% c("id00034636", "id00000826", "id00034197",
                                                                "id00039553", "id00228633", "id00245039",
                                                                "id00245041")))

  write.csv(new_Quercus_nixoniana, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_nixoniana_points_removed", ".csv")),row.names = F)
#45
  Quercus_opaca<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_opaca.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_opaca = subset(Quercus_opaca,!(UID %in% c("id00221937", "id00034993", "id00037426",
                                                        "id00034947", "id00037436", "id00299953",
                                                        "id00221929", "id00040964", "id00299939",
                                                        "id00041864", "id00037670", "id00221949",
                                                        "id00034978", "id00018210", "id00018211",
                                                        "id00221942", "id00028871")))

  write.csv(new_Quercus_opaca, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_opaca_points_removed", ".csv")),row.names = F)
#46
  Quercus_paxtalensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_paxtalensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_paxtalensis = subset(Quercus_paxtalensis,!(UID %in% c("id00043308", "id00037244", "id00037295",
                                                                    "id00043237", "id00038556", "id00031969",
                                                                    "id00031962")))

  write.csv(new_Quercus_paxtalensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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

  new_Quercus_radiata = subset(Quercus_radiata,!(UID %in% c("id00032281", "id00127133", "id00127132",
                                                            "id00245153", " id00000934", "id00000934")))

  write.csv(new_Quercus_radiata, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_radiata_points_removed", ".csv")),row.names = F)
#50
  Quercus_rekonis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_rekonis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_rekonis = subset(Quercus_rekonis,!(UID %in% c("id00000946", "id00043840", "id00044316",
                                                            "id00033056", "id00032353", "id00032382",
                                                            "id00038112", "id00043271", "id00032397",
                                                            "id00034599", "id00032386")))

  write.csv(new_Quercus_rekonis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_rekonis_points_removed", ".csv")),row.names = F)
#51
  Quercus_robusta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_robusta.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_robusta = subset(Quercus_robusta,!(UID %in% c("id00039487", "id00041913", "id00042484",
                                                            "id00038166", "id00038899", "id00035328",
                                                            "id00041197", "id00041158", "id00038345",
                                                            "id00041155", "id00038943")))

  write.csv(new_Quercus_robusta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_robusta_points_removed", ".csv")),row.names = F)

#52
  Quercus_rubramenta<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                                    "Quercus_rubramenta.csv"), header = T, na.strings=c("","NA"),
                          colClasses="character")

  new_Quercus_rubramenta = subset(Quercus_rubramenta,!(UID %in% c("id00034198", "id00020022", "id00037617",
                                                                  "id00038922", "id00245157", "id00245156")))

  write.csv(new_Quercus_rubramenta, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_rubramenta_points_removed", ".csv")),row.names = F)
#53
  Quercus_runcinatifolia<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_runcinatifolia.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_runcinatifolia = subset(Quercus_runcinatifolia,!(UID %in% c("id00000951", "id00041072")))
  
  new_Quercus_runcinatifolia2 = subset(new_Quercus_runcinatifolia,!(datasetName %in% "iNaturalist research-grade observations"))

  write.csv(new_Quercus_runcinatifolia2, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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
  
  new_Quercus_tardifolia = subset(Quercus_tardifolia,!(UID %in% c("id00044142","id00044142", "id00035623")))

  write.csv(new_Quercus_tardifolia, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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

  new_Quercus_tomentella = subset(Quercus_tomentella,!(UID %in% c("id00000953", "id00036676", "id00036702",
                                                                  "id00042437", "id00035874", "id00035921",
                                                                  "id00038160", "id00031833", "id00038830",
                                                                  "id00036322", "id00035796", "id00026342",
                                                                  "id00036323", "id00241592", "id00040596",
                                                                  "id00241591", "id00041294", "id00038974", 
                                                                  "id00038852", "id00241591", "id00041294", 
                                                                  "id00038974", "id00038852", "id00035215", 
                                                                  "id00037400", "id00036593", "id00037376",
                                                                  "id00036174", "id00033550", "id00042853", 
                                                                  "id00036336", "id00032023")))


  write.csv(new_Quercus_tomentella, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tomentella_points_removed", ".csv")),row.names = F)
#59
  Quercus_toumeyi<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_toumeyi.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")
  
  new_Quercus_toumeyi = subset(Quercus_toumeyi,!(UID %in% c("id00072873", "id00072995", "id00073032",
                                                            "id00072713", "id00073094", "id00000989",
                                                            "id00029037", "id00024943", "id00072791",
                                                            "id00072789", "id00000960", "id00073204",
                                                            "id00033422", "id00038870", "id00073163")))
  
  write.csv(new_Quercus_toumeyi, file.path(main_dir,data, standard, "taxon_edited_points_removed",
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
  
  new_Quercus_trinitatis = subset(Quercus_trinitatis,!(UID %in% c("id00045369", "id00223143", "id00022950",
                                                                  "id00034827", "id00223142", "id00223139",
                                                                  "id00223163", "id00039369", "id00039389",
                                                                  "id00223165", "id00223140", "id00034189",
                                                                  "id00034200", "id00034846", "id00223137",
                                                                  "id00223147", "id00022950", "id00223122",
                                                                  "id00361389", "id00361353")))

  write.csv(new_Quercus_trinitatis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_trinitatis_points_removed", ".csv")),row.names = F)
#62
  Quercus_tuitensis<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_tuitensis.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_tuitensis = subset(Quercus_tuitensis,!(UID %in% c("id00001008", "id00245870", "id00038373")))

  write.csv(new_Quercus_tuitensis, file.path(main_dir,data, standard, "taxon_edited_points_removed",
                                               paste0("Quercus_tuitensis_points_removed", ".csv")),row.names = F)
#63
  Quercus_undata<-read.csv(file.path(main_dir, data, standard,"taxon_edited_points",
                             "Quercus_undata.csv"), header = T, na.strings=c("","NA"),
                   colClasses="character")

  new_Quercus_undata = subset(Quercus_undata,!(UID %in% c("id00001010", "id00038715", "id00241079",
                                                          "id00241080", "id00241081", "id00241078")))

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
  
  new_Quercus_vicentensis = subset(Quercus_vicentensis,!(UID %in% c("id00038597", "id00037466", "id00380417",
                                                                    "id00038068", "id00031941", "id00001016",
                                                                    "id00038627",  "id00001021", " id00037240",
                                                                    "id00001020", "id00038726", "id00001019",
                                                                    " id00032173", "id00032301", "id00032308",
                                                                    "id00035222", "id00001026", "id00031938",
                                                                    "id00043883", "id00036481", "id00036859",
                                                                    "id00041796", "id00043316", "id00031929",
                                                                    "id00032000", "id00037941", "id00037855",
                                                                    "id00037936", "id00037935", "id00037925",
                                                                    "id00037923", "id00037934", "id00037931",
                                                                    "id00032391", "id00037240", "id00032173",
                                                                    "id00040241")))

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
  
  
    
                                              