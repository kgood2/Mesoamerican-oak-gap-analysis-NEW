################################################################################
### Authors: Emily Beckman Bruns & Kate Good
### Date: October 2022

### DESCRIPTION:
## Takes SurveyMonkey Excel export and format for analysis. Specially-designed
#   to work with a "conservation action questionnaire" that has multiple
#   matrix questions for multiple species and regions. SurveyMonkey exports
#   such questions in a very wide (horizontal) format, and this script puts
#   the data in a more vertical format and aligns results with respondent
#   metadata.

### INPUTS:
## SurveyMonkey Excel (xlsx) export from survey with multiple matrices,
#   species, and regions.

### OUTPUTS:
## Summary CSV for each matrix question, two ways:
#   1) "Grid" format (easier for human viewing)
#   2) "Long" format (best for summarizing using pivot tables)
## CSV with results from all matrix questions, two ways (grid and long)
## Notes from open-ended question about additional target species
#   (AdditionalSpeciesNotes.csv)


################################################################################


## SET UP YOUR WORKSPACE

# set working directory
main_dir <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/conservation"
survey_export_file <- "Cuestionario de acciones.xlsx"

# load libraries
library("tidyverse")
library("readxl")
library("xlsx")


## READ IN DATA

# read in SurveyMonkey export
raw_data <- read_excel(file.path(main_dir,survey_export_file))



## CREATE RESPONDENT INFO DATAFRAME

# list of names for the respondent info columns
#   (can use X1, X2, X3, etc. for cols you don't want)
respondent_col_names <- c("respondent_id","collector_id","start_date",
                          "end_date","ip_address","X1","X2","X3","X4",
                          "dont_make_public","name","institution",
                          "X5","X6","X7","X8","X9","country","email_address",
                          "X10","institution_category",
                          "institution_category_other")

# select and rename respondent info columns
respondent_info <- raw_data[2:nrow(raw_data),1:length(respondent_col_names)]
for(i in 1:ncol(respondent_info)){
  names(respondent_info)[i] <- respondent_col_names[i] }
# remove respondent info columns with "X" in the name
t <- grepl("^X",names(respondent_info))
if(length(unique(t))>1){
  respondent_info <- respondent_info[, -grep("^X", names(respondent_info))] }
str(respondent_info)
# standardize the respondent info a little
##### here you could make everything the same case (some things are capital)
##### and standardize the country names as well


## ORGANIZE DATA BY QUESTIONN TYPE AND ADD REGION COLUMN

## change the following as needed, based on your questionnaire:

# see if any regions had an "Other" option when selecting species
other_col_en <- grep("Other",raw_data[1,])
other_col_es <- grep("Otro",raw_data[1,])
as.data.frame(raw_data[,other_col_en])
as.data.frame(raw_data[,other_col_es])
  # we are going to manually remove these columns when they're in the species
  #   selection section because they greatly complicates the loop below,
  #   since the 'other' selection was not carried through to the questions
  #   themselves
  # note that this might not be necessary for other surveys, depending
  #   on the format
  # if there is any important information being removed, note it manually
  #   somewhere else
  # !! insert the column numbers you'd like to remove, based on above outputs
raw_data <- raw_data[,-c(3870,22,29,6614,9157)]

# unique part of each main question asked
select_sp <- "Seleccione todas las especies"
Q7_match <- "Seleccione todas las actividades de"
Q8_match <- "Seleccione lo que considere la actividad de"
Q9_match <- "Seleccione lo que considere la amenaza"

# create dataframes for collecting results, one for each main question
# QUESTION7
Q7_results <- data.frame(
  "species" = as.character(NA),
  "Recolectar y distribuir germoplasma" = as.character(NA),
  "Horticultura de conservación" = as.character(NA),
  "Criopreservación y / o micropropagación" = as.character(NA),
  "Restauración del hábitat" = as.character(NA),
  "Implementar políticas o regulaciones de protección" = as.character(NA),
  "Encuestas de ocurrencia o monitoreo de población" = as.character(NA),
  "Banco de polen y / o semillas" = as.character(NA),
  "Refuerzo o introducción de la población" = as.character(NA),
  "Proteger y / o gestionar el hábitat" = as.character(NA),
  "Educación o conciencia pública" = as.character(NA),
  "Investigación: Cambio climático" = as.character(NA),
  "Investigación: Genética" = as.character(NA),
  "Investigación: Plagas y patógenos" = as.character(NA),
  "Investigación: Taxonomía" = as.character(NA),
  # metadata you want to add
  "region" = as.character(NA),
  "respondent_id" = as.double(NA))
head(Q7_results)
# QUESTION8
Q8_results <- data.frame(
  "species" = as.character(NA),
  "Recolectar y distribuir germoplasma" = as.character(NA),
  "Horticultura de conservación" = as.character(NA),
  "Criopreservación y / o micropropagación" = as.character(NA),
  "Restauración del hábitat" = as.character(NA),
  "Implementar políticas o regulaciones de protección" = as.character(NA),
  "Encuestas de ocurrencia o monitoreo de población" = as.character(NA),
  "Banco de polen y / o semillas" = as.character(NA),
  "Refuerzo o introducción de la población" = as.character(NA),
  "Proteger y / o gestionar el hábitat" = as.character(NA),
  "Educación o conciencia pública" = as.character(NA),
  "Investigación: Cambio climático" = as.character(NA),
  "Investigación: Genética" = as.character(NA),
  "Investigación: Plagas y patógenos" = as.character(NA),
  "Investigación: Taxonomía" = as.character(NA),
  "Desconocido" = as.character(NA),
  "Ninguna (actualmente no se necesitan acciones de conservación)" = as.character(NA),
  # metadata you want to add
  "region" = as.character(NA),
  "respondent_id" = as.double(NA))
head(Q8_results)
# QUESTION9
Q9_results <- data.frame(
  "species" = as.character(NA),
  "Agricultura, silvicultura y / o ganadería" = as.character(NA),
  "Cambio climático" = as.character(NA),
  "Desarrollo, minería y / o carreteras" = as.character(NA),
  "Modificación del régimen de perturbaciones" = as.character(NA),
  "Endogamia o introgresión" = as.character(NA),
  "Competencia de especies invasoras" = as.character(NA),
  "Plagas o patógenos" = as.character(NA),
  "Turismo o recreación" = as.character(NA),
  "Recolección silvestre" = as.character(NA),
  "Desconocido" = as.character(NA),
  # metadata you want to add
  "region" = as.character(NA),
  "respondent_id" = as.double(NA))
head(Q9_results)

# create list of results dataframes to cycle through
results <- list(Q7_results,Q8_results,Q9_results)

# create list of column numbers to cycle through
#   (column where each question starts)
start <- list(
  grep(Q7_match,colnames(raw_data)),
  grep(Q8_match,colnames(raw_data)),
  grep(Q9_match,colnames(raw_data)),
  ncol(raw_data)+1)
head(start)

# get the column numbers where the species are selected for each region
# 30, 3803, 6550, 9132
start_sp <- grep(select_sp,colnames(raw_data))
# print the region headers so you can see what's present
for(i in 1:length(start_sp)){
  print(substr(colnames(raw_data[0,start_sp[[i]]]),1,80))
}
# create a list of target regions based on the above results
regions <- c("M√©xico y Am√©rica Centra", "sudeste de Asia", "China",
             "Asia occidental, Europa, y √Åfrica")



### LOOP THROUGH AUTOMATICALLY ###

# FOR EACH QUESTIONN
for(q in 1:length(results)){
  # get the number of categories for the question
  #   -3 removes metadata cols (species, region, respondent_id)
  num_fields <- ncol(results[[q]])-3
  # results dataframe for the question
  df_out <- results[[q]]

  # FOR EACH REGION
  for(r in 1:length(regions)){
    # get the raw data for the current question and region only
    data <- raw_data[,start[[q]][r]:(start[[q]][r]+((start[[1]][r]-start_sp[[r]]-1)*num_fields)-1)]
    # use the first row to assign column names
    colnames(data) = data[1, ]
    # leave just species name in first row (cut off rest)
    data <- as.data.frame(lapply(data,function(x) gsub("( -).*", "", x)),
                          stringsAsFactors=F)
    # cycle through each species and add the responses to a dataframe
    start_col <- 1
    end_col <- num_fields

    # FOR EACH SPECIES
      # the -1 below removes the open-ended response col to
      #   get the true number of species
    for(s in 1:(start[[1]][r]-start_sp[[r]]-1)){
      # start at row 2 because there is a second header row
      add <- data[2:nrow(data),start_col:end_col]
      sp_add <- data.frame("species" = rep(data[1,start_col],
                                           times=nrow(data)-1))
      add <- cbind(sp_add,add)
      # add respondent info and region
      add$region <- regions[[r]]
      add$respondent_id <- respondent_info$respondent_id
      # add species to running dataframe for the question
      df_out <- rbind(df_out,setNames(add,names(df_out)))
      # set up index for next species
      start_col <- start_col + num_fields
      end_col <- end_col + num_fields
    }
    # remove rows with no data
    df_out <- df_out[rowSums(is.na(df_out[,2:ncol(df_out)]))<ncol(df_out)-3,]
  }
  results[[q]] <- df_out
  print(str(df_out))
}

## SAVE ALL RESULTS IN ONE FILE:

# join all questions
join_all <- results[[1]]
join_all <- join_all[,c(ncol(join_all),(ncol(join_all)-1),1,2:(ncol(Q7_results)-3))]
for(i in 1:(length(results)-1)){
  join_all <- full_join(join_all,results[[i+1]],
                        by=c("respondent_id","region","species"))
}
join_all <- right_join(respondent_info,join_all)
# sort by whatever variables you'd like:
join_all <- join_all %>% arrange(species,name)
str(join_all)
write.csv(join_all,file.path(main_dir,"Grid_AllResults.csv"),row.names = F)


## SAVE RESULTS IN SEPARATE FILES, BY QUESTIONN:

# join all respondent info to each dataframe and write files
for(q in 1:length(results)){
  results[[q]] <- left_join(results[[q]],respondent_info)
  write.csv(results[[q]],file.path(main_dir,paste0("Grid_Question",q,"Results.csv")),
            row.names = F)
  long_format <- data.frame()
  num_datacol <- (ncol(results[[q]])-ncol(respondent_info)-2)
  for(cat in 1:num_datacol){
    meta <- results[[q]][,c(1,(num_datacol+2):ncol(results[[q]]))]
    add <- results[[q]][,(cat+1)]
    add_all <- cbind(meta,add)
    colnames(add_all)[(ncol(respondent_info)+3)] <- "activity"
    long_format <- rbind(long_format,add_all)
  }
  long_format <- long_format[!is.na(long_format$activity),]
  results[[q]] <- long_format
  write.csv(results[[q]],file.path(main_dir,paste0("Long_Question",q,"Results.csv")),
            row.names = F)
  results[[q]]$question <- q
}

long_results <- Reduce(rbind,results)
str(long_results)
write.csv(long_results,file.path(main_dir,"Long_AllResults.csv"), row.names = F)


## GET NOTES ABOUT ADDITIONAL SPECIES

# dataframe with notes about additional target species suggested, one per region
# phrase to match
Q6_match <- "También puede proporcionar nombres de otras especies que agregaría a la lista anterior de especies en nuestros géneros objetivo; Indique las razones principales de su  vulnerabilidad:"
# find locations of phrase
t <- grep(Q6_match,colnames(raw_data))
# create dataframe
add_species <- data.frame(
  "Additional species and reason" = as.character(NA),
  "region" = as.character(NA),
  "respondent_id" = as.double(NA))
for(i in 1:length(t)){
  data <- raw_data[2:nrow(raw_data),t[i]]
  colnames(data) <- "Additional.species.and.reason"
  data$region <- regions[[i]]
  data$respondent_id <- respondent_info$respondent_id
  add_species <- rbind(add_species,data)
}
# remove rows with no data
add_species <- add_species[!is.na(add_species[,1]),]
# join to rest of respondent info
add_sp_join <- left_join(add_species,respondent_info)
str(add_sp_join)
# write file
write.csv(add_sp_join,file.path(main_dir,"AdditionalSpeciesNotes.csv"),
          row.names = F)
