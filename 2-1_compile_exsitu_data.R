################################################################################

## !! NEEDS RUN-THROUGH AND SOME AREAS NEED WORK STILL !!

## 2-1_compile_exsitu_data.R
### Author: Emily Beckman ### Date: 12/13/2019

### DESCRIPTION:
# This script takes a folder of CSV files representing accessions data from
#   different institutions, combines them into one dataset, and standardizes
#   some important fields.

### DATA IN:
# 1. Folder ("exsitu_standard_column_names") of CSV files whose column names have
#     already be standardized by hand using the
#     "standardizing_accessions_data_fields" template
#     (https://docs.google.com/spreadsheets/d/1p5HAS7vIE-3CbQcUmwrnuBGv5324dXy-42Iu6LlbX0E/edit?usp=sharing)
# 2. respondent_institution_data_table.csv (see linked sheet above for format)
# 2. Target taxa list (target_taxa_with_syn.csv), created through
#     1-0_get_taxonomic_info.R

### DATA OUT:

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'data.table', 'textclean',
                 'measurements', 'naniar','CoordinateCleaner','rnaturalearth',
                 'rnaturalearthdata','maps','raster','spatialEco','geonames')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
# install.packages("naniar")
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#  main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Consortia/Ex situ analysis"
main_dir <- "/Volumes/GoogleDrive/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"
#  script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
# source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
# source('scripts/0-1_set_workingdirectory.R')

# OPTIONAL, depending on workflow: set target genus/genera name (for file reading and writing)
#target_genus <- "Acer"

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))

# function to read in ex situ files from different folders/years and stack
read.exsitu.csv <- function(path,submission_year){
  # create list of paths to ex situ accessions CSV files in folder
  file_list <- list.files(path=path,pattern=".csv",full.names=TRUE)
  # read in each csv in path list to create list of dataframes
  file_dfs <- lapply(file_list,read.csv,header=TRUE,fileEncoding="LATIN1",
                     strip.white=TRUE,colClasses="character",na.strings=c("","NA"))
  print(paste0("Number of files: ",length(file_dfs)))
  #sapply(file_dfs, nrow) # can look at number of rows in each csv
  for(file in seq_along(file_dfs)){
    df <- file_dfs[[file]]
    # add file name as column, to record home institution for each record
    df$filename <- rep(file_list[file],nrow(df))
    # remove file path portion
    df$filename <- mgsub(df$filename,c(paste0(path,"/"),".csv"),"")
    # add year of submission
    df$submission_year <- submission_year
    # remove extra blank columns
    t <- grepl("^X",names(df))
    if(length(unique(t))>1){
      #print(df$filename[1])
      df <- df[, -grep("^X", names(df))]
    }
    # add accession number if there isn't one
    if("acc_num" %in% names(df) & nrow(df[which(is.na(df$acc_num)),]) > 0){
      df[which(is.na(df$acc_num)),]$acc_num <- paste0("added",
                                                      sprintf("%04d", 1:nrow(df[which(is.na(df$acc_num)),])))
      #print(nrow(df))
    } else if ("acc_no" %in% names(df) & nrow(df[which(is.na(df$acc_no)),]) > 0){
      df[which(is.na(df$acc_no)),]$acc_no <- paste0("added",
                                                    sprintf("%04d", 1:nrow(df[which(is.na(df$acc_no)),])))
      #print(nrow(df))
    } else if (!("acc_num" %in% names(df)) & !("acc_no" %in% names(df))){
      df$acc_num <- paste0("added", sprintf("%04d", 1:nrow(df)))
      #print(nrow(df))
    } else {
      #print(paste("NO ACC NUM EDITS:",df$filename[1]))
    }
    # replace old df with new df
    file_dfs[[file]] <- df
    #print(head(file_dfs[[file]],n=2))
  }
  # stack all datasets using rbind.fill, which keeps non-matching columns
  #   and fills with NA; 'Reduce' iterates through and merges with previous
  # this may take a few minutes if you have lots of data
  all_data <- Reduce(rbind.fill, file_dfs)
  print(paste0("Number of rows: ",nrow(all_data)))
  print(paste0("Number of columns: ",ncol(all_data)))
  return(all_data)
}

# remove duplicates in network datasets (e.g., PCN) if institution submitted
#  their own data separately
remove.network.dups <- function(df,rm_inst_names,file_name){
  df <- df[!(df$filename == file_name & df$inst_short %in% rm_inst_names),]
  return(df)
}

################################################################################
################################################################################
# 1. Read in and stack all accessions data
################################################################################

### FIRST: After receiving accession data from institutions, you need to process
#   it manually. See here for instructions:
#   https://docs.google.com/spreadsheets/d/1p5HAS7vIE-3CbQcUmwrnuBGv5324dXy-42Iu6LlbX0E/edit?usp=sharing

## read in data from multiple surveys and stack, or just read in from one folder
##    this function also adds columns for 1) the file name [equivalent to the
##    "inst_short" institution nickname] 2) a sumbission year, 3) an accession
##    number if one isn't given
## Warnings are usually ok here, but you can look at the file causing the
#     warning to see if there is an obvious formatting issue
raw_2021 <- read.exsitu.csv(file.path(main_dir,
                                      "exsitu_standard_column_names","2021_2022"), "2021/2022")
raw_2020 <- read.exsitu.csv(file.path(main_dir,
                                      "exsitu_standard_column_names","2020"), "2020")
raw_2019 <- read.exsitu.csv(file.path(main_dir,
                                      "exsitu_standard_column_names","2019"), "2019")
raw_2018 <- read.exsitu.csv(file.path(main_dir,
                                      "exsitu_standard_column_names","2018"), "2018")
raw_2017 <- read.exsitu.csv(file.path(main_dir,
                                      "exsitu_standard_column_names","2017"), "2017")

# stack all data
to_stack <- list(raw_2021,raw_2020,raw_2019,raw_2018,raw_2017)
all_data_raw <- Reduce(rbind.fill,to_stack)

# create new version before big changes, so can easily go back to original
all_data <- all_data_raw

# replace non-ascii characters
# first fix some common lat/long character issues
all_data$orig_lat <- mgsub(all_data$orig_lat,
                           c("\"","ç","d","'","°","º","Â","¼","،","¡","_","´","*","À","?","`")," ")
all_data$orig_long <- mgsub(all_data$orig_long,
                            c("\"","ç","d","'","°","º","Â","¼","،","¡","_","´","*","À","?","`")," ")
# replace all non-ascii
all_data <- as.data.frame(lapply(all_data,replace_non_ascii),stringsAsFactors=F)

# check out column names
sort(colnames(all_data))
## IF NEEDED: separate column into multiple
#all_data <- all_data %>% separate("specific",
#  c("infra_rank_add","infra_name_add"),sep=" ",remove=T,fill="right")
## IF NEEDED: see which datasets have extraneous columns so you can fix manually
##  in the raw data as desired; update line below
unique(all_data$filename[all_data$genus_species!=""])
## IF NEEDED: merge similar columns (you may not need to do this if no schema
##  mistakes were made when manually editing column names)
all_data <- tidyr::unite(all_data,"taxon_full_name",
                         c("taxon_full_name","ï..sp_full_name","sp_full_name","ï..taxon_full_name","taxon_name"),
                         sep=";",remove=T,na.rm=T)
all_data <- tidyr::unite(all_data,"infra_name",
                         c("infra_name","intra_name","specific_name"),
                         sep=";",remove=T,na.rm=T)
all_data <- tidyr::unite(all_data,"infra_rank",
                         c("infra_rank","intra_rank","specific_rank"),
                         sep=";",remove=T,na.rm=T)
#all_data <- tidyr::unite(all_data,"inst_short", c("inst_short","ï..inst_short"),
#  sep=";",remove=T,na.rm=T)
#all_data <- tidyr::unite(all_data,"num_indiv", c("num_indiv","num_plants"),
#  sep=";",remove=T,na.rm=T)
#all_data <- tidyr::unite(all_data,"coll_year", c("coll_year","acq_year"),
#  sep=";",remove=T,na.rm=T)
#all_data <- tidyr::unite(all_data,"cultivar", c("cultivar","trade_name"),
#  sep=" / ",remove=T,na.rm=T)
## IF NEEDED: remove unused columns or rename columns
#  unique(all_data$condition) # make sure nothing is "dead" or "removed"
#all_data <- all_data[ , -which(names(all_data) %in%
#  c("genus_species","condition","coord_precision","dataset_year","habitat",
#    "private"))]
#colnames(all_data)[colnames(all_data)=="name_determ"] <- "taxon_det"

### CHECK THINGS OUT ###
#sort(colnames(all_data)); ncol(all_data)
# There should be max of 31 columns, including no more than:
# acc_num,assoc_sp,coll_name,coll_num,coll_year,country,county,cultivar,
# filename,garden_loc,genus,germ_type,hybrid,infra_name,infra_rank,inst_short,
# lin_num,locality,municipality,notes,num_indiv,orig_lat,orig_long,orig_source,
# prov_type,rec_as,species,state,submission_year,taxon_det,taxon_full_name

# fill in inst_short column with filename if none provided
all_data$inst_short[is.na(all_data$inst_short)] <-
  all_data$filename[is.na(all_data$inst_short)]
nrow(all_data) #96841
# remove rows with no inst_short
all_data <- all_data[which(all_data$inst_short!=""),]
nrow(all_data) #96841
## IF NEEDED: remove duplicates in network datasets (e.g., PCN) if institution
##   submitted their own data separately
# Network 1
#network_rm <- c("MissouriBG","RanchoSantaAnaBG","MortonArb")
#all_data <- remove.network.dups(all_data,network_rm,"PCNQuercus")
# Network 2
#network_rm <- c("HackfallsArb")
#all_data <- remove.network.dups(all_data,network_rm,"CultivatedOaks")
#nrow(all_data) #95244

### CHECK ALL INSTITUTIONS ARE HERE ###
sort(unique(all_data$inst_short)) #158

# remove leading, trailing, and middle (e.g., double space) whitespace,
#   to prevent future errors
all_data <- as.data.frame(lapply(all_data, function(x) str_squish(x)),
                          stringsAsFactors=F)
# replace "" cells with NA in whole dataset
all_data[all_data == ""] <- NA

################################################################################
# 2. Save raw output of compiled data for target genera
#     (can be used to look for hybrids/cultivars, which are removed in next step)
################################################################################

all_data2 <- all_data

# preserve original taxon name
all_data2$taxon_full_name_orig <- all_data2$taxon_full_name

# fill genus column if not already filled
all_data2 <- all_data2 %>% separate("taxon_full_name","genus_temp",sep=" ",remove=F)
all_data2[which(is.na(all_data2$genus)),]$genus <- all_data2[which(is.na(all_data2$genus)),]$genus_temp
# standardize capitalization
all_data2$genus <- str_to_title(all_data2$genus)

### MAKE SURE NO GENUS MISSPELLINGS OR ABBREVIATIONS ###
sort(unique(all_data2$genus))
all_data2$genus <- mgsub(all_data2$genus,
                         c("^Q$","Querucs","Cyclobalanopsis"),"Quercus",fixed=F)

# remove rows not in target genus/genera
target_genera <- c("Quercus")
all_data3 <- all_data2 %>% filter(genus %in% target_genera)
nrow(all_data2); nrow(all_data3) #95244 ; 45057

### CHECK OUT THE HYBRID COLUMN ###
# standardize a bit
#sort(unique(all_data3$hybrid))
#all_data3$hybrid <- mgsub(all_data3$hybrid,
#  c("_","^1$","XH","Hybrid"," hybrid","\\*","^A$","^H$","X","^H ","Ex ","ExE","^\\("),
#  " x ",fixed=F)
#all_data3$hybrid <- str_squish(all_data3$hybrid)
# make sure everything has an " x " in it somewhere (important later)
#all_data3$hybrid <- mgsub(all_data3$hybrid,
#  c("^hispanica$"),c("x hispanica"),fixed=F)

# create concatenated taxon_full_name column
all_data3 <- tidyr::unite(all_data3, "taxon_full_name_concat",
                          c(genus,hybrid,species,infra_rank,infra_name,cultivar), sep=" ", remove=F,
                          na.rm=T)

# when blank, fill taxon_full_name column with concatenated full name
all_data3[is.na(all_data3$taxon_full_name),]$taxon_full_name <-
  all_data3[is.na(all_data3$taxon_full_name),]$taxon_full_name_concat
unique(all_data3$taxon_full_name)

# standardize common hybrid signifiers in taxon_full_name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
                                   c(" A ","_"," X ","\\*")," x ",fixed=T)
all_data3$taxon_full_name <- str_squish(all_data3$taxon_full_name)

# write copy of all data
# select columns
#all_data_export1 <- all_data3 %>%
#  dplyr::select(inst_short,taxon_full_name,genus,species,infra_rank,infra_name,
#    hybrid,cultivar,prov_type,orig_lat,orig_long,country,state,municipality,
#    locality,assoc_sp,acc_num,lin_num,orig_source,rec_as,num_indiv,germ_type,
#    garden_loc,coll_num,coll_name,coll_year,taxon_det,notes,taxon_full_name_orig)
#write.csv(all_data_export1, file.path(main_dir,"outputs",
#  paste0("ExSitu_AllDataRaw_GenusFilterOnly_", Sys.Date(), ".csv")),row.names = F)

# summary of genera (and submission year, if applicable) for each institution
#summary <- unique(data.frame(inst_short = all_data3$inst_short,
#  genera = all_data3$genus
#  #,submission_year = all_data3$submission_year
#))
#summary <- summary %>%
#  arrange(genera
#    #,submission_year
#  ) %>%
#  group_by(inst_short) %>%
#  mutate(genera = paste(genera,collapse = ", ")
#         #,submission_year = paste(submission_year,collapse = ", ")
#  ) %>%
#  ungroup() %>%
#  arrange(inst_short) %>%
#  distinct(inst_short,.keep_all=T)
#as.data.frame(summary)

################################################################################
# 3. Further standardize taxon name, then keep data for target taxa only
#     (removes hybrids and cultivars without specific epithet)
################################################################################

## FINISH STANDARDIZING TAXON NAMES

# add space after periods in taxon_full_name
all_data3$taxon_full_name <- gsub(".",". ",all_data3$taxon_full_name,fixed=T)
# replace unwanted characters in taxon_full_name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
                                   c("(",")",";","[","]",",","^","#")," ")
all_data3$taxon_full_name <- str_squish(all_data3$taxon_full_name)

## REMOVE HYBRIDS

# remove if anything in hyrbid column
all_data4 <- all_data3 %>% filter(is.na(hybrid))
# remove hybrids based on " x " in taxon_full_name_concat and/or taxon_full_name
all_data4 <- all_data3 %>% filter(!grepl(" x ",taxon_full_name_concat))
nrow(all_data3); nrow(all_data4) #45057 ; 43363
all_data4 <- all_data4 %>% filter(!grepl(" x ",taxon_full_name))
nrow(all_data4) #41936
# see hybrids removed:
sort(unique(anti_join(all_data3,all_data4)$taxon_full_name))

## CREATE NEW SEPARATED TAXON NAME COLUMNS

# separate out taxon full name and trim whitespace again
all_data4 <- all_data4 %>% separate("taxon_full_name",
                                    c("genus_new","species_new","extra1","extra2",
                                      "extra3","extra4","extra5","extra6","extra7"),sep=" ",extra="warn",
                                    remove=F,fill="right")
all_data4 <- as.data.frame(lapply(all_data4,str_squish),stringsAsFactors=F)
# replace genus_new with genus, since we fixed that up in the previous section
all_data4$genus_new <- all_data4$genus

## REMOVE CULTIVARS

# remove cultivars with no specific epithet (by looking for quotation mark in
#   species name)

all_data5 <- all_data4 %>%
  filter(!grepl("\"",species_new) & !grepl("\'",species_new))
nrow(all_data5) #38748
# see records removed:
sort(unique(anti_join(all_data4,all_data5)$taxon_full_name))


### CHECK TO MAKE SURE NO GOOD SPECIES WILL BE REMOVED DUE TO TYPOS ###
sort(unique(all_data5[which(grepl("[A-Z]",all_data5$species_new)),]$species_new))
## IF NEEDED: replace errors
#all_data5$species_new <- gsub("cheniiNakai","chenii",all_data5$species_new)
# remove remaining cultivars without species name (based on capital letters)
all_data6 <- all_data5 %>% filter(!grepl("[A-Z]",species_new))
nrow(all_data6) #38415
# see records removed:
#sort(unique(anti_join(all_data5,all_data6)$taxon_full_name))

## FIND INFRATAXA

## look for infrataxa key words
# make data in all "extra" columns lower case
sp_col <- grep("^species_new$", colnames(all_data6))
all_data6[,sp_col:(sp_col+5)] <- as.data.frame(sapply(
  all_data6[,sp_col:(sp_col+5)], tolower), stringsAsFactors=F)
# create matrix of all "extra" species name columns, to search for
#   infraspecific key words
search.col <- matrix(cbind(all_data6$extra1,all_data6$extra2,all_data6$extra3,
                           all_data6$extra4,all_data6$extra5,all_data6$extra6,all_data6$extra7),
                     nrow=nrow(all_data6))
#str(search.col)
# search the "extra" column matrix for matches to infraspecific key words
matches_i <- which(search.col=="variety"|search.col=="var"|search.col=="var."|
                     search.col=="v"|search.col=="v."|search.col=="va"|
                     search.col=="subspecies"|search.col=="subsp"|
                     search.col=="subsp."|search.col=="ssp"|search.col=="ssp."|
                     search.col=="subs."|search.col=="spp."|
                     search.col=="infra"|
                     search.col=="forma"|search.col=="form"|search.col=="fma"|
                     search.col=="fo"|search.col=="fo."|search.col=="f"|
                     search.col=="f.",arr.ind=T)
matches_i[,2] <- matches_i[,2]+sp_col
# create new infra_rank column and fill with "extra" contents that matched
#   infraspecific key words
all_data6$infra_rank_new <- NA
all_data6$infra_rank_new[matches_i] <- all_data6[matches_i]
#unique(all_data6$infra_rank_new) # check results

# create new infra_name column and fill with next column over from "extra"
#   contents that matched infraspecific key word
all_data6$infra_name_new <- NA
matches_i[,2] <- matches_i[,2]+1
all_data6$infra_name_new[matches_i] <- all_data6[matches_i]
#sort(unique(all_data6$infra_name_new))

# standardize infraspecific rank names
all_data6$infra_rank_new <- replace(all_data6$infra_rank_new,
                                    grep("^v$|^v.$|^var$|^variety$|^va$",all_data6$infra_rank_new), "var.")
all_data6$infra_rank_new <- replace(all_data6$infra_rank_new,
                                    grep("^subspecies$|^subsp$|^ssp$|^ssp.$|^subs.$|^spp.$",
                                         all_data6$infra_rank_new), "subsp.")
all_data6$infra_rank_new <- replace(all_data6$infra_rank_new,
                                    grep("^forma$|^form$|^fma$|^fo$|^fo.$|^f$",all_data6$infra_rank_new), "f.")
#unique(all_data6$infra_rank_new)

## CREATE FINAL TAXON FULL NAME FOR FILTERING

# create new taxon full name column
all_data6$taxon_full_name <- NA
# select rows with infraspecific name and concatenate
yes_infra <- which(!is.na(all_data6$infra_rank_new) &
                     !is.na(all_data6$infra_name_new))
all_data6$taxon_full_name[yes_infra] <- paste(all_data6$genus_new[yes_infra],
                                              all_data6$species_new[yes_infra], all_data6$infra_rank_new[yes_infra],
                                              all_data6$infra_name_new[yes_infra],sep=" ")
# select rows without infraspecific name and concatenate
all_data6$taxon_full_name[-yes_infra] <- paste(all_data6$genus_new[-yes_infra],
                                               all_data6$species_new[-yes_infra],sep=" ")
# check out results
#sort(unique(all_data6$taxon_full_name))

# create genus_species column
all_data6$genus_species <- paste(all_data6$genus_new,all_data6$species_new)

## FILTER OUT NON-TARGET TAXA

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list",
                                 "target_taxa_with_syn.csv"),
                       header = T, na.strings = c("","NA"),colClasses = "character")
taxon_list <- taxon_list %>%
  dplyr::select(taxon_name,species_name_acc,list)
head(taxon_list)

# rename some taxon name columns to preserve originals
setnames(all_data6,
         old = c("genus","species","infra_rank","infra_name"),
         new = c("genus_orig","species_orig","infra_rank_orig","infra_name_orig"))
setnames(all_data6,
         old = c("genus_new","species_new","infra_rank_new","infra_name_new",
                 "taxon_full_name"),
         new = c("genus","species","infra_rank","infra_name","taxon_name"))

# join dataset to taxa list
# join by taxon name
all_data7 <- left_join(all_data6,taxon_list)
# if no taxon match, join again just by species name
need_match <- all_data7[which(is.na(all_data7$list)),]
nrow(need_match) #8851
# remove columns from first taxon name match
need_match <- need_match[,1:(ncol(all_data7)-ncol(taxon_list)+1)]
# rename column for matching
need_match <- need_match %>% rename(taxon_full_name = taxon_name)
need_match$taxon_name <- need_match$genus_species
# new join
need_match <- left_join(need_match,taxon_list)
# bind together new matches and previously matched
matched <- all_data7[which(!is.na(all_data7$list)),]
matched$taxon_full_name <- matched$taxon_name
all_data8 <- rbind(matched,need_match)
table(all_data8$list) # desiderata: 30240 | synonym: 491
# see how many rows have taxon name match
nrow(all_data8[which(!is.na(all_data8$list)),]) #30731

### CHECK UNMATCHED SPECIES, TO ADD TO SYNONYM LIST AS NECESSARY ###
check <- all_data8 %>% filter(is.na(list) &
                                !grepl("\\.|[0-9]|\\*| x$|NA|\\?|/| sp$| aff$|hybrid$| cf$| cultivar$",
                                       genus_species))
# remove names with punctuation
sort(unique(check$taxon_full_name))
# write file for checking, as desired
write.csv(sort(unique(check$taxon_full_name)), file.path(main_dir,"outputs",
                                                         "ExSitu_UnmatchedSpecies.csv"),row.names = F)

# keep only matched names
all_data9 <- all_data8 %>% filter(!is.na(list))
nrow(all_data9) #30731
unique(all_data9$hybrid) # should be NA
sort(unique(all_data9$inst_short))

#identify column names to edit end of script below, if needed
# want: taxon_full_name, filename, inst_short, submission_year
colnames(all_data9)

# compare species in the new file and old file for an institution
# if the same species are in the old and new dataset, you can just use the new
unique(all_data9[which(all_data9$inst_short=="ABG" | all_data9$inst_short=="AtlantaBG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="CBG" | all_data9$inst_short=="ChicagoBG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="JCRA" | all_data9$inst_short=="JCRaulstonArb"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="RanchoSantaAnaBG" | all_data9$inst_short=="CalBG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="SDBG" | all_data9$inst_short=="SanDiegoBG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="SBBG" | all_data9$inst_short=="SantaBarbaraBG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="SHHG" | all_data9$inst_short=="SirHarloldHillierG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="TBG" | all_data9$inst_short=="TulsaBG"),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="UCBG" | all_data9$inst_short=="UCalBGBerkeley"),c(143,34,40,35)])


# look at institutions and species in the PCNQuercus file
unique(all_data9[which(all_data9$filename=="PCNQuercus"),c(143,34,40,35)])

# once you know the institutions in the PCNQuercus file, you can check each of them to see what’s in their institution file compared to the PCNQuercus file
# if there is only “PCNQuercus” in the filename column, then there are no target species in the new dataset
# if there is another filename (e.g. BartlettArb), then those target species are in the new file
unique(all_data9[which(all_data9$inst_short=="BartlettArb" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="BartlettArb")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="ChicagoBG" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="ChicagoBG")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="DevnerBG" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="DenverBG")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="RanchoSantaAnaBG" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="RanchoSantaAnaBG")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="StarhillForestArb" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="StarhillForestArb")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="UCalBGBerkeley" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="UCalBerkeley")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="UCDavisArb" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="UCDavisArb")),c(143,34,40,35)])
unique(all_data9[which(all_data9$inst_short=="UWashingtonBG" | (all_data9$filename=="PCNQuercus" & all_data9$inst_short=="UWashingtonBG")),c(143,34,40,35)])




################################################################################
# 4. Standardize important columns
################################################################################

# keep only necessary columns
all_data10 <- all_data9 %>% dplyr::select(
  # key data
  inst_short,submission_year,species_name_acc,#target_species,
  rl_year,rl_category,
  prov_type,num_indiv,acc_num,
  # locality
  orig_lat,orig_long,locality,municipality,county,state,country,assoc_sp,
  # source
  orig_source,lin_num,coll_num,coll_name,coll_year,
  # material info
  germ_type,garden_loc,rec_as,
  # other metadata
  notes,filename,list,
  # taxon name details
  taxon_name_acc,taxon_full_name,genus,species,infra_rank,infra_name,cultivar,
  taxon_full_name_orig,taxon_full_name_concat,taxon_det)

# add institution metadata
inst_data <- read.csv(file.path(main_dir,"inputs",#"respondent_institution_data_table",
                                "respondent_institution_data_table_2019.csv"),stringsAsFactors = F)
str(inst_data)
all_data11 <- left_join(all_data10,inst_data)
str(all_data11)

##
## Provenance type
##

# look at column contents and change below phrases as needed
all_data11$prov_type <- str_to_lower(all_data11$prov_type)
sort(unique(all_data11$prov_type))
## IF NEEDED: transfer contents of one column to another column, if data
#   needs to be preserved but is in wrong place
#all_data11$notes[grep("Of known, direct wild origin - Florence County, SC.",
#all_data11$prov_type)] <- "Florence County, SC"

# standardize column by searching for keywords and replacing with standard value
# remove confusing words/phrases
all_data11$prov_type <- mgsub(all_data11$prov_type,
                              c(". accession not of wild source"), "")
# ex wild (Z)
all_data11$prov_type <- ifelse(grepl(paste(
  c("indirect","ex wild","^z$","cultivated from wild","propagatedfromwild",
    "ex W","g from w plant","c ex w"),
  collapse = "|"), all_data11$prov_type),"Z",all_data11$prov_type)
# wild (W)
all_data11$prov_type <- ifelse(grepl(paste(
  c("wild","wld","collect","^w$","^\\(w\\)$","wd","nativecoll","coll2000",
    "w\\?","native"),
  collapse = "|"), all_data11$prov_type),"W",all_data11$prov_type)
# unknown (U)
all_data11$prov_type <- ifelse(grepl(paste(
  c("^\\(u\\)$","^u$","unsure","insufficient data","unknown","c\\?"),
  collapse = "|"), all_data11$prov_type),"U",all_data11$prov_type)
# cultivated (H)
all_data11$prov_type <- ifelse(grepl(paste(
  c("cultiva","garden","^c$","^g$","^h$","horticult"),
  collapse = "|"), all_data11$prov_type),"H",all_data11$prov_type)
# not given (NG) ; everything else
all_data11$prov_type <- ifelse(all_data11$prov_type!= "W" &
                                 all_data11$prov_type != "Z" & all_data11$prov_type != "H" &
                                 all_data11$prov_type != "N" & all_data11$prov_type != "U",
                               "NG",all_data11$prov_type)
all_data11$prov_type[which(is.na(all_data11$prov_type))] <- "NG"

# check results
unique(all_data11$prov_type)
table(all_data11$prov_type)

##
## B) Number of Individuals
##

sort(unique(all_data11$num_indiv))
## IF NEEDED: replace unwanted characters
all_data11$num_indiv <- mgsub(all_data11$num_indiv,
                              c("1:1:1:1","1:1:1:1:1:1:1:1:1:1","1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1",
                                "1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1:1","3+3","2+3",
                                "A A A A A A A A A A A"),
                              c("4","10","19","28","6","5",""))
sort(unique(all_data11$num_indiv))
all_data11$num_indiv <- mgsub(all_data11$num_indiv,c("*"," in terra","+"), "")
sort(unique(all_data11$num_indiv))
# change type to numeric and replace NA with 1
all_data11$num_indiv <- as.numeric(all_data11$num_indiv)
all_data11$num_indiv[which(is.na(all_data11$num_indiv))] <- 1

# check results
sort(unique(all_data11$num_indiv))
nrow(all_data11) #30731

# remove records with no individuals
all_data11 <- all_data11[which(all_data11$num_indiv > 0),]
nrow(all_data11) #30653
all_data11[which(all_data11$orig_source == "dead"),]$orig_source <- "dead?"

##
## c) Combine duplicates (same institution and accession number)
##

all_data11$orig_acc_num <- all_data11$acc_num

# combine duplicates (same acc num)
all_data11 <- all_data11 %>%
  group_by(inst_short,acc_num) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv))) %>%
  ungroup() %>%
  distinct(inst_short,acc_num,.keep_all=T)
nrow(all_data11) #28501

# can look at what will be removed in the acc_num;
#   these patterns seem to work for all
all_data11[which(grepl("\\*",all_data11$acc_num)),]$acc_num
all_data11[which(grepl("_",all_data11$acc_num)),]$acc_num
all_data11[which(grepl("/[1-9]$",all_data11$acc_num)),]$acc_num

# remove individual-specific identifiers (to combine dup accessions)
all_data11 <- all_data11 %>%
  separate("acc_num","acc_num",
           sep="\\*|_|/[1-9]$",remove=F) %>%
  group_by(inst_short,acc_num,species_name_acc) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv))) %>%
  ungroup() %>%
  distinct(inst_short,acc_num,species_name_acc,.keep_all=T)
nrow(all_data11) #25665

# create subset of records with acc_num longer than 9 characters
#   (these are usually the ones with plant identifiers; some are missed
#    but this gets most of them)
check_accnum <- all_data11[which(nchar(all_data11$acc_num)>9),]
nrow(check_accnum) #5122
no_check_accnum <- setdiff(all_data11,check_accnum)
nrow(no_check_accnum) #20543

# can look at what will be removed in the acc_num
sort(check_accnum[which(grepl("/[0-9][1-9]$",check_accnum$acc_num)),]$acc_num)
sort(check_accnum[which(grepl("\\.[0-9][1-9]$",check_accnum$acc_num)),]$acc_num)
sort(check_accnum[which(grepl("\\.[0-9][0-9][1-9]$",check_accnum$acc_num)),]$acc_num)
sort(check_accnum[which(grepl("[A-F]$",check_accnum$acc_num)),]$acc_num)
sort(check_accnum[which(grepl("-[1-9]$",check_accnum$acc_num)),]$acc_num)
sort(check_accnum[which(grepl("-[0-9][1-9]$",check_accnum$acc_num)),]$acc_num)
#as.data.frame(check_accnum[which(grepl("A 1971-432",check_accnum$acc_num)),])

# remove individual-specific identifiers (to combine dup accessions)
check_accnum <- check_accnum %>%
  separate("acc_num","acc_num",
           sep="/[0-9][1-9]$|\\.[0-9][1-9]$|\\.[0-9][0-9][1-9]$|[A-F]$|-[1-9]$|-[0-9][1-9]$",
           remove=F) %>%
  group_by(inst_short,acc_num,species_name_acc) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv)),
         germ_type = paste(unique(germ_type), collapse="; ")) %>%
  ungroup() %>%
  distinct(inst_short,acc_num,species_name_acc,.keep_all=T)
nrow(check_accnum) #4405

all_data12 <- full_join(check_accnum,no_check_accnum)
nrow(all_data12) #24948

# look at acc_num with potential qualifiers that were not removed;
#   can fix manually if desired
#all_data12[which(grepl("\\*",all_data12$acc_num)),]$acc_num
#all_data12[which(grepl("_",all_data12$acc_num)),]$acc_num
#all_data12[which(grepl("/[1-9]$",all_data12$acc_num)),]$acc_num
all_data12[which(grepl("/[0-9][1-9]$",all_data12$acc_num)),]$acc_num
all_data12[which(grepl("\\.[0-9][1-9]$",all_data12$acc_num)),]$acc_num
all_data12[which(grepl("\\.[0-9][0-9][1-9]$",all_data12$acc_num)),]$acc_num
all_data12[which(grepl("[A-F]$",all_data12$acc_num)),]$acc_num
all_data12 <- all_data12 %>% separate("acc_num","acc_num",sep="[A-F]$",remove=F)
#as.data.frame(all_data12[which(grepl("159[A-F]",all_data12$acc_num)),])
sort(all_data12[which(grepl("-[1-9]$",all_data12$acc_num) & nchar(all_data12$acc_num)>6),]$acc_num)
all_data12[which(grepl("10796-",all_data12$acc_num)),]$acc_num <- "10796"
all_data12[which(grepl("88I54-",all_data12$acc_num)),]$acc_num <- "88I54"
all_data12[which(grepl("-[0-9][1-9]$",all_data12$acc_num)),]$acc_num

# combine duplicates one final time
all_data12 <- all_data12 %>%
  group_by(inst_short,acc_num,species_name_acc) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv)),
         germ_type = paste(unique(germ_type), collapse="; ")) %>%
  ungroup() %>%
  distinct(inst_short,acc_num,species_name_acc,.keep_all=T)
nrow(all_data12) #24919
all_data12$germ_type <- gsub("NA; NA","NA",all_data12$germ_type)

##
## ** ADD Unique ID Column
##

# add unique ID column
# create UID with institution name, acc num, provenance type, and taxon name
# also remove duplicates based on new UID and sum individuals
# first, fix up number of individuals column before summing
# now create UID and remove dups
nms <- names(all_data12)
nrow(all_data12)
all_data12 <- all_data12 %>%
  arrange(orig_lat,locality) %>%
  mutate(UID = paste(inst_short,acc_num,prov_type,taxon_name_acc,sep="~")) %>%
  group_by(UID) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv))) %>%
  distinct(UID,.keep_all=T) %>%
  ungroup() %>%
  dplyr::select(c("UID",all_of(nms)))
nrow(all_data12) #24919

##
## C) Latitude and Longitude
##

# preserve original lat and long columns
all_data12$lat_dd <- all_data12$orig_lat
all_data12$long_dd <- all_data12$orig_long

# replace comma with decimal (european notation)
all_data12$lat_dd <- mgsub(all_data12$lat_dd, c(","), ".")
all_data12$long_dd <- mgsub(all_data12$long_dd, c(","), ".")

# replace unwanted characters
## latitude
# replace random unnecessary characters
all_data12$lat_dd <- mgsub(all_data12$lat_dd,
                           c("N","\\","/","M","A",": ","E","AZ","R","d","a"," .")," ")
# remove leading zero
all_data12$lat_dd[which(grepl("^ *[0][1-9]+",all_data12$lat_dd))] <- gsub(
  "^ *[0]","",all_data12$lat_dd[which(grepl("^ *[0][1-9]+",all_data12$lat_dd))])
all_data12$lat_dd[which(grepl("^S *[0][1-9]+",all_data12$lat_dd))] <- gsub(
  "^S *[0]","-",all_data12$lat_dd[which(grepl("^S *[0][1-9]+",all_data12$lat_dd))])
# add negative sign if south and remove "S"
all_data12$lat_dd[grep("S",all_data12$lat_dd,ignore.case=T)] <-
  paste("-",all_data12$lat_dd[grep("S",all_data12$lat_dd,ignore.case=T)],sep="")
all_data12$lat_dd <- gsub("S","",all_data12$lat_dd)
all_data12$lat_dd <- gsub("--","-",all_data12$lat_dd)
# remove double spaces or leading/trailing whitespace
all_data12$lat_dd <- str_squish(all_data12$lat_dd)
#sort(unique(all_data12$lat_dd))
# check source of specific values that aren't formatted correctly
#all_data12[which(all_data12$lat_dd == "422538"),]
## longitude
all_data12$long_dd <- replace_non_ascii(all_data12$long_dd,
                                        replacement=" ", remove.nonconverted=T)
all_data12$long_dd <- mgsub(all_data12$long_dd,
                            c("E","\\","/","NR","d","A","a"," .","o","O")," ")
all_data12$long_dd[which(grepl("^ *[0][1-9]+",all_data12$long_dd))] <- gsub(
  "^ *[0]","",all_data12$long_dd[which(grepl("^ *[0][1-9]+",all_data12$long_dd))])
all_data12$long_dd[which(grepl("^W *[0][1-9]+",all_data12$long_dd))] <- gsub(
  "^W *[0]","-",all_data12$long_dd[which(grepl("^W *[0][1-9]+",
                                               all_data12$long_dd))])
all_data12$long_dd[grep("W",all_data12$long_dd,ignore.case=T)] <-
  paste("-",all_data12$long_dd[grep("W",all_data12$long_dd,ignore.case=T)],sep="")
all_data12$long_dd <- gsub("W","",all_data12$long_dd)
all_data12$long_dd <- mgsub(all_data12$long_dd,c("--","- "),"-")
all_data12$long_dd <- str_squish(all_data12$long_dd)
#sort(unique(all_data12$long_dd))

# convert decimal-minutes-seconds (dms) to decimal degrees (dd)
#   [d, m, and s must be in the same cell, with 1 space between each value]
#   format = ## ## ## (DMS) OR ## ##.### (DM)
# mark rows that need to be converted
convert <- all_data12[which(grepl(" ",all_data12$lat_dd) |
                              grepl(" ",all_data12$long_dd)),]
nrow(convert) #451
unique(convert$lat_dd)
good <- anti_join(all_data12, convert)
# separate by dec_min_sec and deg_dec_min then convert to decimal degrees
# latitude
dms <- convert[which(str_count(convert$lat_dd," ") == 2),]; nrow(dms)
ddm <- convert[which(str_count(convert$lat_dd," ") == 1),]; nrow(ddm)
other <- convert[which((str_count(convert$lat_dd," ") != 1 &
                          str_count(convert$lat_dd," ") != 2) | is.na(str_count(convert$lat_dd," "))),]
nrow(other)
dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec',
                                     to = 'dec_deg')
ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min',
                                     to = 'dec_deg')
convert <- rbind(dms,ddm,other); nrow(convert)
# longitude
dms <- convert[which(str_count(convert$long_dd," ") == 2),]; nrow(dms)
ddm <- convert[which(str_count(convert$long_dd," ") == 1),]; nrow(ddm)
other <- convert[which((str_count(convert$long_dd," ") != 1 &
                          str_count(convert$long_dd," ") != 2) | is.na(str_count(convert$long_dd," "))),]
nrow(other)
dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec',
                                      to = 'dec_deg')
ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min',
                                      to = 'dec_deg')
convert <- rbind(dms,ddm,other); nrow(convert) #455; 348
# join everything back together
all_data12 <- rbind(good,convert); nrow(all_data12) #28593; 24912

# check validity of lat and long
all_data12$lat_dd <- as.numeric(all_data12$lat_dd)
#sort(unique(all_data12$lat_dd))
all_data12$long_dd <- as.numeric(all_data12$long_dd)
#sort(unique(all_data12$long_dd))
# if coords are both 0, set to NA
zero <- which(all_data12$lat_dd == 0 & all_data12$long_dd == 0)
all_data12$lat_dd[zero] <- NA; all_data12$long_dd[zero] <- NA
# flag non-numeric and not available coordinates and lat > 90, lat < -90,
# lon > 180, and lon < -180
coord_test <- cc_val(all_data12, lon = "long_dd",lat = "lat_dd",
                     value = "flagged", verbose = TRUE) #Flagged 20670 records.
# try switching lat and long for invalid points and check validity again
all_data12[!coord_test,c("lat_dd","long_dd")] <-
  all_data12[!coord_test,c("long_dd","lat_dd")]
coord_test <- cc_val(all_data12,lon = "long_dd",lat = "lat_dd",
                     value = "flagged",verbose = TRUE) #Flagged 20663 records.
# make coords NA if they are still flagged
all_data12[!coord_test,"lat_dd"] <- NA
all_data12[!coord_test,"long_dd"] <- NA

# check if geolocated points are in water and mark
world_polygons <- ne_countries(type = 'countries', scale = 'medium')
geo_pts <- all_data12 %>% filter(!is.na(lat_dd) & !is.na(long_dd))
in_water <- geo_pts[is.na(map.where(world_polygons,
                                    geo_pts$long_dd,geo_pts$lat_dd)),]
nrow(in_water)
all_data12$flag <- ""
all_data12[which(all_data12$UID %in% in_water$UID),]$flag <-
  "Given lat-long is in water"
table(all_data12$flag) #30
#all_data12[which(all_data12$UID %in% in_water$UID),]$lat_dd <- NA
#all_data12[which(all_data12$UID %in% in_water$UID),]$long_dd <- NA

# mark lat-long for records with same inst lat-long and wild lat-long
all_data12$lat_round <- round(all_data12$lat_dd,digits=1)
all_data12$long_round <- round(all_data12$long_dd,digits=1)
all_data12$inst_lat_round <- round(all_data12$inst_lat,digits=1)
all_data12$inst_long_round <- round(all_data12$inst_long,digits=1)
garden_latlong <- all_data12 %>% filter(lat_round == inst_lat_round &
                                          long_round == inst_long_round & prov_type != "N")
unique(garden_latlong$inst_short)
nrow(garden_latlong)
all_data12[which(all_data12$UID %in% garden_latlong$UID),]$flag <-
  "Given lat-long is at institution, use only if native to grounds"
#all_data12[all_data12$UID %in% garden_latlong$UID,]$lat_dd <- NA
#all_data12[all_data12$UID %in% garden_latlong$UID,]$long_dd <- NA
table(all_data12$flag) #804

# add country-level information to check if lat-long in right spot
# create SpatialPointsDataFrame
proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
geo_pts_spatial <- SpatialPointsDataFrame(geo_pts[,c("long_dd",
                                                     "lat_dd")], geo_pts, proj4string = CRS(proj4string4poly))
# add country polygon data to each point based on lat-long location
load(file.path(main_dir, "inputs", "gis_data", "admin_shapefiles.RData"))
geo_pts <- point.in.poly(geo_pts_spatial, adm0.poly, sp=TRUE)@data
# try switching lat and long for points in Antarctica
geo_pts[which(geo_pts$country.name == "Antarctica"),c("lat_dd","long_dd")]<-
  geo_pts[which(geo_pts$country.name == "Antarctica"),c("long_dd","lat_dd")]
# round 2: add country-level information to check if lat-long in right spot
geo_pts <- geo_pts %>%
  dplyr::select(-country.name,-country.iso_a2,-country.iso_a3,-country.continent)
geo_pts_spatial <- SpatialPointsDataFrame(geo_pts[,c("long_dd",
                                                     "lat_dd")], geo_pts, proj4string = CRS(proj4string4poly))
geo_pts <- point.in.poly(geo_pts_spatial, adm0.poly, sp=TRUE)@data
geo_pts <- geo_pts %>% dplyr::select(UID,country.name) %>%
  rename(latlong_country = country.name)
all_data12 <- full_join(all_data12,geo_pts)

# add gps_det (gps determination) column
all_data12$gps_det <- NA
all_data12$gps_det[which(all_data12$prov_type == "H")] <- "H"
all_data12$gps_det[which(!is.na(all_data12$lat_dd) &
                           !is.na(all_data12$long_dd))] <- "G"
table(all_data12$gps_det)
#     G     H
#  4249  4546

# where prov_type is "H" but lat-long is given, change to "H?"
# create new prov type column
all_data12$orig_prov_type <- all_data12$prov_type
all_data12$prov_type[which(all_data12$gps_det == "G" &
                             all_data12$prov_type == "H")] <- "H?"
table(all_data12$prov_type)

##
## D) Collection year
##

unique(all_data12$coll_year)
## IF NEEDED: replace non-year words/characters
#all_data12$coll_year <- mgsub(all_data12$coll_year,
#  c("([0-9]+);","about ","ca.","Unknown","original","Estate","estate"),"")
#all_data12$coll_year[which(all_data12$coll_year == "")] <- NA

# remove extra elements so its just year
all_data12$coll_year <- gsub(";[1-2][0-9][0-9][0-9]","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[0-9][0-9]/[0-9][0-9]/","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[0-9]/[0-9][0-9]/","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[0-9][0-9]/[0-9]/","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[0-9]/[0-9]/","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[1-9] [A-Z][a-z][a-z] ","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[A-Z][a-z][a-z] ","",all_data12$coll_year)
#all_data12$coll_year <- gsub("[1-9]-[A-Z][a-z][a-z]-","",all_data12$coll_year)

# make column numeric
all_data12$coll_year <- as.numeric(all_data12$coll_year)
## IF NEEDED: add first two numbers in year
#  # assume 2000s if values is less than 21
#all_data12$coll_year[which(all_data12$coll_year < 10)] <-
#  paste0("200",as.character(all_data12$coll_year[which(all_data12$coll_year < 10)]))
#all_data12$coll_year <- as.numeric(all_data12$coll_year)
#all_data12$coll_year[which(all_data12$coll_year < 21)] <-
#  paste0("20",as.character(all_data12$coll_year[which(all_data12$coll_year < 21)]))
#all_data12$coll_year <- as.numeric(all_data12$coll_year)
#  # assume 1900s if values is greater than or equal to 21
#all_data12$coll_year[which(all_data12$coll_year < 100)] <-
#  paste0("19",as.character(all_data12$coll_year[which(all_data12$coll_year < 100)]))
#all_data12$coll_year <- as.numeric(all_data12$coll_year)
sort(unique(all_data12$coll_year))

##
## E) Lineage number
##

# remove lin_num when same as acc_num
all_data12[which(all_data12$acc_num == all_data12$lin_num),]$lin_num <- NA

##
## F) Locality
##

# create all_locality column
all_data12$latitude <- round(all_data12$lat_dd,digits=3)
all_data12$longitude <- round(all_data12$long_dd,digits=3)
all_data12 <- unite(all_data12, "all_locality",
                    c(locality,municipality,county,state,country,orig_source,#notes,
                      lin_num,coll_num,coll_name,coll_year,
                      latitude,longitude),sep = " | ",remove = F)
# remove NA in concatenated locality column
all_data12$all_locality <- gsub("NA","",all_data12$all_locality)
# if no locality info at all, make it NA
all_data12$all_locality[which(all_data12$all_locality ==
                                " |  |  |  |  |  |  |  |  |  |  | ")] <- NA

before_dup_removal <- all_data12[,c(1:48,53:54)]
write.csv(before_dup_removal, file.path(main_dir,"outputs",
                                        paste0("ExSitu_Compiled_DupsNotCondensed_", Sys.Date(), ".csv")),row.names = F)

##
## SELECT AND ORDER FINAL COLUMNS
##

all_data12 <- as.data.frame(lapply(all_data12, function(x) str_squish(x)),
                            stringsAsFactors=F)
all_data12 <- as.data.frame(lapply(all_data12, function(x) gsub(",",";",x)),
                            stringsAsFactors=F)

all_data13 <- all_data12 %>%
  ### combine duplicates at all_locality level
  group_by(inst_short,species_name_acc,prov_type,all_locality) %>%
  mutate(
    UID = paste(UID, collapse="|"),
    notes = paste(unique(notes), collapse="; "),
    assoc_sp = paste(unique(assoc_sp), collapse="; "),
    acc_num = paste(acc_num, collapse="|"),
    sum_num_indiv = sum(as.numeric(num_indiv)),
    germ_type = paste(unique(germ_type), collapse="; "),
    garden_loc = paste(unique(garden_loc), collapse="; "),
    rec_as = paste(unique(rec_as), collapse="; "),
    taxon_det = paste(unique(taxon_det), collapse="; "),
    taxon_name_acc = paste(unique(taxon_name_acc), collapse="; "),
    taxon_full_name = paste(unique(taxon_full_name), collapse="; "),
    taxon_full_name_orig = paste(unique(taxon_full_name_orig), collapse="; "),
    taxon_full_name_concat = paste(unique(taxon_full_name_concat), collapse="; "),
    cultivar = paste(unique(cultivar), collapse="; "),
    sum_num_acc = n()) %>%
  ungroup() %>%
  distinct(inst_short,species_name_acc,prov_type,all_locality,.keep_all=T) %>%
  dplyr::select(
    # grouping data
    inst_short,species_name_acc,prov_type,all_locality,
    # key data
    UID,gps_det,flag,lat_dd,long_dd,
    # locality
    locality,municipality,county,state,country,latlong_country,
    orig_source,notes,orig_lat,orig_long,assoc_sp,
    # source
    acc_num,lin_num,coll_num,coll_name,coll_year,
    # material info
    sum_num_indiv,sum_num_acc,germ_type,garden_loc,rec_as,taxon_det,
    # taxon name
    list,taxon_name_acc,taxon_full_name,genus,
    taxon_full_name_orig,taxon_full_name_concat,cultivar,
    # species metadata
    rl_year,rl_category,
    # institution metadata
    inst_name,inst_country,inst_lat,inst_long,filename)
nrow(all_data13) #13867
head(as.data.frame(all_data13))

# write file
write.csv(all_data13, file.path(main_dir,"outputs",
                                paste0("ExSitu_Compiled_Standardized_", Sys.Date(), ".csv")),row.names = F)










### NOT USING YET/CURRENTLY ###


##
## RENAME FOR GEOLOCATE AND (optionally.. SPLIT BY SPECIES)
##

# FIRST CHECK TO BE SURE THIS IS ZERO !!
all_data12[which(grepl("\\|",all_data12$acc_num)),]

# add GEOLocate standard columns
all_data12$correction.status <- NA
all_data12$precision <- NA
all_data12$error.polygon <- NA
all_data12$multiple.results <- NA
all_data12$uncertainty <- NA

all_data14 <- all_data12 %>%
  # filter to remove cultivated records and those without locality info
  #filter(rl_category == "CR" | rl_category == "EN" |
  #       rl_category == "VU" | rl_category == "NT") %>%
  #filter(prov_type != "H") %>%
  #filter(!is.na(all_locality)) %>%
  # rename to GEOLocate standard columns
  rename(locality.string = all_locality) %>%
  # order with NA lat-long records on top
  arrange(locality.string) %>%
  arrange(!is.na(latitude),latitude) %>%
  # replace NA with "" to make simpler to view in GEOLocate
  replace(., is.na(.), "") %>%
  # group by all non-ID fields
  group_by(
    locality.string,country,state,county,latitude,longitude,
    flag,gps_det,prov_type,lin_num,coll_num,coll_name,coll_year,
    inst_short,filename,inst_lat,inst_long,
    list,species_name_acc,taxon_full_name) %>%
  # concatenate values in ID fields
  mutate(
    UID = paste(UID, collapse="|"),
    acc_num = paste(acc_num, collapse="|"),
    sum_num_indiv = sum(as.numeric(num_indiv))) %>%
  ungroup() %>%
  # remove duplicates
  distinct(
    locality.string,country,state,county,latitude,longitude,
    flag,gps_det,prov_type,lin_num,coll_num,coll_name,coll_year,
    inst_short,filename,inst_lat,inst_long,
    list,species_name_acc,taxon_full_name,
    .keep_all=T) %>%
  # reorder columns
  dplyr::select(
    ## GeoLocate
    locality.string,country,state,county,latitude,longitude,
    correction.status,precision,error.polygon,multiple.results,uncertainty,
    ## record metadata
    flag,gps_det,prov_type,acc_num,lin_num,coll_num,coll_name,coll_year,sum_num_indiv,
    ## institituion metadata
    inst_short,filename,inst_lat,inst_long,
    ## taxon name & record ID
    list,species_name_acc,taxon_full_name,UID) %>%
  # rename concatenated fields to make that clear
  rename(
    acc_num_CAT = acc_num,
    UID_CAT = UID)

# remove dot in column names (replace with space) for GEOLocate
names(all_data14) <- gsub(x = names(all_data14),pattern = "\\.",
                          replacement = " ")
str(all_data14)
head(as.data.frame(all_data14),n=30)

# write file
write.csv(all_data14, file.path(main_dir,"outputs","to_geolocate",
                                paste0("To_Geolocate_CR-EN-VU-NT_", Sys.Date(), ".csv")),row.names = F)


# create one CSV for each target species
sp_split <- split(all_data14, as.factor(all_data14$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(!dir.exists(file.path(main_dir,"outputs","to_geolocate")))
  dir.create(file.path(main_dir,"outputs","to_geolocate"),
             recursive=T)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
                                                  file.path(main_dir,"outputs","to_geolocate",
                                                            paste0(names(sp_split)[[i]], ".csv")),row.names = F))




### Read geolocated CSVs back in and add geolocate info to rest of data

# read in geolocated CSVs
file_list <- list.files(
  path=file.path(main_dir,"Compiled ex situ data","Geolocated_CSV_by_target_species",target_genus),
  pattern=".csv",full.names=TRUE)
file_dfs <- lapply(file_list,read.csv,header=TRUE,fileEncoding="LATIN1",
                   strip.white=TRUE,stringsAsFactors=F,na.strings=c("","NA"))
# check that geolocated CSVs each have same number of rows as inital export;
# if they don't, you'll need to manually see where the mistake is
for(i in 1:length(sp_split)){
  for(j in 1:nrow(sp_split[[i]])){
    if(sp_split[[i]]$inst_short[j] !=file_dfs[[i]]$inst_short[j]){
      print(sp_split[[i]]$species_name_acc[1])
    } else {
      print(i)
    }
  }
}

# bind geolocated CSVs together
post_geo <- Reduce(rbind.fill, file_dfs)
# fix a few inconsistencies
## provenance type column
unique(post_geo$prov_type)
# check "H?" rows to see if should be "W" and
# if all are "X" gps_det, change prov_type to "H"
post_geo[which(post_geo$prov_type == "H?"),]
post_geo[which(post_geo$prov_type == "H?"),]$prov_type <- "H"
# check prov_type for rows with coordinates
unique(post_geo[which(!is.na(post_geo$latitude)),]$prov_type)
post_geo[which(!is.na(post_geo$latitude) & post_geo$prov_type == "U"),]
## gps determination column
unique(post_geo$gps_det)
post_geo[which(is.na(post_geo$gps_det)),]$latitude
post_geo[which(is.na(post_geo$gps_det)),]$gps_det <- "X"
## uncertainty column
post_geo$uncertainty <- gsub(" m","",post_geo$uncertainty)
post_geo[which(post_geo$uncertainty == "0"),]$uncertainty <- NA
post_geo$uncertainty <- as.numeric(post_geo$uncertainty)
## lat and long
sort(unique(post_geo$latitude))
sort(unique(post_geo$longitude))

# keep only edited columns
geolocated <- post_geo %>%
  rename(UID = UID_CAT) %>%
  dplyr::select(country,state,county,latitude,longitude,precision,
                uncertainty,gps_det,prov_type,UID) #multiple.results
head(as.data.frame(geolocated),n = 40)

## for Quercus, because geolocated before changed UID system
# bind itial export together, to join UID to geolocated data
#pre_geo <- Reduce(rbind.fill, sp_split)
#pre_geo <- pre_geo %>% dplyr::select(UID_CAT)
# add UID to geolocated rows and separate to accession level again
#geolocated <- cbind(post_geo,pre_geo)

# separate to accession level again
geolocated2 <- separate_rows(geolocated, UID, sep="\\|")
geolocated2 <- separate_rows(geolocated2, UID, sep="; ")
geolocated2 <- geolocated2 %>%
  rename(lat_dd = latitude, long_dd = longitude,
         coord_precision = uncertainty)

# see if all UIDs are matching up
yes_geo <- all_data13 %>% filter(UID %in% geolocated2$UID)
# should be character(0)
setdiff(geolocated2$UID,yes_geo$UID)
# add gelocated rows to data
yes_geo <- yes_geo %>% dplyr::select(-prov_type,-gps_det,-lat_dd,
                                     -long_dd,-county,-state,-country,-coord_precision)
yes_geo <- full_join(yes_geo,geolocated2)
# bind all data together (geolocated and garden origin)
no_geo <- all_data13 %>% filter(!(UID %in% geolocated2$UID))
no_geo$precision <- NA
all_data15 <- rbind(yes_geo,no_geo)

# make gps_det "X" if NA -- not doing this now to distinquish records
#   that have been checked to see if can geolocate versus those that haven't
#all_data15[which(is.na(all_data15$gps_det)),]$gps_det <- "X"
unique(all_data15$gps_det)

# arrange columns
all_data15 <- all_data15 %>%
  arrange(species_name_acc,UID) %>%
  rename(gps_notes = flag) %>%
  dplyr::select(
    # key data
    UID,inst_short,submission_year,species_name_acc,target_species,
    prov_type,gps_det,lat_dd,long_dd,coord_precision,precision,gps_notes,
    all_locality,
    # locality
    locality,municipality,county,state,country,latlong_country,
    orig_lat,orig_long,
    orig_source,notes,assoc_sp,habitat,num_indiv,acc_num,
    # source
    lin_num,coll_num,coll_name,coll_year,
    # material info
    germ_type,garden_loc,rec_as,condition,name_determ,
    # other metadata
    dataset_year,private,filename,list,
    # taxon name
    taxon_name_acc,taxon_full_name,genus,species,infra_rank,infra_name,
    taxon_full_name_orig,taxon_full_name_concat,cultivar,trade_name,
    # institution metadata
    inst_name,inst_country,inst_lat,inst_long)
all_data15[which(all_data15$gps_notes == ""),]$gps_notes <- NA
str(all_data15)

# write file
write.csv(all_data15, file.path(main_dir,"Compiled ex situ data",
                                "ALL DATA - POST GEOLOCATE",
                                paste0(target_genus,"_POSTGEO_exsitu_compiled_standardized.csv")),
          row.names = F)

# create one CSV for each target species
all_data_target <- all_data15 %>% filter(target_species == "Y")
sp_split2 <- split(all_data_target,
                   as.factor(all_data_target$species_name_acc))
names(sp_split2) <- gsub(" ","_",names(sp_split2))

# write files
if(!dir.exists(file.path(main_dir,"Compiled ex situ data",
                         "ALL DATA - POST GEOLOCATE","target_species")))
  dir.create(file.path(main_dir,"Compiled ex situ data",
                       "ALL DATA - POST GEOLOCATE","target_species"),recursive=T)
lapply(seq_along(sp_split2), function(i) write.csv(sp_split2[[i]],
                                                   file.path(main_dir,"Compiled ex situ data","ALL DATA - POST GEOLOCATE",
                                                             "target_species",
                                                             paste0(names(sp_split2)[[i]], "_ALL_POSTGEO.csv")),row.names = F))












### TESTING GEONAMES PACKAGE ###

# read in compiled ex situ data
df <- read.csv(file.path(main_dir,"outputs",
                         "exsitu_compiled_standardized_2021-02-11_firstpassGpsDet.csv"),
               header = T, colClasses="character")
str(df)
unique(df$gps_det)
tail(df[which(df$gps_det == "L"),])

# create login: https://www.geonames.org/login
# https://www.geonames.org/manageaccount
#   the account is not yet enabled to use the free web services. Click here to enable.
usethis::edit_r_environ()
devtools::install_github("ropensci/geonames")
library(geonames)

# https://rstudio-pubs-static.s3.amazonaws.com/489236_0259d8532a354ad6945d818bc4c052f1.html

login <- read_lines(log_loc)
username  <- login[4]
options(geonamesUsername = username)
head(GNsearch(name = "Chicago"))
#  adminCode1       lng geonameId               toponymName countryId fcl population countryCode                      name           fclName adminCodes1.ISO3166_2   countryName                                      fcodeName adminName1      lat fcode
#1         IL -87.65005   4887398                   Chicago   6252001   P    2720546          US                   Chicago city, village,...                    IL United States seat of a second-order administrative division   Illinois 41.85003 PPLA2
#2         IL -87.89062   6955104 Chicago-Naperville-Joliet   6252001   L    9569624          US Chicago-Naperville-Joliet   parks,area, ...                    IL United States                                economic region   Illinois 41.70778  RGNE
#3         IL  -87.6979  12070033 Chicago metropolitan area   6252001   L    9472676          US Chicago metropolitan area   parks,area, ...                    IL United States                                         region   Illinois  41.8902   RGN
#4         OH -82.72629   5176830                   Willard   6252001   P       6063          US                   Willard city, village,...                    OH United States                                populated place       Ohio 41.05311   PPL
#5         IL -87.69644   4887463              Chicago Lawn   6252001   P      55551          US              Chicago Lawn city, village,...                    IL United States                     section of populated place   Illinois 41.77503  PPLX
#6         IL -87.55425   4911863             South Chicago   6252001   P      28095          US             South Chicago city, village,...                    IL United States                                populated place   Illinois 41.73977   PPL

lanc_coords <- lanc_df[1, c("lng", "lat")]