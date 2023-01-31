
 
occ_data <- "/Users/kateschowe/Desktop/Occurrences_Compiled_2023-01-24.csv"
manual_edits <- "/Volumes/GoogleDrive/My Drive/Franklinia/Mesoamerican Oak Gap Analysis 2023/occurrence_points/inputs/manual_point_edits.xlsx - manual_point_edits.csv"

occ_data <- as.data.frame(occ_data)
manual_edits <- as.data.frame(manual_edits)

  ## (optinally) check document with manual point edits to see if anything needs 
  ##  to be removed or added back in
if(!missing(manual_edits)){
    for(i in 1:length(unique(occ_data$taxon_name_acc))){
      target_taxon <- unique(occ_data$taxon_name_acc)[i]
      print(paste0("Checking manual edits for ",target_taxon))
      taxon_edits <- as.data.frame(manual_edits[which(
        manual_edits$taxon_name_acc == target_taxon),])
      # remove if in bounding box
      # bounding box is in format: lat-max, long-min, lat-min, long-max 
      #  (i.e. the SE US would be something like 34, -101, 25, -77)
      #if(!is.na(taxon_edits$bounding_box)){
        #bounds <- unlist(strsplit(taxon_edits$bounding_box,"; "))
        #for(j in 1:length(bounds)){
          #within <- unlist(strsplit(bounds[j],", "))
          #remove <- occ_fltr %>%
            #dplyr::filter(
              #taxon_name_accepted == target_taxon &
                #(decimalLatitude < as.numeric(within[1]) &
                   #decimalLongitude > as.numeric(within[2]) &
                   #decimalLatitude > as.numeric(within[3]) &
                   #decimalLongitude < as.numeric(within[4])))
          #occ_fltr <- occ_fltr %>%
            #dplyr::filter(!(UID %in% unique(remove$UID)))
          #print(paste0("--Removed ", nrow(remove),
                       #" points based on bounding box"))
        #}
      #}
      # remove if ID listed in remove_id
      if(!is.na(taxon_edits$remove)){
        remove <- unlist(strsplit(taxon_edits$remove,"; "))
        occ_fltr <- occ_fltr %>% dplyr::filter(!(UID %in% remove))
        print(paste0("--Removed ", length(remove),
                     " points based on IDs to remove"))
      }
      # add back if ID listed in keep_id
      if(!is.na(taxon_edits$keep)){
        keep <- unlist(strsplit(taxon_edits$keep,"; "))
        add <- occ_fltr %>% dplyr::filter(UID %in% keep)
        occ_fltr <- suppressMessages(full_join(occ_fltr,add))
        print(paste0("--Added back ", length(keep),
                     " points based on IDs to keep"))
      }
    }
  }
  
  ## you can also add additional edits for specific taxa
  # for example, here we add the geographic outliers back in for Carya ovata:
  add <- occ_data %>% 
    dplyr::filter(taxon_name_accepted == "Carya ovata" & !.outl)
  occ_fltr <- suppressMessages(full_join(occ_fltr,add))
  # another example, we remove anything that is Prunus glandulosa (a bad
  #  synonym):
  occ_fltr <- occ_fltr %>% 
    dplyr::filter(taxon_name != "Prunus glandulosa")
  
  return(occ_fltr)
  
}