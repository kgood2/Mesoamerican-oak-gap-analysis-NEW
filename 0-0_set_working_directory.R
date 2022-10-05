################################################################################

## 0-0_set_workingdirectory.R
### Authors: Shannon Still & Emily Beckman Bruns
### Date: 05/21/2020 ; Updated October 2022 for Mesoamerican oak gap analysis

### DESCRIPTION:
# This script sets the working environment for the computer on which you are
#   working.

################################################################################
# Set working environment depending on your computer
################################################################################

# Use this to check your "nodename"
# Sys.info()[4]

## For Emily Beckman Bruns:
if (Sys.info()[4] == "Africa.local") {
  # set main working directory
  main_dir <- "/Volumes/GoogleDrive-103729429307302508433/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"
  # set location of scripts
  script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "./Desktop/*work"
  # set location for personal login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "IMLS_passwords.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## For Kate Good home computer:
} else if (Sys.info()[4] == "XXXXXXXX") {
  # set main working directory
  main_dir <- "XXXXXXXX"
  # set location of scripts
  script_dir <- "XXXXXXXX"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "XXXXXXXX"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "XXXXXXXX.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## For Kate Good work computer:
} else if (Sys.info()[4] == "XXXXXXXX") {
  # set main working directory
  main_dir <- "XXXXXXXX"
  # set location of scripts
  script_dir <- "XXXXXXXX"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "XXXXXXXX"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "XXXXXXXX.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## can add as many additional "else if" sections as needed to cover other
#   workstations

} else {
  # default, which sets the working driectory as the folder from which you
  #   opened the scripts/project
  setwd(getwd())
  print("You should add your info to the 0-0_set_workingdirectory.R script so
    this line automatically sets up all the working directories you'll be using!")
}
