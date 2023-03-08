library(tidyverse)

# Specify parameters for checks
select.COHORT <- "ADMCI" # Options: ADMCI, ALS, FTD, PD, VCI
VISIT <- "01" # Options: 01, 04, 06, 08
path.filelist.csv <- "OND01_ADMCI_01_NIMG_DEFACEDPDT2_2022DEC21_DATAPKG/DEFACEDPDT2s/DATAFILES/OND01_ADMCI_01_NIMG_DATAFILES_2022DEC07_FILELIST.csv"
path.filenames <- "OND01_ADMCI_01_NIMG_DEFACEDPDT2_2022DEC21_DATAPKG/DEFACEDPDT2s/DATAFILES/"
path.missing <- "OND01_ADMCI_01_NIMG_DEFACEDPDT2_2022DEC21_DATAPKG/OND01_ADMCI_01_NIMG_DEFACEDPDT2_2022DEC21_MISSING.csv" # if no missing file exists, set to ""

# Read in the list of files in the DATAFILES folder, FILELIST.csv, valid ids, and missing file (if it exists). Then, clean the bids-friendly file names so they are formatted to conventional ONDRI IDs
filelist.dat <- read_csv(path.filelist.csv)

valid_ids <- read_csv("VALID_IDS_COHORTS_FOR_DISTRIBUTION_2020JAN06.csv")

filelist.cleaned <- filelist.dat %>% 
  mutate(FILENAME = str_sub(FILENAME, 5, 16)) %>% 
  mutate(FILENAME = paste0(str_sub(FILENAME,1,5),
                           "_",
                           str_sub(FILENAME,6,8),
                           "_",
                           str_sub(FILENAME,9,12)))

filenames <- list.files(path.filenames)[!str_detect(list.files(path.filenames), "FILELIST.csv")]
filenames.subject.ids <- paste0(str_sub(filenames, 5,9),
                                "_",
                                str_sub(filenames, 10,12),
                                "_",
                                str_sub(filenames, 13,16)
)

if(path.missing != ""){
  missing <- read_csv(path.missing)
}

# Check Subject IDs in file names contain valid subject IDs
if(!all(filenames.subject.ids %in% valid_ids$SUBJECT)){
  cat("ERROR DETECTED: The following subject IDs in the DATAFILES folder are not present in the valid IDs file and should be removed:\n",
  paste0(unique(filenames.subject.ids[which(!filenames.subject.ids %in% valid_ids$SUBJECT)]), sep="\n")
  )
}else(
  cat("PASSED: All participant IDs in the file names are included in the valid IDs file")
)

# Subject IDs that are not valid for the current COHORT and VISIT
if(VISIT == "01"){
  visit_colname <- "INCLUDE_IN_SCREENING_BASELINE"
}else {
  if(VISIT == "04"){
    visit_colname <- "INCLUDE_IN_YEAR_1"
  } else{
    if(VISIT == "06"){
      visit_colname <- "INCLUDE_IN_YEAR_2"
    } else{
      if(VISIT == "08"){
        visit_colname <- "INCLUDE_IN_YEAR_3"
      } else{
        cat("ERROR: Visit code not recognized")
      }}
  }}


ids_for_cohort_visit <- valid_ids %>% 
  filter(COHORT == select.COHORT) %>% 
  filter(!!sym(visit_colname) == "YES") %>% 
  pull(SUBJECT)

if(length(setdiff(filenames.subject.ids, ids_for_cohort_visit)) > 0){
  cat("ERROR DETECTED: The following subject IDs in the DATAFILES are not present in the Valid ID file for the", select.COHORT, "cohort and visit", VISIT, ":\n",
      paste0(unique(setdiff(filenames.subject.ids, ids_for_cohort_visit)), sep = "\n"))
}else{
  cat("PASSED: Subject IDs in the DATAFILES are all eligible for this cohort and visit")
}


# File name IDs match what is listed in the FILELIST?
mismatch.filenames.filelist <- c(setdiff(filenames.subject.ids, filelist.cleaned$SUBJECT),
setdiff(filelist.cleaned$SUBJECT, filenames.subject.ids) # in SUBJECT column of filelist.csv but not in the file names
)

if(length(mismatch.filenames.filelist) > 0){
  cat("ERROR DETECTED: The following subject IDs are NOT found in both the DATAFILES folder and FILELIST.csv:\n",
      paste0(mismatch.filenames.filelist), sep="\n")
} else{
  cat("PASSED: The DATAFILES subject IDs match the subject IDs found in FILELIST.csv")
}

# FILELIST entries match the subject IDs in the filenames (compare columns within filelist.csv )
mismatch.filelist.ids <- filelist.cleaned %>% 
  mutate(check = SUBJECT == FILENAME) %>% 
  filter(check == FALSE)

if(nrow(mismatch.filelist.ids) > 0 ){
  cat("ERROR DETECTED: The following subject IDs are mismatched within FILELIST.csv. Please check the following participants in the SUBJECT column of FILELIST.csv:\n",
      paste0(mismatch.filelist.ids, sep="\n"))
}else{
  cat("PASSED: All subject IDs within FILELIST.csv match across the SUBJECT and FILENAME columns")
}

# Does MISSING file and DATAFILES$SUBJECT match VALID IDS
if(path.missing != ""){
  missing.ids <- setdiff(ids_for_cohort_visit, unique(c(filenames.subject.ids, missing$SUBJECT)))
  
}else{
  missing.ids <- setdiff(ids_for_cohort_visit, unique(filenames.subject.ids))
}

if(length(missing.ids) > 0){
  cat("ERROR DETECTED: The following subject IDs are expected but not found in the DATAFILES folder or MISSING file:\n",
      paste0(unique(missing.ids), sep = "\n"))
}else{
  cat("PASSED: All subject IDs for this cohort and visit have been accounted for in the DATAFILES folder or MISSING file")
}
