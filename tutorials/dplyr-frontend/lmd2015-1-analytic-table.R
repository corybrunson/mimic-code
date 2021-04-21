#' ---
#' title: "Variable extraction for personalized mortality prediction"
#' author:
#'   - "Evelyn Nitch-Griffin"
#'   - "Amy Peterson"
#'   - "Yara Skaf"
#'   - "Jason Cory Brunson"
#' date: "`r format(Sys.time(), '%Y %B %d')`"
#' output:
#'   #html_document
#'   md_document
#'   #pdf_document:
#'   #  keep_tex: true
#' ---
#' 
#' This script generates an analytic table (a "tidy" data frame of one row per admission) of the variables used by Lee, Maslove, and Dubin (2015), both to compute their patient similarity measure and to build generalized regression and machine learning models of clinical outcomes for ICU patients. The authors did not share their code or describe their variable definitions in a completely reproducible way; moreover, they used the earlier MIMIC-II database, whereas our analysis uses MIMIC-III. Our table, and any analyses performed on it, will therefore differ from those obtained by the original authors.
#' 
#' ## Setup
#' 
#' ### Database connection
#' 
#' The first code chunk connects to the local MIMIC-III database instance, using the credentials suggested in the [PhysioNet tutorial](https://mimic.physionet.org/tutorials/install-mimic-locally-ubuntu/). The helper function `tbl_mimic()` queries tables from the database and returns a "lazy" query table (of class "tbl_dbi"). When printed, it queries the database until it can print the first few rows. Printing is minimized, especially toward the end of the tutorial, to reduce runtime.
#' 
## ----connect to local instance-------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# library
library(tidyverse)
library(dbplyr)
library(RPostgres)
#library(devtools)
#library(widyr)
#library(progress)
# connect to the database
mimic <- dbConnect(
  Postgres(),
  #dbname = "mimicdemo",
  dbname = "mimic",
  host = "localhost",
  port = 5432,
  user = "mimicuser",
  password = "mimic"
)
# prepare query table without loading data into memory
tbl_mimic <- function(table) {
  tbl(mimic, dbplyr::in_schema("mimiciii", table))
}

#' 
#' ### Script parameters
#' 
#' `study_units` is a vector of the care units to include when building the analytic file. Only admissions that originate in these units will be included.
#' 
## ----analysis parameters-------------------------------------------------
# which units to include
#study_units <- c("CCU", "CSRU", "MICU", "SICU", "TSICU")
study_units <- c("CCU", "CSRU")

#' 
#' ## Variables
#' 
#' We begin with the list of variables described by Lee &al. We are only interested in the results during the first 24 hours in the ICU. The variables then fall into several categories: categorical, extrema (minima and maxima) over non-overlapping 6-hour periods, extrema over the 24-hour period, and sums over the 6-hour periods. We abbreviate the periods "N6".
#' 
#' ### Categorical variables
#' 
#' * Age
#' * Gender
#' * Admission Type (elective, urgent, emergency)
#' * ICU type (which we restrict to certain units in the chunk above.)
#' * Primary ICD-code
#' * Receipt of vasopressure therapy (T/F)
#' * Use of mechanical Ventilation or Continuous Positive Airway Pressure (T/F)
#' 
#' ### Vitals min/max each N6
#' 
#' * heart rate
#' * mean blood pressure
#' * systolic blood pressure
#' * SpO_2
#' * spontaneous respiratory rate
#' * body temperature
#' 
#' ### Labs min/max 24 Hours
#' 
#' * hematocrit
#' * white blood cell count
#' * serum glucose
#' * serum HCO_3
#' * serum potassium
#' * serum sodium
#' * blood urea nitrogen
#' * serum creatine
#' 
#' ### Measurement totals each N6
#' 
#' * Urine Foley sums every N6
#' 
#' ### Final variables
#' 
#' * Minimum Glasgow Coma Scale Value
#' 
#' ### Master table
#' 
#' For convenience, we organize the variables and the `itemid` fields from which we calculate them into a single master table, also including coded and manual labels. The reason is twofold: First, some measurements are spread across multiple `itemid`s, such as mean blood pressure being recorded as "manual bp" and "arterial BP". Second, MIMIC-III is derived from two EHR databases, due to Beth Israel Deaconess Medical Center having swtiched from CareVue to MetaVision in 2008. This means that any measurement may correspond to two different `itemid`s in the `d_items` table, one for each database. Since no master list links these `itemid`s, we performed separate investigations to determine which ones represent the same measurement.
#' 
#' The laboratory items in the `d_labitems` table derive from a different system and are copied into the `chartevents` table. Technically, then, every lab item corresponds to 3 different `itemid`s. We use only the `itemid` of each lab item, which removes ambiguity and allows us to source their values directly from the `d_labitems` table.
#' 
#' Ventilation is not included in the master list because some specific `itemid`s need corrections. There is also a significant number of them, and there are therefore instead included in a later code chunk.
#' 
## ----Master Variable Table-----------------------------------------------
itemid_links <- tribble(
  ~name, ~itemid, ~linksto, ~type,
  "heart rate", 211, "chart", "N6",
  "heart rate", 220045, "chart", "N6",
  "mean BP", 52, "chart", "N6",
  "mean BP", 456, "chart", "N6",
  "mean BP", 220052, "chart", "N6",
  "mean BP", 220181, "chart", "N6",
  "temp f", 678, "chart", "N6",
  "temp f", 679, "chart", "N6", #Taken from a calculated Celsius temp
  "temp f", 223761, "chart", "N6",
  "temp f", 223762, "chart", "N6", #Actually a Celsius measurement
  "systolic BP", 51, "chart", "N6",
  "systolic BP", 455, "chart", "N6",
  "systolic BP", 220050, "chart", "N6",
  "systolic BP", 220179, "chart", "N6",
  "SPO2", 646, "chart", "N6",
  "SPO2", 220277, "chart", "N6",
  #"spont resp rate",         ,"chart", "N6",
  #"spont resp rate",         ,"chart", "N6",
  "urine foley", 40055, "output", "SN6",
  "GCS", 184, "chart", "min",
  "GCS", 454, "chart", "min",
  "GCS", 723, "chart", "min",
  "GCS", 220739, "chart", "min",
  "GCS", 223900, "chart", "min",
  "GCS", 223901, "chart", "min",
  "urine foley", 40055, "output", "sumN6",
  "urine foley", 226559, "output", "sunN6",
  "dopamine", 30043, "input", "vaso",
  "dopamine", 30307, "input", "vaso", #Drip, few measurements
  "dopamine", 221662, "input", "vaso",
  "dobutamine", 30042, "input", "vaso",
  "dobutamine", 30306, "input", "vaso", #Drip
  "dobutamine", 221653, "input", "vaso",
  "norepinephrine", 30120, "input", "vaso",
  "norepinephrine", 30120, "input", "vaso",
  "norepinephrine", 221906, "input", "vaso",
  "vasopressin", 30051, "input", "vaso",
  "vasopressin", 222315, "input", "vaso",
  "phenylephrine", 30127, "input", "vaso",
  "phenlyephrine", 30128, "input", "vaso",
  "phenlyephrine", 221749, "input", "vaso",
  "epinephrine", 30044, "input", "vaso",
  "epinephrine", 30119, "input", "vaso",
  "epinephrine", 30309, "input", "vaso", #Drip
  "epinephrine", 221289, "input", "vaso"
)
itemid_lab_links <- tbl_mimic("d_labitems") %>% 
  select(itemid, label) %>%
  collect() %>% 
  right_join(
    tribble(
      ~name, ~itemid, ~linksto, ~type,
      "hematocrit", 51221, "lab", "MM24",
      "WBC", 51301, "lab", "MM24",
      "glucose", 50931, "lab", "MM24",
      "HCO3", 50882, "lab", "MM24",
      "potassium", 50971, "lab", "MM24",
      "sodium", 50983, "lab", "MM24",
      "blood urea nitrogen", 51006, "lab", "MM24",
      "creatinine", 50912, "lab", "MM24"
    ), by = "itemid"
  )
tbl_mimic("d_items") %>% 
  select(itemid, label) %>%
  collect() %>%
  right_join(itemid_links, by = "itemid") %>% 
  full_join(itemid_lab_links) %>%
  print() -> master_table
#Create a master joining table for quicker syntax
master_join <- select(master_table, itemid, name)

#' 
#' ## Initialization (Patient- and admission-level data)
#' 
#' Each patient may have more than one admission event, and each admission may include more than one ICU stay. Though we may refer to patients, admissions, or just subjects, the ICU stays will be our observational units.
#' We assemble a stay-level table from the `transfers` table in MIMIC-III, join admissions-level variables from the `admissions` and `diagnoses_icd` tables, and join patient-level variables obtained directly from the `patients` table or, in the case of race/ethnicity, reconciled from the `admissions` table.
#' We include the time each patient was admitted into each ICU, a stay-level variable needed to calculate the N6 periods, and the time they left each ICU, needed to calculate the readmission outcome.
#' 
## ----Study Admissions Table----------------------------------------------
#SHOULD THE ANCHOR TABLE BE `study_stays` RATHER THAN `study_admissions`?
#The time each patient was admitted to the ICU specifically
tbl_mimic("transfers") %>%
  #Only units of interest
  filter(curr_careunit %in% study_units) %>%
  select(icustay_id, hadm_id, intime, outtime) %>%
  group_by(hadm_id, icustay_id) %>%
  #For each admission, find the earliest time the patient was transferred to the
  #specific ICU (removes duplicates). It is possible that the patient was
  #admitted to another unit and transferred; we want the time they arrived in
  #the ICU unit of interest.
  summarise(
    intime = min(intime, na.rm = TRUE),
    outtime = max(outtime, na.rm = TRUE)
  ) %>%
  ungroup() ->
  study_admissions
#Admission time for each (distinct) admission
tbl_mimic("admissions") %>%
  select(
    subject_id, hadm_id, admission_type,
    #Dischare time 'dischtime' is not currently used. If specifically looking
    #for readmission to the hospital, as opposed to just the ICU, it could
    #possibly be useful in the future.
    admittime, dischtime, #deathtime,
    hospital_expire_flag
  ) %>%
  #Removes repeating items; should be irrelevant since 'hadm_id' is supposed to
  #be unique.
  distinct() %>%
  #Join to form global admissions table
  left_join(study_admissions, by = c("hadm_id")) ->
  study_admissions

#' 
## ----Initialization of Patient Table-------------------------------------
#Aquire an age for each "patient". (Include gender for later joining.)
tbl_mimic("patients") %>%
  select(subject_id, dob, gender) %>%
  #Organized by 'subject_id' only
  #right_join(admission_time, by = c("subject_id")) %>%
  inner_join(select(study_admissions,
                    subject_id, hadm_id, admittime, admission_type),
             by = c("subject_id")) %>%
  mutate(age = date_part("year", admittime) - date_part("year", dob)) %>%
  #Adapted from dplyr frontend tutorial
  mutate(age = age - ifelse(
    date_part("month", admittime) < date_part("month", dob) |
      (
        date_part("month", admittime) == date_part("month", dob) &
          date_part("day", admittime) < date_part("day", dob)
      ),
    1,
    0
  )) %>%
  #No longer need 'dob'; will use 'intime' in favor of 'admittime'
  select(-dob, -admittime) %>%
  #Ages 90 and above are artificially inflated
  filter(age < 90) ->
  #inner_join(study_admissions, by = c("hadm_id")) ->
  #REMOVE `arrange()` STEP TO AVOID `ORDER BY` WARNING -Cory
  #arrange(hadm_id) ->
  study_cats

#' 
#' We recode 'ethnicity' values to a coarser classification scheme (with fewer classes). We reconcile different values recorded for the same patient by selecting the most common value from this coarser scheme.
#' 
## ----Ethnicity-----------------------------------------------------------
tbl_mimic("admissions") %>%
  select(subject_id, ethnicity) %>%
  semi_join(study_cats, by = "subject_id") %>%
  distinct() ->
  subject_ethnicity
unknown_ethnicity <- c(
  "UNABLE TO OBTAIN",
  "UNKNOWN/NOT SPECIFIED",
  "PATIENT DECLINED TO ANSWER",
  "UNKNOWN"
)
other_ethnicity <- c(
  "OTHER",
  "MULTI RACE ETHNICITY",
  "PORTUGUESE",
  "MIDDLE EASTERN",
  "SOUTH AMERICAN"
)
subject_ethnicity %>%
  rename(eg = ethnicity) %>%
  mutate(eg = ifelse(str_detect(eg, "ASIAN"), "Asian", eg)) %>%
  mutate(eg = ifelse(str_detect(eg, "BLACK"), "Black", eg)) %>%
  mutate(eg = ifelse(str_detect(eg, "CARIBBEAN"), "Black", eg)) %>%
  mutate(eg = ifelse(str_detect(eg, "HISPANIC"), "Hispanic", eg)) %>%
  mutate(eg = ifelse(str_detect(eg, "AMERICAN INDIAN"), "Native", eg)) %>%
  mutate(eg = ifelse(str_detect(eg, "PACIFIC ISLANDER"), "Pacific", eg)) %>%
  mutate(eg = ifelse(str_detect(eg, "WHITE"), "White", eg)) %>%
  mutate(eg = ifelse(eg %in% unknown_ethnicity, "Unknown", eg)) %>%
  mutate(eg = ifelse(eg %in% other_ethnicity, "Other", eg)) %>%
  rename(ethnicity = eg) ->
  subject_ethnicity
# determine best guess for ethnicity (earliest recorded)
subject_ethnicity %>%
  filter(! is.na(ethnicity)) %>%
  group_by(subject_id) %>%
  mutate(n = n()) %>%
  slice_max(order_by = n) %>%
  #REPLACE `arrange()` STEP TO AVOID `ORDER BY` WARNING -Cory
  #group_by(subject_id, ethnicity) %>%
  #arrange(desc(n)) %>%
  #group_by(subject_id) %>%
  #mutate(ethnicity = first(ethnicity)) %>%
  ungroup() %>%
  select(subject_id, ethnicity) %>%
  distinct() ->
  subject_ethnicity
# join into categorical variables table
study_cats %>%
  left_join(subject_ethnicity, by = c("subject_id")) ->
  study_cats

#' 
#' We grab the primary ICD-9-CM code for each patient admission. This is the last categorical variable, so the result is joined with the admissions info table to get a final table of categorical variables.
#' 
## ----primary-diagnosis---------------------------------------------------
# primary diagnosis assigned to each patient and hospital admission
tbl_mimic("diagnoses_icd") %>%
  # restrict to primary diagnosis
  filter(seq_num == 1L) %>%
  select(hadm_id, icd9_code) %>%
  right_join(study_cats, by = c("hadm_id")) ->
  #COLLECT -Cory
  study_cats

#' 
#' ## Binary Variables
#' 
#' The analysis includes two binary variables:
#' 
#' - Whether the patient received vasopressor therapy
#' - Whether the patient was mechanically ventilated or received a CPAP (continuous positive airway pressure)
#' 
#' The next two code chunks construct binarized variables based on the appearances of vasopressor and ventilation/CPAP codes in the record of each ICU stay.
#' Vasopressor therapy will have been recorded in the two "input events" tables (`inputevents_cv` and `inputevents_mv`), while ventilation/CPAP will have been recorded in the `chartevents` table.
#' The `*_cv` and `*_mv` tables correspond to the CareVue and MetaVision EHR databases, and the databases use different timing variables to determine when doses were administered.
#' We're only interested in whether or not the patient received any vasopressor therapy, so we can mostly ignore the differences, but technically there may be some discrepancies:
#' For CareVue, we determine that any 'charttime' that includes a rate or amount wthin the first 24 hours as an instance of vasopressor therapy.
#' For MetaVision, we use the 'starttime' entry instead, and remove entries with the "Rewritten" tag.
#' These times are determined slightly differently within each database.
#' 
## ----Vasopressor Binarization--------------------------------------------
vaso_itemid <- pull(filter(master_table, type == "vaso"), itemid)
tbl_mimic("inputevents_cv") %>% 
  select(hadm_id, subject_id, icustay_id, itemid, charttime, amount,  rate) %>% 
  #For efficiency, combine with the MV table and reformat it so they join
  #smoothly
  full_join(
    tbl_mimic("inputevents_mv") %>% 
      select(hadm_id, subject_id, icustay_id, itemid, starttime,
             amount, rate, statusdescription) %>% 
      #indicates entry was written over bc errors, so remove for the following
      #code to work, turn starttime into charttime
      filter(statusdescription != "Rewritten") %>%
      mutate(charttime = starttime) %>% 
      select(-statusdescription, -starttime)
  ) %>%
  #Pull out all the vasopressor items
  filter(itemid %in% !! vaso_itemid) %>% 
  #If everything is NA, remove it
  filter(! is.na(amount) & ! is.na(rate)) %>% 
  #Combine with admissions, using only relevant columns
  right_join(
    select(study_admissions, subject_id, hadm_id, icustay_id, intime), 
           by = c("hadm_id", "subject_id", "icustay_id")) %>%
  #Remove all measurements past 24 hrs
  #But keep measurements with no charttime; these are patients with no
  #vasopressor therapy
  filter(
    (date_part("days", charttime - intime) == 0 &
       date_part("hours", charttime -intime) > 0) |
      is.na(charttime)
  ) %>%
  #Remaining table will be study_admission patients joined with vaso events
  #Patients who have events receive a 1, patients who don't get a -1
  mutate(vaso = if_else(is.na(amount), 0L, 1L)) %>% 
  #For each admission we take the maximum across the binarization to reduce down
  #to a single variable, 1 or 0
  group_by(icustay_id) %>% 
  summarise(vaso = max(vaso, na.rm = TRUE)) ->
  #COLLECT -Cory
  study_vaso

#' 
## ----Ventilation Binarization--------------------------------------------
#If a patient received any of the following measurements (with some
#qualifications), we say they were ventilated.
vent_type = c(720, 223848) #value must not be 'Other/Remarks' or 'Other'
vent_mode = c(722, 223849)
vent = c(467) #value must be 'Ventilator'
gcodes1 <- c(
  ## minute volume
  445, 448, 449, 450, 1340, 1486, 1600, 224687
  ## tidal volume
  , 639, 654, 681, 682, 683, 684, 224685, 224684, 224686
  ## High/Low/Peak/Mean/Neg insp force ("RespPressure")
  , 218, 436, 535, 444, 459, 224697, 224695, 224696, 224746, 224747
  ## Insp pressure
  , 221, 1, 1211, 1655, 2000, 226873, 224738, 224419, 224750, 227187
  ## PlateauPressure
  , 543
  ## APRV pressure
  , 5865, 5866, 224707, 224709, 224705, 224706
  ## PEEP
  , 60, 437, 505, 506, 686, 220339, 224700
  ## high pressure relief
  , 3459
  ## PCV
  , 501, 502, 503, 224702
  ## TCPCV
  , 223, 667, 668, 669, 670, 671, 672
  ## PSVlevel
  , 224701
)
#Ventilation
tbl_mimic("chartevents") %>% 
  select(hadm_id, subject_id, icustay_id, itemid, value, charttime) %>%
  filter(itemid %in% c(vent_type, vent_mode, vent, gcodes1)) %>% 
  #Combine with admissions, using only relevant columns
  right_join(
    select(study_admissions, subject_id, hadm_id, icustay_id, intime), 
           by = c("hadm_id", "subject_id", "icustay_id")) %>% 
  #Remove all measurements past 24 hrs
  #Keep any NA chartimes
  filter(
    (date_part("days", charttime - intime) == 0 &
    date_part("hours", charttime -intime) > 0) |
      is.na(charttime)
  ) %>%
  mutate(mech_vent = if (is.na(value)) {
    0L
    #WHY OMIT SECOND 'vent_mode' VALUE? -Cory
  } else if (itemid %in% c(720, 223848, 722) &
             value %in% c("Other", "Other/Remarks")) {
    0L
  } else if (itemid == 467 & ! value == "Ventilator") {
    0L
  } else {
    1L
  }) %>% 
  select(-itemid, -value) %>% 
  group_by(icustay_id) %>% 
  summarise(mech_vent = max(mech_vent, na.rm = TRUE)) ->
  #COLLECT -Cory
  study_vent

#' 
#' ## Min/Max 6-hour intervals
#' 
#' Next, we grab the relevant items from the `chartevents` table, then calculate their minimum and maximum values over each 6-hour interval, following their 'intime' in the `study_admissions` table.
#' 
## ----Min/Max for N6 Intervals--------------------------------------------
n6_itemid <- pull(filter(master_table, type == "N6"), itemid)
tbl_mimic("chartevents") %>%
  select(subject_id, hadm_id, icustay_id,
         itemid, charttime, value, valuenum) %>%
  filter(itemid %in% !! n6_itemid) %>% 
  #Combine with admissions, using only relevant columns
  right_join(
    select(study_admissions, subject_id, hadm_id, icustay_id, intime), 
           by = c("hadm_id", "subject_id", "icustay_id")) %>%
  #We need to convert Celsius measurements to Farenheight
  mutate(valuenum = if_else(itemid == 223762, valuenum*1.8 + 32, valuenum)) %>% 
  #label every itemid with the manual label
  left_join(master_join, by = "itemid", copy = TRUE) %>% 
  #Remove all measurements past 24 hrs
  filter(
    date_part("days", charttime - intime) == 0,
    date_part("hours", charttime -intime) > 0
  ) %>%
  #Separate measurements into four 6 hour intervals
  mutate(period = floor(date_part("hours", charttime - intime)/6) + 1) %>%
  group_by(icustay_id, name, period) %>%
   summarise(
    min = min(valuenum, na.rm = TRUE),
    max = max(valuenum, na.rm = TRUE)
  ) %>% 
  ungroup() ->
  #COLLECT -Cory
  study_charts

#' 
#' Our script for the `labevents` table should basically be the same, except we do not need a period adjustment.
#' Lab events are not taken by 'icustay_id'; must use 'hadm_id' and 'subject_id';
#' this must be noted in future joins.
#' 
## ----Lab Values----------------------------------------------------------
mm24_itemid <- pull(filter(master_table, type == "MM24"), itemid)
tbl_mimic("labevents") %>%
  select(subject_id, hadm_id, itemid, charttime, value, valuenum) %>%
  filter(itemid %in% !! mm24_itemid) %>% 
  #Combine with previous table, using only relevant columns
  right_join(
    select(study_admissions, subject_id, hadm_id, intime), 
           by = c("hadm_id", "subject_id")) %>%
  #label every itemid with the manual label
  left_join(master_join, by = "itemid", copy = TRUE) %>% 
  #Remove all measurements past 24 hrs
  filter(
    date_part("days", charttime - intime) == 0,
    date_part("hours", charttime -intime) > 0
  ) %>%
  group_by(hadm_id, name) %>% 
   summarise(
    min = min(valuenum, na.rm = TRUE),
    max = max(valuenum, na.rm = TRUE)
  ) %>% 
  ungroup() ->
  #COLLECT -Cory
  study_labs

#' 
#' Urine Foley measurements come from the output table:
#' 
## ----Output Events (Urine)-----------------------------------------------
sum_n6_itemid <- pull(filter(master_table, type == "sumN6"), itemid)
tbl_mimic("outputevents") %>%
  select(subject_id, hadm_id,icustay_id, itemid, charttime, value) %>%
  filter(itemid %in% !! sum_n6_itemid) %>% 
  #Combine with previous table, using only relevant columns
  right_join(
    select(study_admissions, subject_id, hadm_id, icustay_id, intime), 
           by = c("hadm_id", "subject_id", "icustay_id")) %>%
  #label every itemid with the manual label
  left_join(master_join, by = "itemid", copy = TRUE) %>% 
  #Remove all measurements past 24 hrs
  filter(
    date_part("days", charttime - intime) == 0,
    date_part("hours", charttime -intime) > 0
  ) %>%
  #Separate measurements into four 6 hour intervals
  mutate(period = floor(date_part("hours", charttime - intime)/6) + 1) %>%
  group_by(icustay_id, name, period) %>% 
   summarise(
    sum = sum(value, na.rm = TRUE)
  ) %>% 
  ungroup() ->
  #COLLECT -Cory
  study_output

#' 
#' Lastly, we need the minimum GCS values:
#' 
## ----GCS-Minimums--------------------------------------------------------
min_itemid <- pull(filter(master_table, type == "min"), itemid)
tbl_mimic("chartevents") %>%
  select(subject_id, hadm_id, itemid, icustay_id,
         charttime, value, valuenum) %>%
  filter(itemid %in% !! min_itemid) %>% 
  #Combine with previous table, using only relevant columns
  right_join(
    select(study_admissions, subject_id, hadm_id, icustay_id, intime), 
           by = c("hadm_id", "subject_id", "icustay_id")) %>%
  #label every itemid with the manual label
  left_join(master_join, by = "itemid", copy = TRUE) %>% 
  #Remove all measurements past 24 hrs
  filter(
    date_part("days", charttime - intime) == 0,
    date_part("hours", charttime -intime) > 0
  ) %>%
  group_by(icustay_id, name) %>% 
   summarise(
    min_GCS = min(valuenum, na.rm = TRUE)
  ) %>% 
  ungroup() ->
  #COLLECT -Cory
  study_GCS

#' 
#' ## Outcome Variables
#' 
#' In order to predict mortality (or readmission), we need to know whether or not the patient has actually died, or been readmitted. The following chunks determine whether or not the patient died within 30 days of being admiited or discharged, and whether or not they had been readmitted within 30 days.
#' 
## ----30d-Readmission-----------------------------------------------------
study_admissions %>% 
  select(hadm_id, subject_id, icustay_id, intime) %>% 
  group_by(subject_id) %>% 
  #Count the number of appearances of each subject
  summarise(count = count(subject_id)) %>% 
  ungroup() %>% 
  #Remove people who only showed up once
  filter(count > 1) %>%
  #select(-count) %>% 
  #join to `study_admissions` so we only take the people we found above
  inner_join(
    select(study_admissions, hadm_id, subject_id, icustay_id, outtime),
    by = "subject_id") %>% 
  #The above table contains each distinct ICU admission for patients who were
  #admitted more than once
  #We join to this table all the 'intimes' by 'subject_id'
  #So, for each distinct admission, we have their discharge time, and all past
  #and present intimes.
  left_join(select(study_admissions, subject_id, intime), by = "subject_id") %>%
  #arrange(hadm_id) %>% #for easier visuliazation
  #Remove all past values, and entries over 30 days, leaving a table with each
  #readmission
  #Table has 'outtime', followed any later 'intimes'
  filter(
    date_part("days", intime - outtime) < 30,
    date_part("days", intime - outtime) > 0,
    date_part("hours", intime - outtime) > 0
  ) %>% 
  mutate(readm_30d = 1) %>% 
  select(hadm_id, icustay_id, readm_30d) %>% 
  distinct() -> #remove repeating values
  #COLLECT -Cory
  readmissions

#' 
## ----Mortality-Code------------------------------------------------------
tbl_mimic("patients") %>%
  select(subject_id, dod) %>%
  #right join takes dod and adds it to study_admissions
  right_join(
    select(study_admissions, subject_id, hadm_id, icustay_id, intime, outtime),
    by = "subject_id"
  ) %>% 
  #Calculates days to death from intime and ICU outtime
  mutate(dayfromad = date_part("days", dod - intime)) %>% 
  mutate(dayfromdis = date_part("days", dod - outtime)) %>% 
  #Converts to T/F for time based mortality
  mutate(add_mort30d = ! is.na(dayfromad) & dayfromad <= 30) %>% 
  mutate(disch_mort30d = ! is.na(dayfromad) & dayfromad <= 30) %>% 
  #mutate(add_mort180d = ! is.na(dayfromad) & dayfromad <= 180) %>%
  #mutate(disch_mort180d = ! is.na(dayfromad) & dayfromad <= 180) %>% 
  select(-dod, -intime, -outtime, -dayfromad, -dayfromdis) %>% 
  distinct() %>% 
  filter(! is.na(hadm_id) | ! is.na(icustay_id)) %>%
  left_join(readmissions, by = c("hadm_id", "icustay_id")) %>%
  #Convert everything to 0s and 1s
  #Note as.numeric does not work on query tables
  mutate(
    add_mort30d = if_else(add_mort30d == TRUE, 1L, 0L),
    disch_mort30d = if_else(disch_mort30d == TRUE, 1L, 0L),
    readm_30d = if_else(! is.na(readm_30d) == TRUE, 1L, 0L)
  ) ->
  #COLLECT -Cory
  outcome_events

#' 
#' ## Final Analytic Table
#' 
#' The above tables need to all be converted to wide tables and then joined.
#' 
## ----6-hour extrema to wide table----------------------------------------
study_charts %>%
  mutate(period = paste0("p", period)) %>%
  as_tibble() %>%
  pivot_wider(names_from = c(name, period), values_from = c(min, max)) ->
  charts_wide

#' 
## ----Labs to wide--------------------------------------------------------
study_labs %>% 
  as_tibble() %>% 
  pivot_wider(names_from = name, values_from = c(min, max)) ->
  labs_wide

#' 
## ----Output wide---------------------------------------------------------
study_output %>%
  mutate(period = paste0("p", period)) %>%
  as_tibble() %>%
  pivot_wider(names_from = c(name, period), values_from = sum) %>% 
  #Some periods may have no measurements; for tough bladders or other reason
  #When widening, these will show up as NA, so we replace them with 0
  replace(is.na(.), 0) ->
  output_wide

#' 
#' Combine and then write to file. Note that the vasopressor and ventilation tables only include a single variable each, and don't need to be widened.
#' 
## ----Combine wide tables-------------------------------------------------
#Begin with the admissions table (tibbling it) and add the chunks in order
as_tibble(study_admissions) %>%
  inner_join(as_tibble(study_cats),
             by = c("subject_id", "hadm_id", "admission_type")) %>%
  inner_join(charts_wide, by = "icustay_id") %>% 
  #Recall: labs don't have icustay_id
  inner_join(labs_wide, by = "hadm_id") %>% 
  inner_join(output_wide, by = "icustay_id") %>% 
  #The GCS table did not need to be made wide, so we need to tibble it here.
  inner_join(as_tibble(select(study_GCS, -name)), by = "icustay_id") %>% 
  inner_join(as_tibble(study_vent), by = "icustay_id") %>% 
  inner_join(as_tibble(study_vaso), by = "icustay_id") %>% 
  inner_join(as_tibble(outcome_events),
             by = c("hadm_id", "subject_id", "icustay_id")) %>% 
  drop_na() ->
  analytic_wide
#Inspect (collected) table to file
#write_rds(analytic_wide, here::here("data", "analytic-wide.rds"))
print(analytic_wide)

