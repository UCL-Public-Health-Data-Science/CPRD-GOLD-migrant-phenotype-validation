# 01_Description ---------------------------------------------------------------------------

# Cleaning and analysis for CPRD GOLD migration phenotype validation 
# Date started: 03/02/2020
# Author: Neha Pathak

# 02_Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Hmisc)

# 03_Data cleaning --------------------------------------------------------

## Import data -----------------------------------------------------------------------

# Cohort files 
all_patients <- read_csv("filepath/all_patients.csv") # full cohort file (Jan 2019 build) extracted from CPRD
migrants <- read_tsv("filepath/migrants.tsv") # subset of cohort file extracted from CPRD, includes migrants only

# Events files
migration <- read_csv("filepath/migration.csv")
ethnicity <- read_csv("filepath/ethnicity.csv")

# Linked data 
patient_imd <- read_tsv("filepath/patient_imd2015.txt")
practice_imd <- read_tsv("filepath/practice_imd2015.txt")


## Explore raw data -------------------------------------------------------------------

# Migrants
glimpse(migrants)
names(migrants)
summary(migrants)
head(migrants)
tail(migrants)

# All patients
glimpse(all_patients)
names(all_patients)
summary(all_patients)
head(all_patients)
tail(all_patients)

# Migration
glimpse(migration)
names(migration)
summary(migration)
head(migration)
tail(migration)

# Ethnicity
glimpse(ethnicity)
names(ethnicity)
summary(ethnicity)
head(ethnicity)
tail(ethnicity)

# Patient IMD
glimpse(patient_imd)
names(patient_imd)
summary(patient_imd)
head(patient_imd)
tail(patient_imd)

# Practice IMD
glimpse(practice_imd)
names(practice_imd)
summary(practice_imd)
head(practice_imd)
tail(practice_imd)

# Visualise numeric variables as histogram

## Migrants
hist(migrants$patid)
hist(migrants$pracid)
hist(migrants$vmid)
hist(migrants$gender)
hist(migrants$yob)
hist(migrants$mob)
hist(migrants$marital)
hist(migrants$famnum)
hist(migrants$chsreg)
hist(migrants$prescr)
hist(migrants$capsup)
hist(migrants$regstat)
hist(migrants$reggap)
hist(migrants$internal)
hist(migrants$toreason)
hist(migrants$accept)

## All patients
hist(all_patients$patid)
hist(all_patients$pracid)
hist(all_patients$prac_region)
hist(all_patients$gender)
hist(all_patients$toreason)
hist(all_patients$data_in)
hist(all_patients$eligible)
hist(all_patients$hes16)
hist(all_patients$death16)
hist(all_patients$cancer16)
hist(all_patients$lsoa16)
hist(all_patients$mh16)
hist(all_patients$hes_e7)
hist(all_patients$death_e7)
hist(all_patients$cr_e7)
hist(all_patients$lsoa_e7)
hist(all_patients$mh_e7)

# Migration
hist(migration$patid)
hist(migration$constype)
hist(migration$consid)
hist(migration$medcode)
hist(migration$staffid)
hist(migration$episode)
hist(migration$enttype)
hist(migration$adid)
hist(migration$category)

# Ethnicity
hist(ethnicity$patid)
hist(ethnicity$constype)
hist(ethnicity$consid)
hist(ethnicity$medcode)
hist(ethnicity$staffid)
hist(ethnicity$episode)
hist(ethnicity$enttype)
hist(ethnicity$adid)
hist(ethnicity$category)

# patient imd
hist(patient_imd$patid)
hist(patient_imd$pracid)
hist(patient_imd$imd2015_5)

# practice imd
hist(practice_imd$pracid)
hist(practice_imd$e2015_imd_5)

## Tidy data  -----------------------------------------------------------------------

# Migrants

## Check for duplicate observations (whole row and patid) 
n_distinct(migrants) == count(migrants)
n_distinct(migrants) == n_distinct(distinct(migrants, patid, .keep_all = TRUE)) 

## Change variables to the correct data type based on CPRD GOLD data specification & mapping
tidy_migrants <-  migrants %>% 
  mutate_if(is.numeric, as.integer) %>%
  mutate_if(is.character, ymd)
tidy_migrants$gender <- factor(tidy_migrants$gender, levels = c(0,1,2,3,4), 
                               labels = c("Data Not Entered", "Male", "Female", "Indeterminate", "Unknown"))
tidy_migrants$mob <- factor(tidy_migrants$mob, levels = c(0:12), labels = c("Data Not Entered",
                                                                            "January", "February", "March", "April", "May", "June", "July", "August", "September", 
                                                                            "October", "November", "December"))
tidy_migrants$marital <- factor(tidy_migrants$marital, levels = c(0:11), labels = c("Data Not Entered",
                                                                                    "Single", "Married", "Widowed", "Divorced", "Separated", "Unknown", "Engaged", "Co-habiting", "Remarried", 
                                                                                    "Stable relationship", "Civil partnership"))
tidy_migrants$prescr <- factor(tidy_migrants$prescr, levels = c(0:21), labels = c("Data Not Entered",
                                                                                  "Under 16 years of age", "16, 17 or 18 and in full-time education", "Woman aged 60 or over", "Man aged 60 or over",
                                                                                  "Has a maternity/medical exemption certificate", "Has a prescription prepayment certificate", 
                                                                                  "Receives Income Support/Family credit et", "Has a War Pension exemption certificate", "Not Exempt",
                                                                                  "Get Disability Working Allowance", "Receives Income-based Jobseeker's Allowance", 
                                                                                  "Is named on a current HC2 charges certificate", "Was prescribed a free-of-charge contraceptive", 
                                                                                  "Has a maternity exemption certificate", "Has a medical exemption certificate", "Receives Income Support", 
                                                                                  "Has WTFC exemption or gets full or reduced WFTC", "Has DPTC exemption or get full or reduced DPTC", 
                                                                                  "Aged 60 or over", "Entitled to/has a valid NHS Tax Credit Exemption Certificate", 
                                                                                  "Has a partner who gets Pension Credit guarantee credit PCGC"))
tidy_migrants$capsup<- factor(tidy_migrants$capsup, levels = c(0,1,2,3,4), 
                              labels = c("Data Not Entered", "Low", "Medium", "High", "Not Applicable"))
tidy_migrants$toreason <- factor(tidy_migrants$toreason, levels = c(0:34), labels = c("Data Not Entered",
                                                                                      "Death", "Removal to new TP/HB/CSA", "Internal Transfer", "Mental Hospital", "Embarkation", 
                                                                                      "New TP/HB/CSA/Same GP", "Adopted Child", "Services", "Deduction at GP's Request", 
                                                                                      "Registration Cancelled","Service Dependant",  "Deduction at Patient's Request", "Other reason",
                                                                                      "Enlistment", "Institution", "Transfer within Practice", "Linkage", "Untraced - Miscellaneous", 
                                                                                      "Untraced - Immig", "Untraced - GP Resign", "Untraced - College", "Untraced - outwith Practice", 
                                                                                      "Untraced - outwith HB", "Multiple Transfer", "Intra-consortium transfer", "Returned Undelivered",
                                                                                      "Internal Transfer - Address Change", "Internal Transfer within Partnership", 
                                                                                      "Correspondence states gone away", "Practice advise outside their area", 
                                                                                      "Practice advise patient no longer resident", "Practive advise removal via screening system",
                                                                                      "Practice advise removal via vaccination data", "Removal from Residential Institute"))
tidy_migrants$accept <- factor(tidy_migrants$accept, levels = c(0,1), 
                              labels = c("unacceptable", "acceptable"))


## Check correct coercion of variable classes
glimpse(tidy_migrants)

## Check factors correctly labelled
levels(tidy_migrants$gender)
levels(tidy_migrants$mob)
levels(tidy_migrants$marital)
levels(tidy_migrants$prescr)
levels(tidy_migrants$capsup)
levels(tidy_migrants$toreason)
levels(tidy_migrants$accept)
levels(tidy_migrants$migcertainty)

## Find missing values
any(is.na(tidy_migrants))
sum(is.na(tidy_migrants))
summary(is.na(tidy_migrants)) # All NAs related to chs date, tod, and death date therefore do not remove

## Look for outliers and obvious errors
summary(tidy_migrants)

## Look at categorical variables for outliers and obvious errors (where previous command did not print)
summary(tidy_migrants$mob)
summary(tidy_migrants$marital)
summary(tidy_migrants$prescr)
summary(tidy_migrants$toreason)

# All patients

## Check for duplicates (whole row and patid only)
n_distinct(all_patients) == count(all_patients) 
n_distinct(distinct(all_patients, patid)) == count(all_patients)

## Check new variables and classes
glimpse(all_patients)

## Change variables to the correct data type based on CPRD GOLD data specification & mapping
tidy_all_patients <-  all_patients %>% mutate_if(is.numeric, as.integer) 
tidy_all_patients$prac_region <- factor(tidy_all_patients$prac_region, levels = c(1:13), labels = c("North East",
                                                                                                    "North West", "Yorkshire & The Humber", "East Midlands", "West Midlands", "East of England", 
                                                                                                    "South West", "South Central", "London", "South East Coast", "Northern Ireland", "Scotland", "Wales"))
tidy_all_patients$gender <- factor(tidy_all_patients$gender, levels = c(0,1,2,3,4), 
                                   labels = c("Data Not Entered", "Male", "Female", "Indeterminate", "Unknown"))
tidy_all_patients$toreason <- factor(tidy_all_patients$toreason, levels = c(0:34), labels = c("Data Not Entered",
                                                                                              "Death", "Removal to new TP/HB/CSA", "Internal Transfer", "Mental Hospital", "Embarkation", 
                                                                                              "New TP/HB/CSA/Same GP", "Adopted Child", "Services", "Deduction at GP's Request", 
                                                                                              "Registration Cancelled","Service Dependant",  "Deduction at Patient's Request", "Other reason",
                                                                                              "Enlistment", "Institution", "Transfer within Practice", "Linkage", "Untraced - Miscellaneous", 
                                                                                              "Untraced - Immig", "Untraced - GP Resign", "Untraced - College", "Untraced - outwith Practice", 
                                                                                              "Untraced - outwith HB", "Multiple Transfer", "Intra-consortium transfer", "Returned Undelivered",
                                                                                              "Internal Transfer - Address Change", "Internal Transfer within Partnership", 
                                                                                              "Correspondence states gone away", "Practice advise outside their area", 
                                                                                              "Practice advise patient no longer resident", "Practive advise removal via screening system",
                                                                                              "Practice advise removal via vaccination data", "Removal from Residential Institute"))
tidy_all_patients$data_in <- factor(tidy_all_patients$data_in, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$eligible <- factor(tidy_all_patients$eligible, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$hes16<- factor(tidy_all_patients$hes16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$death16 <- factor(tidy_all_patients$death16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$cancer16 <- factor(tidy_all_patients$cancer16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$lsoa16 <- factor(tidy_all_patients$lsoa16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$mh16 <- factor(tidy_all_patients$mh16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$hes_e_17 <- factor(tidy_all_patients$hes_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$death_e_17 <- factor(tidy_all_patients$death_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$cr_e_17 <- factor(tidy_all_patients$cr_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$lsoa_e_17 <- factor(tidy_all_patients$lsoa_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$mh_e_17 <- factor(tidy_all_patients$mh_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$mob <- factor(tidy_all_patients$mob, levels = c(0:12), labels = c("Data Not Entered",
                                                                                    "January", "February", "March", "April", "May", "June", "July", "August", "September", 
                                                                                    "October", "November", "December"))
tidy_all_patients$marital <- factor(tidy_all_patients$marital, levels = c(0:11), labels = c("Data Not Entered",
                                                                                            "Single", "Married", "Widowed", "Divorced", "Separated", "Unknown", "Engaged", "Co-habiting", "Remarried", 
                                                                                            "Stable relationship", "Civil partnership"))
tidy_all_patients$chsreg <- factor(tidy_all_patients$chsreg, levels = c(0,1,2), labels = c("Data Not Entered", "Yes", "No"))
tidy_all_patients$prescr <- factor(tidy_all_patients$prescr, levels = c(0:21), labels = c("Data Not Entered",
                                                                                          "Under 16 years of age", "16, 17 or 18 and in full-time education", "Woman aged 60 or over", "Man aged 60 or over",
                                                                                          "Has a maternity/medical exemption certificate", "Has a prescription prepayment certificate", 
                                                                                          "Receives Income Support/Family credit et", "Has a War Pension exemption certificate", "Not Exempt",
                                                                                          "Get Disability Working Allowance", "Receives Income-based Jobseeker's Allowance", 
                                                                                          "Is named on a current HC2 charges certificate", "Was prescribed a free-of-charge contraceptive", 
                                                                                          "Has a maternity exemption certificate", "Has a medical exemption certificate", "Receives Income Support", 
                                                                                          "Has WTFC exemption or gets full or reduced WFTC", "Has DPTC exemption or get full or reduced DPTC", 
                                                                                          "Aged 60 or over", "Entitled to/has a valid NHS Tax Credit Exemption Certificate", 
                                                                                          "Has a partner who gets Pension Credit guarantee credit PCGC"))
tidy_all_patients$capsup<- factor(tidy_all_patients$capsup, levels = c(0,1,2,3,4), 
                                  labels = c("Data Not Entered", "Low", "Medium", "High", "Not Applicable"))
tidy_all_patients$accept<- factor(tidy_all_patients$accept, levels = c(0,1), 
                                  labels = c("unacceptable", "acceptable"))

## Check correct coercion of variables
glimpse(tidy_all_patients)

## Check factors correctly labelled
levels(tidy_all_patients$prac_region)
levels(tidy_all_patients$gender)
levels(tidy_all_patients$toreason)
levels(tidy_all_patients$data_in)
levels(tidy_all_patients$eligible)
levels(tidy_all_patients$hes16)
levels(tidy_all_patients$death16)
levels(tidy_all_patients$cancer16)
levels(tidy_all_patients$lsoa16)
levels(tidy_all_patients$mh16)
levels(tidy_all_patients$hes_e_17)
levels(tidy_all_patients$death_e_17)
levels(tidy_all_patients$cr_e_17)
levels(tidy_all_patients$lsoa_e_17)
levels(tidy_all_patients$mh_e_17)
levels(tidy_all_patients$mob)
levels(tidy_all_patients$marital)
levels(tidy_all_patients$chsreg)
levels(tidy_all_patients$prescr)
levels(tidy_all_patients$capsup)
levels(tidy_all_patients$toreason)
levels(tidy_all_patients$accept)

## Find missing values
any(is.na(tidy_all_patients))
sum(is.na(tidy_all_patients))
summary(is.na(tidy_all_patients)) # All NAs related to famnum, chs date, tod, and death date therefore do not remove

## Look for outliers and obvious errors
summary(tidy_all_patients)

## Look at categorical variables for outliers and obvious errors (where previous command did not print)
summary(tidy_all_patients$mob)
summary(tidy_all_patients$marital)
summary(tidy_all_patients$prescr)
summary(tidy_all_patients$toreason)
summary(tidy_all_patients$prac_region)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_all_patients$patid, xlab = "patid")
boxplot(tidy_all_patients$pracid, xlab = "pracid")
boxplot(tidy_all_patients$dob, xlab = "dob")
boxplot(tidy_all_patients$yob, xlab = "yob")
boxplot(tidy_all_patients$famnum, xlab = "famnum")
boxplot(tidy_all_patients$regstat, xlab = "regstat")
boxplot(tidy_all_patients$reggap, xlab = "reggap")
boxplot(tidy_all_patients$internal, xlab = "internal")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_all_patients$patid)
hist(tidy_all_patients$pracid)
hist(tidy_all_patients$vmid)
hist(tidy_all_patients$yob)
hist(tidy_all_patients$famnum)
hist(tidy_all_patients$regstat)
hist(tidy_all_patients$reggap)
hist(tidy_all_patients$internal)

# Migration

## Remove whole row duplicates
n_distinct(migration) == count(migration)
distinct_migration <- migration %>% distinct()

## Remove duplicates based on patid + medcode + date (likely clinical coding errors)
n_distinct(distinct_migration) == count(distinct(distinct_migration, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_migration_2 <- distinct_migration %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)

## Select index event for duplicates based on patid and medcode (i.e. same details but coded more than once) 
distinct_migration_3 <- distinct_migration_2 %>% arrange(eventdate) %>% distinct(patid, medcode, .keep_all = TRUE)

## Drop unneeded variables
tidy_migration <-  select(distinct_migration_3, -c(sysdate, constype, episode, enttype, 
                                             adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_migration <-  tidy_migration %>% 
  mutate_if(is.numeric, as.integer) 
tidy_migration$category <- factor(tidy_migration$category, levels = c(1,2,3,4), 
                                  labels = c("Non-UK origin", "Born outside of the UK", "First/main language not English", 
                                             "Visa status indicating migration"))

## Check correct coercion of variable classes
glimpse(tidy_migration)

## Check factors correctly labelled
levels(tidy_migration$category)

## Find missing values
any(is.na(tidy_migration))
sum(is.na(tidy_migration))
summary(is.na(tidy_migration)) # NAs for event date - keep as these events are still coded to indicate migration

## Look at summary of all variables for outliers and obvious errors
summary(tidy_migration)

# Ethnicity

## Remove duplicate rows
n_distinct(ethnicity) == count(ethnicity)
distinct_ethnicity <- ethnicity %>% distinct()

## Remove duplicates of combined patid + medcode + date (likely clinical coding errors)
n_distinct(distinct_ethnicity) == count(distinct(distinct_ethnicity, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_ethnicity_2 <- distinct_ethnicity %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)

## Remove duplicate medcodes for unique patids by selecting first recorded medcode event
distinct_ethnicity_3 <- distinct_ethnicity_2 %>% arrange(eventdate) %>% distinct(patid, medcode, .keep_all = TRUE)

## Drop unneeded variables 
tidy_ethnicity <-  select(distinct_ethnicity_3, -c(sysdate, constype, episode, enttype, 
                                                   adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_ethnicity <-  tidy_ethnicity %>% 
  mutate_if(is.numeric, as.integer) 
tidy_ethnicity$category <- factor(tidy_ethnicity$category, levels = c(1:21), 
                                  labels = c("White British", "White Irish", "White Other", "White NOS",
                                             "Mixed White and Black", "Mixed White and Asian", "Mixed Asian and Black", "Mixed Other",
                                             "Mixed NOS", "Indian", "Pakistani", "Bangladeshi", "Other Asian", "14 deleted from phenotype", 
                                             "Black Caribbean","Black African", "Black Other", "18 deleted from phenotype" , "Chinese", "Other ethnic group", 
                                             "Ethnic group not specified"))

## Check correct coercion of variable classes
glimpse(tidy_ethnicity)

## Check factors correctly labelled
levels(tidy_ethnicity$category)

## Find missing values
any(is.na(tidy_ethnicity))
sum(is.na(tidy_ethnicity))
summary(is.na(tidy_ethnicity)) # NAs for event date - keep as these events are still coded to indicate ethnicity

## Look at summary of all fields for outliers and obvious errors
summary(tidy_ethnicity)

# Patient IMD

## Check for any duplicate based on whole row
n_distinct(patient_imd) == count(patient_imd) 

## Check for any duplicates based on combined patid and pracid
n_distinct(patient_imd) == count(distinct(patient_imd, patid,pracid,  .keep_all = TRUE)) 

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_patient_imd <-  patient_imd %>% 
  mutate_if(is.numeric, as.integer) 
tidy_patient_imd$imd2015_5 <- factor(tidy_patient_imd$imd2015_5, levels = c(1:5), 
                                  labels = c("IMD 1", "IMD 2", "IMD 3", "IMD 4","IMD 5" ))

## Check correct coercion of variable classes
glimpse(tidy_patient_imd)

## Check factors correctly labelled
levels(tidy_patient_imd$imd2015_5)

## Find missing values
any(is.na(tidy_patient_imd))
sum(is.na(tidy_patient_imd))
summary(is.na(tidy_patient_imd))  

## Look at summary of all fields for outliers and obvious errors
summary(tidy_patient_imd)

# Practice IMD 

## Check for any duplicate based on whole row
n_distinct(practice_imd) == count(practice_imd) 

## Check for any duplicates based on practice id
n_distinct(practice_imd) == count(distinct(practice_imd, pracid,  .keep_all = TRUE)) 

## drop unneeded variables: imd for NI, scotland, walers (beacuse not provided by CPRD)
tidy_practice_imd <- select(practice_imd, -c(country, ni2017_imd_5,s2016_imd_5, w2014_imd_5))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_practice_imd$pracid <-  as.integer(tidy_practice_imd$pracid)
tidy_practice_imd$e2015_imd_5 <- factor(tidy_practice_imd$e2015_imd_5, levels = c(1:5), 
                                     labels = c("IMD 1", "IMD 2", "IMD 3", "IMD 4","IMD 5" ))

## Check correct coercion of variable classes
glimpse(tidy_practice_imd)

## Check factors correctly labelled
levels(tidy_practice_imd$e2015_imd_5)

## Find missing values
any(is.na(tidy_practice_imd))
sum(is.na(tidy_practice_imd))
summary(is.na(tidy_practice_imd)) 

## Look at summary of all fields for outliers and obvious errors
summary(tidy_practice_imd)

## Recoding/renaming ethnicity -------------------------------------------------------------------------------------------

# Ethnicity

## remove records for patients with the same 18 category ethnicity coded more than once 
count(tidy_ethnicity)
n_distinct(distinct(tidy_ethnicity, patid, category)) 
ethnicity_clean <- tidy_ethnicity %>% distinct(patid, category, .keep_all = TRUE)
count(ethnicity_clean) 

## Drop records with ethnicity recorded as 'ethnic group not specified' or NA (i.e. no ethnicity data available for that patient) - note: later after joining to all_patient file, ethnicity N/A's retained as 'Unknown' category
sum(ethnicity_clean$category == "Ethnic group not specified")
sum(is.na(ethnicity_clean$category))
ethnicity_NOS <- ethnicity_clean%>%
  filter(category == "Ethnic group not specified") 
ethnicity_clean <- ethnicity_clean%>%
  filter(category != "Ethnic group not specified") %>%
  filter(!is.na(category)) 

n_distinct(ethnicity_clean$patid) 

## Keep the most recent ethnic code for those with more than one ethnicity (for those with event date)
recent_ethnicity_clean <- ethnicity_clean %>%
  filter(!is.na(eventdate)) 
recent_ethnicity_clean <- recent_ethnicity_clean %>%
  group_by(patid) %>%
  slice(which.max(eventdate))
n_distinct(recent_ethnicity_clean$patid) # patients all with one ethnic code 
nrow(recent_ethnicity_clean) == n_distinct(recent_ethnicity_clean$patid) 

## Select most frequently occurring ethnicity for those without event dates
no_eventdate <- ethnicity_clean %>% 
  filter(is.na(eventdate)) 
n_distinct(no_eventdate$patid) # check for duplicates 

mode <- function(x) {
  ux <- unique(x)
  ux [which.max(tabulate(match(x,ux)))]
} # function to identify the most frequently occurring category

no_eventdate <- no_eventdate %>% 
  group_by(patid) %>%
  mutate(category=mode(category))  # selected the most frequently occurring category

no_eventdate <- no_eventdate %>%
  distinct(patid, category) # remove duplicates

nrow(no_eventdate) == n_distinct(no_eventdate$patid) 

ethnicity_clean <- bind_rows(recent_ethnicity_clean, no_eventdate) # distinct patients matching distinct no. of rows 
nrow(ethnicity_clean) == n_distinct(ethnicity_clean$patid) # doesn't match if some patients have some codes with eventdate and others with no eventdate 

## Select the ethnicity with a corresponding eventdate and remove those with no eventdate (for patients who have both)
ethnicity_clean <- ethnicity_clean %>%
  mutate(eventdate=replace(eventdate, is.na(eventdate), as.Date("1900-01-01")))

ethnicity_clean <- ethnicity_clean %>%
  group_by(patid) %>%
  slice(which.max(eventdate)) 

n_distinct(ethnicity_clean$patid) 
nrow(ethnicity_clean) == n_distinct(ethnicity_clean$patid) # check if they match, and if it matches original distinct patient number before selections were made

ethnicity_clean <- ethnicity_clean %>%
  mutate(eventdate=replace(eventdate, eventdate == "1900-01-01", NA))

## Rename ethnicity category label
ethnicity_clean <- rename(ethnicity_clean, ethnicat = category)

## Create 6 group ethnicity variable 
ethnicity_clean <- ethnicity_clean %>%
  mutate(ethnicat6 = ethnicat)
levels(ethnicity_clean$ethnicat6) <-  c(levels(ethnicity_clean$ethnicat6),"White British", "White Non-British", "Mixed", 
                                        "Asian/Asian British", "Black/Black British", "Other ethnic group" )
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "White British"] <- "White British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "White Irish"] <- "White Non-British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "White Other"] <- "White Non-British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "White NOS"] <- "White Non-British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed White and Black"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed White and Asian"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed Asian and Black"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed Other"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed NOS"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Indian"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Pakistani"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Bangladeshi"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Chinese"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Other Asian"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Black Caribbean"] <- "Black/Black British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Black African"] <- "Black/Black British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Black Other"] <- "Black/Black British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Other ethnic group"] <- "Other ethnic group"

## Relevel ethnicat6
ethnicity_clean <- as.data.frame(ethnicity_clean)
ethnicity_clean <- within(ethnicity_clean, ethnicat6 <- relevel (ethnicat6, ref="White British") )
ethnicity_clean$ethnicat6 <- droplevels(ethnicity_clean$ethnicat6)
levels(ethnicity_clean$ethnicat6)

## Check if duplicates need to be removed based on patid and category for 6 group classification and check number of patients with conflicting events i.e. more than one of 6 group classification
count(ethnicity_clean)
n_distinct(distinct(ethnicity_clean, patid, ethnicat)) # if same, no need to drop any records 

## Remove eventdate variable
ethnicity_clean <- subset(ethnicity_clean, select = -eventdate)


## Select relevant fields  -------------------------------------------------

migrants_clean <- select(tidy_migrants, c(patid, pracid, gender, frd, deathdate, yob, migcertainty))
all_patients_clean <- select(tidy_all_patients, c(patid, pracid, prac_region, gender, dob, data_start, data_end, frd, deathdate, eligible, yob))
migration_clean <- select(tidy_migration, c(patid, medcode, eventdate, category))
ethnicity_clean <- select(ethnicity_clean, c(patid, medcode, ethnicat, ethnicat6))
patient_imd_clean <- tidy_patient_imd
practice_imd_clean <- tidy_practice_imd

# Add missing fields from all patients to migrants
migrants_clean <- inner_join(migrants_clean, all_patients_clean, by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                                                        "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob"))

# Drop unused factor levels
migrants_clean <- droplevels(migrants_clean)
all_patients_clean <- droplevels(all_patients_clean)
migration_clean <- droplevels(migration_clean)
ethnicity_clean <- droplevels(ethnicity_clean) 
patient_imd_clean <- droplevels(patient_imd_clean)
practice_imd_clean <- droplevels(practice_imd_clean)

## Save clean .Rdata files ---------------------------------------------------------

save(migrants_clean, file = "filepath/migrants_clean.Rdata")
save(all_patients_clean, file = "filepath/all_patients_clean.Rdata")
save(migration_clean, file = "filepath/migration_clean.Rdata")
save(ethnicity_clean, file = "filepath/ethnicity_clean.Rdata")
save(patient_imd_clean, file = "filepath/patient_imd_clean.Rdata")
save(practice_imd_clean, file = "filepath/practice_imd_clean.Rdata")

## Load clean .Rdata files -------------------------------------------------

load(file = "filepath/migrants_clean.Rdata")
load(file = "filepath/all_patients_clean.Rdata")
load(file = "filepath/migration_clean.Rdata")
load(file = "filepath/ethnicity_clean.Rdata")
load(file = "filepath/patient_imd_clean.Rdata")
load(file = "filepath/practice_imd_clean.Rdata")


# 04_Data analysis - Completeness --------------------------------------------


## Completeness (all years)  --------------------------------------------------------------------------------------

## Percentage of migrants in CPRD
total_migrants <- n_distinct(migrants_clean$patid)
total_cprd <- n_distinct(all_patients_clean$patid)
percentage_migrants_in_cprd <- (total_migrants/total_cprd)*100
percentage_migrants_in_cprd

## Counts per category of distinct patients
n_distinct(migration_clean) == count(distinct(migration_clean, patid, category,  .keep_all = TRUE)) 
distinct_category_migration_clean <- migration_clean %>% distinct(patid, category,  .keep_all = TRUE)
migration_category <- count(group_by(distinct_category_migration_clean, category))
migration_category

## Percentage of migrants per category
percentage_migrants_per_category <- migration_category %>% 
  mutate(percentage_of_migrants = (n/sum(migration_category$n))*100)
percentage_migrants_per_category <- migration_category %>% 
  mutate(percentage_of_migrants = (n/sum(migration_category$n))*100)

## Percentage of cprd per category
percentage_cprd_per_category <- percentage_migrants_per_category %>% 
  mutate(percentage_of_all_patients = (n/total_cprd)*100) 
write_csv(percentage_cprd_per_category, "filepath/percentage_cprd_per_category.csv")

## Create migration certainty categories
levels(distinct_category_migration_clean$category) <- c(levels(distinct_category_migration_clean$category),"Definite", "Probable", "Possible" )
distinct_category_migration_clean$category[distinct_category_migration_clean$category == "Born outside of the UK" ] <- "Definite"
distinct_category_migration_clean$category[distinct_category_migration_clean$category == "Visa status indicating migration" ] <- "Definite"
distinct_category_migration_clean$category[distinct_category_migration_clean$category == "First/main language not English" ] <- "Probable"
distinct_category_migration_clean$category[distinct_category_migration_clean$category == "Non-UK origin" ] <- "Possible"
distinct_category_migration_clean_by_certainty <- distinct_category_migration_clean %>% distinct(patid, category,  .keep_all = TRUE)

definite <- distinct_category_migration_clean_by_certainty %>% filter(category == "Definite")
probable <- distinct_category_migration_clean_by_certainty %>% filter(category == "Probable") %>% 
  filter(patid %nin% definite$patid )
possible <- distinct_category_migration_clean_by_certainty %>% filter(category == "Possible") %>% 
  filter(patid %nin% definite$patid ) %>% filter(patid %nin% probable$patid)
definite
probable
possible

## Create cohort files of definite, probable, possible, definite + probable migrants for later sub analyses
definite_migrants <- left_join(definite, migrants_clean,by = c("patid" = "patid"))
probable_migrants <- left_join(probable, migrants_clean,by = c("patid" = "patid"))
possible_migrants <- left_join(possible, migrants_clean,by = c("patid" = "patid"))
definite_probable_migrants <- full_join(definite_migrants, probable_migrants, 
                                        by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender",
                                               "frd" = "frd", "deathdate" = "deathdate", "yob" = "yob", 
                                               "prac_region" = "prac_region",
                                               "dob" = "dob", "data_start" = "data_start", "data_end" = "data_end", 
                                               "eligible" = "eligible", "medcode" = "medcode", 
                                               "eventdate" = "eventdate", "category" = "category"))

## Percentage of migrants with an event for "Non-UK origin"
distinct_non_uk_origin <- migration_clean %>% filter(category == "Non-UK origin") %>% distinct(patid, .keep_all = TRUE)
percentage_non_uk_origin <- (count(migrants_clean)/count(all_patients_clean))*100

## Number of migrants with more than one migration event
event_count <- migration_clean %>% group_by(patid) %>% summarise(count = n())
event_count_summary <- event_count %>% group_by(count) %>% summarise(number_of_patients = n())
event_count_summary

## Number of migrants with more than one event for "Non-UK origin"
multiple_category_non_uk_origin <- migration_clean %>% filter(category == "Non-UK origin") %>% group_by(patid) %>% summarise(n = n())
multiple_category_non_uk_origin_summary <- multiple_category_non_uk_origin %>% group_by(n) %>% summarise(number_of_patients = n())
multiple_category_non_uk_origin_summary

## Number of migrants with more than one event for "Born outside of the UK"
multiple_category_born_outside_uk <- migration_clean %>% filter(category == "Born outside of the UK") %>% group_by(patid) %>% summarise(n = n())
multiple_category_born_outside_uk_summary <- multiple_category_born_outside_uk %>% group_by(n) %>% summarise(number_of_patients = n())
multiple_category_born_outside_uk_summary

## Number of migrants with more than one event for "First/main language not English"
multiple_category_language_not_english <- migration_clean %>% filter(category ==  "First/main language not English") %>% group_by(patid) %>% summarise(n = n())
multiple_category_language_not_english_summary <- multiple_category_language_not_english %>% group_by(n) %>% summarise(number_of_patients = n())
multiple_category_language_not_english_summary 

## Number of migrants with more than one event for "Visa status indicating migration"
multiple_category_visa <- migration_clean %>% filter(category ==  "Visa status indicating migration") %>% group_by(patid) %>% summarise(n = n())
multiple_category_visa_summary <- multiple_category_visa %>% group_by(n) %>% summarise(number_of_patients = n())
multiple_category_visa_summary


## Description (all years) --------------------------------------------------------------------------------------

total_definite <- n_distinct(definite$patid)
total_probable <- n_distinct(probable$patid)
total_possible <- n_distinct(possible$patid)

## *Sex ---

## Sex breakdown of all migrants and  all patients 
migrants_gender <- count(group_by(migrants_clean, gender)) %>% mutate(percentage_of_migrants = (n/total_migrants)*100)
migrants_gender
all_patients_gender <- count(group_by(all_patients_clean, gender)) %>% mutate(percentage_of_all_patients = (n/sum(total_cprd))*100)
all_patients_gender

## Migrants by sex as percentage of all patients
counts_gender <- inner_join(migrants_gender, all_patients_gender, by = c("gender" = "gender")) 
percentage_by_gender <- mutate(counts_gender, percentage_migrants_in_cprd_by_gender = (n.x/n.y)*100)
percentage_by_gender

## sex breakdown by certainty of migration status : number and percent
definite_gender_percent <- count(group_by(definite_migrants, gender)) %>% 
  mutate(percentage_of_definite_migrants = (n/total_definite)*100)
definite_gender_percent
probable_gender_percent <- count(group_by(probable_migrants, gender)) %>% 
  mutate( percentage_of_probable_migrants = (n/total_probable)*100)
probable_gender_percent
possible_gender_percent <- count(group_by(possible_migrants, gender)) %>% 
  mutate( percentage_of_possible_migrants = (n/total_possible)*100)
possible_gender_percent
definite_probable_gender_percent <- count(group_by(definite_probable_migrants, gender)) %>% 
  mutate(percentage_of_definite_probable_migrants = (n/(total_probable + total_definite))*100)
definite_probable_gender_percent


## *Year of birth ---
  
## Median year of birth 
summary(migrants_clean$yob)

## number and percent of all migrants in yob groups
migrants_yob_1900_1919 <- migrants_clean %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_migrants)*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
migrants_yob_1920_1939 <- migrants_clean %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_migrants)*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
migrants_yob_1940_1959 <- migrants_clean %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_migrants)*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
migrants_yob_1960_1979 <- migrants_clean %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_migrants)*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
migrants_yob_1980_1999 <- migrants_clean %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_migrants)*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
migrants_yob_2000_2018 <- migrants_clean %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_migrants)*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

migrants_yob_gps <- full_join(migrants_yob_1900_1919, migrants_yob_1920_1939, 
                              by = c("yob_grp" = "yob_grp", "n" = "n",
                                     "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(migrants_yob_1940_1959,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(migrants_yob_1960_1979,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(migrants_yob_1980_1999,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(migrants_yob_2000_2018,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") )
migrants_yob_gps  

## Definite migrants yob grps 

definite_migrants_yob_1900_1919 <- definite_migrants %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_definite)*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
definite_migrants_yob_1920_1939 <- definite_migrants %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_definite)*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
definite_migrants_yob_1940_1959 <- definite_migrants %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_definite)*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
definite_migrants_yob_1960_1979 <- definite_migrants %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_definite)*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
definite_migrants_yob_1980_1999 <- definite_migrants %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_definite)*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
definite_migrants_yob_2000_2018 <- definite_migrants %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_definite)*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

definite_migrants_yob_gps <- full_join(definite_migrants_yob_1900_1919, definite_migrants_yob_1920_1939, 
                              by = c("yob_grp" = "yob_grp", "n" = "n",
                                     "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(definite_migrants_yob_1940_1959,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_migrants_yob_1960_1979,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_migrants_yob_1980_1999,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_migrants_yob_2000_2018,by = c("yob_grp" = "yob_grp", "n" = "n",
                                          "percent_of_migrants" = "percent_of_migrants") )
definite_migrants_yob_gps 

## Probable migrants yob grps 

probable_migrants_yob_1900_1919 <- probable_migrants %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_probable)*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
probable_migrants_yob_1920_1939 <- probable_migrants %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_probable)*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
probable_migrants_yob_1940_1959 <- probable_migrants %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_probable)*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
probable_migrants_yob_1960_1979 <- probable_migrants %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_probable)*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
probable_migrants_yob_1980_1999 <- probable_migrants %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_probable)*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
probable_migrants_yob_2000_2018 <- probable_migrants %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_probable)*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

probable_migrants_yob_gps <- full_join(probable_migrants_yob_1900_1919, probable_migrants_yob_1920_1939, 
                                       by = c("yob_grp" = "yob_grp", "n" = "n",
                                              "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(probable_migrants_yob_1940_1959,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(probable_migrants_yob_1960_1979,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(probable_migrants_yob_1980_1999,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(probable_migrants_yob_2000_2018,by = c("yob_grp" = "yob_grp", "n" = "n",
                                         "percent_of_migrants" = "percent_of_migrants") )
probable_migrants_yob_gps

## Possible migrants yob grps 

possible_migrants_yob_1900_1919 <- possible_migrants %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_possible)*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
possible_migrants_yob_1920_1939 <- possible_migrants %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_possible)*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
possible_migrants_yob_1940_1959 <- possible_migrants %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_possible)*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
possible_migrants_yob_1960_1979 <- possible_migrants %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_possible)*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
possible_migrants_yob_1980_1999 <- possible_migrants %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_possible)*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
possible_migrants_yob_2000_2018 <- possible_migrants %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/total_possible)*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

possible_migrants_yob_gps <- full_join(possible_migrants_yob_1900_1919, possible_migrants_yob_1920_1939, 
                                       by = c("yob_grp" = "yob_grp", "n" = "n",
                                              "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(possible_migrants_yob_1940_1959,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(possible_migrants_yob_1960_1979,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(possible_migrants_yob_1980_1999,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(possible_migrants_yob_2000_2018,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") )
possible_migrants_yob_gps

## Definite and probable migrants yob grps 

definite_probable_migrants_yob_1900_1919 <- definite_probable_migrants %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(total_probable + total_definite))*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
definite_probable_migrants_yob_1920_1939 <- definite_probable_migrants %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(total_probable + total_definite))*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
definite_probable_migrants_yob_1940_1959 <- definite_probable_migrants %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(total_probable + total_definite))*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
definite_probable_migrants_yob_1960_1979 <- definite_probable_migrants %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(total_probable + total_definite))*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
definite_probable_migrants_yob_1980_1999 <- definite_probable_migrants %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(total_probable + total_definite))*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
definite_probable_migrants_yob_2000_2018 <- definite_probable_migrants %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(total_probable + total_definite))*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

definite_probable_migrants_yob_gps <- full_join(definite_probable_migrants_yob_1900_1919, definite_probable_migrants_yob_1920_1939, 
                                       by = c("yob_grp" = "yob_grp", "n" = "n",
                                              "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(definite_probable_migrants_yob_1940_1959,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_probable_migrants_yob_1960_1979,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_probable_migrants_yob_1980_1999,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_probable_migrants_yob_2000_2018,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                   "percent_of_migrants" = "percent_of_migrants") )
definite_probable_migrants_yob_gps 

## *Practice region ---

## Count and percentage by practice region
prac_region_count <- count(group_by(migrants_clean, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/total_migrants)*100)
prac_region_count 

## Count and percentage by practice region : definite migrants 
prac_region_count_definite <- count(group_by(definite_migrants, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/total_definite)*100)
prac_region_count_definite 

## Count and percentage by practice region : probable migrants 
prac_region_count_probable <- count(group_by(probable_migrants, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/total_probable)*100)
prac_region_count_probable 

## Count and percentage by practice region : possible migrants 
prac_region_count_possible <- count(group_by(possible_migrants, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/total_possible)*100)
prac_region_count_possible

## Count and percentage by practice region : definite + probable migrants 
prac_region_count_definite_probable <- count(group_by(definite_probable_migrants, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/(total_probable + total_definite))*100)
prac_region_count_definite_probable

## What proportion of patients registered in each region are migrants 
all_patients_prac_region <- all_patients_clean %>% group_by(prac_region) %>% count()
migrants_prac_region <- migrants_clean %>% group_by(prac_region) %>% count()
prac_region_proportions <- full_join(migrants_prac_region, all_patients_prac_region, b = c("prac_region" = "prac_region")) %>%
  mutate(proportion_of_region = (n.x/n.y)*100) %>%
  rename(n_migrants = n.x) %>%
  rename(n_allpatients = n.y)
write_csv(prac_region_proportions, "filepath/prac_region_proportions.csv")


## *Ethnic groups ---

## Join migrant cohort file to ethnicity events data to find all ethnicity data relating to migrants
migrants_ethnicity <- left_join(migrants_clean, ethnicity_clean, by = c("patid" = "patid"))

## Number of migrants per ethnic group - 6 group classification 
count_migrants_ethnicity <- count(group_by(migrants_ethnicity, ethnicat6))
count_percent_migrants_ethnicity <- count_migrants_ethnicity %>% 
  mutate(percent = (n/total_migrants)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_migrants_ethnicity
count_percent_migrants_ethnicity$n_percent <- paste(count_percent_migrants_ethnicity$n, count_percent_migrants_ethnicity$percent, sep =",")
count_percent_migrants_ethnicity

## 6 group ethnic classification : definite migrants

definite_migrants_ethnicity <- left_join(definite_migrants, migrants_ethnicity, 
          by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                 "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                 "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_migrants_ethnicity <- count(group_by(definite_migrants_ethnicity, ethnicat6)) 
count_percent_definite_migrants_ethnicity <- count_definite_migrants_ethnicity %>%
  mutate (percentage = (n/total_definite)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_definite_migrants_ethnicity

## 6 group ethnic classification : probable migrants

probable_migrants_ethnicity <- left_join(probable_migrants, migrants_ethnicity, 
                                         by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_probable_migrants_ethnicity <- count(group_by(probable_migrants_ethnicity, ethnicat6)) 
count_percent_probable_migrants_ethnicity <- count_probable_migrants_ethnicity %>%
  mutate (percentage = (n/total_probable)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_probable_migrants_ethnicity

## 6 group ethnic classification : possible migrants

possible_migrants_ethnicity <- left_join(possible_migrants, migrants_ethnicity, 
                                         by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_possible_migrants_ethnicity <- count(group_by(possible_migrants_ethnicity, ethnicat6)) 
count_percent_possible_migrants_ethnicity <- count_possible_migrants_ethnicity %>%
  mutate (percentage = (n/total_possible)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_possible_migrants_ethnicity

## 6 group ethnic classification : definite and probable migrants

definite_probable_migrants_ethnicity <- left_join(definite_probable_migrants, migrants_ethnicity, 
                                         by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_probable_migrants_ethnicity <- count(group_by(definite_probable_migrants_ethnicity, ethnicat6)) 
count_percent_definite_probable_migrants_ethnicity <- count_definite_probable_migrants_ethnicity %>%
  mutate (percentage = (n/(total_probable + total_definite))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_definite_probable_migrants_ethnicity

## *Region of birth ---

## Join migrant cohort file to migration events data to find all migration data relating to migrants
migrants_migration <- left_join(migrants_clean, migration_clean, by = c("patid" = "patid"))

## Filter for migrants with migration event data indicating a country of birth 
migrants_cob <- migrants_migration %>% filter(category == "Born outside of the UK")

## Count number and percent of migrants with any cob recorded
distinct_migrants_cob <-  migrants_cob %>% 
  distinct(patid,  .keep_all = TRUE)
count_distinct_migrants_cob <- count(distinct_migrants_cob)
count_percent_migrants_cob <- count_distinct_migrants_cob %>% 
  mutate(percentage_of_migrants = (n/total_migrants)*100)
count_percent_migrants_cob 

## Confirm some migrants have more than one country of birth event assigned
count(migrants_cob) > count(distinct_migrants_cob)

## Import and tidy categorisation of born in codes into WHO region and ONS region
migration_region_categories <- read_csv("filepath/migration_region_categories.csv") # codelist available in codelist folder
tidy_migration_region_categories <-  migration_region_categories %>% 
  mutate_if(is.numeric, as.integer) 
tidy_migration_region_categories$category <- factor(tidy_migration_region_categories$category, levels = c(1,2,3,4), 
                                  labels = c("Non-UK origin", "Born outside of the UK", "First/main language not English", 
                                             "Visa status indicating migration"))
tidy_migration_region_categories$who_region <- factor(tidy_migration_region_categories$who_region, levels = 
                                                        c("afro","euro", "emro", "paho", "searo", "wpro"), 
                                                      labels = c("afro","euro", "emro", "paho", "searo", "wpro"))
tidy_migration_region_categories$ons_nomis_region <- factor(tidy_migration_region_categories$ons_nomis_region,
                                                           levels = c("Africa","Europe", "Middle East & Asia",
                                                                      "The Americas & Caribbean", "Antarctica & Oceania"), 
                                                      labels = c("Africa","Europe", "Middle East & Asia",
                                                                 "The Americas & Caribbean", "Antarctica & Oceania"))
glimpse(tidy_migration_region_categories)
levels(tidy_migration_region_categories$category)
levels(tidy_migration_region_categories$who_region)
levels(tidy_migration_region_categories$ons_nomis_region)
tidy_migration_region_categories <- tidy_migration_region_categories %>% 
  select(category, medcode, who_region, ons_nomis_region)

## join region categories to all migrants + migration events with cob data 
migrants_cob_regions <- left_join(migrants_cob, tidy_migration_region_categories, by = c("medcode", "category"))


## Check for and remove duplicate records based on patid and who_region 
distinct_migrants_cob_who_region <- migrants_cob_regions %>% 
  distinct(patid, who_region,  .keep_all = TRUE)

## Count number of migrants with > 1 WHO region category i.e. conflicting categories 
migrants_with_multiple_who_region <- count(distinct_migrants_cob_who_region) - count_distinct_migrants_cob 
migrants_with_multiple_who_region

## Find number of conflicting WHO region records per patient, number of patients per number of conflicting records, create list of migrant patids with no conflicting records
number_of_who_region_records_per_patid <- count(group_by(distinct_migrants_cob_who_region, patid))
unique(number_of_who_region_records_per_patid$n)
number_of_migrants_per_number_of_who_region_records_per_patient <- count(group_by(number_of_who_region_records_per_patid, n))
number_of_migrants_per_number_of_who_region_records_per_patient
migrants_with_no_conflicting_who_region <- number_of_who_region_records_per_patid %>% filter(n == 1)

## Filter for patients with no conflicting records of WHO region
migrants_who_region_no_conflicts <- distinct_migrants_cob_who_region %>% 
  filter(patid %in% migrants_with_no_conflicting_who_region$patid )

## Number of migrants per who_region 
count_migrants_who_region_no_conflicts <- count(group_by(migrants_who_region_no_conflicts, who_region))
count_migrants_who_region_no_conflicts
count_percent_migrants_who_region_no_conflicts <- count_migrants_who_region_no_conflicts %>% 
  mutate(percentage_of_migrants = (n/total_migrants)*100)
count_percent_migrants_who_region_no_conflicts
valid_region_migrants <-nrow(migrants_who_region_no_conflicts)
no_valid_who_region <- total_migrants - valid_region_migrants 
no_valid_who_region_percent <- (no_valid_who_region/total_migrants)*100
no_valid_who_region
no_valid_who_region_percent

## WHO region : definite migrants

definite_migrants_who_region <- left_join(definite_migrants, migrants_who_region_no_conflicts, 
                                             by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                    "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                    "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_migrants_who_region <- count(group_by(definite_migrants_who_region, who_region)) 
count_percent_definite_migrants_who_region <- count_definite_migrants_who_region %>%
  mutate (percentage_of_definite_migrants = (n/total_definite)*100)
count_percent_definite_migrants_who_region

## Check for and remove duplicate records based on patid and ons nomis region i.e. patient ethnicity events that are not conflicting 
distinct_migrants_cob_ons_nomis_region <- migrants_cob_regions %>% 
  distinct(patid, ons_nomis_region,  .keep_all = TRUE)

## Count number of migrants with > 1 ONS nomis region category i.e. conflicting categories 
migrants_with_multiple_ons_nomis_region <- count(distinct_migrants_cob_ons_nomis_region) - count_distinct_migrants_cob 
migrants_with_multiple_ons_nomis_region

## Find number of conflicting WHO region records per patient, number of patients per number of conflicting records, create list of migrant patids with no conflicting records
number_of_ons_nomis_region_records_per_patid <- count(group_by(distinct_migrants_cob_ons_nomis_region, patid))
unique(number_of_ons_nomis_region_records_per_patid$n)
number_of_migrants_per_number_of_ons_nomis_region_records_per_patient <- count(group_by(number_of_ons_nomis_region_records_per_patid, n))
number_of_migrants_per_number_of_ons_nomis_region_records_per_patient
migrants_with_no_conflicting_ons_nomis_region <- number_of_ons_nomis_region_records_per_patid %>% filter(n == 1)

## Filter for patients with no conflicting records of ONS Nomis region
migrants_ons_nomis_region_no_conflicts <- distinct_migrants_cob_ons_nomis_region %>% 
  filter(patid %in% migrants_with_no_conflicting_ons_nomis_region$patid)

## Number of migrants per ons_nomis_region
count_migrants_ons_nomis_region_no_conflicts <- count(group_by(migrants_ons_nomis_region_no_conflicts, ons_nomis_region))
count_migrants_ons_nomis_region_no_conflicts
count_percent_migrants_ons_nomis_region_no_conflicts <- count_migrants_ons_nomis_region_no_conflicts %>% 
  mutate(percentage_of_migrants = (n/total_migrants)*100)
count_percent_migrants_ons_nomis_region_no_conflicts
valid_nomis_region_migrants <-nrow(migrants_ons_nomis_region_no_conflicts)
no_valid_ons_nomis_region <- total_migrants - valid_nomis_region_migrants
no_valid_ons_nomis_region_percent <- (no_valid_ons_nomis_region/total_migrants)*100
no_valid_ons_nomis_region
no_valid_ons_nomis_region_percent

## ONS nomis region : definite migrants

definite_migrants_ons_nomis_region <- left_join(definite_migrants, migrants_ons_nomis_region_no_conflicts, 
                                          by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                 "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                 "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_migrants_ons_nomis_region <- count(group_by(definite_migrants_ons_nomis_region, ons_nomis_region)) 
count_percent_definite_migrants_ons_nomis_region <- count_definite_migrants_ons_nomis_region %>%
  mutate (percentage_of_definite_migrants = (n/total_definite)*100)
count_percent_definite_migrants_ons_nomis_region


## Completeness (by year) --------------------------------------------------------------------------------------

## Number of migrants by year 
migrants_1997 <- migrants_clean %>% 
  filter(data_start <= "1997-12-31" & data_end >= "1997-01-01") %>% 
  count() %>% 
  mutate(Year = "1997")
migrants_1998 <- migrants_clean %>% filter(data_start <= "1998-12-31" & data_end >= "1998-01-01") %>% count() %>% mutate(Year = "1998")
migrants_1999 <- migrants_clean %>% filter(data_start <= "1999-12-31" & data_end >= "1999-01-01") %>% count() %>% mutate(Year = "1999")
migrants_2000 <- migrants_clean %>% filter(data_start <= "2000-12-31" & data_end >= "2000-01-01") %>% count() %>% mutate(Year = "2000")
migrants_2001 <- migrants_clean %>% filter(data_start <= "2001-12-31" & data_end >= "2001-01-01") %>% count() %>% mutate(Year = "2001")
migrants_2002 <- migrants_clean %>% filter(data_start <= "2002-12-31" & data_end >= "2002-01-01") %>% count() %>% mutate(Year = "2002")
migrants_2003 <- migrants_clean %>% filter(data_start <= "2003-12-31" & data_end >= "2003-01-01") %>% count() %>% mutate(Year = "2003")
migrants_2004 <- migrants_clean %>% filter(data_start <= "2004-12-31" & data_end >= "2004-01-01") %>% count() %>% mutate(Year = "2004")
migrants_2005 <- migrants_clean %>% filter(data_start <= "2005-12-31" & data_end >= "2005-01-01") %>% count() %>% mutate(Year = "2005")
migrants_2006 <- migrants_clean %>% filter(data_start <= "2006-12-31" & data_end >= "2006-01-01") %>% count() %>% mutate(Year = "2006")
migrants_2007 <- migrants_clean %>% filter(data_start <= "2007-12-31" & data_end >= "2007-01-01") %>% count() %>% mutate(Year = "2007")
migrants_2008 <- migrants_clean %>% filter(data_start <= "2008-12-31" & data_end >= "2008-01-01") %>% count() %>% mutate(Year = "2008")
migrants_2009 <- migrants_clean %>% filter(data_start <= "2009-12-31" & data_end >= "2009-01-01") %>% count() %>% mutate(Year = "2009")
migrants_2010 <- migrants_clean %>% filter(data_start <= "2010-12-31" & data_end >= "2010-01-01") %>% count() %>% mutate(Year = "2010")
migrants_2011 <- migrants_clean %>% filter(data_start <= "2011-12-31" & data_end >= "2011-01-01") %>% count() %>% mutate(Year = "2011")
migrants_2012 <- migrants_clean %>% filter(data_start <= "2012-12-31" & data_end >= "2012-01-01") %>% count() %>% mutate(Year = "2012")
migrants_2013 <- migrants_clean %>% filter(data_start <= "2013-12-31" & data_end >= "2013-01-01") %>% count() %>% mutate(Year = "2013")
migrants_2014 <- migrants_clean %>% filter(data_start <= "2014-12-31" & data_end >= "2014-01-01") %>% count() %>% mutate(Year = "2014")
migrants_2015 <- migrants_clean %>% filter(data_start <= "2015-12-31" & data_end >= "2015-01-01") %>% count() %>% mutate(Year = "2015")
migrants_2016 <- migrants_clean %>% filter(data_start <= "2016-12-31" & data_end >= "2016-01-01") %>% count() %>% mutate(Year = "2016")
migrants_2017 <- migrants_clean %>% filter(data_start <= "2017-12-31" & data_end >= "2017-01-01") %>% count() %>% mutate(Year = "2017")
migrants_2018 <- migrants_clean %>% filter(data_start <= "2018-12-31" & data_end >= "2018-01-01") %>% count() %>% mutate(Year = "2018")

migrants_per_year <- full_join(migrants_1997,migrants_1998, by = c("Year" = "Year", "n" = "n")) %>% select(Year, n)
migrants_per_year <- full_join(migrants_per_year, migrants_1999, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2000, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2001, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2002, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2003, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2004, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2005, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2006, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2007, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2008, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2009, by = c("Year" = "Year", "n" = "n"))
migrants_per_year <- full_join(migrants_per_year, migrants_2010, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2011, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2012, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2013, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2014, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2015, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2016, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2017, by = c("Year" = "Year", "n" = "n")) 
migrants_per_year <- full_join(migrants_per_year, migrants_2018, by = c("Year" = "Year", "n" = "n")) 

print(migrants_per_year, n = 22)

## Number of all patients per year 
all_patients_1997 <- all_patients_clean %>% filter(data_start <= "1997-12-31" & data_end >= "1997-01-01") %>% count() %>% mutate(Year = "1997")
all_patients_1998 <- all_patients_clean %>% filter(data_start <= "1998-12-31" & data_end >= "1998-01-01") %>% count() %>% mutate(Year = "1998")
all_patients_1999 <- all_patients_clean %>% filter(data_start <= "1999-12-31" & data_end >= "1999-01-01") %>% count() %>% mutate(Year = "1999")
all_patients_2000 <- all_patients_clean %>% filter(data_start <= "2000-12-31" & data_end >= "2000-01-01") %>% count() %>% mutate(Year = "2000")
all_patients_2001 <- all_patients_clean %>% filter(data_start <= "2001-12-31" & data_end >= "2001-01-01") %>% count() %>% mutate(Year = "2001")
all_patients_2002 <- all_patients_clean %>% filter(data_start <= "2002-12-31" & data_end >= "2002-01-01") %>% count() %>% mutate(Year = "2002")
all_patients_2003 <- all_patients_clean %>% filter(data_start <= "2003-12-31" & data_end >= "2003-01-01") %>% count() %>% mutate(Year = "2003")
all_patients_2004 <- all_patients_clean %>% filter(data_start <= "2004-12-31" & data_end >= "2004-01-01") %>% count() %>% mutate(Year = "2004")
all_patients_2005 <- all_patients_clean %>% filter(data_start <= "2005-12-31" & data_end >= "2005-01-01") %>% count() %>% mutate(Year = "2005")
all_patients_2006 <- all_patients_clean %>% filter(data_start <= "2006-12-31" & data_end >= "2006-01-01") %>% count() %>% mutate(Year = "2006")
all_patients_2007 <- all_patients_clean %>% filter(data_start <= "2007-12-31" & data_end >= "2007-01-01") %>% count() %>% mutate(Year = "2007")
all_patients_2008 <- all_patients_clean %>% filter(data_start <= "2008-12-31" & data_end >= "2008-01-01") %>% count() %>% mutate(Year = "2008")
all_patients_2009 <- all_patients_clean %>% filter(data_start <= "2009-12-31" & data_end >= "2009-01-01") %>% count() %>% mutate(Year = "2009")
all_patients_2010 <- all_patients_clean %>% filter(data_start <= "2010-12-31" & data_end >= "2010-01-01") %>% count() %>% mutate(Year = "2010")
all_patients_2011 <- all_patients_clean %>% filter(data_start <= "2011-12-31" & data_end >= "2011-01-01") %>% count() %>% mutate(Year = "2011")
all_patients_2012 <- all_patients_clean %>% filter(data_start <= "2012-12-31" & data_end >= "2012-01-01") %>% count() %>% mutate(Year = "2012")
all_patients_2013 <- all_patients_clean %>% filter(data_start <= "2013-12-31" & data_end >= "2013-01-01") %>% count() %>% mutate(Year = "2013")
all_patients_2014 <- all_patients_clean %>% filter(data_start <= "2014-12-31" & data_end >= "2014-01-01") %>% count() %>% mutate(Year = "2014")
all_patients_2015 <- all_patients_clean %>% filter(data_start <= "2015-12-31" & data_end >= "2015-01-01") %>% count() %>% mutate(Year = "2015")
all_patients_2016 <- all_patients_clean %>% filter(data_start <= "2016-12-31" & data_end >= "2016-01-01") %>% count() %>% mutate(Year = "2016")
all_patients_2017 <- all_patients_clean %>% filter(data_start <= "2017-12-31" & data_end >= "2017-01-01") %>% count() %>% mutate(Year = "2017")
all_patients_2018 <- all_patients_clean %>% filter(data_start <= "2018-12-31" & data_end >= "2018-01-01") %>% count() %>% mutate(Year = "2018")

all_patients_per_year <- full_join(all_patients_1997, all_patients_1998, by = c("Year" = "Year", "n" = "n")) %>% select(Year, n)
all_patients_per_year <- full_join(all_patients_per_year, all_patients_1999, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2000, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2001, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2002, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2003, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2004, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2005, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2006, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2007, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2008, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2009, by = c("Year" = "Year", "n" = "n"))
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2010, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2011, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2012, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2013, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2014, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2015, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2016, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2017, by = c("Year" = "Year", "n" = "n")) 
all_patients_per_year <- full_join(all_patients_per_year, all_patients_2018, by = c("Year" = "Year", "n" = "n")) 

print(all_patients_per_year, n = 22)

## Percentage of migrants in CPRD per year 
percentage_per_year <- full_join(migrants_per_year, all_patients_per_year, by = c("Year" = "Year")) %>% 
  rename(c("n_migrants" = "n.x", "n_all_patients" = "n.y")) %>%
  mutate(percentage_migrants_in_cprd = (n_migrants/n_all_patients)*100)
print(percentage_per_year, n = 22)

## Min and max percentages per year 
summary(percentage_per_year)

## Number of definite migrants per year 
definite_1997 <- definite_migrants %>% 
  filter(data_start <= "1997-12-31" & data_end >= "1997-01-01") %>% 
  count() %>% 
  mutate(Year = "1997") %>% 
  mutate(percent_cprd = (n/(all_patients_1997$n))*100)
definite_1998 <- definite_migrants %>% 
  filter(data_start <= "1998-12-31" & data_end >= "1998-01-01") %>% 
  count() %>% 
  mutate(Year = "1998")%>% 
  mutate(percent_cprd = (n/(all_patients_1998$n))*100)
definite_1999 <- definite_migrants %>% 
  filter(data_start <= "1999-12-31" & data_end >= "1999-01-01") %>% 
  count() %>% 
  mutate(Year = "1999")%>% 
  mutate(percent_cprd = (n/(all_patients_1999$n))*100)
definite_2000 <- definite_migrants %>% 
  filter(data_start <= "2000-12-31" & data_end >= "2000-01-01") %>% 
  count() %>% 
  mutate(Year = "2000")%>% 
  mutate(percent_cprd = (n/(all_patients_2000$n))*100)
definite_2001 <- definite_migrants %>% 
  filter(data_start <= "2001-12-31" & data_end >= "2001-01-01") %>% 
  count() %>% 
  mutate(Year = "2001") %>% 
  mutate(percent_cprd = (n/(all_patients_2001$n))*100)
definite_2002 <- definite_migrants %>% 
  filter(data_start <= "2002-12-31" & data_end >= "2002-01-01") %>% 
  count() %>% 
  mutate(Year = "2002") %>% 
  mutate(percent_cprd = (n/(all_patients_2002$n))*100)
definite_2003 <- definite_migrants %>% 
  filter(data_start <= "2003-12-31" & data_end >= "2003-01-01") %>% 
  count() %>% 
  mutate(Year = "2003") %>% 
  mutate(percent_cprd = (n/(all_patients_2003$n))*100)
definite_2004 <- definite_migrants %>% 
  filter(data_start <= "2004-12-31" & data_end >= "2004-01-01") %>% 
  count() %>% 
  mutate(Year = "2004") %>% 
  mutate(percent_cprd = (n/(all_patients_2004$n))*100)
definite_2005 <- definite_migrants %>% 
  filter(data_start <= "2005-12-31" & data_end >= "2005-01-01") %>% 
  count() %>% 
  mutate(Year = "2005") %>% 
  mutate(percent_cprd = (n/(all_patients_2005$n))*100)
definite_2006 <- definite_migrants %>% 
  filter(data_start <= "2006-12-31" & data_end >= "2006-01-01") %>% 
  count() %>% 
  mutate(Year = "2006") %>% 
  mutate(percent_cprd = (n/(all_patients_2006$n))*100)
definite_2007 <- definite_migrants %>% 
  filter(data_start <= "2007-12-31" & data_end >= "2007-01-01") %>% 
  count() %>% 
  mutate(Year = "2007") %>% 
  mutate(percent_cprd = (n/(all_patients_2007$n))*100)
definite_2008 <- definite_migrants %>% 
  filter(data_start <= "2008-12-31" & data_end >= "2008-01-01") %>% 
  count() %>% 
  mutate(Year = "2008") %>% 
  mutate(percent_cprd = (n/(all_patients_2008$n))*100)
definite_2009 <- definite_migrants %>% 
  filter(data_start <= "2009-12-31" & data_end >= "2009-01-01") %>% 
  count() %>% 
  mutate(Year = "2009") %>% 
  mutate(percent_cprd = (n/(all_patients_2009$n))*100)
definite_2010 <- definite_migrants %>% 
  filter(data_start <= "2010-12-31" & data_end >= "2010-01-01") %>% 
  count() %>% 
  mutate(Year = "2010") %>% 
  mutate(percent_cprd = (n/(all_patients_2010$n))*100)
definite_2011 <- definite_migrants %>% 
  filter(data_start <= "2011-12-31" & data_end >= "2011-01-01") %>% 
  count() %>% 
  mutate(Year = "2011") %>% 
  mutate(percent_cprd = (n/(all_patients_2011$n))*100)
definite_2012 <- definite_migrants %>% 
  filter(data_start <= "2012-12-31" & data_end >= "2012-01-01") %>% 
  count() %>% 
  mutate(Year = "2012") %>% 
  mutate(percent_cprd = (n/(all_patients_2012$n))*100)
definite_2013 <- definite_migrants %>% 
  filter(data_start <= "2013-12-31" & data_end >= "2013-01-01") %>% 
  count() %>% 
  mutate(Year = "2013") %>% 
  mutate(percent_cprd = (n/(all_patients_2013$n))*100)
definite_2014 <- definite_migrants %>% 
  filter(data_start <= "2014-12-31" & data_end >= "2014-01-01") %>% 
  count() %>% 
  mutate(Year = "2014") %>% 
  mutate(percent_cprd = (n/(all_patients_2014$n))*100)
definite_2015 <- definite_migrants %>% 
  filter(data_start <= "2015-12-31" & data_end >= "2015-01-01") %>% 
  count() %>% 
  mutate(Year = "2015") %>% 
  mutate(percent_cprd = (n/(all_patients_2015$n))*100)
definite_2016 <- definite_migrants %>% 
  filter(data_start <= "2016-12-31" & data_end >= "2016-01-01") %>% 
  count() %>% 
  mutate(Year = "2016") %>% 
  mutate(percent_cprd = (n/(all_patients_2016$n))*100)
definite_2017 <- definite_migrants %>% 
  filter(data_start <= "2017-12-31" & data_end >= "2017-01-01") %>% 
  count() %>% 
  mutate(Year = "2017") %>% 
  mutate(percent_cprd = (n/(all_patients_2017$n))*100)
definite_2018 <- definite_migrants %>% 
  filter(data_start <= "2018-12-31" & data_end >= "2018-01-01") %>% 
  count() %>% 
  mutate(Year = "2018") %>% 
  mutate(percent_cprd = (n/(all_patients_2018$n))*100)

definite_per_year <- full_join(definite_1997, definite_1998, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_1999, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2000, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2001, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2002, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2003, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2004, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2005, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2006, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2007, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2008, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2009, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2010, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2011, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2012, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2013, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2014, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2015, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2016, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2017, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_2018, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  select(Year, n, percent_cprd)

print(definite_per_year)

## Number of probable migrants per year 
probable_1997 <- probable_migrants %>% 
  filter(data_start <= "1997-12-31" & data_end >= "1997-01-01") %>% 
  count() %>% 
  mutate(Year = "1997") %>%
  mutate( percent_cprd = (n/(all_patients_1997$n))*100)
probable_1998 <- probable_migrants %>% 
  filter(data_start <= "1998-12-31" & data_end >= "1998-01-01") %>% 
  count() %>% 
  mutate(Year = "1998")  %>%
  mutate( percent_cprd = (n/(all_patients_1998$n))*100)
probable_1999 <- probable_migrants %>% 
  filter(data_start <= "1999-12-31" & data_end >= "1999-01-01") %>% 
  count() %>% 
  mutate(Year = "1999")  %>%
  mutate( percent_cprd = (n/(all_patients_1999$n))*100)
probable_2000 <- probable_migrants %>% 
  filter(data_start <= "2000-12-31" & data_end >= "2000-01-01") %>% 
  count() %>% 
  mutate(Year = "2000")  %>%
  mutate( percent_cprd = (n/(all_patients_2000$n))*100) 
probable_2001 <- probable_migrants %>% 
  filter(data_start <= "2001-12-31" & data_end >= "2001-01-01") %>% 
  count() %>% 
  mutate(Year = "2001") %>%
  mutate( percent_cprd = (n/(all_patients_2001$n))*100)
probable_2002 <- probable_migrants %>% 
  filter(data_start <= "2002-12-31" & data_end >= "2002-01-01") %>% 
  count() %>% 
  mutate(Year = "2002") %>%
  mutate( percent_cprd = (n/(all_patients_2002$n))*100)
probable_2003 <- probable_migrants %>% 
  filter(data_start <= "2003-12-31" & data_end >= "2003-01-01") %>% 
  count() %>% 
  mutate(Year = "2003") %>%
  mutate( percent_cprd = (n/(all_patients_2003$n))*100)
probable_2004 <- probable_migrants %>% 
  filter(data_start <= "2004-12-31" & data_end >= "2004-01-01") %>% 
  count() %>% 
  mutate(Year = "2004") %>%
  mutate( percent_cprd = (n/(all_patients_2004$n))*100)
probable_2005 <- probable_migrants %>% 
  filter(data_start <= "2005-12-31" & data_end >= "2005-01-01") %>% 
  count() %>% 
  mutate(Year = "2005")  %>%
  mutate( percent_cprd = (n/(all_patients_2005$n))*100)
probable_2006 <- probable_migrants %>% 
  filter(data_start <= "2006-12-31" & data_end >= "2006-01-01") %>% 
  count() %>% 
  mutate(Year = "2006")  %>%
  mutate( percent_cprd = (n/(all_patients_2006$n))*100)
probable_2007 <- probable_migrants %>% 
  filter(data_start <= "2007-12-31" & data_end >= "2007-01-01") %>% 
  count() %>% 
  mutate(Year = "2007")  %>%
  mutate( percent_cprd = (n/(all_patients_2007$n))*100)
probable_2008 <- probable_migrants %>% 
  filter(data_start <= "2008-12-31" & data_end >= "2008-01-01") %>% 
  count() %>% 
  mutate(Year = "2008")  %>%
  mutate( percent_cprd = (n/(all_patients_2008$n))*100)
probable_2009 <- probable_migrants %>% 
  filter(data_start <= "2009-12-31" & data_end >= "2009-01-01") %>% 
  count() %>% 
  mutate(Year = "2009")  %>%
  mutate( percent_cprd = (n/(all_patients_2009$n))*100)
probable_2010 <- probable_migrants %>% 
  filter(data_start <= "2010-12-31" & data_end >= "2010-01-01") %>% 
  count() %>% 
  mutate(Year = "2010")  %>%
  mutate( percent_cprd = (n/(all_patients_2010$n))*100)
probable_2011 <- probable_migrants %>% 
  filter(data_start <= "2011-12-31" & data_end >= "2011-01-01") %>% 
  count() %>% 
  mutate(Year = "2011")  %>%
  mutate( percent_cprd = (n/(all_patients_2011$n))*100)
probable_2012 <- probable_migrants %>% 
  filter(data_start <= "2012-12-31" & data_end >= "2012-01-01") %>% 
  count() %>% 
  mutate(Year = "2012")  %>%
  mutate( percent_cprd = (n/(all_patients_2012$n))*100)
probable_2013 <- probable_migrants %>% 
  filter(data_start <= "2013-12-31" & data_end >= "2013-01-01") %>% 
  count() %>% 
  mutate(Year = "2013") %>%
  mutate( percent_cprd = (n/(all_patients_2013$n))*100)
probable_2014 <- probable_migrants %>% 
  filter(data_start <= "2014-12-31" & data_end >= "2014-01-01") %>% 
  count() %>% 
  mutate(Year = "2014") %>%
  mutate( percent_cprd = (n/(all_patients_2014$n))*100)
probable_2015 <- probable_migrants %>% 
  filter(data_start <= "2015-12-31" & data_end >= "2015-01-01") %>% 
  count() %>% 
  mutate(Year = "2015") %>%
  mutate( percent_cprd = (n/(all_patients_2015$n))*100)
probable_2016 <- probable_migrants %>% 
  filter(data_start <= "2016-12-31" & data_end >= "2016-01-01") %>% 
  count() %>% 
  mutate(Year = "2016") %>%
  mutate( percent_cprd = (n/(all_patients_2016$n))*100)
probable_2017 <- probable_migrants %>% 
  filter(data_start <= "2017-12-31" & data_end >= "2017-01-01") %>% 
  count() %>% 
  mutate(Year = "2017") %>%
  mutate( percent_cprd = (n/(all_patients_2017$n))*100)
probable_2018 <- probable_migrants %>% 
  filter(data_start <= "2018-12-31" & data_end >= "2018-01-01") %>% 
  count() %>% 
  mutate(Year = "2018")  %>%
  mutate( percent_cprd = (n/(all_patients_2018$n))*100)

probable_per_year <- full_join(probable_1997, probable_1998, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_1999, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2000, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2001, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2002, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2003, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2004, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2005, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2006, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2007, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2008, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2009, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2010, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2011, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2012, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2013, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2014, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2015, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2016, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2017, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(probable_2018, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  select(Year, n, percent_cprd)

print(probable_per_year)


## Number of possible migrants per year 
possible_1997 <- possible_migrants %>% 
  filter(data_start <= "1997-12-31" & data_end >= "1997-01-01") %>% 
  count() %>% 
  mutate(Year = "1997") %>% 
  mutate(percent_cprd = (n/(all_patients_1997$n))*100)
possible_1998 <- possible_migrants %>% 
  filter(data_start <= "1998-12-31" & data_end >= "1998-01-01") %>% 
  count() %>% 
  mutate(Year = "1998")%>% 
  mutate(percent_cprd = (n/(all_patients_1998$n))*100)
possible_1999 <- possible_migrants %>% 
  filter(data_start <= "1999-12-31" & data_end >= "1999-01-01") %>% 
  count() %>% 
  mutate(Year = "1999") %>% 
  mutate(percent_cprd = (n/(all_patients_1999$n))*100)
possible_2000 <- possible_migrants %>% 
  filter(data_start <= "2000-12-31" & data_end >= "2000-01-01") %>% 
  count() %>% 
  mutate(Year = "2000") %>% 
  mutate(percent_cprd = (n/(all_patients_2000$n))*100)
possible_2001 <- possible_migrants %>% 
  filter(data_start <= "2001-12-31" & data_end >= "2001-01-01") %>% 
  count() %>% 
  mutate(Year = "2001") %>% 
  mutate(percent_cprd = (n/(all_patients_2001$n))*100)
possible_2002 <- possible_migrants %>% 
  filter(data_start <= "2002-12-31" & data_end >= "2002-01-01") %>% 
  count() %>% 
  mutate(Year = "2002") %>% 
  mutate(percent_cprd = (n/(all_patients_2002$n))*100)
possible_2003 <- possible_migrants %>% 
  filter(data_start <= "2003-12-31" & data_end >= "2003-01-01") %>% 
  count() %>% 
  mutate(Year = "2003") %>% 
  mutate(percent_cprd = (n/(all_patients_2003$n))*100)
possible_2004 <- possible_migrants %>% 
  filter(data_start <= "2004-12-31" & data_end >= "2004-01-01") %>% 
  count() %>% 
  mutate(Year = "2004") %>% 
  mutate(percent_cprd = (n/(all_patients_2004$n))*100)
possible_2005 <- possible_migrants %>% 
  filter(data_start <= "2005-12-31" & data_end >= "2005-01-01") %>% 
  count() %>% 
  mutate(Year = "2005") %>% 
  mutate(percent_cprd = (n/(all_patients_2005$n))*100)
possible_2006 <- possible_migrants %>% 
  filter(data_start <= "2006-12-31" & data_end >= "2006-01-01") %>% 
  count() %>% 
  mutate(Year = "2006") %>% 
  mutate(percent_cprd = (n/(all_patients_2006$n))*100)
possible_2007 <- possible_migrants %>% 
  filter(data_start <= "2007-12-31" & data_end >= "2007-01-01") %>% 
  count() %>% 
  mutate(Year = "2007") %>% 
  mutate(percent_cprd = (n/(all_patients_2007$n))*100)
possible_2008 <- possible_migrants %>% 
  filter(data_start <= "2008-12-31" & data_end >= "2008-01-01") %>% 
  count() %>% 
  mutate(Year = "2008") %>% 
  mutate(percent_cprd = (n/(all_patients_2008$n))*100)
possible_2009 <- possible_migrants %>% 
  filter(data_start <= "2009-12-31" & data_end >= "2009-01-01") %>% 
  count() %>% 
  mutate(Year = "2009") %>% 
  mutate(percent_cprd = (n/(all_patients_2009$n))*100)
possible_2010 <- possible_migrants %>% 
  filter(data_start <= "2010-12-31" & data_end >= "2010-01-01") %>% 
  count() %>% 
  mutate(Year = "2010") %>% 
  mutate(percent_cprd = (n/(all_patients_2010$n))*100)
possible_2011 <- possible_migrants %>% 
  filter(data_start <= "2011-12-31" & data_end >= "2011-01-01") %>% 
  count() %>% 
  mutate(Year = "2011") %>% 
  mutate(percent_cprd = (n/(all_patients_2011$n))*100)
possible_2012 <- possible_migrants %>% 
  filter(data_start <= "2012-12-31" & data_end >= "2012-01-01") %>% 
  count() %>% 
  mutate(Year = "2012") %>% 
  mutate(percent_cprd = (n/(all_patients_2012$n))*100)
possible_2013 <- possible_migrants %>% 
  filter(data_start <= "2013-12-31" & data_end >= "2013-01-01") %>% 
  count() %>% 
  mutate(Year = "2013") %>% 
  mutate(percent_cprd = (n/(all_patients_2013$n))*100)
possible_2014 <- possible_migrants %>% 
  filter(data_start <= "2014-12-31" & data_end >= "2014-01-01") %>% 
  count() %>% 
  mutate(Year = "2014") %>% 
  mutate(percent_cprd = (n/(all_patients_2014$n))*100)
possible_2015 <- possible_migrants %>% 
  filter(data_start <= "2015-12-31" & data_end >= "2015-01-01") %>% 
  count() %>% 
  mutate(Year = "2015") %>% 
  mutate(percent_cprd = (n/(all_patients_2015$n))*100)
possible_2016 <- possible_migrants %>% 
  filter(data_start <= "2016-12-31" & data_end >= "2016-01-01") %>% 
  count() %>% 
  mutate(Year = "2016") %>% 
  mutate(percent_cprd = (n/(all_patients_2016$n))*100)
possible_2017 <- possible_migrants %>% 
  filter(data_start <= "2017-12-31" & data_end >= "2017-01-01") %>% 
  count() %>% 
  mutate(Year = "2017") %>% 
  mutate(percent_cprd = (n/(all_patients_2017$n))*100)
possible_2018 <- possible_migrants %>% 
  filter(data_start <= "2018-12-31" & data_end >= "2018-01-01") %>% 
  count() %>% 
  mutate(Year = "2018") %>% 
  mutate(percent_cprd = (n/(all_patients_2018$n))*100)

possible_per_year <- full_join(possible_1997, possible_1998, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_1999, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2000, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2001, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2002, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2003, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2004, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2005, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2006, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2007, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2008, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2009, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2010, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2011, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2012, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2013, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2014, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2015, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2016, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2017, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(possible_2018, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  select(Year, n, percent_cprd)

print(possible_per_year)

## Number of definite + probable migrants per year 
definite_probable_1997 <- definite_probable_migrants %>% 
  filter(data_start <= "1997-12-31" & data_end >= "1997-01-01") %>% 
  count() %>% 
  mutate(Year = "1997") %>%
  mutate(percent_cprd = (n/(all_patients_1997$n))*100)
definite_probable_1998 <- definite_probable_migrants %>% 
  filter(data_start <= "1998-12-31" & data_end >= "1998-01-01") %>% 
  count() %>% 
  mutate(Year = "1998") %>%
  mutate(percent_cprd = (n/(all_patients_1998$n))*100)
definite_probable_1999 <- definite_probable_migrants %>% 
  filter(data_start <= "1999-12-31" & data_end >= "1999-01-01") %>% 
  count() %>% 
  mutate(Year = "1999") %>%
  mutate(percent_cprd = (n/(all_patients_1999$n))*100)
definite_probable_2000 <- definite_probable_migrants %>% 
  filter(data_start <= "2000-12-31" & data_end >= "2000-01-01") %>% 
  count() %>% 
  mutate(Year = "2000") %>%
  mutate(percent_cprd = (n/(all_patients_2000$n))*100)
definite_probable_2001 <- definite_probable_migrants %>% 
  filter(data_start <= "2001-12-31" & data_end >= "2001-01-01") %>% 
  count() %>% 
  mutate(Year = "2001") %>%
  mutate(percent_cprd = (n/(all_patients_2001$n))*100)
definite_probable_2002 <- definite_probable_migrants %>% 
  filter(data_start <= "2002-12-31" & data_end >= "2002-01-01") %>% 
  count() %>% 
  mutate(Year = "2002") %>%
  mutate(percent_cprd = (n/(all_patients_2002$n))*100)
definite_probable_2003 <- definite_probable_migrants %>% 
  filter(data_start <= "2003-12-31" & data_end >= "2003-01-01") %>% 
  count() %>% 
  mutate(Year = "2003")  %>%
  mutate(percent_cprd = (n/(all_patients_2003$n))*100)
definite_probable_2004 <- definite_probable_migrants %>% 
  filter(data_start <= "2004-12-31" & data_end >= "2004-01-01") %>% 
  count() %>% 
  mutate(Year = "2004")  %>%
  mutate(percent_cprd = (n/(all_patients_2004$n))*100)
definite_probable_2005 <- definite_probable_migrants %>% 
  filter(data_start <= "2005-12-31" & data_end >= "2005-01-01") %>% 
  count() %>% 
  mutate(Year = "2005")  %>%
  mutate(percent_cprd = (n/(all_patients_2005$n))*100)
definite_probable_2006 <- definite_probable_migrants %>% 
  filter(data_start <= "2006-12-31" & data_end >= "2006-01-01") %>% 
  count() %>% 
  mutate(Year = "2006")  %>%
  mutate(percent_cprd = (n/(all_patients_2006$n))*100)
definite_probable_2007 <- definite_probable_migrants %>% 
  filter(data_start <= "2007-12-31" & data_end >= "2007-01-01") %>% 
  count() %>% 
  mutate(Year = "2007") %>%
  mutate(percent_cprd = (n/(all_patients_2007$n))*100)
definite_probable_2008 <- definite_probable_migrants %>% 
  filter(data_start <= "2008-12-31" & data_end >= "2008-01-01") %>% 
  count() %>% 
  mutate(Year = "2008")  %>%
  mutate(percent_cprd = (n/(all_patients_2008$n))*100)
definite_probable_2009 <- definite_probable_migrants %>% 
  filter(data_start <= "2009-12-31" & data_end >= "2009-01-01") %>% 
  count() %>% 
  mutate(Year = "2009") %>%
  mutate(percent_cprd = (n/(all_patients_2009$n))*100)
definite_probable_2010 <- definite_probable_migrants %>% 
  filter(data_start <= "2010-12-31" & data_end >= "2010-01-01") %>% 
  count() %>% 
  mutate(Year = "2010")  %>%
  mutate(percent_cprd = (n/(all_patients_2010$n))*100)
definite_probable_2011 <- definite_probable_migrants %>% 
  filter(data_start <= "2011-12-31" & data_end >= "2011-01-01") %>% 
  count() %>% 
  mutate(Year = "2011")  %>%
  mutate(percent_cprd = (n/(all_patients_2011$n))*100)
definite_probable_2012 <- definite_probable_migrants %>% 
  filter(data_start <= "2012-12-31" & data_end >= "2012-01-01") %>% 
  count() %>% 
  mutate(Year = "2012") %>%
  mutate(percent_cprd = (n/(all_patients_2012$n))*100)
definite_probable_2013 <- definite_probable_migrants %>% 
  filter(data_start <= "2013-12-31" & data_end >= "2013-01-01") %>% 
  count() %>% 
  mutate(Year = "2013")  %>%
  mutate(percent_cprd = (n/(all_patients_2013$n))*100)
definite_probable_2014 <- definite_probable_migrants %>% 
  filter(data_start <= "2014-12-31" & data_end >= "2014-01-01") %>% 
  count() %>% 
  mutate(Year = "2014")  %>%
  mutate(percent_cprd = (n/(all_patients_2014$n))*100)
definite_probable_2015 <- definite_probable_migrants %>% 
  filter(data_start <= "2015-12-31" & data_end >= "2015-01-01") %>% 
  count() %>% 
  mutate(Year = "2015")  %>%
  mutate(percent_cprd = (n/(all_patients_2015$n))*100)
definite_probable_2016 <- definite_probable_migrants %>% 
  filter(data_start <= "2016-12-31" & data_end >= "2016-01-01") %>% 
  count() %>% 
  mutate(Year = "2016")  %>%
  mutate(percent_cprd = (n/(all_patients_2016$n))*100)
definite_probable_2017 <- definite_probable_migrants %>% 
  filter(data_start <= "2017-12-31" & data_end >= "2017-01-01") %>% 
  count() %>% 
  mutate(Year = "2017")  %>%
  mutate(percent_cprd = (n/(all_patients_2017$n))*100)
definite_probable_2018 <- definite_probable_migrants %>% 
  filter(data_start <= "2018-12-31" & data_end >= "2018-01-01") %>% 
  count() %>% 
  mutate(Year = "2018")  %>%
  mutate(percent_cprd = (n/(all_patients_2018$n))*100)

definite_probable_year <- full_join(definite_probable_1997, definite_probable_1998, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_1999, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2000, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2001, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2002, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2003, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2004, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2005, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2006, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2007, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2008, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2009, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2010, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2011, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2012, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2013, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2014, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2015, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2016, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2017, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  full_join(definite_probable_2018, by = c("Year" = "Year", "n" = "n", "percent_cprd" = "percent_cprd")) %>% 
  select(Year, n, percent_cprd)

print(definite_probable_year)

## Completeness (at the time of 2011 Census) --------------------------------------------------------------------------------------

# 2011 census occurred on 27th March 2011
# Only applicable to England and Wales

## All migrants

migrants_census <- migrants_clean %>% 
  filter(data_start <= ymd("2011-03-27") & data_end >= ymd("2011-03-27")) %>% 
  filter(prac_region != "Scotland") %>% 
  filter(prac_region != "Northern Ireland")
cprd_census <- all_patients_clean %>% 
  filter(data_start <= ymd("2011-03-27") & data_end >= ymd("2011-03-27")) %>% 
  filter(prac_region != "Scotland") %>% 
  filter(prac_region != "Northern Ireland")
migrants_census_count <- count(migrants_census)
cprd_census_count <- count(cprd_census)
migrants_census_percent_cprd <- (migrants_census_count / cprd_census_count) *100
migrants_census_count
cprd_census_count
migrants_census_percent_cprd

## Definite migrants 

definite_migrants_census <- definite_migrants %>% 
  filter(data_start <= ymd("2011-03-27") & data_end >= ymd("2011-03-27")) %>% 
  filter(prac_region != "Scotland") %>% 
  filter(prac_region != "Northern Ireland")
definite_migrants_census_count <- count(definite_migrants_census)
definite_migrants_census_percent_cprd <- (definite_migrants_census_count / cprd_census_count) *100
definite_migrants_census_percent_migrants <- (definite_migrants_census_count / migrants_census_count) *100
definite_migrants_census_count
definite_migrants_census_percent_cprd
definite_migrants_census_percent_migrants

## Probable migrants

probable_migrants_census <- probable_migrants %>% 
  filter(data_start <= ymd("2011-03-27") & data_end >= ymd("2011-03-27")) %>% 
  filter(prac_region != "Scotland") %>% 
  filter(prac_region != "Northern Ireland")
probable_migrants_census_count <- count(probable_migrants_census)
probable_migrants_census_percent_cprd <- (probable_migrants_census_count / cprd_census_count) *100
probable_migrants_census_percent_migrants <- (probable_migrants_census_count / migrants_census_count) *100
probable_migrants_census_count
probable_migrants_census_percent_cprd
probable_migrants_census_percent_migrants

# Possible migrants

possible_migrants_census <- possible_migrants %>% 
  filter(data_start <= ymd("2011-03-27") & data_end >= ymd("2011-03-27")) %>% 
  filter(prac_region != "Scotland") %>% 
  filter(prac_region != "Northern Ireland")
possible_migrants_census_count <- count(possible_migrants_census)
possible_migrants_census_percent_cprd <- (possible_migrants_census_count / cprd_census_count) *100
possible_migrants_census_percent_migrants <- (possible_migrants_census_count / migrants_census_count) *100
possible_migrants_census_count
possible_migrants_census_percent_cprd
possible_migrants_census_percent_migrants

## Definite and probable migrants

definite_probable_migrants_census <- full_join(definite_migrants_census, probable_migrants_census,
                                               by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender",
                                                      "frd" = "frd", "deathdate" = "deathdate", "yob" = "yob", 
                                                      "prac_region" = "prac_region",
                                                      "dob" = "dob", "data_start" = "data_start", "data_end" = "data_end", 
                                                      "eligible" = "eligible", "medcode" = "medcode", 
                                                      "eventdate" = "eventdate", "category" = "category"))
definite_probable_migrants_census_count <- count(definite_probable_migrants_census)
definite_probable_migrants_census_percent_cprd <- (definite_probable_migrants_census_count / cprd_census_count) *100
definite_probable_migrants_census_percent_migrants <- (definite_probable_migrants_census_count / migrants_census_count) *100
definite_probable_migrants_census_count
definite_probable_migrants_census_percent_cprd
definite_probable_migrants_census_percent_migrants


## Description (at the time of 2011 Census)----------------------------------------------------

## *Sex ---

## Sex breakdown of all migrants and  all patients at time of census 
migrants_census_gender <- count(group_by(migrants_census, gender)) %>% mutate( percentage_of_migrants = (n/migrants_census_count)*100)
migrants_census_gender
cprd_census_gender <- count(group_by(cprd_census, gender)) %>% mutate( percentage_of_all_patients = (n/cprd_census_count)*100)
cprd_census_gender

## sex breakdown of by certainty of migration status : number and percent
definite_census_gender_percent <- count(group_by(definite_migrants_census, gender)) %>% 
  mutate( percentage_of_definite_migrants = (n/(nrow(definite_migrants_census)))*100)
definite_census_gender_percent
probable_census_gender_percent <- count(group_by(probable_migrants_census, gender)) %>% 
  mutate( percentage_of_probable_migrants = (n/(nrow(probable_migrants_census)))*100)
probable_census_gender_percent
possible_census_gender_percent <- count(group_by(possible_migrants_census, gender)) %>% 
  mutate( percentage_of_possible_migrants = (n/(nrow(possible_migrants_census)))*100)
possible_census_gender_percent
definite_probable_census_gender_percent <- count(group_by(definite_probable_migrants_census, gender)) %>% 
  mutate( percentage_of_definite_probable_migrants = (n/(nrow(definite_probable_migrants_census)))*100)
definite_probable_census_gender_percent

## *Year of birth ---

## number and percent of all migrants in Yob groups
migrants_yob_1900_1919_census <- migrants_census %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(migrants_census)))*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
migrants_yob_1920_1939_census <- migrants_census %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(migrants_census)))*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
migrants_yob_1940_1959_census <- migrants_census %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(migrants_census)))*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
migrants_yob_1960_1979_census <- migrants_census %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(migrants_census)))*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
migrants_yob_1980_1999_census <- migrants_census %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(migrants_census)))*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
migrants_yob_2000_2018_census <- migrants_census %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(migrants_census)))*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

migrants_yob_gps_census <- full_join(migrants_yob_1900_1919_census, migrants_yob_1920_1939_census, 
                                     by = c("yob_grp" = "yob_grp", "n" = "n",
                                            "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(migrants_yob_1940_1959_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                 "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(migrants_yob_1960_1979_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                 "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(migrants_yob_1980_1999_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                 "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(migrants_yob_2000_2018_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                 "percent_of_migrants" = "percent_of_migrants") )
migrants_yob_gps_census  

## Definite migrants yob grps 

definite_migrants_yob_1900_1919_census <- definite_migrants_census %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
definite_migrants_yob_1920_1939_census <- definite_migrants_census %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
definite_migrants_yob_1940_1959_census <- definite_migrants_census %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
definite_migrants_yob_1960_1979_census <- definite_migrants_census %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
definite_migrants_yob_1980_1999_census <- definite_migrants_census %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
definite_migrants_yob_2000_2018_census <- definite_migrants_census %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

definite_migrants_yob_gps_census <- full_join(definite_migrants_yob_1900_1919_census, definite_migrants_yob_1920_1939_census, 
                                              by = c("yob_grp" = "yob_grp", "n" = "n",
                                                     "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(definite_migrants_yob_1940_1959_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_migrants_yob_1960_1979_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_migrants_yob_1980_1999_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_migrants_yob_2000_2018_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") )
definite_migrants_yob_gps_census 

## Probable migrants yob grps 

probable_migrants_yob_1900_1919_census  <- probable_migrants_census %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
probable_migrants_yob_1920_1939_census <- probable_migrants_census %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
probable_migrants_yob_1940_1959_census <- probable_migrants_census %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
probable_migrants_yob_1960_1979_census <- probable_migrants_census %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
probable_migrants_yob_1980_1999_census <- probable_migrants_census %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
probable_migrants_yob_2000_2018_census <- probable_migrants_census %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

probable_migrants_yob_gps_census <- full_join(probable_migrants_yob_1900_1919_census, probable_migrants_yob_1920_1939_census, 
                                              by = c("yob_grp" = "yob_grp", "n" = "n",
                                                     "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(probable_migrants_yob_1940_1959_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(probable_migrants_yob_1960_1979_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(probable_migrants_yob_1980_1999_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(probable_migrants_yob_2000_2018_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") )
probable_migrants_yob_gps_census

## Possible migrants yob grps 

possible_migrants_yob_1900_1919_census <- possible_migrants_census %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
possible_migrants_yob_1920_1939_census <- possible_migrants_census %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
possible_migrants_yob_1940_1959_census <- possible_migrants_census %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
possible_migrants_yob_1960_1979_census <- possible_migrants_census %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
possible_migrants_yob_1980_1999_census <- possible_migrants_census %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
possible_migrants_yob_2000_2018_census <- possible_migrants_census %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

possible_migrants_yob_gps_census <- full_join(possible_migrants_yob_1900_1919_census, possible_migrants_yob_1920_1939_census, 
                                              by = c("yob_grp" = "yob_grp", "n" = "n",
                                                     "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(possible_migrants_yob_1940_1959_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(possible_migrants_yob_1960_1979_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(possible_migrants_yob_1980_1999_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(possible_migrants_yob_2000_2018_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                          "percent_of_migrants" = "percent_of_migrants") )
possible_migrants_yob_gps_census

## Definite and probable migrants yob grps 

definite_probable_migrants_yob_1900_1919_census <- definite_probable_migrants_census %>% 
  filter(yob <= "1919" & yob >= "1900") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1900_1919") %>% select(c(3,1,2))
definite_probable_migrants_yob_1920_1939_census <- definite_probable_migrants_census %>% 
  filter(yob <= "1939" & yob >= "1920") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1920_1939") %>% select(c(3,1,2))
definite_probable_migrants_yob_1940_1959_census <- definite_probable_migrants_census %>% 
  filter(yob <= "1959" & yob >= "1940") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1940_1959") %>% select(c(3,1,2))
definite_probable_migrants_yob_1960_1979_census <- definite_probable_migrants_census %>% 
  filter(yob <= "1979" & yob >= "1960") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1960_1979") %>% select(c(3,1,2))
definite_probable_migrants_yob_1980_1999_census <- definite_probable_migrants_census %>% 
  filter(yob <= "1999" & yob >= "1980") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "1980_1999") %>% select(c(3,1,2))
definite_probable_migrants_yob_2000_2018_census <- definite_probable_migrants_census %>% 
  filter(yob <= "2018" & yob >= "2000") %>% 
  count %>%
  mutate(percent_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate (yob_grp = "2000_2018") %>% select(c(3,1,2))

definite_probable_migrants_yob_gps_census <- full_join(definite_probable_migrants_yob_1900_1919_census, definite_probable_migrants_yob_1920_1939_census, 
                                                       by = c("yob_grp" = "yob_grp", "n" = "n",
                                                              "percent_of_migrants" = "percent_of_migrants")) %>%
  full_join(definite_probable_migrants_yob_1940_1959_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_probable_migrants_yob_1960_1979_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_probable_migrants_yob_1980_1999_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                                   "percent_of_migrants" = "percent_of_migrants") ) %>%
  full_join(definite_probable_migrants_yob_2000_2018_census,by = c("yob_grp" = "yob_grp", "n" = "n",
                                                                   "percent_of_migrants" = "percent_of_migrants") )
definite_probable_migrants_yob_gps_census 

## *Practice region ---

## Count and percentage by practice region
prac_region_count_census <- count(group_by(migrants_census, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/(nrow(migrants_census)))*100)
prac_region_count_census 

## Count and percentage by practice region : definite migrants 
prac_region_count_definite_census <- count(group_by(definite_migrants_census, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/(nrow(definite_migrants_census)))*100)
prac_region_count_definite_census 

## Count and percentage by practice region : probable migrants 
prac_region_count_probable_census <- count(group_by(probable_migrants_census, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/(nrow(probable_migrants_census)))*100)
prac_region_count_probable_census 

## Count and percentage by practice region : possible migrants 
prac_region_count_possible_census <- count(group_by(possible_migrants_census, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/(nrow(possible_migrants_census)))*100)
prac_region_count_possible_census

## Count and percentage by practice region : definite + probable migrants 
prac_region_count_definite_probable_census <- count(group_by(definite_probable_migrants_census, prac_region)) %>% 
  arrange(desc(n)) %>% 
  mutate(percentage_of_migrants = (n/(nrow(definite_probable_migrants_census)))*100)
prac_region_count_definite_probable_census


## What proption of patients registered in each region are migrants 
cprd_census_prac_region <- cprd_census %>% group_by(prac_region) %>% count()
migrants_census_prac_region <- migrants_census %>% group_by(prac_region) %>% count()
prac_region_proportions_census <- full_join(migrants_census_prac_region, cprd_census_prac_region, b = c("prac_region" = "prac_region")) %>%
  mutate(proportion_of_region = (n.x/n.y)*100) %>%
  rename(n_migrants = n.x) %>%
  rename(n_allpatients = n.y)
write_csv(prac_region_proportions_census, "filepath/prac_region_proportions_census.csv")


## *Ethnic groups ---

## Join migrant cohort file to ethnicity events data to find all ethnicity data relating to migrants
migrants_ethnicity_census <- left_join(migrants_census, ethnicity_clean, by = c("patid" = "patid"))

## Number of migrants per ethnic group - 18 groups classification
count_migrants_ethnicity_census_18 <- count(group_by(migrants_ethnicity_census, ethnicat))
count_percent_migrants_ethnicity_census_18 <- count_migrants_ethnicity_census_18 %>% 
  mutate(percent = (n/(nrow(migrants_census)))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_migrants_ethnicity_census_18
count_percent_migrants_ethnicity_census_18$n_percent <- paste(count_percent_migrants_ethnicity_census_18$n, count_percent_migrants_ethnicity_census_18$percent, sep =",")
count_percent_migrants_ethnicity_census_18
write_csv(count_percent_migrants_ethnicity_census_18, "filepath/count_percent_migrants_ethnicity_census_18.csv")

## Number of migrants per ethnic group - 6 group classification 
count_migrants_ethnicity_census <- count(group_by(migrants_ethnicity_census, ethnicat6))
count_percent_migrants_ethnicity_census <- count_migrants_ethnicity_census %>% 
  mutate(percent = (n/(nrow(migrants_census)))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_migrants_ethnicity_census
count_percent_migrants_ethnicity_census$n_percent <- paste(count_percent_migrants_ethnicity_census$n, count_percent_migrants_ethnicity_census$percent, sep =",")
count_percent_migrants_ethnicity_census

## 6 group ethnic classification : definite migrants

definite_migrants_ethnicity_census <- left_join(definite_migrants_census, migrants_ethnicity, 
                                                by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                       "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                       "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_migrants_ethnicity_census <- count(group_by(definite_migrants_ethnicity_census, ethnicat6)) 
count_percent_definite_migrants_ethnicity_census <- count_definite_migrants_ethnicity_census %>%
  mutate (percentage = (n/(nrow(definite_migrants_census)))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_definite_migrants_ethnicity_census

## 6 group ethnic classification : probable migrants

probable_migrants_ethnicity_census <- left_join(probable_migrants_census, migrants_ethnicity, 
                                                by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                       "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                       "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_probable_migrants_ethnicity_census <- count(group_by(probable_migrants_ethnicity_census, ethnicat6)) 
count_percent_probable_migrants_ethnicity_census <- count_probable_migrants_ethnicity_census %>%
  mutate (percentage = (n/(nrow(probable_migrants_census)))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_probable_migrants_ethnicity_census

## 6 group ethnic classification : possible migrants

possible_migrants_ethnicity_census <- left_join(possible_migrants_census, migrants_ethnicity, 
                                                by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                       "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                       "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_possible_migrants_ethnicity_census <- count(group_by(possible_migrants_ethnicity_census, ethnicat6)) 
count_percent_possible_migrants_ethnicity_census <- count_possible_migrants_ethnicity_census %>%
  mutate (percentage = (n/(nrow(possible_migrants_census)))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_possible_migrants_ethnicity_census

## 6 group ethnic classification : definite and probable migrants

definite_probable_migrants_ethnicity_census <- left_join(definite_probable_migrants_census, migrants_ethnicity, 
                                                         by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                                "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                                "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_probable_migrants_ethnicity_census <- count(group_by(definite_probable_migrants_ethnicity_census, ethnicat6)) 
count_percent_definite_probable_migrants_ethnicity_census <- count_definite_probable_migrants_ethnicity_census %>%
  mutate (percentage = (n/(nrow(definite_probable_migrants_census)))*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
count_percent_definite_probable_migrants_ethnicity_census


## *Region of birth ---

## Join migrant cohort file to migration events data to find all migration data relating to migrants
migrants_migration_census <- left_join(migrants_census, migration_clean, by = c("patid" = "patid"))

## Filter for migrants with migration event data indicating a country of birth 
migrants_cob_census <- migrants_migration_census %>% filter(category == "Born outside of the UK")

## Count number and percent of migrants with any cob recorded
distinct_migrants_cob_census <-  migrants_cob_census %>% 
  distinct(patid,  .keep_all = TRUE)
count_distinct_migrants_cob_census <- count(distinct_migrants_cob_census)
count_percent_migrants_cob_census <- count_distinct_migrants_cob_census %>% 
  mutate(percentage_of_migrants = (n/(nrow(migrants_census)))*100)
count_percent_migrants_cob_census 

## Confirm some migrants have more than one country of birth event assigned
count(migrants_cob_census) > count(distinct_migrants_cob_census)

## Import and tidy categorisation of born in codes into WHO region and ONS region
migration_region_categories_census <- read_csv("filepath/migration_region_categories.csv")
tidy_migration_region_categories_census <-  migration_region_categories_census %>% 
  mutate_if(is.numeric, as.integer) 
tidy_migration_region_categories_census$category <- factor(tidy_migration_region_categories_census$category, levels = c(1,2,3,4), 
                                                           labels = c("Non-UK origin", "Born outside of the UK", "First/main language not English", 
                                                                      "Visa status indicating migration"))
tidy_migration_region_categories_census$who_region <- factor(tidy_migration_region_categories_census$who_region, levels = 
                                                               c("afro","euro", "emro", "paho", "searo", "wpro"), 
                                                             labels = c("afro","euro", "emro", "paho", "searo", "wpro"))
tidy_migration_region_categories_census$ons_nomis_region <- factor(tidy_migration_region_categories_census$ons_nomis_region,
                                                                   levels = c("Africa","Europe", "Middle East & Asia",
                                                                              "The Americas & Caribbean", "Antarctica & Oceania"), 
                                                                   labels = c("Africa","Europe", "Middle East & Asia",
                                                                              "The Americas & Caribbean", "Antarctica & Oceania"))
glimpse(tidy_migration_region_categories_census)
levels(tidy_migration_region_categories_census$category)
levels(tidy_migration_region_categories_census$who_region)
levels(tidy_migration_region_categories_census$ons_nomis_region)
tidy_migration_region_categories_census <- tidy_migration_region_categories_census %>% 
  select(category, medcode, who_region, ons_nomis_region)

## join region categories to all migrants + migration events with cob data 
migrants_cob_regions_census <- left_join(migrants_cob_census, tidy_migration_region_categories_census, by = c("medcode", "category"))


## Check for and remove duplicate records based on patid and who_region i.e. patient cob events that are not conflicting based on 18 group classification
distinct_migrants_cob_who_region_census <- migrants_cob_regions_census %>% 
  distinct(patid, who_region,  .keep_all = TRUE)

## Count number of migrants with > 1 WHO region category i.e. conflicting categories 
migrants_with_multiple_who_region_census <- count(distinct_migrants_cob_who_region_census) - count_distinct_migrants_cob_census
migrants_with_multiple_who_region_census

## Find number of conflicting WHO region records per patient, number of patients per number of conflicting records, create list of migrant patids with no conflicting records
number_of_who_region_records_per_patid_census <- count(group_by(distinct_migrants_cob_who_region_census, patid))
unique(number_of_who_region_records_per_patid_census$n)
number_of_migrants_per_number_of_who_region_records_per_patient_census <- count(group_by(number_of_who_region_records_per_patid_census, n))
number_of_migrants_per_number_of_who_region_records_per_patient_census
migrants_with_no_conflicting_who_region_census <- number_of_who_region_records_per_patid_census %>% filter(n == 1)

## Filter for patients with no conflicting records of WHO region
migrants_who_region_no_conflicts_census <- distinct_migrants_cob_who_region_census %>% 
  filter(patid %in% migrants_with_no_conflicting_who_region_census$patid )

## Number of migrants per who_region 
count_migrants_who_region_no_conflicts_census <- count(group_by(migrants_who_region_no_conflicts_census, who_region))
count_migrants_who_region_no_conflicts_census
count_percent_migrants_who_region_no_conflicts_census <- count_migrants_who_region_no_conflicts_census %>% 
  mutate(percentage_of_migrants = (n/(nrow(migrants_census)))*100)
count_percent_migrants_who_region_no_conflicts_census
no_valid_who_region_census <- (nrow(migrants_census)) - (nrow(migrants_with_no_conflicting_who_region_census))
no_valid_who_region_percent_census <- (no_valid_who_region_census/(nrow(migrants_census)))*100
no_valid_who_region_census
no_valid_who_region_percent_census

## WHO region : definite migrants

definite_migrants_who_region_census <- left_join(definite_migrants_census, migrants_who_region_no_conflicts_census, 
                                                 by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                        "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                        "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_migrants_who_region_census <- count(group_by(definite_migrants_who_region_census, who_region)) 
count_percent_definite_migrants_who_region_census <- count_definite_migrants_who_region_census %>%
  mutate (percentage_of_definite_migrants_census = (n/(nrow(definite_migrants_census)))*100)
count_percent_definite_migrants_who_region_census

## Check for and remove duplicate records based on patid and ons nomis region i.e. events that are not conflicting 
distinct_migrants_cob_ons_nomis_region_census <- migrants_cob_regions_census %>% 
  distinct(patid, ons_nomis_region,  .keep_all = TRUE)

## Count number of migrants with > 1 ONS nomis region category i.e. conflicting categories 
migrants_with_multiple_ons_nomis_region_census <- count(distinct_migrants_cob_ons_nomis_region_census) - count_distinct_migrants_cob_census
migrants_with_multiple_ons_nomis_region_census

## Find number of conflicting WHO region records per patient, number of patients per number of conflicting records, create list of migrant patids with no conflicting records
number_of_ons_nomis_region_records_per_patid_census <- count(group_by(distinct_migrants_cob_ons_nomis_region_census, patid))
unique(number_of_ons_nomis_region_records_per_patid_census$n)
number_of_migrants_per_number_of_ons_nomis_region_records_per_patient_census <- count(group_by(number_of_ons_nomis_region_records_per_patid_census, n))
number_of_migrants_per_number_of_ons_nomis_region_records_per_patient_census
migrants_with_no_conflicting_ons_nomis_region_census <- number_of_ons_nomis_region_records_per_patid_census %>% filter(n == 1)

## Filter for patients with no conflicting records of ONS Nomis region
migrants_ons_nomis_region_no_conflicts_census <- distinct_migrants_cob_ons_nomis_region_census %>% 
  filter(patid %in% migrants_with_no_conflicting_ons_nomis_region_census$patid)

## Number of migrants per ons_nomis_region
count_migrants_ons_nomis_region_no_conflicts_census <- count(group_by(migrants_ons_nomis_region_no_conflicts_census, ons_nomis_region))
count_migrants_ons_nomis_region_no_conflicts_census
count_percent_migrants_ons_nomis_region_no_conflicts_census <- count_migrants_ons_nomis_region_no_conflicts_census %>% 
  mutate(percentage_of_migrants = (n/(nrow(migrants_census)))*100)
count_percent_migrants_ons_nomis_region_no_conflicts_census
no_valid_ons_nomis_region_census <- (nrow(migrants_census)) - (nrow(migrants_with_no_conflicting_ons_nomis_region_census))
no_valid_ons_nomis_region_percent_census <- (no_valid_ons_nomis_region_census/(nrow(migrants_census)))*100
no_valid_ons_nomis_region_census
no_valid_ons_nomis_region_percent_census

## ONS nomis region : definite migrants

definite_migrants_ons_nomis_region_census <- left_join(definite_migrants_census, migrants_ons_nomis_region_no_conflicts_census, 
                                                       by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", "frd" = "frd", 
                                                              "deathdate" = "deathdate", "yob" = "yob", "prac_region" = "prac_region", "dob" = "dob",
                                                              "data_start" = "data_start", "data_end" = "data_end", "eligible" = "eligible"))
count_definite_migrants_ons_nomis_region_census <- count(group_by(definite_migrants_ons_nomis_region_census, ons_nomis_region)) 
count_percent_definite_migrants_ons_nomis_region_census <- count_definite_migrants_ons_nomis_region_census %>%
  mutate (percentage_of_definite_migrants = (n/(nrow(definite_migrants_census)))*100)
count_percent_definite_migrants_ons_nomis_region_census


# 05_Data analysis - Representativeness --------------------------------------


## Representativeness (all years) --------------------------------------------------------------------------------------

## Max and min percentages of all migrants
percentage_per_year_desc <- arrange(percentage_per_year, desc(percentage_migrants_in_cprd)) 

## Join all yearly counts and percentages into one table with category for certainty of migration status
percentage_per_year_category <- percentage_per_year %>% 
  mutate(category = "All migrants") %>% 
  rename("percent_cprd" = "percentage_migrants_in_cprd")
percentage_per_year_category$Year <- as.integer(percentage_per_year_category$Year)
definite_per_year_category <- definite_per_year %>% 
  full_join(all_patients_per_year, by = c("Year" = "Year")) %>%
  rename(c("n_migrants" = "n.x","n_all_patients" = "n.y")) %>% 
  mutate(category = "Definite")
definite_per_year_category$Year <- as.integer(definite_per_year_category$Year)
probable_per_year_category <- probable_per_year %>% 
  full_join(all_patients_per_year, by = c("Year" = "Year")) %>%
  rename(c("n_migrants" = "n.x","n_all_patients" = "n.y")) %>% 
  mutate(category = "Probable")
probable_per_year_category$Year <- as.integer(probable_per_year_category$Year)
possible_per_year_category <- possible_per_year %>% 
  full_join(all_patients_per_year, by = c("Year" = "Year")) %>%
  rename(c("n_migrants" = "n.x","n_all_patients" = "n.y")) %>% 
  mutate(category = "Possible")
possible_per_year_category$Year <- as.integer(possible_per_year_category$Year)
definite_probable_year_category <- definite_probable_year %>% 
  full_join(all_patients_per_year, by = c("Year" = "Year")) %>%
  rename(c("n_migrants" = "n.x","n_all_patients" = "n.y")) %>% 
  mutate(category = "Definite + Probable")
definite_probable_year_category$Year <- as.integer(definite_probable_year_category$Year)
all_per_year_category <- full_join(percentage_per_year_category, definite_per_year_category, 
                                   by = c("Year" = "Year", "n_migrants", "n_all_patients", 
                                          "percent_cprd" = "percent_cprd", "category" = "category")) %>%
  full_join(probable_per_year_category, by = c("Year" = "Year", "n_migrants", "n_all_patients", 
                                              "percent_cprd" = "percent_cprd", "category" = "category")) %>%
  full_join(possible_per_year_category, by = c("Year" = "Year", "n_migrants", "n_all_patients", 
                                              "percent_cprd" = "percent_cprd", "category" = "category") ) %>%
  full_join(definite_probable_year_category, by = c("Year" = "Year", "n_migrants", "n_all_patients", 
                                                    "percent_cprd" = "percent_cprd", "category" = "category")) 

all_per_year_category$category <- factor(all_per_year_category$category, levels = c("ONS", "All migrants", "Definite", "Probable", 
                                                                                     "Possible", "Definite + Probable"))
all_per_year_category$category <- recode(all_per_year_category$category, "ONS" = "Migrants (ONS)", "All migrants" = "Migrants (CPRD GOLD)",
                                                                                    "Definite" = "Definite Migrants (CPRD GOLD)", "Probable" =  "Probable Migrants (CPRD GOLD)"  , 
                                                                                    "Possible" = "Possible Migrants (CPRD GOLD)" ,
                                                                                    "Definite + Probable" = "Definite + Probable Migrants (CPRD GOLD)")  
levels(all_per_year_category$category)
all_per_year_category <- all_per_year_category %>% 
  rename(c("percent" = "percent_cprd", "n_population" = "n_all_patients" ))

# import and tidy yearly counts for ons to compare
ons_counts <- read_csv("filepath/ons_counts.csv") # available in repository
ons_counts$Year <- as.integer(ons_counts$Year)
ons_counts$n_population <- as.integer(ons_counts$n_population)
ons_counts$n_migrants <- as.integer(ons_counts$n_migrants)
ons_counts$n_non_eu_migrants <- as.integer(ons_counts$n_non_eu_migrants)
ons_counts$category <- factor(ons_counts$category, 
                              levels =  c("ONS", "All migrants", "Definite", "Probable", 
                                          "Possible", "Definite + Probable"))
ons_counts$category <- recode(ons_counts$category, "ONS" = "Migrants (ONS)", "All migrants" = "Migrants (CPRD GOLD)",
                                         "Definite" = "Definite Migrants (CPRD GOLD)", "Probable" =  "Probable Migrants (CPRD GOLD)"  , 
                                         "Possible" = "Possible Migrants (CPRD GOLD)" ,
                                         "Definite + Probable" = "Definite + Probable Migrants (CPRD GOLD)")  

glimpse(ons_counts)
ons_counts_per_year <-  select(ons_counts, -n_non_eu_migrants)

## join ons to all_per_ year
all_per_year_category_ons <- full_join(all_per_year_category, ons_counts_per_year, by = c("Year" = "Year", "n_migrants",  "n_population", 
                                                        "percent" = "percent", "category" = "category"))
                                                                
## plot cprd percents by migration status certainty and ons
# drop definite + probable first
all_per_year_category_ons_nodp <- all_per_year_category_ons %>% filter(category != "Definite + Probable Migrants (CPRD GOLD)")

time_plot <-  ggplot(all_per_year_category_ons_nodp) + geom_line(aes(x= Year, y = percent, group = category, color = category)) +
  geom_point(aes(x = Year, y = percent, group = category, color = category)) +
  ggtitle("Percentage Migrants in ONS compared to CPRD stratfied by certainty of migration status ") +
  labs(y="Percentage Migrants in ONS compared to CPRD stratfied by certainty of migration status ", color = "Key")
print(time_plot)

## plot cprd percents by migration status certainty and ons for 2004 onwards only 
all_per_year_category_ons_2004_2018 <-  all_per_year_category_ons_nodp %>% filter(Year >= "2004")
time_plot_2004_2018 <-  ggplot(all_per_year_category_ons_2004_2018) + geom_line(aes(x= Year, y = percent, group = category, color = category)) +
  geom_point(aes(x = Year, y = percent, group = category, color = category)) +
  ggtitle("Percentage of Migrants in ONS compared to CPRD stratified by certainty of migration status 2004-2018 ") +
  labs(y="Percentage of Population", x = "Year", color = "Key")
print(time_plot_2004_2018)
ggsave(width = 8, height = 4, dpi = 450, "filepath/time_plot_2004_2018.jpg")

all_per_year_category_ons_2004_2018_nmigrants <- all_per_year_category_ons_2004_2018 %>%
  filter(category == "Migrants (CPRD GOLD)") %>%
  select(c(Year, n_migrants))

## Calculate 95% confidence intervals for CPRD data
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2004]),(all_patients_2004$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2005]),(all_patients_2005$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2006]),(all_patients_2006$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2007]),(all_patients_2007$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2008]),(all_patients_2008$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2009]),(all_patients_2009$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2010]),(all_patients_2010$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2011]),(all_patients_2011$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2012]),(all_patients_2012$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2013]),(all_patients_2013$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2014]),(all_patients_2014$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2015]),(all_patients_2015$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2016]),(all_patients_2016$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2017]),(all_patients_2017$n))
prop.test((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2018]),(all_patients_2018$n))

## calculate proportion of CPRD n migrants over ONS n migrants
percentage_per_year$Year <- as.integer(percentage_per_year$Year)
n_migrants_cprd_ons <- full_join(percentage_per_year, ons_counts_per_year, 
                                                      by = c("Year" = "Year")) %>%
  mutate(proportion_x_y = n_migrants.x/n_migrants.y)

## Chi squared test for difference between ONS proportions and CPRD proportions

prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2004]),(all_patients_2004$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2004]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2004])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2005]),(all_patients_2005$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2005]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2005])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2006]),(all_patients_2006$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2006]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2006])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2007]),(all_patients_2007$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2007]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2007])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2008]),(all_patients_2008$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2008]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2008])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2009]),(all_patients_2009$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2009]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2009])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2010]),(all_patients_2010$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2010]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2010])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2011]),(all_patients_2011$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2011]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2011])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2012]),(all_patients_2012$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2012]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2012])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2013]),(all_patients_2013$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2013]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2013])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2014]),(all_patients_2014$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2014]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2014])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2015]),(all_patients_2015$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2015]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2015])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2016]),(all_patients_2016$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2016]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2016])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2017]),(all_patients_2017$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2017]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2017])))
prop.test(c((all_per_year_category_ons_2004_2018_nmigrants$n_migrants[all_per_year_category_ons_2004_2018_nmigrants$Year == 2018]),(all_patients_2018$n)), c((ons_counts_per_year$n_migrants[ons_counts_per_year$Year == 2018]),(ons_counts_per_year$n_population[ons_counts_per_year$Year == 2018])))

## Plot ratio of percentages 

n_migrants_cprd_ons_ratio <- n_migrants_cprd_ons %>% mutate(ratio = percentage_migrants_in_cprd/percent) %>% 
  filter(Year >= 2004 ) %>% select(c(Year,ratio))
ratio_plot_2004_2018 <-  ggplot(n_migrants_cprd_ons_ratio) + geom_line(aes(x= Year, y = ratio)) +
  geom_point(aes(x = Year, y = ratio), colour = "navyblue") +
  ggtitle("Ratio of total percentages of migrants in CPRD and ONS from 2004 to 2018") +
  labs(y="Ratio of percentages (CPRD:ONS)", x = "Year") 
print(ratio_plot_2004_2018)
ggsave(width = 8, height = 4, dpi = 450, "filepath/ratio_plot_2004_2018.jpg")



## Representativeness (sex)  --------------------------------------------------------------------------------------

## count all migrants
migrants_census_count

## sex breakdown of migrants active on census date in England and Wales
migrants_census_sex <- count(group_by(migrants_census, gender)) %>% 
  mutate( percentage = (n/(nrow(migrants_census)))*100) %>%
  mutate(data = "CPRD GOLD" )
migrants_census_sex$gender <- factor(migrants_census_sex$gender, 
                           levels = c("Male", "Female"))
migrants_census_sex$data <- factor(migrants_census_sex$data, levels = c("CPRD GOLD", "2011 Census"))
migrants_census_sex

## import census sex breakdown
census_sex <- read_csv("filepath/census_sex.csv") # available in repository
census_sex$gender <-factor(census_sex$gender, 
                              levels = c("Male", "Female"))
census_sex$n <- as.integer(census_sex$n)
census_sex$data <- factor(census_sex$data, levels = c("CPRD GOLD", "2011 Census"))
census_sex

## link breakdown migrants cprd and census
sex_linked_cprd_census <- full_join(migrants_census_sex, census_sex, 
                                    by = c("gender" = "gender", "n" = "n", "percentage" = "percentage", "data" = "data"))
sex_linked_cprd_census <- rename(sex_linked_cprd_census, c("Gender" = "gender", "Data" = "data"))
sex_linked_cprd_census

## plot sex breakdown in cprd versus census 

sex_plot <-  ggplot(sex_linked_cprd_census) + 
  geom_col(aes(x= Data, y = percentage, group = Gender, fill = Gender), position = "dodge",  width = 0.3) +
  ggtitle("Percentage Migrants by sex in CPRD versus English Census 2011") +
  labs(y="Percentage of Migrants", x = "Dataset", color = "Key")
print(sex_plot)
ggsave("filepath/sex_plot_2011.jpg")

## plot sex breakdown in males and females with cprd vs census

sex_plot_b <-  ggplot(sex_linked_cprd_census) + 
  geom_col(aes(x= Gender, y = percentage, group = Data, fill = Data), position = "dodge",  width = 0.3) +
  ggtitle("Percentage Migrants in CPRD versus English Census 2011 by sex") +
  labs(y="Percentage of Migrants", x = "Sex", color = "Key")
print(sex_plot_b)
ggsave("filepath/sex_plot_2011_b.jpg")

## Representativeness (age) --------------------------------------------------------------------------------------

## create function to calculate age on a specific date
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

## calc age of cprd individuals on census date
migrants_census <- migrants_census %>%
  mutate(age = calc_age(dob, "2011-03-27"))
cprd_census <- cprd_census %>% 
  mutate(age = calc_age(dob, "2011-03-27"))

migrants_0_15_census <- migrants_census %>%  filter(age >=0 & age <= 15 ) %>% mutate(age_band = "0 to 15" )
migrants_16_24_census <- migrants_census %>%  filter(age >=16 & age <= 24 ) %>% mutate(age_band = "16 to 24" )
migrants_25_34_census <- migrants_census %>%  filter(age >=25 & age <= 34 ) %>% mutate(age_band = "25 to 34" )
migrants_35_49_census <- migrants_census %>%  filter(age >=35 & age <= 49 ) %>% mutate(age_band = "35 to 49" )
migrants_50_64_census <- migrants_census %>%  filter(age >=50 & age <= 64 ) %>% mutate(age_band = "50 to 64" )
migrants_65_74_census <- migrants_census %>%  filter(age >=65 & age <= 74 ) %>% mutate(age_band = "65 to 74" )
migrants_75_84_census <- migrants_census %>%  filter(age >=75 & age <= 84 ) %>% mutate(age_band = "75 to 84" )
migrants_85_plus_census <- migrants_census %>%  filter(age >=85) %>% mutate(age_band = "85 plus" )

migrants_census_age_bands <- full_join(migrants_0_15_census, migrants_16_24_census,
                                  by = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                          "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                          "prac_region" = "prac_region", "dob" = "dob", 
                                          "data_start" = "data_start", "data_end" = "data_end", 
                                          "eligible" = "eligible", "age" = "age", "age_band" = "age_band")) %>%
  full_join(migrants_25_34_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(migrants_35_49_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(migrants_50_64_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(migrants_65_74_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(migrants_75_84_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(migrants_85_plus_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") ) 

## cprd into age band 
cprd_0_15_census <- cprd_census %>%  filter(age >=0 & age <= 15 ) %>% mutate(age_band = "0 to 15" )
cprd_16_24_census <- cprd_census %>%  filter(age >=16 & age <= 24 ) %>% mutate(age_band = "16 to 24" )
cprd_25_34_census <- cprd_census %>%  filter(age >=25 & age <= 34 ) %>% mutate(age_band = "25 to 34" )
cprd_35_49_census <- cprd_census %>%  filter(age >=35 & age <= 49 ) %>% mutate(age_band = "35 to 49" )
cprd_50_64_census <- cprd_census %>%  filter(age >=50 & age <= 64 ) %>% mutate(age_band = "50 to 64" )
cprd_65_74_census <- cprd_census %>%  filter(age >=65 & age <= 74 ) %>% mutate(age_band = "65 to 74" )
cprd_75_84_census <- cprd_census %>%  filter(age >=75 & age <= 84 ) %>% mutate(age_band = "75 to 84" )
cprd_85_plus_census <- cprd_census %>%  filter(age >=85) %>% mutate(age_band = "85 plus" )

cprd_census_age_bands <- full_join(cprd_0_15_census, cprd_16_24_census,
                                       by = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                               "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                               "prac_region" = "prac_region", "dob" = "dob", 
                                               "data_start" = "data_start", "data_end" = "data_end", 
                                               "eligible" = "eligible", "age" = "age", "age_band" = "age_band")) %>%
  full_join(cprd_25_34_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(cprd_35_49_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(cprd_50_64_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(cprd_65_74_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(cprd_75_84_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                     "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                     "prac_region" = "prac_region", "dob" = "dob", 
                                     "data_start" = "data_start", "data_end" = "data_end", 
                                     "eligible" = "eligible", "age" = "age", "age_band" = "age_band") )  %>%
  full_join(cprd_85_plus_census, by  = c( "patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                       "frd" = "frd",  "deathdate" = "deathdate", "yob" = "yob", 
                                       "prac_region" = "prac_region", "dob" = "dob", 
                                       "data_start" = "data_start", "data_end" = "data_end", 
                                       "eligible" = "eligible", "age" = "age", "age_band" = "age_band") ) 

## count and percentage migrants in age band in cprd

migrants_census_age_bands_count <- count(group_by(migrants_census_age_bands, age_band)) %>% 
  rename ("n_migrants" = "n")
cprd_census_age_bands_count <- count(group_by(cprd_census_age_bands, age_band)) %>% 
  rename ("n_cprd" = "n")
age_bands_count_percent <- full_join(migrants_census_age_bands_count, cprd_census_age_bands_count,
                             by = c("age_band" = "age_band")) %>% 
  mutate(percent = (n_migrants/n_cprd)*100) %>%
  mutate(data = "CPRD GOLD" )
age_bands_count_percent$age_band <- factor(age_bands_count$age_band, 
                                    levels = c("0 to 15", "16 to 24", "25 to 34", "35 to 49", 
                                               "50 to 64", "65 to 74", "75 to 84", "85 plus"))
age_bands_count_percent$data <- factor(age_bands_count$data, levels = c("CPRD GOLD", "2011 Census"))
age_bands_count_percent

## Import census data
census_age_bands <- read_csv("filepath/census_age_bands.csv") # available in repository
census_age_bands$age_band <- factor(census_age_bands$age_band, 
                                     levels = c("0 to 15", "16 to 24", "25 to 34", "35 to 49", 
                                                "50 to 64", "65 to 74", "75 to 84", "85 plus"))
census_age_bands$n_migrants <- as.integer(census_age_bands$n_migrants)
census_age_bands$n_census <- as.integer(census_age_bands$n_census)
census_age_bands$data <- factor(census_age_bands$data, levels = c("CPRD GOLD", "2011 Census"))


## Join census and cprd data

## link breakdown migrants cprd and census
age_bands_linked_cprd_census <- full_join(age_bands_count_percent, census_age_bands, 
                                    by = c("age_band" = "age_band", "percent" = "percent", "data" = "data"))
age_bands_linked_cprd_census <- rename(age_bands_linked_cprd_census, c("Data" = "data"))
age_bands_linked_cprd_census

## plot age bands in cprd versus census 

age_bands_linked_cprd_census$Data <- factor(age_bands_linked_cprd_census$Data, levels = c("CPRD GOLD", "2011 Census"))
age_band_plot <-  ggplot(age_bands_linked_cprd_census) + 
  geom_col(aes(x= age_band, y = percent, group = Data, fill = Data), position = "dodge",  width = 0.7) +
  ggtitle("Percentage Migrants by age band in CPRD versus English Census 2011") +
  labs(y="Percentage of migrants in the population", x = "Age band", color = "Key")
print(age_band_plot)
ggsave(width = 8, height = 4, dpi = 450, "filepath/age_band_plot_2011.jpg")

## make table of comparison of percentages in CPRD versus ONS
age_bands_linked_cprd_census_reduced <- age_bands_linked_cprd_census %>% 
  select(age_band,Data,percent)
age_bands_linked_cprd_census_reduced
age_bands_linked_cprd_census_wide <- age_bands_linked_cprd_census_reduced %>% spread(Data,percent, sep = "_")
age_bands_linked_cprd_census_wide

age_bands_linked_cprd_census_ratios <- age_bands_linked_cprd_census_wide %>% mutate (ratio = `Data_CPRD GOLD`/`Data_2011 Census` )
age_bands_linked_cprd_census_ratios

rename(age_bands_linked_cprd_census_ratios, c("CPRD GOLD" = "Data_CPRD GOLD", "2011 Census" = "Data_2011 Census"))

# Plot ratio of percentages 


age_bands_linked_cprd_census_ratios$age_band <- factor(age_bands_linked_cprd_census_ratios$age_band)
ratio_plot_age_band <-  ggplot(age_bands_linked_cprd_census_ratios) + 
  geom_line(aes(x= age_band, y = ratio, group = 1)) +
  geom_point(aes(x= age_band, y = ratio, group = 1), colour = "navyblue") +
  ggtitle("Ratio of total percentages of migrants in CPRD and ONS by age band at the time of the 2011 English Census") +
  labs(y="Ratio of percentages (CPRD:ONS)", x = "Age band") 
print(ratio_plot_age_band)
ggsave(width = 8, height = 4, dpi = 450, "filepath/ratio_plot_age_band.jpg")



## Make stacked age chart to show age breakdown in CPRD and ONS for all migrants, males and females
total_migrants_cprd_census <- sum(age_bands_linked_cprd_census$n_migrants.x, na.rm = TRUE)
total_migrants_ons_census <- sum(age_bands_linked_cprd_census$n_migrants.y, na.rm = TRUE)

age_bands_count_percent_breakdown <- age_bands_count_percent %>%
  mutate(breakdown = ((n_migrants/total_migrants_cprd_census)*100))
census_age_bands_breakdown <- census_age_bands %>%
  mutate(breakdown = ((n_migrants/total_migrants_ons_census)*100))
age_bands_linked_cprd_census_breakdown <- full_join(age_bands_count_percent_breakdown, census_age_bands_breakdown, 
                                                    by = c("age_band" = "age_band", "percent" = "percent", 
                                                           "data" = "data", "breakdown" = "breakdown"))
age_bands_linked_cprd_census_breakdown
age_bands_linked_cprd_census_breakdown$data <- factor(age_bands_linked_cprd_census_breakdown$data, levels = c("CPRD GOLD", "2011 Census"))
age_breakdown_plot_2011 <- ggplot(age_bands_linked_cprd_census_breakdown) + 
  geom_col(aes(x = data, y = breakdown, fill = age_band), width = 0.3) +
  theme(aspect.ratio = 2/1) +
  ggtitle("All migrants") +
  labs(y="Percentage of Migrants", x = "Data source", fill = "Age bands")
print(age_breakdown_plot_2011)
ggsave("filepath/age_breakdown_plot_2011.jpg")
write_csv(age_bands_linked_cprd_census_breakdown, "filepath/age_bands_breakdown_2011.csv")

## Make unstacked age chart to show age breakdown in CPRD and ONS for all migrants
age_breakdown_unstacked_plot_2011 <- ggplot(age_bands_linked_cprd_census_breakdown) + 
  geom_col(aes(x = age_band, y = breakdown, fill = data), position = "dodge", width = 0.7) +
  ggtitle("All migrants") +
  labs(y="Percentage of the migrant population", x = "Age bands", fill = "Data")
print(age_breakdown_unstacked_plot_2011)
ggsave(width = 8, height = 4, dpi = 450, "filepath/age_breakdown_unstacked_plot_2011.jpg")

## Representativeness (country of birth) --------------------------------------------------------------------------------------

## country of birth who region breakdown of migrants active on census date in England and Wales

count(migrants_who_region_no_conflicts_census)
migrants_who_region_no_conflicts_census_dropped_na <- migrants_who_region_no_conflicts_census %>% drop_na(who_region)
regions_in_census <- c("afro", "euro", "paho")
migrants_who_regions_census_reduced <- migrants_who_region_no_conflicts_census_dropped_na %>% 
  filter(who_region %in% regions_in_census )
count(migrants_who_regions_census_reduced)
migrants_who_regions_census_breakdown <- count(group_by(migrants_who_regions_census_reduced,who_region)) %>%
  mutate(who_percentage = (n/(nrow(migrants_who_regions_census_reduced)))*100) %>% 
  mutate (data = "CPRD GOLD")
migrants_who_regions_census_breakdown$who_percentage <- as.numeric(migrants_who_regions_census_breakdown$who_percentage)
migrants_who_regions_census_breakdown$data <- factor(migrants_who_regions_census_breakdown$data, 
                                                                 levels = c("CPRD GOLD", " 2011 Census"))
migrants_who_regions_census_breakdown

## country of birth on nomis region breakdown of migrants active on census date in England and Wales

count(migrants_ons_nomis_region_no_conflicts_census)
migrants_ons_nomis_region_no_conflicts_census_dropped_na <- migrants_ons_nomis_region_no_conflicts_census %>% drop_na(ons_nomis_region)
count(migrants_ons_nomis_region_no_conflicts_census_dropped_na)
migrants_ons_nomis_region_no_conflicts_census_breakdown <- count(group_by(migrants_ons_nomis_region_no_conflicts_census_dropped_na, ons_nomis_region)) %>%
  mutate(ons_percentage = (n/(nrow(migrants_ons_nomis_region_no_conflicts_census_dropped_na)))*100) %>% 
  mutate (data = "CPRD GOLD")
migrants_ons_nomis_region_no_conflicts_census_breakdown$percentage <- as.numeric(migrants_ons_nomis_region_no_conflicts_census_breakdown$percentage)
migrants_ons_nomis_region_no_conflicts_census_breakdown$data <- factor(migrants_ons_nomis_region_no_conflicts_census_breakdown$data, 
                                                                 levels = c("CPRD GOLD", " 2011 Census"))
migrants_ons_nomis_region_no_conflicts_census_breakdown

## Import ONS data on WHO region + ONS region
ons_who_region <- read_csv("filepath/ons_who_and_ons_region_counts.csv")
ons_who_region$data <- factor(ons_who_region$data, levels = c("CPRD GOLD", "2011 Census"))
ons_who_region$who_region <- factor(ons_who_region$who_region, levels = c("afro","emro", "euro" ,"paho", "searo", "wpro"))
ons_who_region$n <- as.integer(ons_who_region$n)
ons_who_region$N <- as.integer(ons_who_region$N)
ons_who_region$who_percentage <- as.numeric(ons_who_region$who_percentage)
ons_who_region$ons_region <- factor(ons_who_region$ons_region, 
                                    levels = c("Africa", "Europe",  "Middle East & Asia", 
                                               "Antarctica & Oceania", "The Americas & Caribbean"))
ons_who_region$n_ons <- as.integer(ons_who_region$n_ons)
ons_who_region$N_ons <- as.integer(ons_who_region$N_ons)
ons_who_region$ons_percentage <- as.numeric(ons_who_region$ons_percentage)
ons_who_region

## Link data on who region  
linked_who_regions_census <- full_join(migrants_who_regions_census_breakdown, ons_who_region, 
                                        by = c( "data" = "data" , "who_region" = "who_region", 
                                                "n" = "n", "who_percentage" = "who_percentage" )) %>%
  select(who_region, data, who_percentage) %>%
  drop_na(who_region)
linked_who_regions_census

## produce stacked bar chart of who region breakdown in cprd migrant data and ons migrant data 

who_region_breakdown_plot_2011 <- ggplot(linked_who_regions_census) + 
  geom_col(aes(x = data, y = who_percentage, fill = who_region), width = 0.3) +
  theme(aspect.ratio = 1/1) +
  ggtitle("Breakdown by WHO Region of Birth") +
  labs(y="Percentage of Migrants", x = "Data source", fill = "WHO Region")
print(who_region_breakdown_plot_2011)
ggsave("filepath/who_region_breakdown_plot_2011.jpg")

## Make unstacked bar chart to show who_region breakdown in CPRD and ONS for all migrantss


linked_who_regions_census$who_region <- recode_factor(linked_who_regions_census$who_region, afro = "AFRO", emro = "EMRO", euro = "EURO" , paho = "PAHO", searo = "SEARO", wpro = "WPRO")
who_region_breakdown_unstacked_plot_2011 <- ggplot(linked_who_regions_census) + 
  geom_col(aes(x = who_region, y = who_percentage, fill = data), position = "dodge", width = 0.7) +
  ggtitle("All migrants - WHO breakdown") +
  labs(y="Percentage of the migrant population", x = "WHO Region", fill = "Data")
print(who_region_breakdown_unstacked_plot_2011)
ggsave(width = 8, height = 4, dpi = 450, "filepath/who_region_breakdown_unstacked_plot_2011.jpg")


## Link data on ons nomis region  
linked_ons_nomis_regions_census <- full_join(migrants_ons_nomis_region_no_conflicts_census_breakdown, ons_who_region, 
                                       by = c( "ons_nomis_region" = "ons_region", 
                                               "n" = "n_ons", "ons_percentage" = "ons_percentage", "data" = "data" )) %>%
  select(ons_nomis_region, data, ons_percentage)
linked_ons_nomis_regions_census

## produce stacked bar chart of on nomis region breakdown in cprd migrant data and ons migrant data 

ons_nomis_region_breakdown_plot_2011 <- ggplot(linked_ons_nomis_regions_census) + 
  geom_col(aes(x = data, y = ons_percentage, fill = ons_nomis_region), width = 0.3) +
  theme(aspect.ratio = 1.5/1) +
  ggtitle("Breakdown by Continent of Birth") +
  labs(y="Percentage of Migrants", x = "Data source", fill = "Continent")
print(ons_nomis_region_breakdown_plot_2011)
ggsave(width = 5, "filepath/ons_nomis_region_breakdown_plot_2011.jpg")


## Make unstacked bar chart to show ons nomis breakdown in CPRD and ONS for all migrants
ons_nomis_breakdown_unstacked_plot_2011 <- ggplot(linked_ons_nomis_regions_census) + 
  geom_col(aes(x = ons_nomis_region, y = ons_percentage, fill = data), position = "dodge", width = 0.7) +
  ggtitle("All migrants - ONS Nomis breakdown") +
  labs(y="Percentage of the migrant population", x = "Continent", fill = "Data") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
print(ons_nomis_breakdown_unstacked_plot_2011)
ggsave(width = 8, height = 4, dpi = 450, "filepath/ons_nomis_breakdown_unstacked_plot_2011.jpg")


## Representativeness (ethnicity)  ----- 

## Breakdown ethnicity of migrants in CPRD at time of census 
count_percent_migrants_ethnicity_census
count_percent_migrants_ethnicity_census_nona <- count_percent_migrants_ethnicity_census %>% top_n(6, ethnicat6)
count_percent_migrants_ethnicity_census_nona$ethnicat6 <- recode(count_percent_migrants_ethnicity_census_nona$ethnicat6, 
                                 "White British" = "White British", "White non-British" = "White Non-British", 
                                 "Mixed" = "Mixed", "Asian/Asian British" = "Asian/Asian British", 
                                 "Black/Black British" = "Black/Black British", "Other ethnic group" = "Other")
count_percent_migrants_ethnicity_census_nona$ethnicat6 <- ordered(count_percent_migrants_ethnicity_census_nona$ethnicat6,
                                                                  c("White British", "White Non-British", "Mixed", "Asian/Asian British", "Black/Black British", "Other"))  
count_percent_migrants_ethnicity_census_nona <- arrange(count_percent_migrants_ethnicity_census_nona, ethnicat6)
count_percent_migrants_ethnicity_census_nona
sum(count_percent_migrants_ethnicity_census_nona$n)
count_percent_migrants_ethnicity_census_nona_breakdown <- count_percent_migrants_ethnicity_census_nona %>%
  mutate(breakdown = ((n/(sum(count_percent_migrants_ethnicity_census_nona$n)))*100)) %>%
  mutate (data = "CPRD GOLD")
count_percent_migrants_ethnicity_census_nona_breakdown$data <- factor(count_percent_migrants_ethnicity_census_nona_breakdown$data, 
                                                                                              levels = c("CPRD GOLD", "2011 Census"))
count_percent_migrants_ethnicity_census_nona_breakdown

## Breakdown ethnicity of migrants in 2011 census 

ons_ethnicity <- read_csv("filepath/ons_ethnicity.csv")
ons_ethnicity$data <- factor(ons_ethnicity$data, levels = c("CPRD GOLD", "2011 Census"))
ons_ethnicity$category <- ordered(ons_ethnicity$category, 
                                  levels = c("White British", "White Non-British", "Mixed", "Asian/Asian British", "Black/Black British", "Other"))
ons_ethnicity$n <- as.integer(ons_ethnicity$n)
ons_ethnicity$breakdown <- as.numeric(ons_ethnicity$breakdown)
ons_ethnicity

## Link ONS and CPRD ethnicity 
linked_cprd_ons_ethnicity <- full_join(count_percent_migrants_ethnicity_census_nona_breakdown, ons_ethnicity, 
                                       by = c("data" = "data", "ethnicat6" = "category", "n" = "n", "breakdown" = "breakdown"))

## Make a plot of breakdown of ethnicity in cprd and ons at time of census 2011
linked_cprd_ons_ethnicity_plot_2011 <- ggplot(linked_cprd_ons_ethnicity) + 
  geom_col(aes(x = data, y = breakdown, fill = ethnicat6), width = 0.3) +
  theme(aspect.ratio = 1.5/1) +
  ggtitle("Breakdown by Ethnicity") +
  labs(y="Percentage of Migrants", x = "Data source", fill = "Ethnic group")
print(linked_cprd_ons_ethnicity_plot_2011)
ggsave(width = 5, "filepath/linked_cprd_ons_ethnicity_plot_2011.jpg")

## Make unstacked bar chart to show ethnicity breakdown in CPRD and ONS for all migrants
ethnicity_breakdown_unstacked_plot_2011 <- ggplot(linked_cprd_ons_ethnicity) + 
  geom_col(aes(x = ethnicat6, y = breakdown, fill = data), position = "dodge", width = 0.7) +
  ggtitle("All migrants - Ethnicity breakdown") +
  labs(y="Percentage of the migrant population", x = "Ethnic group", fill = "Data") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
print(ethnicity_breakdown_unstacked_plot_2011)
ggsave(width = 8, height = 4, dpi = 450, "filepath/ethnicity_breakdown_unstacked_plot_2011.jpg")


