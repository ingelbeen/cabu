###################################
# CABU-DRC                        #
# PATIENT EXIT INTERVIEWS         #
###################################

#### 0. CLEAN & LOAD DATA & PACKAGES - run this before analyses ####
#### install and load packages ####
# install.packages("pacman")
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr, knitr, epitools, naniar)

#### import & clean patient databases ####
# 0.1 DRC Kisantu patientdb
patientkis <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/CABU data analysis/db/Questionnaire patient CABU_RDC_Kisantu.csv",
                       stringsAsFactors = FALSE)
patientkis$SubmissionDate <- NULL
patientkis$site <- "DRC - Kisantu"

# replace "" with NA
patientkis %>% replace_with_na_all(condition = ~.x == "")

# patientnr
patientkis <- subset(patientkis,patientkis$patientnr!="Test") # drop test observation
patientkis$patientnr[patientkis$patientnr=="0j03"] <- "1j03"
patientkis$patientnr[patientkis$patientnr=="1AO7"] <- "1A07"
patientkis$patientnr <- toupper(patientkis$patientnr)
patientkis$patientnr[patientkis$patientnr=="5G05"&patientkis$SubmissionDate=="okt 26, 2019 5:09:34 PM"] <- "5G03"
patientkis$patientnr[patientkis$patientnr=="5H05"] <- "6G?"
patientkis$patientnr[patientkis$patientnr=="1C01"& patientkis$interviewdate=="okt 28, 2019"] <- "XC01"
patientkis$patientnr[patientkis$patientnr=="1C02"& patientkis$interviewdate=="okt 28, 2019"] <- "XC02"
patientkis$patientnr[patientkis$patientnr=="1C03"& patientkis$interviewdate=="okt 28, 2019"] <- "XC03"
patientkis$patientnr[patientkis$patientnr=="1C04"& patientkis$interviewdate=="okt 28, 2019"] <- "XC04"
patientkis$patientnr[patientkis$patientnr=="1C05"& patientkis$interviewdate=="okt 28, 2019"] <- "XC05"
patientkis$patientnr[patientkis$patientnr=="1C06"& patientkis$interviewdate=="okt 28, 2019"] <- "XC06"
patientkis$patientnr[patientkis$patientnr=="1C07"& patientkis$interviewdate=="okt 28, 2019"] <- "XC07"
patientkis$patientnr[patientkis$patientnr=="1C08"& patientkis$interviewdate=="okt 28, 2019"] <- "XC08"
patientkis$patientnr[patientkis$patientnr=="1C09"& patientkis$interviewdate=="okt 28, 2019"] <- "XC09"
patientkis$patientnr[patientkis$patientnr=="1C10"& patientkis$interviewdate=="okt 28, 2019"] <- "XC10"
patientkis$patientnr[patientkis$patientnr=="1C11"& patientkis$interviewdate=="okt 28, 2019"] <- "XC11"
patientkis$patientnr[patientkis$patientnr=="1C012"& patientkis$interviewdate=="okt 28, 2019"] <- "XC12"
patientkis$patientnr[patientkis$patientnr=="1C13"& patientkis$interviewdate=="okt 28, 2019"] <- "XC13"
patientkis$patientnr[patientkis$patientnr=="1C14"& patientkis$interviewdate=="okt 28, 2019"] <- "XC14"
patientkis$patientnr[patientkis$patientnr=="1C15"& patientkis$interviewdate=="okt 28, 2019"] <- "XC15"
patientkis$patientnr[patientkis$patientnr=="1C16"& patientkis$interviewdate=="okt 28, 2019"] <- NA # recruté par équipe E (2E06)
patientkis$patientnr[patientkis$patientnr=="1C17"& patientkis$interviewdate=="okt 28, 2019"] <- NA # recruté par équipe E (3E06)
patientkis$patientnr[patientkis$patientnr=="1C18"& patientkis$interviewdate=="okt 28, 2019"] <- "4C10"
patientkis$patientnr[patientkis$patientnr=="1C19"& patientkis$interviewdate=="okt 28, 2019"] <- "4C11"
patientkis$patientnr[patientkis$patientnr=="1C20"& patientkis$interviewdate=="okt 28, 2019"] <- "5C03"
patientkis$patientnr[patientkis$patientnr=="1C21"& patientkis$interviewdate=="okt 28, 2019"] <- "5C04"
patientkis$patientnr[patientkis$patientnr=="1C22"& patientkis$interviewdate=="okt 28, 2019"] <- "XC16"
patientkis$patientnr[patientkis$patientnr=="1C23"& patientkis$interviewdate=="okt 28, 2019"] <- "XC17"
patientkis$patientnr[patientkis$patientnr=="1C24"& patientkis$interviewdate=="okt 28, 2019"] <- "XC18"
patientkis$patientnr[patientkis$patientnr=="1C25"& patientkis$interviewdate=="okt 28, 2019"] <- "XC19"
patientkis$patientnr[patientkis$patientnr=="1C26"& patientkis$interviewdate=="okt 28, 2019"] <- "7C01" # team C restarted numbering from day 1 during the second week of data collection
patientkis$patientnr[patientkis$patientnr=="1C27"& patientkis$interviewdate=="okt 28, 2019"] <- "7C02"
patientkis$patientnr[patientkis$patientnr=="1C28"& patientkis$interviewdate=="okt 28, 2019"] <- "7C03"
patientkis$patientnr[patientkis$patientnr=="1C29"& patientkis$interviewdate=="okt 28, 2019"] <- "7C04"
patientkis$patientnr[patientkis$patientnr=="1C30"& patientkis$interviewdate=="okt 28, 2019"] <- "7C05"
patientkis$patientnr[patientkis$patientnr=="2C01"& patientkis$interviewdate=="okt 29, 2019"] <- "8C01"
patientkis$patientnr[patientkis$patientnr=="2C02"& patientkis$interviewdate=="okt 29, 2019"] <- "8C02"
patientkis$patientnr[patientkis$patientnr=="2C03"& patientkis$interviewdate=="okt 29, 2019"] <- "8C03"
patientkis$patientnr[patientkis$patientnr=="2C04"& patientkis$interviewdate=="okt 29, 2019"] <- "8C04"
patientkis$patientnr[patientkis$patientnr=="2C05"& patientkis$interviewdate=="okt 29, 2019"] <- "8C05"
patientkis$patientnr[patientkis$patientnr=="2C06"& patientkis$interviewdate=="okt 29, 2019"] <- "8C06"
patientkis$patientnr[patientkis$patientnr=="2CO7"& patientkis$interviewdate=="okt 29, 2019"] <- "8C07"
patientkis$patientnr[patientkis$patientnr=="2C08"& patientkis$interviewdate=="okt 29, 2019"] <- "8C08"
patientkis$patientnr[patientkis$patientnr=="2C09"& patientkis$interviewdate=="okt 29, 2019"] <- "8C09"
patientkis$patientnr[patientkis$patientnr=="2C10"& patientkis$interviewdate=="okt 29, 2019"] <- "8C10"
patientkis$patientnr[patientkis$patientnr=="2C11"& patientkis$interviewdate=="okt 29, 2019"] <- "8C11"
patientkis$patientnr[patientkis$patientnr=="2C12"& patientkis$interviewdate=="okt 29, 2019"] <- "8C12"
patientkis$patientnr[patientkis$patientnr=="2C13"& patientkis$interviewdate=="okt 29, 2019"] <- "8C13"
patientkis$patientnr[patientkis$patientnr=="0j03"& patientkis$interviewdate=="okt 21, 2019"] <- "1J03" # retrouvé sur base du deviceID
patientkis$patientnr[patientkis$patientnr=="0B13"& patientkis$interviewdate=="okt 26, 2019"] <- "6I05"
patientkis$patientnr[patientkis$patientnr=="0B13"& patientkis$interviewdate=="okt 24, 2019" & patientkis$deviceid=="50:01:BB:4E:78:99"] <- "4J05"
# interviewdate
table(patientkis$interviewdate, useNA = "always")
patientkis$interviewday <- substr(patientkis$patientnr, 1, 1)
# insert date for the missing interviewdates, based on patient nrs
patientkis$interviewdate[patientkis$interviewdate==""&substr(patientkis$patientnr, 1, 1)=="1"] <- "okt 21, 2019" 
# set to date format
patientkis$interviewdate <- as.Date(patientkis$interviewdate, tryFormats = c("%B %d, %Y"))
# drop test or empty observations
table(patientkis$patiennr[patientkis$interviewday=="0"])
patientkis <- subset(patientkis,patientkis$interviewday!="0") 
# team 
patientkis$equipe <- substr(patientkis$patientnr, 2, 2)
patientkis$equipe2[grepl("A", patientkis$patientnr)==TRUE] <- "A"
patientkis$equipe2[grepl("B", patientkis$patientnr)==TRUE] <- "B"
patientkis$equipe2[grepl("C", patientkis$patientnr)==TRUE] <- "C"
patientkis$equipe2[grepl("D", patientkis$patientnr)==TRUE] <- "D"
patientkis$equipe2[grepl("E", patientkis$patientnr)==TRUE] <- "E"
patientkis$equipe2[grepl("F", patientkis$patientnr)==TRUE] <- "F"
patientkis$equipe2[grepl("G", patientkis$patientnr)==TRUE] <- "G"
patientkis$equipe2[grepl("H", patientkis$patientnr)==TRUE] <- "H"
patientkis$equipe2[grepl("I", patientkis$patientnr)==TRUE] <- "I"
patientkis$equipe2[grepl("J", patientkis$patientnr)==TRUE] <- "J"
# providertype
patientkis$providertype[patientkis$patientnr=="2H04"] <- "privateclinic"
patientkis$providertype[patientkis$patientnr=="2H05"] <- "privateclinic"
patientkis$providertype[patientkis$equipe=="H" & patientkis$providertype=="healthcentre"] <- "privateclinic" # Ndinga ma Mbote and Mbote Ma Nkangu
patientkis$providertype[patientkis$equipe=="A" & patientkis$providertype=="healthcentre"] <- "privateclinic" # PS kimpudi
patientkis$providertype[patientkis$equipe=="G" & patientkis$providertype=="healthpost"] <- "privateclinic" # PS croix rouge
patientkis$providertype[patientkis$equipe=="G" & patientkis$providertype=="healthpost"] <- "privateclinic" # PS croix rouge
# age 
patientkis$ageyears[patientkis$patientnr=="2E06"] <- 25
patientkis$ageyears[patientkis$patientnr=="3E06"] <- 3
# birthyear
patientkis$birthyear[patientkis$birthyear=="okt 1, 2019"] <- NA # drop birthyears which have been touched but not filled in
patientkis$birthyear <- as.Date(patientkis$birthyear, tryFormats = c("%B %d, %Y"))
# format dates
patientkis$date_onset <- as.Date(patientkis$date_onset, tryFormats = c("%B %d, %Y"))
# antibiotics takes
patientkis$antibiotic[patientkis$patientnr=="1D02"] <- "no" # was an antibiotic for external use
# drop cols which are not in Kimpese df
patientkis$equipe <- NULL
patientkis$equipe2 <- NULL
patientkis$interviewday <- NULL
patientkis$SET.OF.ab <- NULL

# 0.2 DRC Kimpese patientdb & reformat to match kisantu db
patientkim <- read_excel("db/Questionnaire patient CABU_RDC_Kimpese_20200219.xlsx", 
                         sheet = "BUILD_35580902_CORE", na = "NULL")
# replace "NA" with NA
patientkim$educationlevel[ patientkim$educationlevel == "NA" ] <- NA
# remove cols not used in kisantu db
patientkim$"_CREATOR_URI_USER" <- NULL
patientkim$"_CREATION_DATE" <- NULL
patientkim$"_LAST_UPDATE_URI_USER" <- NULL
patientkim$"_LAST_UPDATE_DATE" <- NULL
patientkim$"_IS_COMPLETE" <- NULL # 2 non complete but no duplicates
patientkim$"_SUBMISSION_DATE" <- NULL
patientkim$"_MARKED_AS_COMPLETE_DATE" <- NULL
patientkim$"...49" <- NULL
colnames(patientkim) <- tolower(colnames(patientkim))
patientkim$groupkiandu1ndu1_providerarea <- NULL
patientkim$group2_preciser <- NULL
colnames(patientkim)[1] <- "KEY"
colnames(patientkim)[26] <- "meta.instanceID"
colnames(patientkim)[18] <- "duration_without_AB"
colnames(patientkim)[35] <- "providerarea"
# add a var site
patientkim$site <- "DRC - Kimpese"
# set dates to Date format
patientkim$birthyear <- as.Date(patientkim$birthyear, tryFormats = c("%B %d, %Y"))
patientkim$date_onset <- as.Date(patientkim$date_onset, tryFormats = c("%B %d, %Y"))
patientkim$interviewdate <- as.Date(patientkim$interviewdate, tryFormats = c("%B %d, %Y"))
# add variables that were not in the Kimpese db (reason unclear)
patientkim$previoususe_origin <- NA
patientkim$diag_test <- NA
patientkim$symptoms <- NA
patientkim$hospitaldept <- NA

# 0.3 merge Kisantu and Kimpese
patient <- rbind(patientkis,patientkim)
# remove duplicate rows
patient <- patient %>% distinct()
# clean merged database
# agefromdateofbirth
patient$interviewdate_num <- as.numeric(patient$interviewdate)
patient$birthyear_num <- as.numeric(patient$birthyear)
patient$ageyears_dob <-  (patient$interviewdate_num-patient$birthyear_num)/365.25
patient$ageyears[is.na(patient$ageyears)] <- patient$ageyears_dob[is.na(patient$ageyears)]
# check mismatchign ages
tabconflictingages <- patient %>%
  filter(!is.na(birthyear) & !is.na(ageyears) & ((patient$ageyears_dob-patient$ageyears)>1|(patient$ageyears_dob-patient$ageyears)<(-1))) %>%
  select(patientnr, site, ageyears, ageyears_dob, birthyear, caretaker) 
# combined
patient$age <-patient$ageyears_dob
patient$age[is.na(patient$ageyears_dob)==TRUE] <- patient$ageyears[is.na(patient$ageyears_dob)==TRUE]
patient$age[patient$patientnr=="7H12"] <- patient$ageyears[patient$patientnr=="7H12"] # some discordant ages checked and corrected
patient$age[patient$patientnr=="6H02"] <- patient$ageyears[patient$patientnr=="6H02"]
patient$age[patient$patientnr=="11D15"] <- patient$ageyears[patient$patientnr=="11D15"]
patient$age[patient$patientnr=="03G09"] <- patient$ageyears[patient$patientnr=="03G09"]
patient$age[patient$patientnr=="03G10"] <- patient$ageyears[patient$patientnr=="03G10"]
patient$age[patient$birthyear_num==18262&patient$site=="DRC - Kimpese"] <- patient$ageyears[patient$birthyear_num==18262&patient$site=="DRC - Kimpese"] # often mistakenly dob "2020-01-01" entered in Kimpese
# age of 02C08 to be checked (2 patients, one of which unsure if child)
patient$providertype[patient$providertype=="MD_nurse"] <- "privateclinic" # at first, we thought to break up independent MDs nurses from more 'formal' private facilities, but they too frequently overlap
patient$providertype[patient$providertype=="healthpost"] <- "healthcentre" # only healthposts in Kimpese
patient$providertype[patient$providertype=="informalstore"] <- "privatepharmacy" # only labeled as such in Kimpese, but difficult to distinguish from pharmacies -> merged
patient$providertype[patient$providertype=="church"] <- "religiousleader" # to harmonise with HCUS db

# var agegroups
patient$agegroups[patient$age<5] <- "0-4 yr"
patient$agegroups[patient$age>4.999] <- "5-17 yr"
patient$agegroups[patient$age>17.999] <- "18-64 yr"
patient$agegroups[patient$age>64.999] <- "65+ yr"
# var agegroup children vs adults
patient$adoadult[patient$age>17.99] <- "adult"
patient$adoadult[patient$age<18] <- "child/adolescent"
# duration symptoms until consultation
patient$date_onset_num <- as.numeric(patient$date_onset)
patient$duration <- patient$interviewdate_num - patient$date_onset_num

# check missing ages & sex
missingages <- patient %>%
  filter(is.na(patient$age)) %>%
  select(patientnr, site, ageyears, ageyears_dob, birthyear, caretaker) 
missingsex <- patient %>%
  filter(is.na(patient$sex))

#### import & clean antibiotic databases ####
abkis <- read.csv("db/Questionnaire antibiotiques CABU_RDC_Kisantu.csv", 
                  stringsAsFactors = FALSE)
abkim <- read_excel("db/Questionnaire antibiotiques CABU_RDC_Kimpese_20200219.xlsx", na = "NULL")
# adapt the formats and col names so that they can be appended
colnames(abkis) <- tolower(colnames(abkis))
colnames(abkim) <- tolower(colnames(abkim))
abkim$`_creation_date` <- NULL
abkim$`_uri` <- NULL
abkim$`_ordinal_number`<- NULL
abkis$key <- NULL
abkis$set.of.ab <- NULL
colnames(abkim) <- c("parent_key","abgeneric","abname","abroute","abroute_other","abdose","abfreq","abunits","abduration","abproducer","abexpiry","abprix")
# create cols for those variables that were not collected in Kimpese
abkim$abphotoext <- NA
abkim$abphotoint <- NA
# format expiry dates to date format
abkim$abexpiry <- as.Date(abkim$abexpiry, tryFormats = c("%B %d, %Y"))
abkis$abexpiry <- as.Date(abkis$abexpiry, tryFormats = c("%B %d, %Y"))
# append both antibiotic datasets
ab <- rbind(abkis,abkim)

# remove duplicate rows
ab <- ab %>% distinct()

# replace "" or other missings with NA
na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available", "")
ab <- ab %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

# remove test observations
ab <- subset (ab, ab$parent_key !="uuid:d03a9bcb-111c-4acb-aa83-8af813ff7de5")
ab <- subset (ab, ab$parent_key !="uuid:f4b53765-6d47-4888-a5c0-1b8e66a5008d")

# abroute cleaning (twice gentamicin intramuscular use should be IV use)
ab$abroute[ab$abgeneric=="procaine_penicillin" & (ab$abroute=="po_flacon"|ab$abroute=="po_tab")] <- "iv_vial" # not possible
ab$abroute[ab$abroute_other=="Intra musculaire"|ab$abroute_other=="Intramusculaire"] <- "iv_vial"
ab$abroute[ab$abroute_other=="Vial"|ab$abroute_other=="Ampoule"] <- "iv_vial"
ab$abroute[ab$abroute=="intraveneux (flacon, bouteille)"] <- "iv_vial"
ab_su$abroute[ab_su$abroute==''&ab_su$abgeneric=="ceftriaxone"] <- 'iv_vial'#### more accurate to call it p
table(ab$abroute, useNA = "always")
table(ab$abroute_other[ab$abroute=="other"])
table(ab$abgeneric[ab$abroute_other=="Purge"]) # tetracycline -> probably some form of eye drops

# new var iv vs oral vs other
ab$ab_iv_po[ab$abroute=="intraveneux (flacon, bouteille)"|ab$abroute=="iv_bag"|ab$abroute=="iv_vial"] <- "parenteral"
ab$ab_iv_po[ab$abroute=="orale (comp./cap.)"|ab$abroute=="orale (sirop/suspension)"|ab$abroute=="po_flacon"|ab$abroute=="po_tab"] <- "oral"
table(ab$abroute, ab$ab_iv_po, useNA = "always")

# clean the names of antibiotics - for systemic use 
ab$abgeneric <- tolower(ab$abgeneric)
ab$abgeneric[ab$abgeneric=="0meprazole gellule"] <- NA # proton pump inhibitor
ab$abgeneric[ab$abgeneric=='Ã£â°rythromycine']='azithromycin'
ab$abgeneric[ab$abgeneric=="amoxicilline"] <- "amoxicillin"
ab$abgeneric[grepl("amox", ab$abgeneric)==TRUE] <- "amoxicillin"
ab$abgeneric[ab$abgeneric=="taxclav"] <- "cefixime"
ab$abgeneric[grepl("solbactam+ampicillin", ab$abgeneric)==TRUE] <- "ampicillin and beta-lactamase inhibitor" 
ab$abgeneric[grepl("penicillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="pã©nicillin"&ab$abname=="PÃ©nicillin vk"] <- "phenoxymethylpenicillin"
ab$abgeneric[grepl("penicilin", ab$abgeneric)==TRUE & ab$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
ab$abgeneric[grepl("penidur", ab$abgeneric)==TRUE & ab$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
ab$abgeneric[grepl("penecillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
ab$abgeneric[grepl("pencillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
ab$abgeneric[grepl("pinicillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
ab$abgeneric[grepl("penidur", ab$abgeneric)==TRUE & ab$ab_iv_po=="parenteral"] <- "benzylpenicillin"
ab$abgeneric[grepl("penecillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="parenteral"] <- "benzylpenicillin"
ab$abgeneric[grepl("pencillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="parenteral"] <- "benzylpenicillin"
ab$abgeneric[grepl("pinicillin", ab$abgeneric)==TRUE & ab$ab_iv_po=="parenteral"] <- "benzylpenicillin"
ab$abgeneric[grepl("piperacillin + taz", ab$abgeneric)==TRUE] <- "piperacillin and beta-lactamase inhibitor"
ab$abgeneric[grepl("piptaz", ab$abgeneric)==TRUE] <- "piperacillin and beta-lactamase inhibitor"
ab$abgeneric[grepl("tazobactam + piperacillin", ab$abgeneric)==TRUE] <- "piperacillin and beta-lactamase inhibitor"
ab$abgeneric[ab$abgeneric=="inj. tazobactem pipracillin"] <- "piperacillin and beta-lactamase inhibitor"
ab$abgeneric[grepl("sulfadiazine", ab$abgeneric)==TRUE] <- "sulfadiazine"
ab$abgeneric[grepl("metronidazol", ab$abgeneric)==TRUE] <- "metronidazole"
ab$abgeneric[ab$abname=="Metronidazole"] <- "metronidazole"
ab$abgeneric[grepl("mã©tronid", ab$abgeneric)==TRUE] <- "metronidazole" # when used orally or IV, can be either J01 (AB systemic use), but P01 ANTIPROTOZOALS
ab$abgeneric[ab$abgeneric=="inj. metron"] <- "metronidazole"
ab$abgeneric[ab$abgeneric=="meetronidazole"] <- "metronidazole"
ab$abgeneric[ab$abgeneric=="metraonidazole"] <- "metronidazole"
ab$abgeneric[ab$abgeneric=="metromidazole"] <- "metronidazole"
ab$abgeneric[ab$abgeneric=="metrondazole"] <- "metronidazole"
ab$abgeneric[ab$abgeneric=="metronedazole"] <- "metronidazole"
ab$abgeneric[ab$abgeneric=="nitronidazol"] <- "metronidazole"
ab$abgeneric[grepl("ampicill", ab$abgeneric)==TRUE] <- "ampicillin"
ab$abgeneric[ab$abgeneric=="actrapid injectable"]<- NA ### not an abx
ab$abgeneric[grepl("aldactone",ab$abgeneric)==T]<- NA #### not an abx
ab$abgeneric[ab$abgeneric=="aminoside"]<- "gentamycin"#### brand name indicated gentamycin
ab$abgeneric[ab$abgeneric=="amitriptyline sirop"]<- NA ##### not an abx
ab$abgeneric[grepl("amlox",ab$abgeneric)==T]<- NA ##### not an abx
ab$abgeneric[ab$abgeneric=="amocycilline"]<- "amoxicillin"
ab$abgeneric[ab$abgeneric=="ampiciline"]<- "ampicillin"
ab$abgeneric[ab$abgeneric=="amykacine"]<- "amikacin"
ab$abgeneric[ab$abgeneric=="analgÃ£Â©siques"]<- NA ##### not an abx
ab$abgeneric[ab$abgeneric=="anpicillin"]<-"ampicillin"
ab$abgeneric[ab$abname=="Ampicilline"]<-"ampicillin"
ab$abgeneric[ab$abgeneric=="anset injectable"]<- NA ##not an abx
ab$abgeneric[ab$abgeneric=="antacid"]<- NA ## not an abx
ab$abgeneric[ab$abgeneric=="araucip"]<-"ciprofloxacin"
ab$abgeneric[ab$abgeneric=="cirpofloxacine"]<-"ciprofloxacin"
ab$abgeneric[ab$abgeneric=="araumet"]<-"metronidazole"
ab$abgeneric[ab$abgeneric=="argyrol gouttes nasales"]<- NA ## not an abx
ab$abgeneric[ab$abgeneric=="artemether"]<- NA ### not anabx (antimarial)
ab$abgeneric[ab$abgeneric=="artesunate injectable"]<-NA ##not anabx (antimarial)
ab$abgeneric[ab$abgeneric=="aspirine junior comprimÃ£Â©"]<-NA ##not anabx (analgesic)
ab$abgeneric[grepl("atenolol", ab$abgeneric)==TRUE] <- NA ##not anabx (analgesic)
ab$abgeneric[ab$abgeneric=="avepen"]<-"phenoxymethylpenicillin"#### penicillin vk from brand name
ab$abgeneric[grepl("pã‰nicilline v", ab$abgeneric)==TRUE] <- "phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="bactox"]<-"amoxicillin"
ab$abgeneric[ab$abname=="Amoxycilline"]<-"amoxicillin"
ab$abgeneric[ab$abname=="Amoxyxilline"]<-"amoxicillin"
ab$abgeneric[grepl("bactri", ab$abgeneric)==TRUE]<-"sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("benzyl", ab$abgeneric)==TRUE]<-"benzylpenicillin"
ab$abgeneric[ab$abgeneric=="beta-lactam"]<-"betalactamine"
ab$abgeneric[ab$abgeneric=="betalactamine"&ab$abname=="Penicilline"]<-"benzathine penicillin"
ab$abgeneric[ab$abgeneric=="betalactamine"&ab$abname=="Moxiclav"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="iniclave"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="cefatax"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cã©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abname=="CÃ©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cefatax"]<-"cefixime"
ab$abgeneric[ab$abgeneric=="cefrriaxone"]<-"ceftriaxone"
ab$abgeneric[grepl("ceftriazone", ab$abgeneric)==TRUE]<-"ceftriaxone"
ab$abgeneric[grepl("cã©ftriaxone", ab$abgeneric)==TRUE]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="ceftrioxone vial"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cã©phalosporine"&ab$abname=="Ceftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cephalosporine"&ab$abname=="CÃ©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cephalosporine"&ab$abname=="Ceftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cephalosporine"&ab$abname=="CÃÂ©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cephalosporine g3"&ab$abname=="Cefixime"]<-"cefixime"
ab$abgeneric[ab$abgeneric=="cephalosporine g3"&ab$abname=="Ceftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cephalosporine g3"&ab$abname=="Cephixime"]<-"cefixime"
ab$abgeneric[ab$abgeneric=="cephalosporine g3"&ab$abname=="CÃÂ©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cephalosporineg3"&ab$abname=="Ceftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cÃ£Â©phalosporine"&ab$abname=="Ceftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cã©phalosporines"&grepl("CÃ©phalosporin Hcl 500mg", ab$abname)]<-"cefalexin"
ab$abgeneric[ab$abgeneric=="2 phÃ£Â©noxymethylpenicilline"]<-"phenoxymethylpenicillin"
ab$abgeneric[grepl("phenoxymethylpenicilli",ab$abgeneric)==T]<-"phenoxymethylpenicillin"
ab$abgeneric[grepl("phenoxymethy",ab$abgeneric)==T]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="penicilline v"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="pã©nicilline comprimã©"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="pã©nicilline"]<-"phenoxymethylpenicillin" 
ab$abgeneric[ab$abgeneric=="penicilin"&ab$abname=="Phenometylpenicilline de potassium"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="penicillin"&ab$abname=="Peni-V"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="penicilline"&ab$abname=="Penicillin-VK"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="penicilline"&ab$abname=="Peni V"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="penicilline"&ab$abname=="Avepen"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abname=="Peni Procaine"]<-"procaine_penicillin"
ab$abgeneric[ab$abname=="Peni procaine"]<-"procaine_penicillin"
ab$abgeneric[ab$abname=="Peniprocaine"]<-"procaine_penicillin"
ab$abgeneric[ab$abname=="Peni-procaine"]<-"procaine_penicillin"
ab$abgeneric[ab$abgeneric=="penicillin"&ab$abname=="Peniprocaine"]<-"procaine_penicillin"
ab$abgeneric[ab$abgeneric=="penicillin"&ab$abname=="Peni-procaine"]<-"procaine_penicillin"
ab$abgeneric[ab$abgeneric=="penicillin"&grepl("proca", ab$abname)==T]<-"procaine_penicillin"
ab$abgeneric[ab$abgeneric=="penicillin"&grepl("Proca", ab$abname)==T]<-"procaine_penicillin"
ab$abgeneric[grepl("procainebenzylpenicillin",ab$abgeneric)==T]<-"procaine_penicillin"
ab$abgeneric[grepl("procaine",ab$abgeneric)==T]<-"procaine_penicillin"
ab$abgeneric[grepl("pã©ni v",ab$abgeneric)==T]<-"phenoxymethylpenicillin"
ab$abgeneric[c(882,1142)]<-"phenoxymethylpenicillin"#####penicillin and PÃÂ©nicillin  vk(brand name indicating pencillin vk)
ab$abgeneric[c(151,320,1182,764,889)]<-"phenoxymethylpenicillin"######brand name indicating pencillin vk
ab$abgeneric[c(952,421)]<-"benzylpenicillin"
ab$abgeneric[ab$abgeneric=="chllrpheniramine"]<-"chloramphenicol"
ab$abgeneric[grepl("chlo", ab$abgeneric)==TRUE]<-"chloramphenicol"
ab$abgeneric[grepl("phenicolã©", ab$abgeneric)==TRUE]<-"chloramphenicol"
ab$abgeneric[grepl("cloraphenicolÃ£Â©s", ab$abgeneric)==TRUE]<-"chloramphenicol"
ab$abgeneric[grepl("cifin", ab$abgeneric)==TRUE]<-"ciprofloxacin"
ab$abgeneric[198]="ciprofloxacin"### under brand name of cifin
ab$abgeneric[ab$abgeneric=="ciorofloxacine"]<-"ciprofloxacin"
ab$abgeneric[ab$abgeneric=="cipro"]<-"ciprofloxacin"
ab$abgeneric[ab$abgeneric=="ciproflaxacine infusion"]<-"ciprofloxacin"
ab$abgeneric[ab$abgeneric=="citrofloxacine"]<-"ciprofloxacin"
ab$abgeneric[ab$abgeneric=="cyprofloxacine"]<-"ciprofloxacin"
ab$abgeneric[grepl("clinda", ab$abgeneric)==TRUE]<-"clindamycin"
ab$abgeneric[ab$abgeneric=="cotrumoxazole"]<-"sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("doxy", ab$abgeneric)==TRUE]<-"doxycycline"
ab$abgeneric[grepl("ery", ab$abgeneric)==TRUE]<-"erythromycin"
ab$abgeneric[ab$abgeneric=="furadentine"]<-"nitrofurantoin"
ab$abgeneric[grepl("nutrofirad", ab$abgeneric)==TRUE]<-"nitrofurantoin"
ab$abgeneric[grepl("nutrofurantoine", ab$abgeneric)==TRUE]<-"nitrofurantoin"
ab$abgeneric[ab$abgeneric=="bruffen comprimÃ£Â©"]<- NA
ab$abgeneric[grepl("buscopam", ab$abgeneric)==TRUE]<- NA ##### not anabx
ab$abgeneric[ab$abgeneric=="cÃ£Â©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="cÃ£Â©phalosporine"&ab$abname=="ceftriaxone"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="isoniade"]<-"isoniazid"
ab$abgeneric[grepl("linconsin", ab$abgeneric)==TRUE]<-"lincomycin"
ab$abgeneric[grepl("lyncomicine", ab$abgeneric)==TRUE]<-"lincomycin"
ab$abgeneric[grepl("mÃ£Â©tronida", ab$abgeneric)==TRUE]<-"metronidazole"
ab$abgeneric[grepl("mã‰tronidazole", ab$abgeneric)==TRUE]<-"metronidazole"
ab$abgeneric[ab$abname=="Metazol"]<-"metronidazole"
ab$abgeneric[grepl("macrolide", ab$abgeneric)==TRUE]<-"erythromycin"
ab$abgeneric[grepl("meya", ab$abgeneric)==TRUE]<-"meyamycin"
ab$abgeneric[grepl("meben", ab$abgeneric)==TRUE]<-"mebendazole"
ab$abgeneric[ab$abgeneric=="mendazole"]<- NA
ab$abgeneric[grepl("mã©tron", ab$abgeneric)==TRUE]<-"metronidazole"
ab$abgeneric[grepl("metron", ab$abgeneric)==TRUE]<-"metronidazole"
ab$abgeneric[ab$abgeneric=="nÃ£Â©omycine"]<-"neomycin"
ab$abgeneric[ab$abgeneric=="neomycine"]<-"neomycin"
ab$abgeneric[ab$abgeneric=="nidazole"]<-"metronidazole"
ab$abgeneric[ab$abgeneric=="nitrofuradentine"]<-"nitrofurantoin"
ab$abgeneric[grepl("nitofurantoine", ab$abgeneric)==TRUE]<-"nitrofurantoin"
ab$abgeneric[ab$abgeneric=="norfloxacine"]<-"norfloxacin"
ab$abgeneric[grepl("normegyl", ab$abgeneric)==TRUE]<-"norfloxacin"
ab$abgeneric[ab$abgeneric=="normegyl comprimÃ£Â©"]<-"metronidazole and norfloxacin"
ab$abgeneric[grepl("menorcin", ab$abgeneric)==TRUE]<-"metronidazole and norfloxacin"
ab$abgeneric[grepl("pÃ£Â©nicillin", ab$abgeneric)==TRUE]<-"penicillin"
ab$abgeneric[grepl("peni v", ab$abgeneric)==TRUE]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="penicellin"&ab$abname=="Peniceline"]<-"penicillin"
ab$abgeneric[ab$abgeneric=="penicelin"&ab$abname=="Peniceline"]<-"penicillin"
ab$abgeneric[ab$abgeneric=="penicellin"&ab$abname=="Peni-V"]<-"phenoxymethylpenicillin"
ab$abgeneric[grepl("penicellin", ab$abgeneric)==TRUE]<-"penicillin"
ab$abgeneric[ab$abgeneric=="pencillinvk"]<-"phenoxymethylpenicillin"
ab$abgeneric[ab$abgeneric=="phatrix"]<-"benzylpenicillin"
ab$abgeneric[ab$abgeneric=="phenicolÃ£Â©"]<-"chloramphenicol"

ab$abgeneric[ab$abgeneric=="rifampicine"]<-"rifampicin"
ab$abgeneric[ab$abgeneric=="sulfadoxine et pyrimethamine"]<-"sulfadoxine and pyrimethamine"
ab$abgeneric[ab$abgeneric=="sulfadoxine"]<-"sulfadoxine and pyrimethamine"
ab$abgeneric[grepl("sulfadox", ab$abgeneric)==TRUE]<-"sulfamethoxazole and trimethoprim"
ab$abgeneric[ab$abgeneric=="sulfatrim"]<-"sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("sulfatrim", ab$abgeneric)==TRUE]<-"sulfamethoxazole and trimethoprim"
ab$abgeneric[ab$abgeneric=="tÃ£Â©tracycline"]<-"tetracycline"
ab$abgeneric[ab$abgeneric=="tã©tracycline"]<-"tetracycline"
ab$abgeneric[ab$abgeneric=="amoxicillin"&ab$abname=="Augmentin"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="benzylpenicillin"&ab$abname=="Peni procaine"]<-"procainebenzylpenicillin"
ab$abgeneric[ab$abgeneric=="amoxicillin"&ab$abname=="AMOXYCILLINE ET CLAVULANATE"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="amoxicillin"&ab$abname=="Clamoxyl"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="amoxicillin"&ab$abname=="Amoxy clav"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="amoxicillin"&ab$abname=="Moxyclav"]<-"amoxicillin and clavulanic acid"
ab$abgeneric[ab$abgeneric=="cedocard comprimÃ£Â© de 5mg"&ab$abname=="Dinitrate d'isosorbide"]<-"isosorbide dinitrate"
ab$abgeneric[ab$abgeneric=="cÃ£Â©phalosporine"&ab$abname=="Prodoxil"]<-"cefadroxil"
ab$abgeneric[ab$abgeneric=="cã©phalosporine"&ab$abname=="Prodoxil"]<-"cefadroxil"
ab$abgeneric[ab$abgeneric=="cÃ£Â©ftriaxone"]<-"ceftriaxone"
ab$abgeneric[grepl("amikaci", ab$abgeneric)==TRUE] <- "amikacin"
ab$abgeneric[ab$abgeneric=="inj. amillacin"] <- "amikacin"
ab$abgeneric[ab$abgeneric=="inj. Amlkacin"] <- "amikacin"
ab$abgeneric[grepl("vancom", ab$abgeneric)==TRUE] <- "vancomycin"
ab$abgeneric[ab$abgeneric=="inj uancomycin"] <- "vancomycin"
ab$abgeneric[grepl("genta", ab$abgeneric)==TRUE] <- "gentamicin"
ab$abgeneric[ab$abgeneric=="gentimicin inj"] <- "gentamicin"
ab$abgeneric[ab$abgeneric=="gentmicine inj"] <- "gentamicin"
ab$abgeneric[ab$abgeneric=="inj. geftriamycin"] <- "gentamicin"
ab$abgeneric[grepl("kanamycin", ab$abgeneric)==TRUE] <- "kanamycin"
ab$abgeneric[grepl("cefixim", ab$abgeneric)==TRUE] <- "cefixime"
ab$abgeneric[grepl("cefex", ab$abgeneric)==TRUE] <- "cefixime"
ab$abgeneric[grepl("cephalexin", ab$abgeneric)==TRUE] <- "cefalexin"
ab$abgeneric[ab$abgeneric=="ceftxime"] <- "cefixime"
ab$abgeneric[ab$abgeneric=="cefazoline"] <- "cefazolin"
ab$abgeneric[grepl("cefurox", ab$abgeneric)==TRUE] <- "cefuroxime"
ab$abgeneric[grepl("zocef", ab$abgeneric)==TRUE] <- "cefuroxime"
ab$abgeneric[grepl("tricef", ab$abgeneric)==TRUE] <- "cefixime"
ab$abgeneric[grepl("cefxime", ab$abgeneric)==TRUE] <- "cefixime"
ab$abgeneric[grepl("cefepim", ab$abgeneric)==TRUE] <- "cefepime"
ab$abgeneric[grepl("cefipim", ab$abgeneric)==TRUE] <- "cefepime"
ab$abgeneric[grepl("cefoperazone", ab$abgeneric)==TRUE] <- "cefoperazone"
ab$abgeneric[grepl("ceftazidi", ab$abgeneric)==TRUE] <- "ceftazidime"
ab$abgeneric[grepl("ceftraixone", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("ceftraxone", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("ceftria", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("ceftrixone", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("cefiriaxone", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("cefitriaxone", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("monocef", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("monocet", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("monocex", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("oframax", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[ab$abgeneric=="inj. oframau"] <- "ceftriaxone"
ab$abgeneric[ab$abgeneric=="inj. cefyriaxone"] <- "ceftriaxone"
ab$abgeneric[ab$abgeneric=="inj. cephalosporin"] <- "ceftriaxone"
ab$abgeneric[ab$abgeneric=="inj.cefaxone"] <- "ceftriaxone"
ab$abgeneric[ab$abgeneric=="leftriaxone"] <- "ceftriaxone"
ab$abgeneric[grepl("ceftriaton", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("cetriax", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("ciftria", ab$abgeneric)==TRUE] <- "ceftriaxone"
ab$abgeneric[grepl("cefotaxim", ab$abgeneric)==TRUE] <- "cefotaxime"
ab$abgeneric[grepl("taxim", ab$abgeneric)==TRUE] <- "cefotaxime"
ab$abgeneric[ab$abgeneric=="inj cefoxaxime"] <- "cefotaxime"
ab$abgeneric[ab$abgeneric=="inj ceftrazidime"] <- "ceftazidime"
ab$abgeneric[ab$abgeneric=="inj. cettazidime"] <- "ceftazidime"
ab$abgeneric[grepl("cefodoxime", ab$abgeneric)==TRUE] <- "cefpodoxime"
ab$abgeneric[grepl("cepodoxime", ab$abgeneric)==TRUE] <- "cefpodoxime"
ab$abgeneric[grepl("cepodoxine", ab$abgeneric)==TRUE] <- "cefpodoxime"
ab$abgeneric[grepl("cloxac", ab$abgeneric)==TRUE] <- "cloxacillin"
ab$abgeneric[ab$abgeneric=="cloxcilin"] <- "cloxacillin"
ab$abgeneric[ab$abgeneric=="cloxiacillin"] <- "cloxacillin"
ab$abgeneric[grepl("trimet",ab$abgeneric)==TRUE] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("sulfamethoxa",ab$abgeneric)==TRUE] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("trimazole",ab$abgeneric)==TRUE] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("tri methoprim",ab$abgeneric)==TRUE] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("co-trimox",ab$abgeneric)==TRUE] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("cotrim",ab$abgeneric)==TRUE] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[ab$abgeneric=="cotrinoxale"] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[ab$abgeneric=="cotrmoxazole 960 mg"] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[ab$abgeneric=="cotrinoxazole"] <- "sulfamethoxazole and trimethoprim"
ab$abgeneric[grepl("sulfadiazin",ab$abgeneric)==TRUE] <- "sulfadiazine"
ab$abgeneric[grepl("doxyc", ab$abgeneric)==TRUE] <- "doxycycline"
ab$abgeneric[grepl("doxic", ab$abgeneric)==TRUE] <- "doxycycline"
ab$abgeneric[ab$abgeneric=="microdox"] <- "doxycycline"
ab$abgeneric[grepl("tetracy", ab$abgeneric)==TRUE] <- "tetracycline"
ab$abgeneric[grepl("chloramphenicol", ab$abgeneric)==TRUE] <- "chloramphenicol"
ab$abgeneric[grepl("erythro", ab$abgeneric)==TRUE] <- "erythromycin"
ab$abgeneric[ab$abgeneric=="eryethromycin"] <- "erythromycin"
ab$abgeneric[grepl("ã‰rythrom", ab$abgeneric)==TRUE] <- "erythromycin"
ab$abgeneric[grepl("azithro", ab$abgeneric)==TRUE] <- "azithromycin"
ab$abgeneric[grepl("azitro", ab$abgeneric)==TRUE] <- "azithromycin"
ab$abgeneric[grepl("azifine", ab$abgeneric)==TRUE] <- "azithromycin"
ab$abgeneric[ab$abgeneric=="roxithromycin"] <- "roxithromycin"
ab$abgeneric[ab$abgeneric=="roxythromycin"] <- "roxithromycin"
ab$abgeneric[grepl("clarithro", ab$abgeneric)==TRUE] <- "clarithromycin"
ab$abgeneric[grepl("claritro", ab$abgeneric)==TRUE] <- "clarithromycin"
ab$abgeneric[grepl("clarethro", ab$abgeneric)==TRUE] <- "clarithromycin"
ab$abgeneric[grepl("clarthro", ab$abgeneric)==TRUE] <- "clarithromycin"
ab$abgeneric[grepl("clarythro", ab$abgeneric)==TRUE] <- "clarithromycin"
ab$abgeneric[grepl("ciproflox", ab$abgeneric)==TRUE] <- "ciprofloxacin"
ab$abgeneric[grepl("liproflox", ab$abgeneric)==TRUE] <- "ciprofloxacin"
ab$abgeneric[grepl("cifran", ab$abgeneric)==TRUE] <- "ciprofloxacin"
ab$abgeneric[grepl("ceproflox", ab$abgeneric)==TRUE] <- "ciprofloxacin"
ab$abgeneric[grepl("levofloxacin", ab$abgeneric)==TRUE] <- "levofloxacin"
ab$abgeneric[ab$abgeneric=="tab leuofloxacin"] <- "levofloxacin"
ab$abgeneric[ab$abgeneric=="tab leuopfloxacin"] <- "levofloxacin"
ab$abgeneric[ab$abgeneric=="tab liprofloxacin"] <- "levofloxacin"
ab$abgeneric[grepl("zanocin", ab$abgeneric)==TRUE] <- "ofloxacin"
ab$abgeneric[ab$abgeneric=="ofloxacin"] <- "ofloxacin"
ab$abgeneric[ab$abgeneric=="ofloxacine"] <- "ofloxacin"
ab$abgeneric[ab$abgeneric=="ofioxacine"] <- "ofloxacin"
ab$abgeneric[grepl("norloxacin", ab$abgeneric)==TRUE] <- "norfloxacin"
ab$abgeneric[grepl("gatifloxacin", ab$abgeneric)==TRUE] <- "gatifloxacin"
ab$abgeneric[grepl("moxiflox", ab$abgeneric)==TRUE] <- "moxifloxacin"
ab$abgeneric[grepl("lincomycin", ab$abgeneric)==TRUE] <- "lincomycin"
ab$abgeneric[grepl("licomycin", ab$abgeneric)==TRUE] <- "lincomycin"
ab$abgeneric[grepl("clindamycin", ab$abgeneric)==TRUE] <- "clindamycin"
ab$abgeneric[grepl("dalacin", ab$abgeneric)==TRUE] <- "clindamycin"
ab$abgeneric[grepl("imipenem", ab$abgeneric)==TRUE] <- "imipenem"
ab$abgeneric[grepl("ertapenem", ab$abgeneric)==TRUE] <- "ertapenem"
ab$abgeneric[grepl("meropenem", ab$abgeneric)==TRUE] <- "meropenem"
ab$abgeneric[grepl("uribid", ab$abgeneric)==TRUE] <- "nitrofurantoin"
ab$abgeneric[grepl("nitrofurantoin", ab$abgeneric)==TRUE] <- "nitrofurantoin"
ab$abgeneric[grepl("nitrofurantion", ab$abgeneric)==TRUE] <- "nitrofurantoin"
ab$abgeneric[grepl("nitrofuransoin", ab$abgeneric)==TRUE] <- "nitrofurantoin"
ab$abgeneric[grepl("nitrofuratoin", ab$abgeneric)==TRUE] <- "nitrofurantoin"
ab$abgeneric[ab$abgeneric=="amoxicillin"&ab$abname=='Clavuzam']='amoxicillin and clavulanic acid'
ab$abgeneric[ab$abname=="Chlorpheniramine"]= NA
ab$abgeneric[ab$abname=="Chlorempheniramine"]=NA
ab$abgeneric[ab$abgeneric=='moxyclav duo 457']='amoxicillin and clavulanic acid'
ab$abgeneric[ab$abname=='Moxyclav Duo 1125mg injectable']='amoxicillin and clavulanic acid'
ab$abgeneric[ab$abname=='CÃÂ©phalosporin Hcl 500mg                                  Ovotan']='cephalexin'
ab$abgeneric[ab$abgeneric=='penicilline orale']='phenoxymethylpenicillin'
ab$abgeneric[grepl("nicilline v", ab$abgeneric)==TRUE] <- "phenoxymethylpenicillin"
ab$abgeneric[c(35,129)]='benzylpenicillin'
ab$abgeneric[ab$abgeneric=='penicillin'&ab$abroute=='po_tab']='phenoxymethylpenicillin'
ab$abgeneric[ab$abgeneric=='penicilline'&ab$abroute=='po_tab']='phenoxymethylpenicillin'
ab$abgeneric[ab$abname=='Phatrix']='ceftriaxone'
ab$abgeneric[ab$abgeneric=="ceftriaxone"&ab$abroute=="po_flacon"]<-"ceftriaxone"
ab$abgeneric[ab$abgeneric=="clavuzam"]='amoxicillin and clavulanic acid'
ab$abgeneric[grepl("decaris", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("analg", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("bruffen", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("cedocard", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("daflon", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("diclofenac", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("enalapril", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("fefol", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("duphaston", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("gripal", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ibuprofen", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("glyben", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("insulin", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("indomethacin", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ipprosec", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("isoniazid", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ketazol", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ketocona", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("opard", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("lasix", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("mebendaz", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("meyamyc", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("parac", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("papaverine", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ranitidine", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("paracetamol", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ondensetron", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("nph", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("nystatin", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ation salts", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("paracã©tamol", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("phenobarbital", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("polygel", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("prednisolone", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("promethazine", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[ab$abgeneric=="relief"] <- NA
ab$abgeneric[ab$abgeneric=="rosuvastatine"] <- NA
ab$abgeneric[ab$abgeneric=="solution physiologique gouttes nasalas"] <- NA
ab$abgeneric[grepl("spasfon", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("tinidazol", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("utrogestan", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("tribex", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("vermox", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("ribexfort", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("vitamine", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("vols grip", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("temperine", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("pyrimethamine", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("xylometazoline", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[ab$abgeneric=="thiamine"] <- NA
ab$abgeneric[ab$abgeneric=="sulfadoxine"] <- NA
ab$abgeneric[ab$abgeneric=="anset"] <- NA
ab$abgeneric[ab$abgeneric=="topicidal"] <- NA
ab$abgeneric[ab$abgeneric=="ubuprofene"] <- NA
ab$abgeneric[ab$abgeneric=="utrogestan"] <- NA
ab$abgeneric[grepl("micrigynon", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("rifampicin", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("nã©omycine", ab$abgeneric)==TRUE] <- "neomycin"
ab$abgeneric[grepl("mucoril", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("clotrimazol", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[ab$abgeneric=="benzathine penicillin"] <- "benzylpenicillin"
ab$abgeneric[grepl("aspirin", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[grepl("motilium", ab$abgeneric)==TRUE] <- NA
ab$abgeneric[ab$abgeneric=="cephalexin"] <- "cefalexin"
ab$abgeneric[grepl("1572423417281.jpg",ab$abgeneric)==T] <- "cefalexin"
ab$abgeneric[ab$abname == "Peniprocaine" & ab$abroute == "po_tab"] <- "phenoxymethylpenicillin"
ab$abgeneric[ab$abname == "Peni-procaine" & ab$abroute == "po_flacon"] <- "phenoxymethylpenicillin"
ab$abgeneric[ab$abname == "Benzyl pÃ©nicillin"] <- "procaine_penicillin"
ab$abgeneric[ab$abname == "Ceftriaxone sodium"] <- "ceftriaxone"
ab$abgeneric[ab$abname == "PÃ©nicillin  vk"] <- "phenoxymethylpenicillin"
ab$abgeneric[ab$abname == "PÃ©nicillin-vk"] <- "phenoxymethylpenicillin"
ab$abgeneric[ab$abname == "Ceftriaxone"] <- "ceftriaxone"

# check missings generic names & add manually if available from other data of photos taken
missinggeneric <- ab %>%
  filter(is.na(ab$abgeneric)==T)
ab$abgeneric[grepl("1571732678376.jpg", ab$abphotoext)==T] <- "amoxicillin"
ab$abdose[grepl("1571732678376.jpg", ab$abphotoext)==T] <- "125mg"
ab$abgeneric[grepl("1571743704934.jpg", ab$abphotoext)==T] <- "doxycycline"
ab$abgeneric[grepl("1571737860530.jpg", ab$abphotoext)==T] <- "amoxicillin"
ab$abgeneric[grepl("1571734522396.jpg", ab$abphotoext)==T] <- "amoxicillin"
ab$abgeneric[grepl("1571926530635.jpg", ab$abphotoext)==T] <- "amoxicillin"
ab$abunits[grepl("1571926530635.jpg", ab$abphotoext)==T] <- "1"
ab$abdose[grepl("1571734522396.jpg", ab$abphotoext)==T] <- "125mg"
ab$abgeneric[grepl("1571659306060.jpg", ab$abphotoext)==T] <- "amoxicillin and clavulanic acid"
ab$abgeneric[ab$abdose=="16 grammes"] <- "amoxicillin and clavulanic acid"
ab_su$abgeneric[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"] <- "sulfamethoxazole and trimethoprim"
ab_su$abgeneric[ab_su$abname=="Falcidox"] <- NA
ab_su$abgeneric[ab_su$abname=="Maladox"] <- NA

# break the combined formulations up in single antibiotics
combined_antibiotic_formaulations <- ab %>% filter(abgeneric=="metronidazole and norfloxacin")
metro_norflox1m <- c("metronidazole", "menorcine", "po_tab", NA, NA, NA, "200mg", "2x1", 10, 5, "Phatkin", "2022-01-01", "2500FC", "5RMD8D8Y3IWYCCAI3YKUMXF5L", "oral")
metro_norflox1n <- c("norfloxacin", "menorcine", "po_tab", NA, NA, NA, "200mg", "2x1", 10, 5, "Phatkin", "2022-01-01", "2500FC", "5RMD8D8Y3IWYCCAI3YKUMXF5L", "oral")
metro_norflox2m <- c("metronidazole", "menorcine", "po_flacon", NA, NA, NA, "100mg", "2x1", 1, 7, "Phatkin", NA, "3500FC", "uuid:024c08a6-26f4-48e1-ae8b-ce790202e11d", "oral")
metro_norflox2n <- c("norfloxacin", "menorcine", "po_flacon", NA, NA, NA, "100mg", "2x1", 1, 7, "Phatkin", NA, "3500FC", "uuid:024c08a6-26f4-48e1-ae8b-ce790202e11d", "oral")
ab <- rbind(ab, metro_norflox1m)
ab <- rbind(ab, metro_norflox1n)
ab <- rbind(ab, metro_norflox2m)
ab <- rbind(ab, metro_norflox2n)

# drop if abgeneric is a combination or NA
ab <- subset (ab, ab$abgeneric!="metronidazole and norfloxacin")
table(ab$abgeneric, useNA = "always")

# drop the antibiotics not used for systemic use (not po or parenteral)
ab_su <- subset(ab, ab$abroute!="other")
# streptomycin should only be labelled as a J01 antibiotic for systemic use when not used to treat TB (J04)

# gen var DDD (version 2020 https://www.whocc.no/atc_ddd_index/?code=ZZZZ)
# import DDD values from database
DDD2019 <- read_excel("db/ATCDDD2019.xlsx", sheet = "DDD", col_types = c("text", "text", "numeric", "text"))
ATCaware2019 <- read_excel("db/ATCDDD2019.xlsx", sheet = "AWaRe")
ATCaware2019$Substance <- tolower(ATCaware2019$Substance)
DDD <- merge(DDD2019, ATCaware2019, by.x = "ATC5...1", by.y = "ATC", all.x = T)
DDD$ab_name_mode <- paste(DDD$Substance, "_", DDD$`Route of administration`)
DDD <- DDD %>% select(ab_name_mode, `DDD value`)
ab_su$ab_P_O[ab_su$ab_iv_po=="oral"] <- "O"
ab_su$ab_P_O[ab_su$ab_iv_po=="parenteral"] <- "P"
ab_su$ab_name_mode <- paste(ab_su$abgeneric, "_", ab_su$ab_P_O)
ab_su <- merge(ab_su, DDD, by = "ab_name_mode", all.x = T)

# amikacin and amoxicllin
ab_su$ddd[ab_su$abgeneric=='amikacin'&ab_su$ab_P_O=="P"]='1'
ab_su$ddd[ab_su$abgeneric=='amoxicillin'&ab_su$ab_P_O=="O"]='1.5'
ab_su$ddd[ab_su$abgeneric=='amoxicillin'&ab_su$ab_P_O=="O"]='1.5'
ab_su$ddd[ab_su$abgeneric=='amoxicillin'&ab_su$ab_P_O=="P"]='3'
# amoxicillin and clavulanic acid
ab_su$ddd[ab_su$abgeneric=='amoxicillin and clavulanic acid'&ab_su$ab_P_O=="O"]='1.5'
ab_su$ddd[ab_su$abgeneric=='amoxicillin and clavulanic acid'&ab_su$ab_P_O=="O"]='1.5'
ab_su$ddd[ab_su$abgeneric=='amoxicillin and clavulanic acid'&ab_su$ab_P_O=="P"]='3'
# ampicillin
ab_su$ddd[ab_su$abgeneric=='ampicillin'&ab_su$ab_P_O=="P"]='6'
ab_su$ddd[ab_su$abgeneric=='ampicillin'&ab_su$ab_P_O=="O"]='2'
# azithromycin
ab_su$ddd[ab_su$abgeneric=='azithromycin'&ab_su$ab_P_O=="O"]='0.3'
ab_su$ddd[ab_su$abgeneric=='azithromycin'&ab_su$ab_P_O=="O"]='0.3'
# benzathine penicillin
ab_su$ddd[ab_su$abgeneric=='benzathine penicillin'&ab_su$ab_P_O=="P"]='3.6'
# benzylpenicillin
ab_su$ddd[ab_su$abgeneric=='benzylpenicillin'&ab_su$ab_P_O=="P"]='3.6'
ab_su$ddd[ab_su$abgeneric=='benzylpenicillin'&ab_su$ab_P_O=="P"]='3.6'
# cefadroxil
ab_su$ddd[ab_su$abgeneric=='cefadroxil'&ab_su$ab_P_O=="O"]='2'
ab_su$ddd[ab_su$abgeneric=='cefadroxil'&ab_su$ab_P_O=="O"]='2'
# cefixime
ab_su$ddd[ab_su$abgeneric=='cefixime'&ab_su$ab_P_O=="O"]='0.4'
#  cefotaxime
ab_su$ddd[ab_su$abgeneric=='cefotaxime'&ab_su$ab_P_O=="P"]='4'
# ceftriaxone
ab_su$ddd[ab_su$abgeneric=='ceftriaxone'&ab_su$ab_P_O=="P"]='2'
ab_su$ddd[ab_su$abgeneric=='ceftriaxone'&ab_su$abroute=='other']='2'
ab_su$ddd[ab_su$abgeneric=='ceftriaxone'&ab_su$ab_P_O=="P"]='2'

# abroute column cleaning missing observation("") (all are ceftriaxone i.e Parenteral(IV_bag,IV_vial,IM)(all are ceftriaxone i.e Parenteral(IV_bag,IV_vial,)(for gentamycin IM there are images)    
ab_su$ddd[ab_su$abgeneric=='ceftriaxone'&ab_su$ab_P_O=="P"]='2'
# cephalexin change to cefalexin and then assign ddd
ab_su$ddd[ab_su$abgeneric=="cefalexin"&ab_su$ab_P_O=="O"]="2"
# chloramphenicol (other mainly eyedrops are there)
ab_su$ddd[ab_su$abgeneric=='chloramphenicol'&ab_su$ab_P_O=="O"]='3'
ab_su$ddd[ab_su$abgeneric=='chloramphenicol'&ab_su$ab_P_O=="O"]='3'
ab_su$ddd[ab_su$abgeneric=='chloramphenicol'&ab_su$ab_P_O=="P"]='3'
# ciprofloxacin
ab_su$ddd[ab_su$abgeneric=='ciprofloxacin'&ab_su$ab_P_O=="O"]='1'
ab_su$ddd[ab_su$abgeneric=='ciprofloxacin'&ab_su$ab_P_O=="P"]='0.8'
ab_su$ddd[ab_su$abgeneric=='ciprofloxacin'&ab_su$ab_P_O=="O"]='1'
ab_su$ddd[ab_su$abgeneric=='ciprofloxacin'&ab_su$ab_P_O=="P"]='0.8'
# clindamycin
ab_su$ddd[ab_su$abgeneric=='clindamycin'&ab_su$ab_P_O=="O"]='1.2'
ab_su$ddd[ab_su$abgeneric=='clindamycin'&ab_su$ab_P_O=="P"]='1.8'
# cloxacillin
ab_su$ddd[ab_su$abgeneric=='cloxacillin'&ab_su$ab_P_O=="O"]='2'
ab_su$ddd[ab_su$abgeneric=="cloxacillin"&ab_su$ab_P_O=="P"]='2'
# doxycycline
ab_su$ddd[ab_su$abgeneric=='doxycycline'&ab_su$ab_P_O=="O"]='0.1'
# erythromycin
ab_su$ddd[ab_su$abgeneric=='erythromycin'&ab_su$ab_P_O=="O"]='1'
ab_su$ddd[ab_su$abgeneric=='erythromycin'&ab_su$ab_P_O=="O"]='1'
ab_su$ddd[ab_su$abgeneric=='erythromycin'&ab_su$ab_P_O=="P"]='1'
# gentamycin
ab_su$ddd[ab_su$abgeneric=='gentamicin'&ab_su$ab_P_O=="P"]='0.24'
# levofloxacin
ab_su$ddd[ab_su$abgeneric=='levofloxacin'&ab_su$ab_P_O=="P"]='0.5'
ab_su$ddd[ab_su$abgeneric=='levofloxacin'&ab_su$ab_P_O=="O"]='0.5'
# lincomycin
ab_su$ddd[ab_su$abgeneric=='lincomycin'&ab_su$ab_P_O=="P"]='1.8'
ab_su$ddd[ab_su$abgeneric=='lincomycin'&ab_su$ab_P_O=="O"]='1.8'
ab_su$ddd[ab_su$abgeneric=='lincomycin'&ab_su$ab_P_O=="P"]='1.8'
# metronidazole
ab_su$ddd[ab_su$abgeneric=='metronidazole'&ab_su$ab_P_O=="P"]='1.5'
ab_su$ddd[ab_su$abgeneric=='metronidazole'&ab_su$ab_P_O=="O"]='2'
ab_su$ddd[ab_su$abgeneric=='metronidazole'&ab_su$ab_P_O=="P"]='1.5'
ab_su$ddd[ab_su$abgeneric=='metronidazole'&ab_su$ab_P_O=="O"]='2'
# metronidazole and norfloxacin
# neomycin
ab_su$ddd[ab_su$abgeneric=='neomycin'&ab_su$ab_P_O=="O"]='1'
# nitrofurantoin
ab_su$ddd[ab_su$abgeneric=='nitrofurantoin'&ab_su$ab_P_O=="O"]='0.2'
# norfloxacin
ab_su$ddd[ab_su$abgeneric=='norfloxacin'&ab_su$ab_P_O=="O"]='0.8'
# ofloxacin
ab_su$ddd[ab_su$abgeneric=='ofloxacin'&ab_su$ab_P_O=="O"]='0.4'
# phenoxymethylpenicillin
ab_su$ddd[ab_su$abgeneric=='phenoxymethylpenicillin'&ab_su$ab_P_O=="O"]='2'
ab_su$ddd[ab_su$abgeneric=='phenoxymethylpenicillin'&ab_su$ab_P_O=="O"]='2'
# procaine_penicillin
ab_su$ddd[ab_su$abgeneric=='procaine_penicillin'&ab_su$ab_P_O=="P"]='0.6'
ab_su$ddd[ab_su$abgeneric=='procaine_penicillin'&ab_su$ab_P_O=="O"]='0.6'
ab_su$ddd[ab_su$abgeneric=='procaine_penicillin'&ab_su$ab_P_O=="O"]='0.6'
# sulfamethoxazole and trimethoprim
ab_su$ddd[ab_su$abgeneric == "sulfamethoxazole and trimethoprim" & ab_su$ab_P_O=="O"] <- 2.4
# tetracycline
ab_su$ddd[ab_su$abgeneric=='tetracycline'&ab_su$ab_P_O=="O"]='1'
# make numeric
ab_su$ddd <- as.numeric(ab_su$ddd)

# compare manually entered ddd with DDD value of table
dddcomparison <- ab_su %>%
  select(ab_name_mode, ab_iv_po, ddd, `DDD value`) %>%
  group_by(ab_name_mode)

table(dddcomparison$ab_name_mode[dddcomparison$ddd!=dddcomparison$`DDD value`]) # no mismatches anymore

# clean abdose ->  all to mg
ab_su$abdose_mg <- ab_su$abdose
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="1gr"] <- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="1g"] <- "1000"
# mistyped doses of amoxicillin 
ab_su$abdose_mg[grepl("gramm",ab_su$abdose)==T & ab_su$abgeneric=="amoxicillin"] <- "500"
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="1mg"] <- "1000"#### no such preparation
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin and clavulanic acid"&ab_su$abdose=="70 ml"] <- "125"#### no such preparation
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin and clavulanic acid"&ab_su$abdose=="16 grammes"] <- "1125"#### no such preparation
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="20gramme"] <- "1000"#### no such preparation
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&is.na(ab_su$abdose)& ab_su$ab_iv_po=="oral"] <- "500" #most missing that can be verified are 500mg
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="2*2"& ab_su$ab_iv_po=="oral"] <- "500" #most missing that can be verified are 500mg
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="2*2/j//2jours"] <- "500"###no such preparation
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="5"] <-"125"
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="500fc"] <-"500"
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="Sirop/125mg/5ml"] <-"125"
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin"&ab_su$abdose=="Sirop/125mg/5ml"] <-"125"
ab_su$abdose_mg[ab_su$abgeneric=="amoxicillin and beta-lactamase inhibitor"&ab_su$abdose=="70 ml"] <-"457"
ab_su$abdose_mg[ab_su$abgeneric=="ampicillin"&ab_su$abdose=="2"] <- "2000"#### no such preparation
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="3g"] <- "3000"
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="3"]<- "3000"
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="1, 5gramme"]<- "1500"
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="3.0g"]<- "3000"
ab_su$abdose_mg[ab_su$abgeneric=="ceftriaxone"&ab_su$abdose=="1gr"]<- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="ceftriaxone"&ab_su$abdose=="3000"]<- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="ceftriaxone"&ab_su$abdose=="1g"]<- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="ceftriaxone"&ab_su$abdose=="1 g"]<- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="ceftriaxone"& ab_su$abdose=="1mg"]<- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="chloramphenicol"&ab_su$abdose=="1gr"]<- "1000"
ab_su$abdose_mg[ab_su$abgeneric=="ciprofloxacin"&ab_su$abdose=="7 grammes"]<- "7000"
ab_su$abdose_mg[ab_su$abgeneric=="ciprofloxacin"&ab_su$abdose=="0, 2g"]<- "200"
ab_su$abdose_mg[ab_su$abgeneric=="ciprofloxacin"&ab_su$abdose=="1"]<- "250"#####PO_flacon
ab_su$abdose_mg[ab_su$abgeneric=="ciprofloxacin"&ab_su$abdose=="2mg dans 100ml"]<- "250"
ab_su$abdose_mg[ab_su$abgeneric=="erythromycin"&ab_su$abdose=="10.000mg"]<- "10000"
ab_su$abdose_mg[ab_su$abgeneric=="lincomycin"&ab_su$abdose=="2ml"]<- "600" # checked from photo
ab_su$abdose_mg[ab_su$abgeneric=="lincomycin"&ab_su$abdose=="1000mg"]<- "600" 
ab_su$abgeneric[ab_su$abgeneric=="metronidazole"&ab_su$abdose=="100mg de metronidazole +100 mg de norfloxacine"]<-"metronidazole and norfloxacin"#### dose indicates it is a combination drug 
ab_su$abgeneric[ab_su$abgeneric=="metronidazole"&ab_su$abdose=="MÃÂ©tro 100mg et norfloxacine 100mg"]<-"metronidazole and norfloxacin"#### dose indicates it is a combination drug 
ab_su$abdose_mg[ab_su$abgeneric=="metronidazole"&ab_su$abdose=="5gramme"]<- "5000"
ab_su$abdose_mg[ab_su$abgeneric=="metronidazole"&ab_su$abdose=="6grammes"]<- "6000"
ab_su$abdose_mg[ab_su$abgeneric=="metronidazole"&ab_su$abdose=="15 grammes"]<- "15000"
ab_su$abdose_mg[ab_su$abgeneric=="metronidazole"&ab_su$abdose=="125ml"]<- "125"
ab_su$abdose_mg[ab_su$abgeneric=="metronidazole"&is.na(ab_su$abdose)]<- "250"
ab_su$abdose_mg[ab_su$abgeneric=="nitrofurantoin"&ab_su$abdose=="2grammes"]<- "2000"
ab_su$abdose_mg[ab_su$abgeneric=="phenoxymethylpenicillin"&ab_su$abdose=="4"]<- "250"
ab_su$abdose_mg[ab_su$abgeneric=="phenoxymethylpenicillin"&is.na(ab_su$abdose)]<- "250"
ab_su$abdose_mg[ab_su$abdose=="1g"]="1000"
# conversion of IU to grams https://mypharmatools.com/othertools/iu
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="3000000 UI"] <-"1800"
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="1.8"] <-"1800"
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="5 000.000iu"] <-"3000"
ab_su$abdose_mg[ab_su$abgeneric=="benzylpenicillin"&ab_su$abdose=="5.000.000 i.u"] <-"3000"
ab_su$abdose_mg[ab_su$abgeneric=="procaine_penicillin"&ab_su$abdose=="3.000.000 IU"]<- "2950"
# removing the mg extension in order to change to numeric
ab_su$abdose_mg[ab_su$abdose=="125mg"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="500mg"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="250mg"]<-"250"
ab_su$abdose_mg[ab_su$abdose=="125 mg"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="500 mg"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="250 mg"]<-"250"
ab_su$abdose_mg[ab_su$abdose=="250m g/5ml"]<-"250"
ab_su$abdose_mg[ab_su$abdose=="250mg/5ml"]<-"250"
ab_su$abdose_mg[ab_su$abdose=="125mg suspension"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="1000mg"]<-"1000"
ab_su$abdose_mg[ab_su$abdose=="562.5mg"]<-"562.5"
ab_su$abdose_mg[ab_su$abdose=="562,5mg"]<-"562.5"
ab_su$abdose_mg[ab_su$abdose=="562, 5mg"]<-"562.5"
ab_su$abdose_mg[ab_su$abdose=="562, 5"]<-"562.5"
ab_su$abdose_mg[ab_su$abdose=="457mg"]<-"457"
ab_su$abdose_mg[ab_su$abdose=="525mg"]<-"525"
ab_su$abdose_mg[ab_su$abdose=="1125mg"]<-"1125"
ab_su$abdose_mg[ab_su$abdose=="1000mg ou 1g"]<-"1000"
ab_su$abdose_mg[ab_su$abdose=="2500mg"]<-"2500"
ab_su$abdose_mg[ab_su$abdose=="125mg/5ml"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="200mg"]<-"200"
ab_su$abdose_mg[ab_su$abdose=="100mg"]<-"100"
ab_su$abdose_mg[ab_su$abdose=="1500mg"]<-"1500"
ab_su$abdose_mg[ab_su$abdose=="100ml"]<-"100"
ab_su$abdose_mg[ab_su$abdose=="200mg/100ml"]<-"200"
ab_su$abdose_mg[ab_su$abgeneric=="ciprofloxacin"&ab_su$abdose=="2000mg par jour"]<-"500"#####looking at the abfreq column
ab_su$abdose_mg[ab_su$abdose=="150mg"]<-"150"
ab_su$abdose_mg[ab_su$abdose=="300mg"]<-"300"
ab_su$abdose_mg[ab_su$abdose=="599"]<-"200"
ab_su$abdose_mg[ab_su$abdose=="100 mg"]<-"100"
ab_su$abdose_mg[ab_su$abdose=="250mg/60ml"]<-"250"
ab_su$abdose_mg[ab_su$abdose=="5mg"]<-"5"
ab_su$abdose_mg[ab_su$abdose=="80mg"]<-"80"
ab_su$abdose_mg[ab_su$abdose=="500mg/100ml"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="125 mg/5ml"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="500mg par jour"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="5oomg"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="400mg"]<-"400"
ab_su$abdose_mg[ab_su$abdose=="500  mg"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="125mg/5mg"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="125, mg"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="125mg/100ml"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="200/100ml"]<-"200"
ab_su$abdose_mg[ab_su$abdose=="4000mg"]<-"4000"
ab_su$abdose_mg[ab_su$abdose=="125m"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="260mg"]<-"260"
ab_su$abdose_mg[ab_su$abdose=="1000 mg par jour"]<-"250"#### 4 times a day so unit equal 250
ab_su$abdose_mg[ab_su$abdose=="600mg"]<-"600"
ab_su$abdose_mg[ab_su$abdose=="2 grammes"]<-"500" # did do the calculation for a freq of 2*2
ab_su$abdose_mg[ab_su$abdose=="2 GRAMMES"]<-"500"
ab_su$abdose_mg[ab_su$abdose=="375 MG"]<-"375"
ab_su$abdose_mg[ab_su$abdose=="? U.I"]<-"1500" # 1572083851937.jpg	
ab_su$abdose_mg[ab_su$abdose=="125mg"]<-"125"
ab_su$abdose_mg[ab_su$abdose=="16000"]<-"1000" #media\1571659306060.jpg
ab_su$abdose_mg[grepl("1571646526762.jpg",ab_su$abphotoext)==T] <- "500"
ab_su$abfreq[grepl("1571646526762.jpg", ab_su$abphotoext)==T] <- "2*2"
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="amoxicillin"&(ab_su$abroute=="po_tab"|ab_su$abroute=="orale (comp./cap.)")]<-"500"
ab_su$abdose_mg[grepl("1571904057429.jpg", ab_su$abphotoext)==T] <- "457" # for the entire bottle of suspension
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="benzylpenicillin"&(ab_su$abroute=="po_tab"|ab_su$abroute=="orale (comp./cap.)")]<-"250"
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="benzylpenicillin"& ab_su$ab_iv_po=="parenteral"]<-"3000"
ab_su$abdose_mg[ab_su$abgeneric =="benzylpenicillin"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="1 GRAMMES"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="benzylpenicillin"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="4"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="benzylpenicillin"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="260"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="benzylpenicillin"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="1gramme"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="1 GRAMME"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="1200fc"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="125 ml"]<-"125"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="parenteral" & is.na(ab_su$abdose)]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="15000"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="300fc"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="5000"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="6000"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="metronidazole"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="15 grammes"]<-"500"
ab_su$abdose_mg[ab_su$abgeneric =="phenoxymethylpenicillin"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="2*3/jour"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="phenoxymethylpenicillin"& ab_su$ab_iv_po=="oral" & is.na(ab_su$abdose)]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="phenoxymethylpenicillin"& ab_su$abdose== "1gramme"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="phenoxymethylpenicillin"& ab_su$abdose== "1 GRAMMES"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="phenoxymethylpenicillin"& ab_su$ab_iv_po=="oral" & ab_su$abdose=="600fc"]<-"250"
ab_su$abdose_mg[ab_su$abgeneric =="procaine_penicillin"& ab_su$ab_iv_po=="parenteral" & ab_su$abdose=="3000mg"]<-"3000"
ab_su$abdose_mg[ab_su$abgeneric =="procaine_penicillin"& ab_su$ab_iv_po=="parenteral" & is.na(ab_su$abdose)]<-"3000"
ab_su$abdose_mg[ab_su$abgeneric =="procaine_penicillin"& ab_su$ab_iv_po=="parenteral" & ab_su$abdose=="3000mg"]<-"3000"
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="cefixime"& ab_su$ab_iv_po=="oral"]<-"200"
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="ceftriaxone"& ab_su$ab_iv_po=="parenteral"]<-"1000" # mentioned in abfreq
ab_su$abdose_mg[ab_su$abdose == "1gramme" & ab_su$abgeneric =="ceftriaxone"]<-"1000" # mentioned in abfreq
ab_su$abdose_mg[ab_su$abdose == "1gramme" & ab_su$abgeneric =="cefotaxime"]<-"1000" # mentioned in abfreq
ab_su$abdose_mg[ab_su$abdose == "3000" & ab_su$abgeneric =="cefotaxime"]<-"1000" # mentioned in abfreq
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="chloramphenicol"& ab_su$ab_iv_po=="oral"]<-"250" # mentioned in abfreq
ab_su$abdose_mg[ab_su$abdose == "1000/1200" & ab_su$abgeneric =="ciprofloxacin"& ab_su$ab_iv_po=="oral"]<-"500" 
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="ciprofloxacin"& ab_su$ab_iv_po=="oral"]<-"500" 
ab_su$abdose_mg[ab_su$abdose == "7000" & ab_su$abgeneric =="ciprofloxacin"& ab_su$ab_iv_po=="oral"]<-"500" 
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="doxycycline"& ab_su$ab_iv_po=="oral"]<-"100" 
ab_su$abdose_mg[ab_su$abdose == "400 MG" & ab_su$abgeneric =="doxycycline"& ab_su$ab_iv_po=="oral"]<-"100" 
ab_su$abdose_mg[ab_su$abdose == "10000" & ab_su$abgeneric =="erythromycin"& ab_su$ab_iv_po=="oral"]<-"250" 
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="erythromycin"& ab_su$ab_iv_po=="oral"]<-"250" 
ab_su$abdose_mg[ab_su$abdose == "160 mg" & ab_su$abgeneric =="gentamicin"]<-"80" 
ab_su$abdose_mg[is.na(ab_su$abdose) & ab_su$abgeneric =="erythromycin"& ab_su$ab_iv_po=="oral"]<-"250" 
ab_su$abdose_mg[ab_su$abdose == "10000" & ab_su$abgeneric =="erythromycin"& ab_su$ab_iv_po=="oral"]<-"250" 
ab_su$abdose_mg[ab_su$abgeneric =="tetracycline"& ab_su$ab_iv_po=="oral"]<-"250" 
# for the combinations drugs (metronidazole and norfloxacin=100+100), which we already broke up in 2 observations each
ab_su$abdose_mg[ab_su$abdose=="100mg de metronidazole +100 mg de norfloxacine"]="100"
ab_su$abdose_mg[ab_su$abdose=="MÃ©tro 100mg et norfloxacine 100mg"]="100"
# for the combination drug sulfamethaole and trimethprim(400+80)(200+40 sus.)(800+160)
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="100 ml"]<-"240"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="100"]<-"240"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="125"]<-"240"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="245mg"]<-"240"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="480mg"]<-"480"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="500"]<-"480"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="160mg"]<-"960"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="1000"]<-"960"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="160mg de trimethoprime, 800mg de sulfamethoxazole"]<-"960"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="120mg"]<-"240"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="240mg"]<-"240"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="480 MG"]<-"480"
ab_su$abdose_mg[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"&ab_su$abdose=="960MG"]<-"960"
ab_su$abdose_mg[ab_su$abgeneric=="neomycin"&ab_su$abdose=="100ml"]<-"2500"
ab_su$abdose_mg[ab_su$abgeneric=="doxycycline"] <- "100"

# overview of cleaned doses
abdoses <- ab_su %>%
  group_by(abgeneric, abroute, abdose_mg) %>%
  summarise(total = n())
abdoses
write.table(abdoses, file = "abdoses.txt")

# check and clean extreme values - checked them one by one manually in ab_su
ab_su$abdose_mg <- as.numeric(ab_su$abdose_mg)
ab_su$abdose_mg[ab_su$abdose_mg==5 & ab_su$abgeneric=="erythromycin"] <- 125
ab_su$abdose_mg[ab_su$abdose_mg==2500 & ab_su$abgeneric=="azithromycin"] <- 250
ab_su$abdose_mg[ab_su$abdose_mg==1500 & ab_su$abgeneric=="ceftriaxone"] <- 1000
ab_su$abdose_mg[ab_su$abdose_mg==7000 & ab_su$abgeneric=="ciprofloxacin"] <- 500
ab_su$abdose_mg[ab_su$abdose_mg==10000 & ab_su$abgeneric=="erythromycin"] <- 250
ab_su$abdose_mg[ab_su$abdose_mg==6000 & ab_su$abgeneric=="metronidazole"] <- 500
ab_su$abdose_mg[ab_su$abdose_mg==5000 & ab_su$abgeneric=="metronidazole"] <- 500
ab_su$abdose_mg[ab_su$abdose_mg==2000 & ab_su$abgeneric=="nitrofurantoin"] <- 100
ab_su$abdose_mg[ab_su$abdose_mg==4000 & ab_su$abgeneric=="norfloxacin"] <- 200


# clean frequency
# export and manually clean, then merge with existinbg db
abfreqtab <- table(ab_su$abfreq)
write.table(abfreqtab, file = "abfreqtab.txt")
abfreqtab_cleaned_excel <- read_excel("db/abfreqtab_cleaned_excel.xlsx", sheet = "abfreqtab_cleaned")
abfreqtab <- abfreqtab_cleaned_excel %>% select(abfreq, abfreq_cleaned)
ab_su <- merge(ab_su, abfreqtab, by = "abfreq", all = T) # added the column with the cleaned variable for abfreq
ab_su$abfreq_cleaned[ab_su$abgeneric=="ciprofloxacin"&ab_su$abfreq=="2x400mg"]<-"4"
ab_su$abfreq_cleaned[ab_su$abgeneric=="metronidazole"&ab_su$abfreq=="2X500mg"]<-"2"
ab_su$abfreq_cleaned[ab_su$abgeneric=="ciprofloxacin"&ab_su$abfreq=="2X500mg"]<-"2"
ab_su$abfreq_cleaned[ab_su$abgeneric=="lincomycin"&ab_su$abfreq=="2*/j//14"]<- NA # no detail on dose given per administr
ab_su$abfreq_cleaned[ab_su$abgeneric=="amoxicillin"&ab_su$abfreq=="2.5ml"]<- NA # no detail on dose given per administr

ab_su$abfreq_cleaned <- as.numeric(ab_su$abfreq_cleaned)

# clean duration
table(ab_su$abduration)
ab_su$abduration <- as.numeric(ab_su$abduration)
ab_su$abduration[ab_su$abduration==90] <- 20 # based on number of units, abunits, and abdose

# clean abunits
table(ab_su$abunits)
ab_su$abunitsnum <- as.numeric(ab_su$abunits)
ab_su$abunitsnum[ab_su$abunitsnum>50] <- NA # drop if more than 50 units (quite unlikely)
ab_su$abunitsnum[is.na(ab_su$abunitsnum)] <- ab_su$abfreq_cleaned[is.na(ab_su$abunitsnum)]*ab_su$abduration[is.na(ab_su$abunitsnum)] # recalculate units from frequency and duration
ab_su$abunitsnum[ab_su$abdose_mg==250 & ab_su$abgeneric=="azithromycin"] <- 10
ab_su$abunitsnum[ab_su$abdose_mg==125 & ab_su$abgeneric=="amoxicillin" & (ab_su$abroute=="orale (sirop/suspension)" | ab_su$abroute == "po_flacon")] <- 12
ab_su$abunitsnum[ab_su$KEY == "uuid:12673139-529d-4789-927b-2ceb812a4a24"] <- 8

# calculate DDDs used
ab_su$totalmg <- ab_su$abfreq_cleaned *ab_su$abduration * ab_su$abdose_mg
ab_su$totalmg_basedonunits <- ab_su$abunitsnum * ab_su$abdose_mg
ab_su$dddused <- ab_su$totalmg/(1000*ab_su$ddd) # factor 1000 to express both num and denom in grams
ab_su$dddused[is.na(ab_su$totalmg)] <- ab_su$totalmg_basedonunits[is.na(ab_su$totalmg)]/(1000*ab_su$ddd[is.na(ab_su$totalmg)]) # factor 1000 to express both num and denom in grams

# manually add the mean DDD of that antibiotic/route if DDD missing by selecting the mean DDDs of those antibiotic/route with same values
ab_su$dddused[ab_su$abgeneric=="sulfamethoxazole and trimethoprim" & is.na(ab_su$dddused)&ab_su$abdose_mg==480 & ab_su$abfreq_cleaned == 1.0] <- mean(ab_su$dddused[ab_su$abgeneric=="sulfamethoxazole and trimethoprim"& !is.na(ab_su$dddused) & ab_su$abdose_mg==480 & ab_su$abfreq_cleaned == 1.0])
ab_su$dddused[ab_su$abgeneric=="chloramphenicol" & is.na(ab_su$dddused) & ab_su$ab_iv_po == "oral"] <- mean(ab_su$dddused[ab_su$abgeneric=="chloramphenicol" & !is.na(ab_su$dddused) & ab_su$ab_iv_po == "oral"])
ab_su$dddused[ab_su$abgeneric=="metronidazole" & is.na(ab_su$dddused) & ab_su$ab_iv_po == "oral" & ab_su$abdose_mg==250] <- mean(ab_su$dddused[ab_su$abgeneric=="metronidazole" & !is.na(ab_su$dddused) & ab_su$ab_iv_po == "oral" & ab_su$abdose_mg==250])
ab_su$dddused[ab_su$abgeneric=="ceftriaxone" & is.na(ab_su$dddused) & ab_su$ab_iv_po == "parenteral" & ab_su$abdose_mg==1000] <- mean(ab_su$dddused[ab_su$abgeneric=="ceftriaxone" & !is.na(ab_su$dddused) & ab_su$ab_iv_po == "parenteral" & ab_su$abdose_mg==1000])
ab_su$dddused[ab_su$abgeneric=="benzylpenicillin" & is.na(ab_su$dddused)] <- mean(ab_su$dddused[ab_su$abgeneric=="benzylpenicillin" & !is.na(ab_su$dddused)])

# unclear for all procaine benzylpeni what dose was administered (probably because mixing up ml/mg/IU) -> based on ideal DDD
ab_su$dddused[ab_su$abgeneric=="procaine_penicillin"] <- ab_su$ddd[ab_su$abgeneric=="procaine_penicillin"]*ab_su$abduration[ab_su$abgeneric=="procaine_penicillin"]
ab_su$dddused[ab_su$abgeneric=="procaine_penicillin" & is.na(ab_su$abduration)] <- mean(ab_su$dddused[ab_su$abgeneric=="procaine_penicillin" & !is.na(is.na(ab_su$abduration))])


# add access, watch and reserve classification drugs according to the 2019 WHO AWaRE classification
# import antibiotic group classification
WHO_EML2019 <- read_excel("db/WHO-EMP-IAU-2019.11-eng.xlsx", sheet = "db")
WHO_EML2019$Antibiotic <- tolower(WHO_EML2019$Antibiotic)
WHO_EML2019$abgeneric <- WHO_EML2019$Antibiotic
WHO_EML2019$Antibiotic <- NULL
WHO_EML2019$Category <- tolower(WHO_EML2019$Category)
WHO_EML2019$Class <- tolower(WHO_EML2019$Class)
WHO_EML2019$abgeneric[WHO_EML2019$abgeneric=="metronidazole (iv)"] <- "metronidazole"
WHO_EML2019$abgeneric[WHO_EML2019$abgeneric=="metronidazole (oral)"] <- NA #IV and oral are both on the EML and access -> not needed to keep separated
# regroup some classes according to CLSI
WHO_EML2019$Class[WHO_EML2019$Class=="first-generation cephalosporins"] <- "cephems"
WHO_EML2019$Class[WHO_EML2019$Class=="third-generation cephalosporins"] <- "cephems"
WHO_EML2019$Class[WHO_EML2019$Class=="nitrofurantoin"] <- "nitroheterocyclics"
WHO_EML2019$Class[WHO_EML2019$Class=="imidazoles"] <- "nitroheterocyclics"
WHO_EML2019$Class[WHO_EML2019$Class=="amphenicols"] <- "phenicols"
WHO_EML2019$Class[WHO_EML2019$Class=="beta lactam - beta lactamase inhibitor"] <- "beta-lactam combination agent"
WHO_EML2019$Class[WHO_EML2019$Class=="lincosamides"] <- "lincomycins"


# rename antibiotics to match the WHO classification
ab_su$abgeneric[ab_su$abgeneric=="amoxicillin and clavulanic acid"] <- "amoxicillin/clavulanic acid"
ab_su$abgeneric[ab_su$abgeneric=="procaine_penicillin"] <- "procaine benzylpenicillin"
ab_su$abgeneric[ab_su$abgeneric=="benzathine penicillin"] <- "benzathine benzylpenicillin"
ab_su$abgeneric[ab_su$abgeneric==" benzylpenicillin"] <- "benzathine benzylpenicillin"
# merge ab_su with WHO EML db
ab_su <- merge(ab_su, WHO_EML2019, by ="abgeneric", all.x = TRUE) # all matched well
table(ab_su$Class, useNA = "always") # all are matched

#### merge patient and antibiotic data ####
db <- merge(patient, ab_su, by.x = "KEY", by.y = "parent_key", all.x = TRUE) # by using all.x, the test observation (n=1) in ab is dropped
db$providerarea <- tolower(db$providerarea)
# move kintanu1 with kavuaya, as many of its residents get their meds there
db$providerarea[db$providerarea=="kintanu1"] <- "Kisantu, Nkandu"
db$providerarea[db$providerarea=="kavuaya"] <- "Kisantu, Kavuaya"
db$providerarea[db$providerarea=="nkandu"] <- "Kisantu, Nkandu"
db$providerarea[db$providerarea=="viaza"] <- "Kimpese, Viaza"
db$providerarea[db$providerarea=="malanga"] <- "Kimpese, Malanga"
# gen var district
db$district[db$providerarea=="Kimpese, Malanga"|db$providerarea=="Kimpese, Viaza"] <- "Kimpese"
db$district[db$providerarea=="Kisantu, Nkandu"|db$providerarea=="Kisantu, Kavuaya"] <- "Kisantu"
# gen var providertype for figures
db$providertype_fig <- db$providertype
db$providertype_fig[db$providertype=="healthcentre"] <- "health centre"
db$providertype_fig[db$providertype=="privateclinic"] <- "private clinic"
db$providertype_fig[db$providertype=="privatepharmacy"] <- "medicine store"
db$providertype_fig[db$providertype=="religiousleader"] <- "religious leader"
db$providertype_fig[db$providertype=="traditionalhealer"] <- "traditional healer"

# db only AB 
db_abonly <- db %>% filter(!is.na(abgeneric)&abroute!="other"& !is.na(Category))
db_abonly_aware <- dcast(db_abonly, KEY + providertype + district + providerarea + ab_iv_po ~ Category) 
# shorter db 
dbshorter <- db %>%
  select(abgeneric, KEY, providertype, abfreq, abunits, abduration, abdose_mg, abfreq_cleaned, abroute, ddd, dddused, abphotoext, abphotoint)

# clean antimalarial 
db$antimalarial_spec <- tolower(db$antimalarial_spec)
db$antimalarial[grepl("sulfadoxine", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("fansidar", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("sulphadoxine", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("sulfadoixine", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("maladox", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("maladar", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("falcidox", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "sulfadoxine/pyrimethamine"
db$antimalarial[grepl("quinine", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "quinine"
db$antimalarial[grepl("artemet", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "artesunate"
db$antimalarial[grepl("arthemet", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "artesunate"
db$antimalarial[grepl("artesun", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "artesunate"
db$antimalarial[grepl("manalaria", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "plant-based treatment"
db$antimalarial[grepl("plantes", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "plant-based treatment"
db$antimalarial[grepl("indigene", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "plant-based treatment"
db$antimalarial[grepl("tisane", db$antimalarial_spec)==TRUE & (db$antimalarial == "other"|db$antimalarial == "")] <- "plant-based treatment"
db$antimalarial[db$antimalarial_spec=="" & (db$antimalarial == "other"|db$antimalarial == "")] <- "no"
db$antimalarial[db$antimalarial == "other"|db$antimalarial == ""] <- "no"
db$antimalarial[is.na(db$antimalarial)==T] <- "no"
# assign var for AB use
db$antibioticuse[is.na(db$abgeneric)] <- 0
db$antibioticuse[!is.na(db$abgeneric)] <- 1
# remove duplicate rows
db <- db %>% distinct()

#### 1. PATIENT CHARACTERISTICS & ####
# count all observations
table(patient$site)
# by area
table(patient$providerarea)
# providertype
table(patient$providertype)
table(patient$providertype, patient$providerarea)
# age & sex distrib
hist(patient$ageyears, xlab = "Age (years)")
table(patient$adoadult)
table(patient$sex)
round(prop.table(table(patient$sex))*100,2)

#### 2. ANTIBIOTIC USE BY PROVIDER ####
# % antibiotic use per provider type
nantibioticsused <- db %>%
  group_by(KEY, providertype, antibioticuse) %>%
  summarise(abuse = n(), nantibiotics=sum(antibioticuse))
# n of antibiotics
tabnab <- table(nantibioticsused$providertype, nantibioticsused$nantibiotics)
propnab <- round(prop.table(table(nantibioticsused$providertype, nantibioticsused$nantibiotics),1)*100,1)
tabnab <- cbind(tabnab, propnab)
tabnab
write.table(tabnab, file = "tabnab.txt")

# frequency of AB use
# replace values with at least one ab or antimalarial
nantibioticsused$antibioticuse_bin <- "no antibiotic used"
nantibioticsused$antibioticuse_bin[nantibioticsused$nantibiotics>0] <- ">/= 1 antibiotic used"
# table frequency antibiotic use by providertype
table(nantibioticsused$providertype, nantibioticsused$antibioticuse_bin)
abfreq <- as.data.frame(round(prop.table(table(nantibioticsused$providertype, nantibioticsused$antibioticuse_bin),1)*100,1))
abfreq <- subset(abfreq, abfreq$Var2 == "at least one AB")
abfreq
names(abfreq) <- c("providertype", "Class", "pctABuse")
round(prop.table(table(nantibioticsused$providertype, nantibioticsused$antibioticuse_bin), 1)*100,1)

# frequency of antibiotic use by population, weighted for HCU
nepisodes_by_provider <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/CABU data analysis/nepisodes_by_provider.txt", sep="")
nepisodes_by_provider
weightedABuse <- merge(abfreq, nepisodes_by_provider, by = "providertype", all.y = T)
weightedABuse$ABfreq <- weightedABuse$pctABuse*weightedABuse$hcufreq/100
weightedABuse
sum(weightedABuse$ABfreq)

# stratified by area
nantibioticsused_byarea <- db %>%
  group_by(KEY, providertype, providerarea, antibioticuse) %>%
  summarise(nantibiotics=sum(antibioticuse))
# summary for SuppFig2
nantibioticsused_byarea$multipleABused <- 0
nantibioticsused_byarea$multipleABused[nantibioticsused_byarea$nantibiotics>1] <- 1
antibioticusefreq_byarea <- nantibioticsused_byarea %>%
  group_by(providertype, providerarea) %>%
  summarise(n = n(), ABuseprev = sum(antibioticuse)/n(), multipleABuseprev = sum(multipleABused)/n())
antibioticusefreq_byarea
write.table(antibioticusefreq_byarea, file = "antibioticusefreq_byarea.txt")

# plot AB use freq by area
plotantibioticusefreq_byarea <- ggplot(antibioticusefreq_byarea, aes(x=providerarea, y=ABuseprev, label = scales::percent(ABuseprev))) +
  geom_col(position = 'dodge') +
  geom_text(data=subset(antibioticusefreq_byarea, ABuseprev != 0), position = position_dodge(width = 0.9),    # move to center of bars
            vjust = 0.3,    # nudge above top of bar
            size = 4) + 
  coord_flip() +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank()) +
  facet_wrap(. ~ providertype, ncol = 1)  
plotantibioticusefreq_byarea
ggsave(plotantibioticusefreq_byarea, filename = "plotantibioticusefreq_byarea.png",  width = 3.5, height = 8, bg = "white")

# AB use frequency by provider, by district
nantibioticsused_bydistrict <- db %>%
  group_by(KEY, providertype_fig, district) %>%
  summarise(nantibiotics=sum(antibioticuse))
nantibioticsused_bydistrict$abu <- 0
nantibioticsused_bydistrict$abu[nantibioticsused_bydistrict$nantibiotics>0] <- 1
antibioticusefreq_bydistrict <- nantibioticsused_bydistrict %>%
  group_by(providertype_fig, district) %>%
  summarise(n = n(), abufreq = sum(abu), ABuseprev = sum(abu)/n())
antibioticusefreq_bydistrict <- as.data.frame(antibioticusefreq_bydistrict)
str(antibioticusefreq_bydistrict)
antibioticusefreq_bydistrict
write.table(antibioticusefreq_bydistrict, file = "antibioticusefreq_bydistrict.txt")

# plot AB use freq by district
brks <- c(0, 0.25, 0.5, 0.75, 1)
plotantibioticusefreq_bydistrict <- ggplot(antibioticusefreq_bydistrict, aes(x=as.factor(district), y=ABuseprev, label = scales::percent(ABuseprev))) +
  geom_col(position = 'dodge') +  
  facet_grid(providertype_fig ~ ., labeller = label_wrap_gen(width=10)) +  
  labs(x="",y="") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  # geom_text(data=subset(antibioticusefreq_bydistrict, antibioticusefreq_bydistrict$ABuseprev != 0), position = position_dodge(width = 0.9),    # move to center of bars
  #           vjust = 0.3,    # nudge above top of bar
  #           size = 4)  +
  coord_flip() +
  ggtitle("   Prevalence of antibiotic use (%)") + theme(plot.title = element_text(size=10))
plotantibioticusefreq_bydistrict
ggsave(plotantibioticusefreq_bydistrict, filename = "plotantibioticusefreq_bydistrict.png",  width = 3.5, height = 4.5, bg = "white")

# % watch use per provider type
watch <- ab_su %>%
  filter(Category=="watch") %>%
  select(parent_key, Category)
watch <- merge(patient, watch, by.x = "KEY", by.y = "parent_key", all.x = TRUE)
watch$Category[is.na(watch$Category)==T] <- "no watch"
table(watch$providertype, watch$Category)
round(prop.table(table(watch$providertype, watch$Category),1)*100,1)
watchfrequencybyprovider <- as.data.frame(round(prop.table(table(watch$providertype, watch$Category),1)*100,1))
watchfrequencybyprovider <- subset(watchfrequencybyprovider, watchfrequencybyprovider$Var2 == "watch")
watchfrequencybyprovider
names(watchfrequencybyprovider) <- c("providertype", "Class", "pctwatch")

# frequency of watch use by population, weighted for HCU
nepisodes_by_provider <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/CABU data analysis/nepisodes_by_provider.txt", sep="")
nepisodes_by_provider
weightedwatch <- merge(watchfrequencybyprovider, nepisodes_by_provider, by = "providertype", all.y = T)
weightedwatch$watchfreq <- weightedwatch$pctwatch*weightedwatch$hcufreq/100
sum(weightedwatch$watchfreq)

# % access vs watch use per provider type per area
# stratified by area, wide 
accesswatchfreq_byarea <- db_abonly_aware %>%
  group_by(providertype, providerarea) %>%
  summarise(n = sum(access) + sum(watch), access = sum(access), watch = sum(watch), watchprev = round(sum(watch)/(sum(watch)+sum(access))*100,1))
accesswatchfreq_byarea
write.table(accesswatchfreq_byarea, file = "accesswatchfreq_byarea.txt")
# stratified by area, long 
db_abonly$AWaRe <- db_abonly$Category 
accesswatchfreq_byarea_long <- db_abonly %>%
  group_by(providertype, providerarea, AWaRe) %>%
  summarise(n = n())
plotaccesswatchfreq_byarea <- ggplot(accesswatchfreq_byarea_long, aes(x=providerarea, y=n, fill=factor(AWaRe, levels = c("watch","access")))) +
  geom_bar(position = "fill",stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() +
  facet_wrap(. ~ providertype, ncol = 1) +  
  scale_fill_manual(values=c("orange", "dark green")) 
plotaccesswatchfreq_byarea
ggsave(plotaccesswatchfreq_byarea, filename = "plotaccesswatchfreq_byarea.png",  width = 3.5, height = 8, bg = "white")

# stratified by district, wide 
accesswatchfreq_bydistrict <- db_abonly_aware %>%
  group_by(providertype_fig, district) %>%
  summarise(n = sum(access) + sum(watch), access = sum(access), watch = sum(watch), watchprev = round(sum(watch)/(sum(watch)+sum(access))*100,1))
accesswatchfreq_bydistrict
# stratified by district, long 
db_abonly$AWaRe <- db_abonly$Category 
accesswatchfreq_bydistrict_long <- db_abonly %>%
  group_by(providertype_fig, district, AWaRe) %>%
  summarise(n = n())
plotaccesswatchfreq_bydistrict <- ggplot(accesswatchfreq_bydistrict_long, aes(x=district, y=n, fill=factor(AWaRe, levels = c("watch","access")))) +
  geom_bar(position = "fill",stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="",y="") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  facet_grid(providertype_fig ~ ., labeller = label_wrap_gen(width=10)) +  
  scale_fill_manual(values=c("orange", "dark green")) +
  ggtitle("   Access vs. Watch group distribution (%)") + theme(plot.title = element_text(size=10))

plotaccesswatchfreq_bydistrict
ggsave(plotaccesswatchfreq_bydistrict, filename = "plotaccesswatchfreq_bydistrict.png",  width = 3.5, height = 4.5, bg = "white")

# watch as first use
db$firstuse[db$firstuse==""] <- NA
# in private pharmacies
round(prop.table(table(db$Category[db$providertype=="privatepharmacy"], db$firstuse[db$providertype=="privatepharmacy"]),1)*100,1)
# in private clinics
round(prop.table(table(db$Category[db$providertype=="privateclinic"], db$firstuse[db$providertype=="privateclinic"]),1)*100,1)
# in health centres
round(prop.table(table(db$Category[db$providertype=="healthcentre"], db$firstuse[db$providertype=="healthcentre"]),1)*100,1)



# % IV vs PO 
# overall
table(db_abonly$providertype, db_abonly$ab_iv_po) 
round(prop.table(table(db_abonly$providertype, db_abonly$ab_iv_po),1)*100,1)

# per provider type per area
iv_po_freq_byarea_long <- db_abonly %>%
  group_by(providertype, providerarea, ab_iv_po) %>%
  summarise(n = n())
plotiv_pofreq_byarea <- ggplot(iv_po_freq_byarea_long, aes(x=providerarea, y=n, fill=factor(ab_iv_po, levels = c("parenteral","oral")))) +
  geom_bar(position = "fill",stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() +
  facet_wrap(. ~ providertype, ncol = 1) +  
  scale_fill_manual(values=c("black", "light grey")) 
plotiv_pofreq_byarea
ggsave(plotiv_pofreq_byarea, filename = "plotiv_pofreq_byarea.png",  width = 3.5, height = 8, bg = "white")

# per provider type per district
iv_po_freq_bydistrict_long <- db_abonly %>%
  group_by(providertype_fig, district, ab_iv_po) %>%
  summarise(n = n())
plotiv_pofreq_bydistrict <- ggplot(iv_po_freq_bydistrict_long, aes(x=district, y=n, fill=factor(ab_iv_po, levels = c("parenteral","oral")))) +
  geom_bar(position = "fill",stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  facet_grid(providertype_fig ~ ., labeller = label_wrap_gen(width=10)) +  
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("black", "light grey")) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ggtitle("   Route of administration (%)") + theme(plot.title = element_text(size=10))
plotiv_pofreq_bydistrict
ggsave(plotiv_pofreq_bydistrict, filename = "plotiv_pofreq_bydistrict.png",  width = 3.5, height = 4.5, bg = "white")

# duration of treatment
DOT <- db_abonly %>%
  filter(!is.na(abduration) & abduration < 30) %>%
  group_by(providertype) %>%
  summarise(median = median(abduration), q1 = quantile(abduration, 0.25), q3 = quantile(abduration, 0.75), mean = mean(abduration), sd = sd(abduration))
DOT

# excessively short DOT
db_abonly$shortDOT[db_abonly$abduration<3] <- "yes"
db_abonly$shortDOT[db_abonly$abduration>2] <- "no"
table(db_abonly$providertype, db_abonly$shortDOT)
round(prop.table(table(db_abonly$providertype, db_abonly$shortDOT),1)*100,1)

# excessively long DOT
db_abonly$longDOT[db_abonly$abduration>8] <- "yes"
db_abonly$longDOT[db_abonly$abduration<9] <- "no"
table(db_abonly$providertype, db_abonly$longDOT)
round(prop.table(table(db_abonly$providertype, db_abonly$longDOT),1)*100,1)

# DDD per provider type 
dddperpatient <- db %>%
  group_by(KEY, providertype) %>%
  summarise(dddperpatient=sum(dddused))
dddperpatient$dddperpatient[is.na(dddperpatient$dddperpatient)] <- 0

averagedddperpatientbyprovider <- dddperpatient %>%
  group_by(providertype) %>%
  summarise(n =n(), meanddd = round(sum(dddperpatient)/n(),2), sd = sd(dddperpatient))
averagedddperpatientbyprovider

# DDD per provider type per area
dddperpatient_byarea <- db %>%
  group_by(KEY, providertype, providerarea) %>%
  summarise(dddperpatient=sum(dddused))
dddperpatient_byarea$dddperpatient[is.na(dddperpatient_byarea$dddperpatient)] <- 0

averagedddperpatientbyprovider_byarea <- dddperpatient_byarea %>%
  group_by(providertype, providerarea) %>%
  summarise(n =n(), meanddd = round(sum(dddperpatient)/n(),2))
averagedddperpatientbyprovider_byarea
write.table(averagedddperpatientbyprovider_byarea, file = "averagedddperpatientbyprovider_byarea.txt")

plotdddperpatientbyprovider_byarea <- ggplot(averagedddperpatientbyprovider_byarea, aes(x=providerarea, y=meanddd, label = meanddd)) +
  geom_col(position = 'dodge') +
  geom_text(data=subset(averagedddperpatientbyprovider_byarea, n != 0), position = position_dodge(width = 0.9),    # move to center of bars
            vjust = 0.3,    # nudge above top of bar
            size = 4) + 
  coord_flip() +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  facet_wrap(. ~ providertype, ncol = 1)  
plotdddperpatientbyprovider_byarea
ggsave(plotdddperpatientbyprovider_byarea, filename = "plotdddperpatientbyprovider_byarea.png",  width = 3.5, height = 8, bg = "white")

# DDD per provider type per district
dddperpatient_bydistrict <- db %>%
  group_by(KEY, providertype_fig, district) %>%
  summarise(dddperpatient=sum(dddused))
dddperpatient_bydistrict$dddperpatient[is.na(dddperpatient_bydistrict$dddperpatient)] <- 0

averagedddperpatientbyprovider_bydistrict <- dddperpatient_bydistrict %>%
  group_by(providertype_fig, district) %>%
  summarise(n =n(), meanddd = round(sum(dddperpatient)/n(),2))
averagedddperpatientbyprovider_bydistrict

plotdddperpatientbyprovider_bydistrict <- ggplot(averagedddperpatientbyprovider_bydistrict, aes(x=district, y=meanddd, label = meanddd)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  facet_grid(providertype_fig ~ ., labeller = label_wrap_gen(width=10)) +  
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  ggtitle("  Mean DDD of antibiotic per visit") + theme(plot.title = element_text(size=10))

plotdddperpatientbyprovider_bydistrict
ggsave(plotdddperpatientbyprovider_bydistrict, filename = "plotdddperpatientbyprovider_bydistrict.png",  width = 3.6, height = 4.5, bg = "white")

# DDD per capita
# import frequency of HCU
nepisodes_by_provider_and_as <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/CABU data analysis/nepisodes_by_provider_and_as.txt", sep="")
# merge frequency of HCU per provider type (nepisodes_by_provider_and_as) and mean DDDs per provider visit (averagedddperpatientbyprovider_byarea)
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="kavuaya"] <- "Kisantu, Kavuaya"
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="nkandu"] <- "Kisantu, Nkandu"
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="viaza"] <- "Kimpese, Viaza" # to check, NOT the same area
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="malanga"] <- "Kimpese, Malanga"
nepisodes_by_provider_and_as$providertype_healtharea <- paste(nepisodes_by_provider_and_as$providertype, nepisodes_by_provider_and_as$healtharea, sep = "-")
nepisodes_by_provider_and_as
averagedddperpatientbyprovider_byarea$providertype_healtharea <- paste(averagedddperpatientbyprovider_byarea$providertype, averagedddperpatientbyprovider_byarea$providerarea, sep = "-")
DDDpercapita <- merge(nepisodes_by_provider_and_as,averagedddperpatientbyprovider_byarea, by = "providertype_healtharea", all.x = T)
DDDpercapita <- subset(DDDpercapita, DDDpercapita$providertype.x != "selftreatment")
DDDpercapita <- subset(DDDpercapita, select = c("providertype.x", "healtharea", "hcufreq", "meanddd"))
# fill hospital ABU for Kimpese (not recorded in hospital) with that of Kisantu
DDDpercapita$meanddd[DDDpercapita$providertype.x=="hospital"] <- DDDpercapita$meanddd[DDDpercapita$providertype.x=="hospital" & DDDpercapita$healtharea=="Kisantu, Nkandu"]
# replace mean DDD if no records of antibiotic use in that health area
DDDpercapita$meanddd[DDDpercapita$providertype.x=="privateclinic"& DDDpercapita$healtharea=="Kimpese, Viaza"] <- DDDpercapita$meanddd[DDDpercapita$providertype.x=="privateclinic" & DDDpercapita$healtharea=="Kimpese, Malanga"] 
DDDpercapita$meanddd[DDDpercapita$providertype.x=="traditionalhealer"& DDDpercapita$healtharea=="Kimpese, Malanga"] <- DDDpercapita$meanddd[DDDpercapita$providertype.x=="traditionalhealer" & DDDpercapita$healtharea=="Kimpese, Viaza"] 
# calculate DDD per 1000 pop
DDDpercapita$dddper1000 <- DDDpercapita$hcufreq*DDDpercapita$meanddd*1000/91 # from estimate for 3 months, to estimate per day
DDDpercapita
write.table(DDDpercapita, file = "DDDpercapita.txt")

# summary by health district
DDDpercapita_area <- DDDpercapita %>%
  group_by(healtharea) %>%
  summarise(hcufreq = sum(hcufreq), dddper1000 = sum(dddper1000))
DDDpercapita_area
DDDpercapita_area$population[DDDpercapita_area$healtharea=="Kimpese, Malanga"] <- 5431
DDDpercapita_area$population[DDDpercapita_area$healtharea=="Kimpese, Viaza"] <- 3788
DDDpercapita_area$population[DDDpercapita_area$healtharea=="Kisantu, Kavuaya"] <- 7617
DDDpercapita_area$population[DDDpercapita_area$healtharea=="Kisantu, Nkandu"] <- 26876
DDDpercapita_area$dddcumul <- DDDpercapita_area$dddper1000*DDDpercapita_area$population
DDDpercapita_area$district[DDDpercapita_area$healtharea=="Kimpese, Malanga"] <- "Kimpese"
DDDpercapita_area$district[DDDpercapita_area$healtharea=="Kimpese, Viaza"] <- "Kimpese"
DDDpercapita_area$district[DDDpercapita_area$healtharea=="Kisantu, Kavuaya"] <- "Kisantu"
DDDpercapita_area$district[DDDpercapita_area$healtharea=="Kisantu, Nkandu"] <- "Kisantu"
DDDpercapita_district <- DDDpercapita_area %>%
  group_by(district) %>%
  summarise(pop = sum(population), dddper1000 = sum(dddcumul)/sum(population))
DDDpercapita_district

# bar chart
# gen var providertype for figures
DDDpercapita$providertype_fig <- DDDpercapita$providertype.x
DDDpercapita$providertype_fig[DDDpercapita$providertype.x=="healthcentre"] <- "health centre"
DDDpercapita$providertype_fig[DDDpercapita$providertype.x=="privateclinic"] <- "private clinic"
DDDpercapita$providertype_fig[DDDpercapita$providertype.x=="privatepharmacy"] <- "medicine store"
DDDpercapita$providertype_fig[DDDpercapita$providertype.x=="religiousleader"] <- "religious leader"
DDDpercapita$providertype_fig[DDDpercapita$providertype.x=="traditionalhealer"] <- "traditional healer"

DDDpercapita_plot <- ggplot(data = DDDpercapita, aes(x = healtharea, y = dddper1000, fill=factor(providertype_fig, level = c("medicine store", "private clinic", "traditional healer", "religious leader", "health centre", "hospital")))) + geom_col() + coord_flip() +
  labs(y="Defined daily doses of antibiotics per 1000 inhabitants per day",x="Health area") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() 
DDDpercapita_plot
ggsave(DDDpercapita_plot, filename = "DDDpercapita_plot.png",  width = 8, height = 2, bg = "white")

# summary by provider
DDDpercapita_provider <- DDDpercapita %>%
  group_by(providertype.x) %>%
  summarise(dddper1000 = sum(dddper1000))
DDDpercapita_provider

# share public vs private sector
(1056+41)/sum(DDDpercapita_provider$dddper1000)
# share hospital use
41/sum(DDDpercapita_provider$dddper1000)

# for each area
DDDpercapita_provider_nkandu <- DDDpercapita %>%
  filter(healtharea == "Kisantu, Nkandu") %>%
  group_by(providertype.x) %>%
  summarise(dddper1000 = sum(dddper1000))
DDDpercapita_provider_nkandu
(163+28)/sum(DDDpercapita_provider_nkandu$dddper1000)
DDDpercapita_provider_kimpese <- DDDpercapita %>%
  filter(healtharea == "Kimpese, Malanga" | healtharea =="Kimpese, Viaza") %>%
  group_by(providertype.x) %>%
  summarise(dddper1000 = sum(dddper1000))
DDDpercapita_provider_kimpese
(199+13)/sum(DDDpercapita_provider_kimpese$dddper1000)

# Watch group DDD per capita, by provider type per area
# gen ddd only for Watch
db$watchdddused <- 0
db$watchdddused[!is.na(db$Category) & db$Category=="watch"] <- db$dddused[!is.na(db$Category) & db$Category=="watch"]
watchdddperpatient_byarea <- db %>%
  group_by(KEY, providertype, providerarea) %>%
  summarise(dddperpatient=sum(watchdddused))
watchdddperpatient_byarea$dddperpatient[is.na(watchdddperpatient_byarea$dddperpatient)] <- 0

averagewatchdddperpatientbyprovider_byarea <- watchdddperpatient_byarea %>%
  group_by(providertype, providerarea) %>%
  summarise(n =n(), meanddd = round(sum(dddperpatient)/n(),2))
averagewatchdddperpatientbyprovider_byarea

# import frequency of HCU
nepisodes_by_provider_and_as <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/CABU data analysis/nepisodes_by_provider_and_as.txt", sep="")
# merge frequency of HCU per provider type (nepisodes_by_provider_and_as) and mean DDDs per provider visit (averagedddperpatientbyprovider_byarea)
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="kavuaya"] <- "Kisantu, Kavuaya"
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="nkandu"] <- "Kisantu, Nkandu"
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="viaza"] <- "Kimpese, Viaza" # to check, NOT the same area
nepisodes_by_provider_and_as$healtharea[nepisodes_by_provider_and_as$healtharea=="malanga"] <- "Kimpese, Malanga"
nepisodes_by_provider_and_as$providertype_healtharea <- paste(nepisodes_by_provider_and_as$providertype, nepisodes_by_provider_and_as$healtharea, sep = "-")
nepisodes_by_provider_and_as
# merge frequency of HCU per provider type (nepisodes_by_provider_and_as) and mean watch DDDs per provider visit (averagewatchdddperpatientbyprovider_byarea)
averagewatchdddperpatientbyprovider_byarea$providertype_healtharea <- paste(averagewatchdddperpatientbyprovider_byarea$providertype, averagewatchdddperpatientbyprovider_byarea$providerarea, sep = "-")
watchDDDpercapita <- merge(nepisodes_by_provider_and_as,averagewatchdddperpatientbyprovider_byarea, by = "providertype_healtharea", all.x = T)
watchDDDpercapita <- subset(watchDDDpercapita, watchDDDpercapita$providertype.x != "selftreatment")
watchDDDpercapita <- subset(watchDDDpercapita, select = c("providertype.x", "healtharea", "hcufreq", "meanddd"))
# fill hospital ABU for Kimpese (not recorded in hospital) with that of Kisantu
watchDDDpercapita$meanddd[watchDDDpercapita$providertype.x=="hospital"] <- watchDDDpercapita$meanddd[watchDDDpercapita$providertype.x=="hospital" & watchDDDpercapita$healtharea=="Kisantu, Nkandu"]
# replace mean DDD if no records of antibiotic use in that health area
watchDDDpercapita$meanddd[watchDDDpercapita$providertype.x=="privateclinic"& watchDDDpercapita$healtharea=="Kimpese, Viaza"] <- watchDDDpercapita$meanddd[watchDDDpercapita$providertype.x=="privateclinic" & watchDDDpercapita$healtharea=="Kimpese, Malanga"] 
watchDDDpercapita$meanddd[watchDDDpercapita$providertype.x=="traditionalhealer"& watchDDDpercapita$healtharea=="Kimpese, Malanga"] <- watchDDDpercapita$meanddd[watchDDDpercapita$providertype.x=="traditionalhealer" & watchDDDpercapita$healtharea=="Kimpese, Viaza"] 
# calculate DDD per 1000 pop per day (corrected from 3mo=91 days to 1 day)
watchDDDpercapita$dddper1000 <- watchDDDpercapita$hcufreq*watchDDDpercapita$meanddd*1000/91
watchDDDpercapita

# summary by health district
watchDDDpercapita_area <- watchDDDpercapita %>%
  group_by(healtharea) %>%
  summarise(dddper1000 = sum(dddper1000)) 
watchDDDpercapita_area

# bar chart
watchDDDpercapita$providertype_fig <- watchDDDpercapita$providertype.x
watchDDDpercapita$providertype_fig[watchDDDpercapita$providertype.x=="healthcentre"] <- "health centre"
watchDDDpercapita$providertype_fig[watchDDDpercapita$providertype.x=="privateclinic"] <- "private clinic"
watchDDDpercapita$providertype_fig[watchDDDpercapita$providertype.x=="privatepharmacy"] <- "medicine store"
watchDDDpercapita$providertype_fig[watchDDDpercapita$providertype.x=="religiousleader"] <- "religious leader"
watchDDDpercapita$providertype_fig[watchDDDpercapita$providertype.x=="traditionalhealer"] <- "traditional healer"

watchDDDpercapita_plot <- ggplot(data = watchDDDpercapita, aes(x = healtharea, y = dddper1000, fill=factor(providertype_fig, level = c("medicine store", "private clinic", "traditional healer", "religious leader", "health centre", "hospital")))) + geom_col() + coord_flip() +
  labs(y="Defined daily doses of Watch group antibiotics per 1000 inhabitants per day",x="Health area") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() + ylim(0,15)
watchDDDpercapita_plot
ggsave(watchDDDpercapita_plot, filename = "watchDDDpercapita_plot.png",  width = 8, height = 2, bg = "white")

# fraction of Watch of all antibiotic use
names(watchDDDpercapita_area) <- c("healtharea", "watchdddper1000")
DDDpercapita_area_pctwatch <- merge(DDDpercapita_area, watchDDDpercapita_area, by = "healtharea")
DDDpercapita_area_pctwatch$pct <- round(DDDpercapita_area_pctwatch$watchdddper1000/DDDpercapita_area_pctwatch$dddper1000*100,1)
DDDpercapita_area_pctwatch

# distribution of antibiotic groups by provider type
ABgroupdistr_provider <- db_abonly %>%
  group_by(providertype, Class) %>%
  summarise(n=n()) # number of episodes per antibiotic class
nperprovider <- db_abonly %>%
  group_by(providertype) %>%
  summarise(nprovider=n()) # number of episodes per class
ABgroupdistr_provider <- merge(ABgroupdistr_provider, nperprovider, by = "providertype", all.x = T)
ABgroupdistr_provider$pct <- round(ABgroupdistr_provider$n/ABgroupdistr_provider$nprovider*100,1)
ABgroupdistr_provider

# antibiotic classes used
ABgroupdistr_overall <- merge(ABgroupdistr_provider, nepisodes_by_provider, by = "providertype", all.x = T)
ABgroupdistr_overall <- subset(ABgroupdistr_overall, providertype!="religiousleader")
ABgroupdistr_overall$pct <- ABgroupdistr_overall$n/ABgroupdistr_overall$nprovider
ABgroupdistr_overall$weighted <- ABgroupdistr_overall$pct*ABgroupdistr_overall$hcufreq
head(ABgroupdistr_overall)
sum(ABgroupdistr_overall$weighted$weighted)
# summarize by class
ABgroupdistr_overallclass <- ABgroupdistr_overall %>%
  group_by(Class) %>%
  summarise(n = sum(weighted))
ABgroupdistr_overallclass$nprovider <- NA
ABgroupdistr_overallclass$providertype <- "OVERALL"
ABgroupdistr_overallclass
# total episodes of AB use
sum(ABgroupdistr_overallclass$n)
# percent of each class
ABgroupdistr_overallclass$total <- sum(ABgroupdistr_overallclass$n)
ABgroupdistr_overallclass$pct <- round(ABgroupdistr_overallclass$n/ABgroupdistr_overallclass$total*100,1)
ABgroupdistr_overallclass
# combine with the plot per provider
ABgroupdistr <- rbind(ABgroupdistr_provider,ABgroupdistr_overallclass)
plotabclass_byprovider_overall <- ggplot(ABgroupdistr, aes(x=factor(providertype, levels = c("religiousleader", "traditionalhealer", "privatepharmacy", "privateclinic", "hospital", "healthcentre", "OVERALL")), y=n, fill=factor(Class, levels = c("penicillins",  "beta-lactam combination agent","phenicols","tetracyclines","aminoglycosides","lincomycins","nitroheterocyclics","macrolides","fluoroquinolones", "cephems")))) +
  geom_bar(position = "fill",stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() +
  scale_fill_manual(values=c("#99CC99", "#99FFCC", "#336600", "#0099cc", "#FF9933", "#FFFF99", "#FFCCCC", "#99CCCC", "#660066", "#990033"))
plotabclass_byprovider_overall
ggsave(plotabclass_byprovider_overall, filename = "plotabclass_byprovider_overall.png",  width = 5, height = 3.5, bg = "white")


#### 3. CHOICE AND TIMING OF PROVIDERTYPE ####
# duration symptoms until visit/consultation
table(patient$duration, useNA = "always")
HCseekingdelay <- patient %>%
  filter(duration>-1 & duration <50 & !is.na(duration)) %>%
  group_by(providertype) %>%
  summarise(n = n(), mediandelay = median(duration), qu25 = quantile(duration, 0.25), qu75 = quantile(duration, 0.75))
HCseekingdelay

# facetted histograms of disease until consultation, in function of type of provider
# to patient$aware_f = factor(patient$AWaRe, levels=c("Access","Watch","Reserve","Unclassified","not_specified"), labels=c("Access","Watch","Reserve","Unclassified","Not specified")) 
patien_excludingchronicillness <- subset(patient, duration<50 & duration>(-1))
HCseekingdelay_plot <- ggplot(patien_excludingchronicillness, aes(x=as.numeric(duration))) + 
  geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  facet_grid(providertype ~ ., scales="free_y") +
  theme_light() +
  labs(x="Days of illness until visit", y="Number of cases") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size=8)) +
  geom_vline(data=filter(patien_excludingchronicillness, providertype=="healthcentre"), aes(xintercept=median(as.numeric(duration), na.rm=T)), linetype="dashed", 
             color = "#e7b800", size=1.2) + 
  geom_vline(data=filter(patien_excludingchronicillness, providertype=="hospital"), aes(xintercept=median(as.numeric(duration), na.rm=T)), linetype="dashed", 
             color = "#e7b800", size=1.2) +
  geom_vline(data=filter(patien_excludingchronicillness, providertype=="privateclinic"), aes(xintercept=median(as.numeric(duration), na.rm=T)), linetype="dashed", 
           color = "#e7b800", size=1.2) +
  geom_vline(data=filter(patien_excludingchronicillness, providertype=="privatepharmacy"), aes(xintercept=median(as.numeric(duration), na.rm=T)), linetype="dashed", 
             color = "#e7b800", size=1.2) +
  geom_vline(data=filter(patien_excludingchronicillness, providertype=="traditionalhealer"), aes(xintercept=median(as.numeric(duration), na.rm=T)), linetype="dashed", 
             color = "#e7b800", size=1.2)  
HCseekingdelay_plot
ggsave(HCseekingdelay_plot, filename = "HCseekingdelay_plot.png",  width = 5, height = 7, bg = "white")

# age by providertype
agebyprovidertype <- patient %>%
  filter(!is.na(ageyears)) %>%
  group_by(providertype) %>%
  summarise(n= n(), medianage = median(ageyears))
agebyprovidertype

#age group by providertype
agegrtab <- table(patient$providertype, patient$agegroups)
prop.table(agegrtab, 1)

# which provider for which symptom
table(patient$symptoms)
# prior consultation: count, %, by providerype
table(patient$firstconsultation) # I can't find this variable anymore
round(prop.table(table(patient$firstconsultation))*100,2)
table(patient$providertype, patient$firstconsultation)
round(prop.table(table(patient$providertype, patient$firstconsultation))*100,2)

#### 4. ANTIMALARIAL USE - only recorded in Kisantu ####

# malaria diagnosis
db$diag_spec_other <- tolower(db$diag_spec_other)
db$malariadiag <- 0
db$malariadiag[db$diag_spec=="malaria"] <- 1
db$malariadiag[grepl("palu", db$diag_spec_other)==TRUE] <- 1
db$malariadiag[grepl("malaria", db$diag_spec_other)==TRUE] <- 1

# malaria test done - only recorded in Kisantu
db$malariatest[!is.na(db$diag_test)] <- "no"
db$malariatest[grepl("RDT_malaria", db$diag_test)==TRUE] <- "yes"
db$malariatest[grepl("microscopy_malaria", db$diag_test)==TRUE] <- "yes"
# keep one line per visit
malariatest <- db %>%
  filter(district=="Kisantu") %>%
  group_by(KEY, providertype, malariatest) %>%
  summarise(antibioticuse = sum(antibioticuse))
table(malariatest$providertype, malariatest$malariatest)
round(prop.table(table(malariatest$providertype, malariatest$malariatest),1)*100,1)

# result test
db$resultmalariatest[db$malariatest=="yes"] <- "negative"
db$resultmalariatest[db$malariatest=="yes"&db$malariadiag==1] <- "positive"
# keep one line per visit
antibioticusewhenmalariatest <- db %>%
  filter(district=="Kisantu" & !is.na(malariatest)) %>%
  group_by(KEY, providertype, resultmalariatest) %>%
  summarise(antibioticuse = sum(antibioticuse))
antibioticusewhenmalariatest$antibioticuse_bin <- "no"
antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$antibioticuse>0] <- "yes"

# 2by2 of ab use and malaria test
# all
table(antibioticusewhenmalariatest$resultmalariatest, antibioticusewhenmalariatest$antibioticuse_bin)
round(prop.table(table(antibioticusewhenmalariatest$resultmalariatest, antibioticusewhenmalariatest$antibioticuse_bin),1)*100,1)
# in healthcentre
table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="healthcentre"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="healthcentre"])
round(prop.table(table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="healthcentre"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="healthcentre"]),1)*100,1)
chisq.test(table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="healthcentre"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="healthcentre"]))

# in medicine store
table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="privatepharmacy"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="privatepharmacy"])
round(prop.table(table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="privatepharmacy"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="privatepharmacy"]),1)*100,1)

# in private clinic
table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="privateclinic"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="privateclinic"])
round(prop.table(table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="privateclinic"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="privateclinic"]),1)*100,1)
chisq.test(table(antibioticusewhenmalariatest$resultmalariatest[antibioticusewhenmalariatest$providertype=="privateclinic"], antibioticusewhenmalariatest$antibioticuse_bin[antibioticusewhenmalariatest$providertype=="privateclinic"]))

# antimalarial use
table(db$providertype, db$antimalarial, useNA = "always")
round(prop.table(table(db$providertype, db$antimalarial, useNA = "always"), 1)*100,1)
# var for use of antimalarials
db$antimalarialuse <- 0
db$antimalarialuse[db$antimalarial!="no"] <- 1
# frequency of antimalarial use
antimalarialfreq <- db %>%
  group_by(KEY, providertype) %>%
  summarise(antibioticuse = sum(antibioticuse), antimalarialuse = sum(antimalarialuse), malariatest = sum(malariatest), malariadiag = sum(malariadiag))
# replace values 
antimalarialfreq$malariadiag_bin <- "no"
antimalarialfreq$malariadiag_bin[antimalarialfreq$malariadiag>0] <- "yes"
antimalarialfreq$malariatest_bin <- "no"
antimalarialfreq$malariatest_bin[antimalarialfreq$malariatest>0] <- "yes"
# freq malaria test
table(antimalarialfreq$malariatest_bin)
prop.table(table(antimalarialfreq$malariatest_bin))
# by providertype
table(antimalarialfreq$providertype, antimalarialfreq$malariatest_bin)
round(prop.table(table(antimalarialfreq$providertype, antimalarialfreq$malariatest_bin),1)*100,1)

# freq malaria diagnosis
table(antimalarialfreq$malariadiag_bin)
prop.table(table(antimalarialfreq$malariadiag_bin))
# by providertype
table(antimalarialfreq$providertype, antimalarialfreq$malariadiag_bin)
round(prop.table(table(antimalarialfreq$providertype, antimalarialfreq$malariadiag_bin),1)*100,1)

# replace values with at least one ab or antimalarial
antimalarialfreq$antibioticuse_bin <- "no AB"
antimalarialfreq$antibioticuse_bin[antimalarialfreq$antibioticuse>0] <- "at least one AB"
antimalarialfreq$antimalarialuse_bin <- "no antimalarial"
antimalarialfreq$antimalarialuse_bin[antimalarialfreq$antimalarialuse>0] <- "at least one antimalarial"

# table frequency of antimalarial & antibiotic use by providertype
table(antimalarialfreq$providertype, antimalarialfreq$antimalarialuse_bin)
round(prop.table(table(antimalarialfreq$providertype, antimalarialfreq$antimalarialuse_bin),1)*100,1)

table(antimalarialfreq$providertype, antimalarialfreq$antibioticuse_bin)
round(prop.table(table(antimalarialfreq$providertype, antimalarialfreq$antibioticuse_bin),1)*100,1)

# frequency of concomitant AB/antimalarial use
antimalarialfreq_ABused <- subset(antimalarialfreq, antimalarialfreq$antibioticuse>0)
antimalarialfreq_ABused$concomitant <- "no"
antimalarialfreq_ABused$concomitant[antimalarialfreq_ABused$antimalarialuse>0] <- "yes"            
table(antimalarialfreq_ABused$providertype, antimalarialfreq_ABused$concomitant)
round(prop.table(table(antimalarialfreq_ABused$providertype, antimalarialfreq_ABused$concomitant),1)*100,1)