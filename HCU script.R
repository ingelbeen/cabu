###################################
# CABU-DRC                        #
# HEALTHCARE UTILISATION          #
###################################

#### 0. CLEAN & LOAD DATA & PACKAGES - run this before analyses ####
#### install and load packages ####
# install.packages("pacman")
pacman::p_load(tidyverse, here,readxl,naniar,lubridate,haven,dplyr,ggplot2,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr, knitr, epitools, naniar)

#### import & clean HCU databases ####
#### 0.1 DRC Kisantu HCU ####
# db with member IDs, age and sex
HHmembersKis <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kisantu/Phase 2 Kisantu HCUS/d_HPA_backup_Household_member_2019.10.28.csv")
colnames(HHmembersKis)[2] <- "hh_id"
HHmembersKis$hh_member_id <-paste0(HHmembersKis$hh_id, "_", as.character(HHmembersKis$member_id)) # combine HH ID and member ID in anew var
HHmembersKis$sex[HHmembersKis$gender=="F?minin"] <- "female"
HHmembersKis$sex[HHmembersKis$gender=="Female"] <- "female"
HHmembersKis$sex[HHmembersKis$gender=="Male"] <- "male"
HHmembersKis$sex[HHmembersKis$gender=="Masculin"] <- "male"
# keep only vars that we'll continue to use
HHmembersKis <- subset(HHmembersKis, select = c("hh_id","hh_member_id","sex","age"))
# create var age groups
HHmembersKis$agegroups[HHmembersKis$age<5] <- "0-4"
HHmembersKis$agegroups[HHmembersKis$age>4&HHmembersKis$age<18] <- "5-17"
HHmembersKis$agegroups[HHmembersKis$age>17] <- "18+"

# db with HH AS and number of HH members per HH
HHstructureKis <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kisantu/Phase 2 Kisantu HCUS/d_HPA_backup_Form3A_2019.10.28.csv")
HHstructureKis$as[HHstructureKis$X3ASUB<41] <- "kavuaya"
HHstructureKis$as[HHstructureKis$X3ASUB>40] <- "nkandu"
HHstructureKis$n_hhmembers <- HHstructureKis$X3A07
HHstructureKis <- subset(HHstructureKis, select = c("X3ASL","as","n_hhmembers"))

# db with actual healthcare utilisation over past 3 months
HCUSKis <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kisantu/Phase 2 Kisantu HCUS/HCUS_Kisantu_mod.xlsx", 
                   sheet = "dupl removed")
HCUSKis <- HCUSKis[-c(821),]  # remove redundant row
HCUSKis$F5HN[HCUSKis$F5HN=="OO14"] <-  "14" # some typos
HCUSKis$F5HN <-  as.numeric(HCUSKis$F5HN) 
HCUSKis$F5SUB <-  as.numeric(HCUSKis$F5SUB) # recode F5SUB to 3 character string var
HCUSKis$member_id <- substr(HCUSKis$F501A, 9,9)# ADD 10th character!!
HCUSKis$hh_member_id <-paste0(HCUSKis$F5SL, "_", HCUSKis$member_id) # combine HH ID and member ID in a new var
# when F2SL is missing, build it from F5SITE-F5SUB-0-F5HN
HCUSKis$F5SUBch <-  as.character(HCUSKis$F5SUB) # recode F5SUB to 4 character string var
HCUSKis$F5SUBch[HCUSKis$F5SUB<100] <-  paste0("0",as.character(HCUSKis$F5SUB[HCUSKis$F5SUB<100])) # recode F5SUB to 4 character string var
HCUSKis$F5SUBch[HCUSKis$F5SUB<10] <-  paste0("00",as.character(HCUSKis$F5SUB[HCUSKis$F5SUB<10])) # recode F5SUB to 4 character string var
HCUSKis$F5HNch <-  paste0("00",as.character(HCUSKis$F5HN)) # recode F5SUB to 4 character string var
HCUSKis$F5HNch[HCUSKis$F5HN<10] <-  paste0("000",as.character(HCUSKis$F5HN[HCUSKis$F5HN<10])) # recode F5SUB to 4 character string var
HCUSKis$hh_member_id[is.na(HCUSKis$F5SL)] <-paste0(HCUSKis$F5SITE[is.na(HCUSKis$F5SL)], HCUSKis$F5SUBch[is.na(HCUSKis$F5SL)], HCUSKis$F5HNch[is.na(HCUSKis$F5SL)] , "_", HCUSKis$member_id[is.na(HCUSKis$F5SL)]) # combine HH ID and member ID in a new var

# add string var for symptoms
HCUSKis$symptom[HCUSKis$F501B=="T"] <- "toux"
HCUSKis$symptom[HCUSKis$F501B=="A"] <- "fever"
HCUSKis$symptom[HCUSKis$F501B=="B"] <- "fever"
HCUSKis$symptom[HCUSKis$F501B=="C"] <- "fever"
HCUSKis$symptom[HCUSKis$F501B=="D"] <- "chills"
HCUSKis$symptom[HCUSKis$F501B=="E"] <- "convulsions"
HCUSKis$symptom[HCUSKis$F501B=="F"] <- "weightloss"
HCUSKis$symptom[HCUSKis$F501B=="G"] <- "dehydration"
HCUSKis$symptom[HCUSKis$F501B=="H"] <- "fatigue"
HCUSKis$symptom[HCUSKis$F501B=="I"] <- "myalgia/arthralgia"
HCUSKis$symptom[HCUSKis$F501B=="J"] <- "headache"
HCUSKis$symptom[HCUSKis$F501B=="K"] <- "vertigo/confusion/lossofconsciousness"
HCUSKis$symptom[HCUSKis$F501B=="L"] <- "blood pressure"
HCUSKis$symptom[HCUSKis$F501B=="M"] <- "cardiac problems"
HCUSKis$symptom[HCUSKis$F501B=="N"] <- "nausea/vomiting"
HCUSKis$symptom[HCUSKis$F501B=="O"] <- "diarrhoea"
HCUSKis$symptom[HCUSKis$F501B=="P"] <- "abdominal pain"
HCUSKis$symptom[HCUSKis$F501B=="Q"] <- "hemorrhage"
HCUSKis$symptom[HCUSKis$F501B=="R"] <- "hemorrhage"
HCUSKis$symptom[HCUSKis$F501B=="S"] <- "sneezing/runny nose"
HCUSKis$symptom[HCUSKis$F501B=="U"] <- "dyspnoea/breathlessness"
HCUSKis$symptom[HCUSKis$F501B=="V"] <- "dyspnoea/breathlessness"
HCUSKis$symptom[HCUSKis$F501B=="W"] <- "rash"
HCUSKis$symptom[HCUSKis$F501B=="X"] <- "oedema"
HCUSKis$symptom[HCUSKis$F501B=="Y"] <- "jaundice"
HCUSKis$provider[HCUSKis$F501C=="1"&HCUSKis$F501D=="2"] <- "healthcentre" # almost all F501D==2 were from Kavuaya -> Kavuaya HC?
HCUSKis$provider[HCUSKis$F501C=="1"&HCUSKis$F501D=="3"] <- "healthcentre" # almost all F501D==3 were from Nkandu -> Nkandu HC?
HCUSKis$provider[HCUSKis$F501C=="1"&HCUSKis$F501D=="1"] <- "hospital" #  all 3 F501D==1 were from Nkandu -> HSLK?
HCUSKis$provider[HCUSKis$F501C=="2"] <- "privateclinic"
HCUSKis$provider[HCUSKis$F501C=="3"] <- "privateclinic"
HCUSKis$provider[HCUSKis$F501C=="4"] <- "privatepharmacy"
HCUSKis$provider[HCUSKis$F501C=="5"] <- "traditionalhealer"
HCUSKis$provider[HCUSKis$F501C=="6"] <- "selftreatment"
HCUSKis$provider[HCUSKis$F501C=="7"] <- "None"
HCUSKis <- subset(HCUSKis, select = c("hh_member_id","symptom", "provider", "F501D"))
HCUSKis <- subset(HCUSKis, HCUSKis$provider == "healthcentre"| HCUSKis$provider == "hospital"| HCUSKis$provider == "privateclinic"| HCUSKis$provider == "privatepharmacy"| HCUSKis$provider == "traditionalhealer" | HCUSKis$provider == "selftreatment")

# merge dbs
popdbKis <- merge(HHmembersKis, HHstructureKis, by.x = "hh_id", by.y = "X3ASL") # the population in which the survey was done
hcudbKis <- merge(popdbKis, HCUSKis, by = "hh_member_id", all = T) # the number of illness episodes in that population


#### 0.2 DRC Kimpese HCU ####
# HH database
# HHkim <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kimpese/Phase II Kimpese/Cabu_phase2_malanga_kilueka.xlsx")
# new database of 13/09/2021
HHkim <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kimpese/Phase II Kimpese/CABU RECHERCHE DES SOINS AS VIAZA_MALANGA.xlsx", 
                    sheet = "malanga_viaza")
# find and remove duplicates
dupli <- HHkim[duplicated(HHkim),] #  none
# replace NULL with NA
na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available", "NULL")
HHkim <- HHkim %>%
  replace_with_na_all(condition = ~.x %in% na_strings)
# n HH members to numeric
HHkim$N_HHMEMBERS <- as.numeric(HHkim$N_HHMEMBERS)
# repeat section on Hc visits (no duplicates found)
HCUkim <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kimpese/Phase II Kimpese/CABU_SOINS_SANTES_HEALTHCAREVISIT.xlsx", na = "NULL")
colnames(HCUkim)[1] <- "HCUid"
HCUkim <- subset(HCUkim, !is.na(HCUkim$PROVIDERTYPE)) # remove empty rows
HCUkim_hospitaldept <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kimpese/Phase II Kimpese/CABU_SOINS_SANTES_HOSPITALDEPT.xlsx")
colnames(HCUkim_hospitaldept)[4] <- "HCUid"
HCUkim_hospitaldept <- subset(HCUkim_hospitaldept, select = c("HCUid", "VALUE"))
HCUkim_hospitaldept %>% group_by(HCUid) %>%  summarize(number=n()) #check if there are any which were in multiple hospital depts - there is 1
table(HCUkim_hospitaldept$VALUE[HCUkim_hospitaldept$HCUid=="uuid:5fee0538-3f0a-46dc-a2cb-6488f544a9ee"]) # we will retain only the hospital dept where the patient eventually ended up
HCUkim_hospitaldept <- HCUkim_hospitaldept[!(HCUkim_hospitaldept$VALUE=="opd"&HCUkim_hospitaldept$HCUid=="uuid:5fee0538-3f0a-46dc-a2cb-6488f544a9ee"),]  # remove this row
colnames(HCUkim_hospitaldept)[2] <- "hospitaldept"
HCUkim_symptoms <- read_excel("C:/Users/bingelbeen/OneDrive - ITG/Medicines use/community antibiotic use/DRC/databases/Kimpese/Phase II Kimpese/CABU_SOINS_SANTES_SYMPTOMS.xlsx")
colnames(HCUkim_symptoms)[4] <- "HCUid"
HCUkim_symptoms <- subset(HCUkim_symptoms, select = c("HCUid", "VALUE"))
# set symptom db to wide
HCUkim_symptoms_wide <- dcast(HCUkim_symptoms, HCUid ~ VALUE) 
# add symptoms to HCU db
HCUkim <- merge(HCUkim, HCUkim_symptoms_wide, by = "HCUid", all.x = T)
# add hospitaldept_spec to HCU db
HCUkim <- merge(HCUkim, HCUkim_hospitaldept, by = "HCUid", all.x = T)
# clean age
HCUkim$HHMEMBER_AGEYEARS[HCUkim$HHMEMBER_AGEYEARS>105&!is.na(HCUkim$HHMEMBER_AGEYEARS)] <- 2020 - HCUkim$HHMEMBER_AGEYEARS[HCUkim$HHMEMBER_AGEYEARS>105&!is.na(HCUkim$HHMEMBER_AGEYEARS)]
# create var age groups
HCUkim$agegroups[HCUkim$HHMEMBER_AGEYEARS<5] <- "0-4"
HCUkim$agegroups[HCUkim$HHMEMBER_AGEYEARS>4&HCUkim$HHMEMBER_AGEYEARS<18] <- "5-17"
HCUkim$agegroups[HCUkim$HHMEMBER_AGEYEARS>17] <- "18+"
# harmonize var providertype
table(HCUkim$PROVIDERTYPE)
HCUkim$providertype <- HCUkim$PROVIDERTYPE
HCUkim$providertype[HCUkim$PROVIDERTYPE=="informalvendor"] <- "privatepharmacy"
HCUkim$providertype[HCUkim$PROVIDERTYPE=="publichealthcentre"] <- "healthcentre"
HCUkim$providertype[HCUkim$PROVIDERTYPE=="otherspiritualleader"] <- "religiousleader"
HCUkim$providertype[HCUkim$PROVIDERTYPE=="church"] <- "religiousleader"

# combine HCU database with HH data
HCUkim <- merge(HHkim, HCUkim, by.x = "_URI", by.y = "_PARENT_AURI", all.x = T)

#### 1. NUMBER OF SURVEYS RECORDED ####
# population surveyed in Kisantu
str(popdbKis$hh_id) # nr of HH
sum(popdbKis$n_hhmembers[!is.na(popdbKis$n_hhmembers)]) #nr of individuals
# population surveyed in Kimpese
npopKim <- sum(HHkim$N_HHMEMBERS[!is.na(HHkim$N_HHMEMBERS)])
npopKim
# episodes recorded in Kimpese
episodesKim <- subset(HCUkim, !is.na(HCUkim$PROVIDERTYPE))
count(episodesKim)

totalnumberofepisodes <- count(HCUkim[!is.na(HCUkim$PROVIDERTYPE)]) 
# by household
summaryHCUvisits <- HCUkim %>%
  group_by(`_URI`) %>%
  summarise(episodesperhh=n())
table(hcudbKis)

#### 2. CHARACTERISTICS OF POPULATION ####
# age respondents
# Kisantu
median(hcudbKis$age[!is.na(hcudbKis$age)])
mean(hcudbKis$age[!is.na(hcudbKis$age)])
#Kimpese
median(HCUkim$HHMEMBER_AGEYEARS[!is.na(HCUkim$HHMEMBER_AGEYEARS)])
mean(HCUkim$HHMEMBER_AGEYEARS[!is.na(HCUkim$HHMEMBER_AGEYEARS)])

# proportion <5 yo
table(hcudbKis$agegroups)
prop.table(table(hcudbKis$agegroups))*100
table(HCUkim$agegroups)
prop.table(table(HCUkim$agegroups))*100

# sex distribution
table(hcudbKis$sex)
round(prop.table(table(hcudbKis$sex))*100,2)

# household size 
nperhhKis <- popdbKis %>%
  group_by(hh_id) %>%
  summarise(nperhh=n()) %>%
  summarize(median(nperhh), quantile(nperhh, 0.25), quantile(nperhh, 0.75))
nperhhKis

nperhhKim <- HHkim %>%
  filter(!is.na(N_HHMEMBERS)) %>%
  summarize(median(N_HHMEMBERS), quantile(N_HHMEMBERS, 0.25), quantile(N_HHMEMBERS, 0.75))
nperhhKim

# number of respondents per AS
n_participants <- table(popdb$as)
n_participants
prop.table(n_participants)
# number of households per AS & median +IQR number of HH members per HH
members_interviewed_per_HH <- popdb %>% group_by(as, hh_id) %>% summarize(n=n())
n_HH_by_AS <- members_interviewed_per_HH %>% group_by(as) %>%  summarize(number=n(), median_n_hhmembers=median(n), quantile(n, 1/4), quantile(n, 3/4))
n_HH_by_AS
# age distribution
hist(popdb$age, xlab = "Age (years)")
table(popdb$agegroups)
round(prop.table(table(popdb$agegroups)),2)
# age distribution by AS
table(popdb$agegroups, popdb$as)
prop.table(table(popdb$agegroups, popdb$as),2)

# sex distribution by AS
table(popdb$sex, popdb$as)
round(prop.table(table(popdb$sex, popdb$as),2),2)

#### 2. HCU ####
#### 2.1 Kisantu HCU ####
# frequency overall
# total population
nperhhKis <- popdbKis %>%
  group_by(hh_id) %>%
  summarise(nperhh=n()) 
npopKis <- nperhhKis %>%
  summarise(total=sum(nperhh))
npopKis
# total episodes
nepisodesKis <- hcudbKis %>%
  filter(!is.na(provider)& !is.na(as)) %>%
  summarise(total = n())
nepisodesKis
# frequency
nepisodesKis/npopKis

# frequency by AS
nperhhKis <- popdbKis %>%
  group_by(hh_id, as) %>%
  summarise(nperhh=n()) 
npopKis_by_as <- nperhhKis %>%
  group_by(as) %>%
  summarise(total=sum(nperhh))
npopKis_by_as # population surveyed by AS
nepisodesKis_by_as <- hcudbKis %>%
  filter(!is.na(provider) & !is.na(as)) %>%
  group_by(as) %>%
  summarise(nepisodes = n())
nepisodesKis_by_as
nepisodesKis_by_as[,2]/npopKis_by_as[,2]

# frequency by AS and by providertype
nperhhKis <- popdbKis %>%
  group_by(hh_id, as) %>%
  summarise(nperhh=n()) 
npopKis_by_as <- nperhhKis %>%
  group_by(as) %>%
  summarise(total=sum(nperhh))
npopKis_by_as # population surveyed by AS
nepisodesKis_by_provider_and_as <- hcudbKis %>%
  filter(!is.na(provider) & !is.na(as)) %>%
  group_by(provider, as) %>%
  summarise(nepisodes = n())
nepisodesKis_by_provider_and_as
# add column with denominator (pop)
nepisodesKis_by_provider_and_as$pop[nepisodesKis_by_provider_and_as$as=="nkandu"] <- npopKis_by_as[2,2]
nepisodesKis_by_provider_and_as$pop[nepisodesKis_by_provider_and_as$as=="kavuaya"] <- npopKis_by_as[1,2]
nepisodesKis_by_provider_and_as <- nepisodesKis_by_provider_and_as %>% 
  mutate(pop = map(pop, as_tibble)) %>% # to show pop as a value instead of a list
  unnest()
nepisodesKis_by_provider_and_as


# # distribution of providers
# # tab
# providertabKis <- table(hcudbKis$provider[hcudbKis$provider!="None"], hcudbKis$as[hcudbKis$provider!="None"])
# providertabKis
# round(prop.table(providertabKis,2)*100,1)
# # stacked bar chart
# providerdistr <- hcudbKis%>%
#   filter(!is.na(provider) &!is.na(as) &!is.na(agegroups)& provider!="None") %>%
#   select(as, agegroups, provider) %>%
#   group_by(as, agegroups, provider) %>%
#   summarise(nvisits = n())
# providerdistr
# providerbarchart <- ggplot(providerdistr, aes(x=as.factor(as), y=nvisits, fill=factor(provider, level = c("selftreatment", "privatepharmacy", "privateclinic", "traditionalhealer", "healthcentre", "hospital")))) +
#   geom_bar(position = "fill",stat = "identity") +
#   coord_flip() +
#   labs(x="", y="Percentage") +
#   scale_y_continuous(labels=scales::percent) +
#   guides(fill=guide_legend(title=NULL)) +
#   theme_minimal() +
#   facet_wrap(. ~ factor(agegroups, level = c("0-4", "5-17", "18+")))  
# # +
# #   scale_fill_manual(values=c("LightGray", "red", "DarkRed", "yellow", "red", "orange", "dark green"))
# providerbarchart

#### 2.2 Kimpese HCU ####
# frequency overall
# population surveyed in Kimpese
npopKim <- sum(HHkim$N_HHMEMBERS[!is.na(HHkim$N_HHMEMBERS)])
npopKim
# episodes recorded in Kimpese
episodesKim <- subset(HCUkim, !is.na(HCUkim$providertype) & !is.na(HHkim$N_HHMEMBERS))
nepisodesKim <- count(episodesKim)
nepisodesKim
# frequency
nepisodesKim/npopKim

# frequency by AS
npopKim_by_as <- HHkim %>%
  filter(!is.na(N_HHMEMBERS)) %>%
  group_by(HEALTHAREA) %>%
  summarise(npop = sum(N_HHMEMBERS))
npopKim_by_as

nepisodesKim_by_as <- HCUkim %>%
  filter(!is.na(HCUkim$N_HHMEMBERS) & !is.na(HCUkim$providertype)) %>%
  group_by(HEALTHAREA) %>%
  summarise(nepisodes = n())
nepisodesKim_by_as

nepisodesKim_by_as[,2]/npopKim_by_as[,2]

# frequency by providertype and AS
npopKim_by_as <- HHkim %>%
  filter(!is.na(N_HHMEMBERS)) %>%
  group_by(HEALTHAREA) %>%
  summarise(npop = sum(N_HHMEMBERS))
npopKim_by_as

nepisodesKim_by_provider_and_as <- HCUkim %>%
  filter(!is.na(HCUkim$N_HHMEMBERS) & !is.na(HCUkim$providertype)) %>%
  group_by(providertype, HEALTHAREA) %>%
  summarise(nepisodes = n())
nepisodesKim_by_provider_and_as
# add column with denominator (pop)
nepisodesKim_by_provider_and_as$pop[nepisodesKim_by_provider_and_as$HEALTHAREA=="malanga"] <- npopKim_by_as$npop[2]
nepisodesKim_by_provider_and_as$pop[nepisodesKim_by_provider_and_as$HEALTHAREA=="viaza"] <- npopKim_by_as$npop[1]
nepisodesKim_by_provider_and_as

# # distribution of providers
# table(HCUkim$providertype)
# round(prop.table(table(HCUkim$providertype))*100,2)
# # provider type distrib by age group
# table(HCUkim$providertype, HCUkim$agegroups)
# round(prop.table(table(HCUkim$providertype, HCUkim$agegroups),2)*100,2)
# # tab
# providertabKis <- table(hcudbKis$provider[hcudbKis$provider!="None"], hcudbKis$as[hcudbKis$provider!="None"])
# providertabKis
# round(prop.table(providertabKis,2)*100,1)
# # stacked bar chart
# providerdistrKim <- HCUkim%>%
#   filter(!is.na(providertype) &!is.na(as) &!is.na(agegroups)& providertype!="None") %>%
#   select(HEALTHAREA, agegroups, providertype) %>%
#   group_by(HEALTHAREA, agegroups, providertype) %>%
#   summarise(nvisits = n())
# providerdistrKim
# providerbarchartKim <- ggplot(providerdistrKim, aes(x=as.factor(HEALTHAREA), y=nvisits, fill=factor(providertype, level = c("selftreatment", "privatepharmacy", "privateclinic", "traditionalhealer", "healthcentre", "hospital")))) +
#   geom_bar(position = "fill",stat = "identity") +
#   coord_flip() +
#   labs(x="", y="Percentage") +
#   scale_y_continuous(labels=scales::percent) +
#   guides(fill=guide_legend(title=NULL)) +
#   theme_minimal() +
#   facet_wrap(. ~ factor(agegroups, level = c("0-4", "5-17", "18+")))  
# providerbarchartKim

#### 2.3 frequency of HCU by provider by area ####
# merge Kis and Kim
names(nepisodesKis_by_provider_and_as) <- c("providertype", "healtharea", "nepisodes", "pop")
names(nepisodesKim_by_provider_and_as) <- c("providertype", "healtharea", "nepisodes", "pop")
nepisodes_by_provider_and_as <- rbind(nepisodesKis_by_provider_and_as, nepisodesKim_by_provider_and_as)
nepisodes_by_provider_and_as
# generate HCUfreq
nepisodes_by_provider_and_as$hcufreq <- nepisodes_by_provider_and_as$nepisodes/nepisodes_by_provider_and_as$pop
nepisodes_by_provider_and_as
write.table(nepisodes_by_provider_and_as, file = "nepisodes_by_provider_and_as.txt")

#### 2.4 frequency of HCU by provider all areas together ####
# exclude selftreatment (not reliably recorded in Kimpese)
nepisodes_by_provider <- nepisodes_by_provider_and_as %>%
  filter(providertype!="selftreatment") %>% # remove this line to have the contribution of self-treatment as well
  group_by(providertype) %>%
  summarise(nepisodes = sum(nepisodes))
nepisodes_by_provider$poptot <- 11140
nepisodes_by_provider$hcufreq <- nepisodes_by_provider$nepisodes/nepisodes_by_provider$poptot
nepisodes_by_provider
write.table(nepisodes_by_provider, file = "nepisodes_by_provider.txt")


#### 2.5 frequency of HCU by area, exclusing self-treatment ####
nepisodes_by_area <- nepisodes_by_provider_and_as %>%
  filter(providertype!="selftreatment") %>%
  group_by(healtharea) %>%
  summarise(nepisodes = sum(nepisodes), poparea = mean(pop))
nepisodes_by_area$hcufreq <- nepisodes_by_area$nepisodes/nepisodes_by_area$poparea
nepisodes_by_area
# summery per district
(246+369)/(4064+3891) #Kimpese
(281+722)/(688+2497) # Kisantu

#### 2.6 frequency of self-treatment by area ####
selftreatment_by_area <- nepisodes_by_provider_and_as %>%
  filter(providertype=="selftreatment") %>%
  group_by(healtharea) %>%
  summarise(nepisodes = sum(nepisodes), poparea = mean(pop))
selftreatment_by_area$hcufreq <- selftreatment_by_area$nepisodes/selftreatment_by_area$poparea
selftreatment_by_area

selftreatment_overall_kisantu <- nepisodes_by_provider_and_as %>%
  filter(providertype=="selftreatment" & (healtharea=="nkandu"|healtharea=="kavuaya")) %>%
  summarise(nepisodes = sum(nepisodes), poparea = mean(pop))
selftreatment_overall_kisantu$hcufreq <- selftreatment_overall_kisantu$nepisodes/selftreatment_overall_kisantu$poparea
selftreatment_overall_kisantu

#### 2.7. plot by area, providertype and agegroup ####
# summary by age group, area and providertype
# Kimpese
nepisodesKim_by_provider_as_agegr <- HCUkim %>%
  filter(!is.na(HCUkim$N_HHMEMBERS) & !is.na(HCUkim$providertype) & !is.na(HCUkim$agegroups)) %>%
  group_by(providertype, HEALTHAREA, agegroups) %>%
  summarise(nepisodes = n())
nepisodesKim_by_provider_as_agegr
# Kisantu
nepisodesKis_by_provider_as_agegr <- hcudbKis %>%
  filter(!is.na(provider) & !is.na(as) & !is.na(agegroups)) %>%
  group_by(provider, as, agegroups) %>%
  summarise(nepisodes = n())
nepisodesKis_by_provider_as_agegr
# append both
names(nepisodesKim_by_provider_as_agegr) <- c("providertype", "healtharea", "agegroups", "nepisodes")
names(nepisodesKis_by_provider_as_agegr) <- c("providertype", "healtharea", "agegroups", "nepisodes")
nepisodes_by_provider_as_agegr <- rbind(nepisodesKis_by_provider_as_agegr, nepisodesKim_by_provider_as_agegr)
nepisodes_by_provider_as_agegr
# exclude self treatment
nepisodes_by_provider_as_agegr_noselftreatment <- subset(nepisodes_by_provider_as_agegr, nepisodes_by_provider_as_agegr$providertype!="selftreatment")
# rename health areas
nepisodes_by_provider_as_agegr_noselftreatment$healtharea[nepisodes_by_provider_as_agegr_noselftreatment$healtharea=="kavuaya"] <- "Kisantu, Kavuaya"
nepisodes_by_provider_as_agegr_noselftreatment$healtharea[nepisodes_by_provider_as_agegr_noselftreatment$healtharea=="nkandu"] <- "Kisantu, Nkandu"
nepisodes_by_provider_as_agegr_noselftreatment$healtharea[nepisodes_by_provider_as_agegr_noselftreatment$healtharea=="viaza"] <- "Kimpese, Viaza" # to check, NOT the same area
nepisodes_by_provider_as_agegr_noselftreatment$healtharea[nepisodes_by_provider_as_agegr_noselftreatment$healtharea=="malanga"] <- "Kimpese, Malanga"
# rename providers and age groups
nepisodes_by_provider_as_agegr_noselftreatment$providertype_fig <- nepisodes_by_provider_as_agegr_noselftreatment$providertype
nepisodes_by_provider_as_agegr_noselftreatment$providertype_fig[nepisodes_by_provider_as_agegr_noselftreatment$providertype=="religiousleader"] <- "religious leader"
nepisodes_by_provider_as_agegr_noselftreatment$providertype_fig[nepisodes_by_provider_as_agegr_noselftreatment$providertype=="privatepharmacy"] <- "medicine store"
nepisodes_by_provider_as_agegr_noselftreatment$providertype_fig[nepisodes_by_provider_as_agegr_noselftreatment$providertype=="privateclinic"] <- "private clinic"
nepisodes_by_provider_as_agegr_noselftreatment$providertype_fig[nepisodes_by_provider_as_agegr_noselftreatment$providertype=="traditionalhealer"] <- "traditional healer"
nepisodes_by_provider_as_agegr_noselftreatment$providertype_fig[nepisodes_by_provider_as_agegr_noselftreatment$providertype=="healthcentre"] <- "health centre"
nepisodes_by_provider_as_agegr_noselftreatment$agegroups_fig[nepisodes_by_provider_as_agegr_noselftreatment$agegroups=="0-4"] <- "0-4 years"
nepisodes_by_provider_as_agegr_noselftreatment$agegroups_fig[nepisodes_by_provider_as_agegr_noselftreatment$agegroups=="5-17"] <- "5-17 years"
nepisodes_by_provider_as_agegr_noselftreatment$agegroups_fig[nepisodes_by_provider_as_agegr_noselftreatment$agegroups=="18+"] <- "18 years or older"

#plot
HCUbarchart <- ggplot(nepisodes_by_provider_as_agegr_noselftreatment, aes(x=as.factor(healtharea), y=nepisodes, fill=factor(providertype_fig, level = c("medicine store", "private clinic", "traditional healer", "religious leader", "health centre", "hospital")))) +
  geom_bar(position = "fill",stat = "identity") +
  coord_flip() +
  labs(x="", y="Percentage") +
  scale_y_continuous(labels=scales::percent) +
  guides(fill=guide_legend(title=NULL)) +
  theme_minimal() +
  facet_wrap(. ~ factor(agegroups_fig, level = c("0-4 years", "5-17 years", "18 years or older")))  
HCUbarchart
ggsave(HCUbarchart, filename = "HCUbarchart.png",  width = 8, height = 2, bg = "white")
ggsave(HCUbarchart, filename = "HCUbarchart.jpg",  width = 8, height = 2, bg = "white")
ggsave(HCUbarchart, filename = "HCUbarchart.tiff",  width = 8, height = 2, bg = "white")

#### 2.8 age specific healthcare seeking ####
nepisodes_by_age <- nepisodes_by_provider_as_agegr_noselftreatment %>%
  group_by(providertype, agegroups) %>%
  summarise(n = sum(nepisodes))
total_episodes_by_agegroup <- nepisodes_by_provider_as_agegr_noselftreatment %>%
  group_by(agegroups) %>%
  summarise(nage = sum(nepisodes))
nepisodes_by_age <- merge(nepisodes_by_age, total_episodes_by_agegroup, by = "agegroups")
nepisodes_by_age$pct <- round(nepisodes_by_age$n*100/nepisodes_by_age$nage,1)
nepisodes_by_age
