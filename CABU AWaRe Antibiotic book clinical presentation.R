###########################################
# CABU                                    #
# PATIENT EXIT INTERVIEWS                 #
# ANTIBIOTIC USE BY CLINICAL PRESENTATION #
###########################################
# Last update 15-dec-2022 by Brecht Ingelbeen

#### install and load packages ####
# install.packages("pacman")
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,tidyr, ggpattern)

#### NANORO, BURKINA FASO ####
#### clean raw database - see that only the anonimysed dataframe is available here, after cleaning raw data ####
dbnanoro <- read_excel("db/Visit_exit_Ongoing.xls")

# age 
dbnanoro$ageyears <- dbnanoro$Age_annés
dbnanoro$ageyears[dbnanoro$Age_Mois1_Anne2==1&!is.na(dbnanoro$Age_Mois)] <- dbnanoro$Age_Mois[dbnanoro$Age_Mois1_Anne2==1&!is.na(dbnanoro$Age_Mois)]/12

# clean voie d'administration
# new var iv vs oral vs other
dbnanoro$ab_iv_po[dbnanoro$Voie_administration=="IV"|dbnanoro$Voie_administration=="IM"] <- "parenteral"
dbnanoro$ab_iv_po[dbnanoro$Voie_administration=="PO"] <- "oral"
dbnanoro$ab_iv_po[dbnanoro$Voie_administration=="IR"|dbnanoro$Voie_administration=="VOIX ANALE"] <- "rectal"
dbnanoro$ab_iv_po[grepl("POMMADE", dbnanoro$Nom_Specialite)==T] <- NA
dbnanoro$ab_iv_po[grepl("POMMADE", dbnanoro$Nom_Generique)==T] <- NA
table(dbnanoro$Autre_voie_administration)
table(dbnanoro$ab_iv_po, useNA = "always")

# clean the names of antibiotics - for systemic use 
dbnanoro$abgenericraw <- tolower(dbnanoro$Nom_Generique)
dbnanoro$abgenericraw[is.na(dbnanoro$abgenericraw)] <- tolower(dbnanoro$Nom_Specialite)
dbnanoro$abgeneric[dbnanoro$abgenericraw=='Ã£â°rythromycine']='azithromycin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicilline"] <- "amoxicillin"
dbnanoro$abgeneric[grepl("amox", dbnanoro$abgenericraw)==TRUE] <- "amoxicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="taxclav"] <- "cefixime"
dbnanoro$abgeneric[grepl("solbactam+ampicillin", dbnanoro$abgenericraw)==TRUE] <- "ampicillin and beta-lactamase inhibitor" 
dbnanoro$abgeneric[grepl("penicillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="pã©nicillin"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("penicilin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("penidur", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("penecillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("pencillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("pinicillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="oral"] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("penidur", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="parenteral"] <- "benzylpenicillin"
dbnanoro$abgeneric[grepl("penecillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="parenteral"] <- "benzylpenicillin"
dbnanoro$abgeneric[grepl("pencillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="parenteral"] <- "benzylpenicillin"
dbnanoro$abgeneric[grepl("pinicillin", dbnanoro$abgenericraw)==TRUE & dbnanoro$ab_iv_po=="parenteral"] <- "benzylpenicillin"
dbnanoro$abgeneric[grepl("piperacillin + taz", dbnanoro$abgenericraw)==TRUE] <- "piperacillin and beta-lactamase inhibitor"
dbnanoro$abgeneric[grepl("piptaz", dbnanoro$abgenericraw)==TRUE] <- "piperacillin and beta-lactamase inhibitor"
dbnanoro$abgeneric[grepl("tazobactam + piperacillin", dbnanoro$abgenericraw)==TRUE] <- "piperacillin and beta-lactamase inhibitor"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. tazobactem pipracillin"] <- "piperacillin and beta-lactamase inhibitor"
dbnanoro$abgeneric[grepl("sulfadiazine", dbnanoro$abgenericraw)==TRUE] <- "sulfadiazine"
dbnanoro$abgeneric[grepl("metronidazol", dbnanoro$abgenericraw)==TRUE] <- "metronidazole"
dbnanoro$abgeneric[grepl("mã©tronid", dbnanoro$abgenericraw)==TRUE] <- "metronidazole" # when used orally or IV, can be either J01 (AB systemic use), but P01 ANTIPROTOZOALS
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. metron"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="meetronidazole"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="metraonidazole"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="metromidazole"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="metrondazole"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="metronedazole"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="nitronidazol"] <- "metronidazole"
dbnanoro$abgeneric[grepl("ampicill", dbnanoro$abgenericraw)==TRUE] <- "ampicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="aminoside"]<- "gentamycin"#### brand name indicated gentamycin
dbnanoro$abgeneric[dbnanoro$abgenericraw=="gentamicin"]<- "gentamycin"#### brand name indicated gentamycin
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amocycilline"]<- "amoxicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ampiciline"]<- "ampicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amykacine"]<- "amikacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="anpicillin"]<-"ampicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="araucip"]<-"ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cirpofloxacine"]<-"ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="araumet"]<-"metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="avepen"]<-"phenoxymethylpenicillin"#### penicillin vk from brand name
dbnanoro$abgeneric[grepl("pã‰nicilline v", dbnanoro$abgenericraw)==TRUE] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="bactox"]<-"amoxicillin"
dbnanoro$abgeneric[grepl("bactri", dbnanoro$abgenericraw)==TRUE]<-"sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("benzyl", dbnanoro$abgenericraw)==TRUE]<-"benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="beta-lactam"]<-"betalactamine"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="iniclave"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cefatax"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cã©ftriaxone"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cefatax"]<-"cefixime"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cefrriaxone"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="eftriaxone"]<-"ceftriaxone"
dbnanoro$abgeneric[grepl("ceftriazone", dbnanoro$abgenericraw)==TRUE]<-"ceftriaxone"
dbnanoro$abgeneric[grepl("cã©ftriaxone", dbnanoro$abgenericraw)==TRUE]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ceftrioxone vial"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="2 phÃ£Â©noxymethylpenicilline"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("phenoxymethylpenicilli",dbnanoro$abgenericraw)==T]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("phenoxymethy",dbnanoro$abgenericraw)==T]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="penicilline v"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="pã©nicilline comprimã©"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="pã©nicilline"]<-"phenoxymethylpenicillin" 
dbnanoro$abgeneric[grepl("procainebenzylpenicillin",dbnanoro$abgenericraw)==T]<-"procaine_penicillin"
dbnanoro$abgeneric[grepl("procaine",dbnanoro$abgenericraw)==T]<-"procaine_penicillin"
dbnanoro$abgeneric[grepl("pã©ni v",dbnanoro$abgenericraw)==T]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[c(882,1142)]<-"phenoxymethylpenicillin"#####penicillin and PÃÂ©nicillin  vk(brand name indicating pencillin vk)
dbnanoro$abgeneric[c(151,320,1182,764,889)]<-"phenoxymethylpenicillin"######brand name indicating pencillin vk
dbnanoro$abgeneric[c(952,421)]<-"benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="chllrpheniramine"]<-"chloramphenicol"
dbnanoro$abgeneric[grepl("chlo", dbnanoro$abgenericraw)==TRUE]<-"chloramphenicol"
dbnanoro$abgeneric[grepl("phenicolã©", dbnanoro$abgenericraw)==TRUE]<-"chloramphenicol"
dbnanoro$abgeneric[grepl("cloraphenicolÃ£Â©s", dbnanoro$abgenericraw)==TRUE]<-"chloramphenicol"
dbnanoro$abgeneric[grepl("cifin", dbnanoro$abgenericraw)==TRUE]<-"ciprofloxacin"
dbnanoro$abgeneric[198]="ciprofloxacin"### under brand name of cifin
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciorofloxacine"]<-"ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cipro"]<-"ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciproflaxacine infusion"]<-"ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="citrofloxacine"]<-"ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cyprofloxacine"]<-"ciprofloxacin"
dbnanoro$abgeneric[grepl("clinda", dbnanoro$abgenericraw)==TRUE]<-"clindamycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cotrumoxazole"]<-"sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("doxy", dbnanoro$abgenericraw)==TRUE]<-"doxycycline"
dbnanoro$abgeneric[grepl("ery", dbnanoro$abgenericraw)==TRUE]<-"erythromycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="furadentine"]<-"nitrofurantoin"
dbnanoro$abgeneric[grepl("nutrofirad", dbnanoro$abgenericraw)==TRUE]<-"nitrofurantoin"
dbnanoro$abgeneric[grepl("nutrofurantoine", dbnanoro$abgenericraw)==TRUE]<-"nitrofurantoin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="bruffen comprimÃ£Â©"]<- NA
dbnanoro$abgeneric[grepl("buscopam", dbnanoro$abgenericraw)==TRUE]<- NA ##### not anabx
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cÃ£Â©ftriaxone"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="isoniade"]<-"isoniazid"
dbnanoro$abgeneric[grepl("linconsin", dbnanoro$abgenericraw)==TRUE]<-"lincomycin"
dbnanoro$abgeneric[grepl("lyncomicine", dbnanoro$abgenericraw)==TRUE]<-"lincomycin"
dbnanoro$abgeneric[grepl("mÃ£Â©tronida", dbnanoro$abgenericraw)==TRUE]<-"metronidazole"
dbnanoro$abgeneric[grepl("mã‰tronidazole", dbnanoro$abgenericraw)==TRUE]<-"metronidazole"
dbnanoro$abgeneric[grepl("macrolide", dbnanoro$abgenericraw)==TRUE]<-"erythromycin"
dbnanoro$abgeneric[grepl("meya", dbnanoro$abgenericraw)==TRUE]<-"meyamycin"
dbnanoro$abgeneric[grepl("mã©tron", dbnanoro$abgenericraw)==TRUE]<-"metronidazole"
dbnanoro$abgeneric[grepl("metron", dbnanoro$abgenericraw)==TRUE]<-"metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="nÃ£Â©omycine"]<-"neomycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="neomycine"]<-"neomycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="nidazole"]<-"metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="nitrofuradentine"]<-"nitrofurantoin"
dbnanoro$abgeneric[grepl("nitofurantoine", dbnanoro$abgenericraw)==TRUE]<-"nitrofurantoin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="norfloxacine"]<-"norfloxacin"
dbnanoro$abgeneric[grepl("normegyl", dbnanoro$abgenericraw)==TRUE]<-"norfloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="normegyl comprimÃ£Â©"]<-"metronidazole and norfloxacin"
dbnanoro$abgeneric[grepl("menorcin", dbnanoro$abgenericraw)==TRUE]<-"metronidazole and norfloxacin"
dbnanoro$abgeneric[grepl("pÃ£Â©nicillin", dbnanoro$abgenericraw)==TRUE]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("peni v", dbnanoro$abgenericraw)==TRUE]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="penicellin"&dbnanoro$ab_iv_po=="oral"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="penicelin"&dbnanoro$ab_iv_po=="oral"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="penicellin"&dbnanoro$ab_iv_po=="oral"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[grepl("penicellin", dbnanoro$abgenericraw)==TRUE]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="pencillinvk"]<-"phenoxymethylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="benzathine penicilline"]<-"benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="benzatine peniceline"]<-"benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="benzathyl penicilline"]<-"benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="beta-lactamine"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="beta-lactamase"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ticasse"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="phenicolÃ£Â©"]<-"chloramphenicol"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="rifampicine"]<-"rifampicin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="sulfadoxine et pyrimethamine"]<-"sulfadoxine and pyrimethamine"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="sulfadoxine"]<-"sulfadoxine and pyrimethamine"
dbnanoro$abgeneric[grepl("sulfadox", dbnanoro$abgenericraw)==TRUE]<-"sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="sulfatrim"]<-"sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("sulfatrim", dbnanoro$abgenericraw)==TRUE]<-"sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="tÃ£Â©tracycline"]<-"tetracycline"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="tã©tracycline"]<-"tetracycline"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicillin"&dbnanoro$abname=="Augmentin"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="benzylpenicillin"&dbnanoro$abname=="Peni procaine"]<-"procainebenzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicillin"&dbnanoro$abname=="AMOXYCILLINE ET CLAVULANATE"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicillin"&dbnanoro$abname=="Clamoxyl"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicillin"&dbnanoro$abname=="Amoxy clav"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicillin"&dbnanoro$abname=="Moxyclav"]<-"amoxicillin and clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cedocard comprimÃ£Â© de 5mg"&dbnanoro$abname=="Dinitrate d'isosorbide"]<-"isosorbide dinitrate"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cÃ£Â©phalosporine"&dbnanoro$abname=="Prodoxil"]<-"cefadroxil"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cã©phalosporine"&dbnanoro$abname=="Prodoxil"]<-"cefadroxil"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cÃ£Â©ftriaxone"]<-"ceftriaxone"
dbnanoro$abgeneric[grepl("amikaci", dbnanoro$abgenericraw)==TRUE] <- "amikacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. amillacin"] <- "amikacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. Amlkacin"] <- "amikacin"
dbnanoro$abgeneric[grepl("vancom", dbnanoro$abgenericraw)==TRUE] <- "vancomycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj uancomycin"] <- "vancomycin"
dbnanoro$abgeneric[grepl("genta", dbnanoro$abgenericraw)==TRUE] <- "gentamicin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="gentimicin inj"] <- "gentamicin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="gentmicine inj"] <- "gentamicin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. geftriamycin"] <- "gentamicin"
dbnanoro$abgeneric[grepl("kanamycin", dbnanoro$abgenericraw)==TRUE] <- "kanamycin"
dbnanoro$abgeneric[grepl("cefixim", dbnanoro$abgenericraw)==TRUE] <- "cefixime"
dbnanoro$abgeneric[grepl("cefex", dbnanoro$abgenericraw)==TRUE] <- "cefixime"
dbnanoro$abgeneric[grepl("cephalexin", dbnanoro$abgenericraw)==TRUE] <- "cefalexin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ceftxime"] <- "cefixime"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cefazoline"] <- "cefazolin"
dbnanoro$abgeneric[grepl("cefurox", dbnanoro$abgenericraw)==TRUE] <- "cefuroxime"
dbnanoro$abgeneric[grepl("zocef", dbnanoro$abgenericraw)==TRUE] <- "cefuroxime"
dbnanoro$abgeneric[grepl("tricef", dbnanoro$abgenericraw)==TRUE] <- "cefixime"
dbnanoro$abgeneric[grepl("cefxime", dbnanoro$abgenericraw)==TRUE] <- "cefixime"
dbnanoro$abgeneric[grepl("cefepim", dbnanoro$abgenericraw)==TRUE] <- "cefepime"
dbnanoro$abgeneric[grepl("cefipim", dbnanoro$abgenericraw)==TRUE] <- "cefepime"
dbnanoro$abgeneric[grepl("cefoperazone", dbnanoro$abgenericraw)==TRUE] <- "cefoperazone"
dbnanoro$abgeneric[grepl("ceftazidi", dbnanoro$abgenericraw)==TRUE] <- "ceftazidime"
dbnanoro$abgeneric[grepl("ceftraixone", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("ceftraxone", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("ceftria", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("ceftrixone", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("cefiriaxone", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("cefitriaxone", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("monocef", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("monocet", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("monocex", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("oframax", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. oframau"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. cefyriaxone"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. cephalosporin"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj.cefaxone"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="leftriaxone"] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("ceftriaton", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("cetriax", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("ciftria", dbnanoro$abgenericraw)==TRUE] <- "ceftriaxone"
dbnanoro$abgeneric[grepl("cefotaxim", dbnanoro$abgenericraw)==TRUE] <- "cefotaxime"
dbnanoro$abgeneric[grepl("taxim", dbnanoro$abgenericraw)==TRUE] <- "cefotaxime"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj cefoxaxime"] <- "cefotaxime"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj ceftrazidime"] <- "ceftazidime"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="inj. cettazidime"] <- "ceftazidime"
dbnanoro$abgeneric[grepl("cefodoxime", dbnanoro$abgenericraw)==TRUE] <- "cefpodoxime"
dbnanoro$abgeneric[grepl("cepodoxime", dbnanoro$abgenericraw)==TRUE] <- "cefpodoxime"
dbnanoro$abgeneric[grepl("cepodoxine", dbnanoro$abgenericraw)==TRUE] <- "cefpodoxime"
dbnanoro$abgeneric[grepl("cloxac", dbnanoro$abgenericraw)==TRUE] <- "cloxacillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cloxcilin"] <- "cloxacillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cloxiacillin"] <- "cloxacillin"
dbnanoro$abgeneric[grepl("trimet",dbnanoro$abgenericraw)==TRUE] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("sulfamethoxa",dbnanoro$abgenericraw)==TRUE] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("trimazole",dbnanoro$abgenericraw)==TRUE] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("tri methoprim",dbnanoro$abgenericraw)==TRUE] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("co-trimox",dbnanoro$abgenericraw)==TRUE] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("cotrim",dbnanoro$abgenericraw)==TRUE] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cotrinoxale"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cotrmoxazole 960 mg"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cotrinoxazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[grepl("sulfadiazin",dbnanoro$abgenericraw)==TRUE] <- "sulfadiazine"
dbnanoro$abgeneric[grepl("doxyc", dbnanoro$abgenericraw)==TRUE] <- "doxycycline"
dbnanoro$abgeneric[grepl("doxic", dbnanoro$abgenericraw)==TRUE] <- "doxycycline"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="microdox"] <- "doxycycline"
dbnanoro$abgeneric[grepl("tetracy", dbnanoro$abgenericraw)==TRUE] <- "tetracycline"
dbnanoro$abgeneric[grepl("chloramphenicol", dbnanoro$abgenericraw)==TRUE] <- "chloramphenicol"
dbnanoro$abgeneric[grepl("erythro", dbnanoro$abgenericraw)==TRUE] <- "erythromycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="eryethromycin"] <- "erythromycin"
dbnanoro$abgeneric[grepl("ã‰rythrom", dbnanoro$abgenericraw)==TRUE] <- "erythromycin"
dbnanoro$abgeneric[grepl("azithro", dbnanoro$abgenericraw)==TRUE] <- "azithromycin"
dbnanoro$abgeneric[grepl("azitro", dbnanoro$abgenericraw)==TRUE] <- "azithromycin"
dbnanoro$abgeneric[grepl("azifine", dbnanoro$abgenericraw)==TRUE] <- "azithromycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="roxithromycin"] <- "roxithromycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="roxythromycin"] <- "roxithromycin"
dbnanoro$abgeneric[grepl("clarithro", dbnanoro$abgenericraw)==TRUE] <- "clarithromycin"
dbnanoro$abgeneric[grepl("claritro", dbnanoro$abgenericraw)==TRUE] <- "clarithromycin"
dbnanoro$abgeneric[grepl("clarethro", dbnanoro$abgenericraw)==TRUE] <- "clarithromycin"
dbnanoro$abgeneric[grepl("clarthro", dbnanoro$abgenericraw)==TRUE] <- "clarithromycin"
dbnanoro$abgeneric[grepl("clarythro", dbnanoro$abgenericraw)==TRUE] <- "clarithromycin"
dbnanoro$abgeneric[grepl("ciproflox", dbnanoro$abgenericraw)==TRUE] <- "ciprofloxacin"
dbnanoro$abgeneric[grepl("liproflox", dbnanoro$abgenericraw)==TRUE] <- "ciprofloxacin"
dbnanoro$abgeneric[grepl("cifran", dbnanoro$abgenericraw)==TRUE] <- "ciprofloxacin"
dbnanoro$abgeneric[grepl("ceproflox", dbnanoro$abgenericraw)==TRUE] <- "ciprofloxacin"
dbnanoro$abgeneric[grepl("levofloxacin", dbnanoro$abgenericraw)==TRUE] <- "levofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="tab leuofloxacin"] <- "levofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="tab leuopfloxacin"] <- "levofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="tab liprofloxacin"] <- "levofloxacin"
dbnanoro$abgeneric[grepl("zanocin", dbnanoro$abgenericraw)==TRUE] <- "ofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ofloxacin"] <- "ofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ofloxacine"] <- "ofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ofioxacine"] <- "ofloxacin"
dbnanoro$abgeneric[grepl("norloxacin", dbnanoro$abgenericraw)==TRUE] <- "norfloxacin"
dbnanoro$abgeneric[grepl("gatifloxacin", dbnanoro$abgenericraw)==TRUE] <- "gatifloxacin"
dbnanoro$abgeneric[grepl("moxiflox", dbnanoro$abgenericraw)==TRUE] <- "moxifloxacin"
dbnanoro$abgeneric[grepl("lincomycin", dbnanoro$abgenericraw)==TRUE] <- "lincomycin"
dbnanoro$abgeneric[grepl("licomycin", dbnanoro$abgenericraw)==TRUE] <- "lincomycin"
dbnanoro$abgeneric[grepl("lincomicine", dbnanoro$abgenericraw)==TRUE] <- "lincomycin"
dbnanoro$abgeneric[grepl("clindamycin", dbnanoro$abgenericraw)==TRUE] <- "clindamycin"
dbnanoro$abgeneric[grepl("dalacin", dbnanoro$abgenericraw)==TRUE] <- "clindamycin"
dbnanoro$abgeneric[grepl("imipenem", dbnanoro$abgenericraw)==TRUE] <- "imipenem"
dbnanoro$abgeneric[grepl("ertapenem", dbnanoro$abgenericraw)==TRUE] <- "ertapenem"
dbnanoro$abgeneric[grepl("meropenem", dbnanoro$abgenericraw)==TRUE] <- "meropenem"
dbnanoro$abgeneric[grepl("uribid", dbnanoro$abgenericraw)==TRUE] <- "nitrofurantoin"
dbnanoro$abgeneric[grepl("nitrofurantoin", dbnanoro$abgenericraw)==TRUE] <- "nitrofurantoin"
dbnanoro$abgeneric[grepl("nitrofurantion", dbnanoro$abgenericraw)==TRUE] <- "nitrofurantoin"
dbnanoro$abgeneric[grepl("nitrofuransoin", dbnanoro$abgenericraw)==TRUE] <- "nitrofurantoin"
dbnanoro$abgeneric[grepl("nitrofuratoin", dbnanoro$abgenericraw)==TRUE] <- "nitrofurantoin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amoxicillin"&dbnanoro$abname=='Clavuzam']='amoxicillin and clavulanic acid'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='moxyclav duo 457']='amoxicillin and clavulanic acid'
dbnanoro$abgeneric[dbnanoro$abname=='Moxyclav Duo 1125mg injectable']='amoxicillin and clavulanic acid'
dbnanoro$abgeneric[dbnanoro$abname=='CÃÂ©phalosporin Hcl 500mg                                  Ovotan']='cephalexin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='penicilline orale']='phenoxymethylpenicillin'
dbnanoro$abgeneric[grepl("nicilline v", dbnanoro$abgenericraw)==TRUE] <- "phenoxymethylpenicillin"
dbnanoro$abgeneric[c(35,129)]='benzylpenicillin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='penicillin'&dbnanoro$ab_iv_po=='oral']='phenoxymethylpenicillin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='penicilline'&dbnanoro$ab_iv_po=='oral']='phenoxymethylpenicillin'
dbnanoro$abgeneric[dbnanoro$abname=='Phatrix']='ceftriaxone'
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ceftriaxone"&dbnanoro$ab_iv_po=="parenteral"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="clavuzam"]='amoxicillin and clavulanic acid'
dbnanoro$abgeneric[grepl("nã©omycine", dbnanoro$abgenericraw)==TRUE] <- "neomycin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="benzathine penicillin"] <- "benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cephalexin"] <- "cefalexin"
dbnanoro$abgeneric[grepl("1572423417281.jpg",dbnanoro$abgenericraw)==T] <- "cefalexin"
dbnanoro$abgeneric[is.na(dbnanoro$abgeneric)&dbnanoro$Nom_Specialite=="AMOXICILLINE"] <- "amoxicillin"
dbnanoro$abgeneric[is.na(dbnanoro$abgeneric)&grepl("CEFTRI",dbnanoro$Nom_Specialite)==T] <- "ceftriaxone"
dbnanoro$abgeneric[is.na(dbnanoro$abgeneric)&grepl("METRON",dbnanoro$Nom_Specialite)==T] <- "metronidazole"
dbnanoro$abgeneric[is.na(dbnanoro$abgeneric)&grepl("TERGYNAN",dbnanoro$Nom_Specialite)==T] <- "metronidazole"
dbnanoro$abgeneric[is.na(dbnanoro$abgeneric)&grepl("NORFLOXA",dbnanoro$Nom_Specialite)==T] <- "norfloxacin"
dbnanoro$abgeneric[is.na(dbnanoro$abgeneric)&grepl("PENIG 1MILLION",dbnanoro$Nom_Specialite)==T] <- "benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="sulphamethoxazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="triaf-inj"]<-"ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="sulffran"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=='peni-v'&dbnanoro$ab_iv_po=='oral']='phenoxymethylpenicillin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='peniv'&dbnanoro$ab_iv_po=='oral']='phenoxymethylpenicillin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='peniceline'&dbnanoro$ab_iv_po=='oral']='phenoxymethylpenicillin'
dbnanoro$abgeneric[dbnanoro$abgenericraw=='penisillin'&dbnanoro$ab_iv_po=='oral']='phenoxymethylpenicillin'
dbnanoro$abgeneric[grepl("nutrofurantoïne", dbnanoro$abgenericraw)==TRUE] <- "nitrofurantoin"
dbnanoro$abgeneric[grepl("norfloxacin", dbnanoro$abgenericraw)==TRUE] <- "norfloxacin"
dbnanoro$abgeneric[grepl("nirfloxacin", dbnanoro$abgenericraw)==TRUE] <- "norfloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="nc0-trimoxazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="metro"] <- "metronidazole"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cotri"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cprofloxacin"] <- "ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cotrmoxazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciprifloxacine"] <- "ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciproflixacin"] <- "ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciprofloacin"] <- "ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciprolex"] <- "ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ciprogloxacine"] <- "ciprofloxacin"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="cefriaxone"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="celtriaxone"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ceft+seringue"] <- "ceftriaxone"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="c0-trimoxazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="c0-trinidazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="co-trinidazole"] <- "sulfamethoxazole/trimethoprim"
dbnanoro$abgeneric[dbnanoro$abgenericraw=="amozicilline+acide clavulanique"] <- 'amoxicillin and clavulanic acid'
dbnanoro$abgeneric[dbnanoro$abgenericraw=="ampicilin"] <- 'ampicillin'

# eliminate those not for systemic use
dbnanoro$abgeneric[is.na(dbnanoro$ab_iv_po)] <- NA
table(dbnanoro$abgeneric, useNA = "always")
# to check again if no AB missed
list_noAB <- table(dbnanoro$abgenericraw[is.na(dbnanoro$abgeneric)&!is.na(dbnanoro$ab_iv_po)], useNA = "always")
write.table(list_noAB, "list_noAB.txt")

# import antibiotic group classification
WHO_EML2019 <- read_excel("db/WHO-EMP-IAU-2019.11-eng.xlsx", sheet = "db")
WHO_EML2019$Antibiotic <- tolower(WHO_EML2019$Antibiotic)
WHO_EML2019$abgeneric <- WHO_EML2019$Antibiotic
WHO_EML2019$Antibiotic <- NULL
WHO_EML2019$Category <- tolower(WHO_EML2019$Category)
WHO_EML2019$Class <- tolower(WHO_EML2019$Class)
WHO_EML2019$abgeneric[WHO_EML2019$abgeneric=="metronidazole (iv)"] <- "metronidazole"
WHO_EML2019 <- WHO_EML2019 %>% filter(abgeneric!="metronidazole (oral)") #IV and oral are both on the EML and access -> not needed to keep separated
# regroup some classes according to CLSI
WHO_EML2019$Class[WHO_EML2019$Class=="first-generation cephalosporins"] <- "cephems"
WHO_EML2019$Class[WHO_EML2019$Class=="third-generation cephalosporins"] <- "cephems"
WHO_EML2019$Class[WHO_EML2019$Class=="nitrofurantoin"] <- "nitroheterocyclics"
WHO_EML2019$Class[WHO_EML2019$Class=="imidazoles"] <- "nitroheterocyclics"
WHO_EML2019$Class[WHO_EML2019$Class=="amphenicols"] <- "phenicols"
WHO_EML2019$Class[WHO_EML2019$Class=="beta lactam - beta lactamase inhibitor"] <- "beta-lactam combination agent"
WHO_EML2019$Class[WHO_EML2019$Class=="lincosamides"] <- "lincomycins"

# rename antibiotics to match the WHO classification
dbnanoro$abgeneric[dbnanoro$abgeneric=="amoxicillin and clavulanic acid"] <- "amoxicillin/clavulanic acid"
dbnanoro$abgeneric[dbnanoro$abgeneric=="procaine_penicillin"] <- "procaine benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgeneric=="benzathine penicillin"] <- "benzathine benzylpenicillin"
dbnanoro$abgeneric[dbnanoro$abgeneric==" benzylpenicillin"] <- "benzathine benzylpenicillin"
table(dbnanoro$abgeneric)

# merge with WHO EML
dbnanoro <- merge(dbnanoro, WHO_EML2019, by ="abgeneric", all.x = TRUE) # all matched well
table(dbnanoro$abgeneric, dbnanoro$Class, useNA = "always") # all are matched
table(dbnanoro$Class, useNA = "always") # all are matched
table(dbnanoro$abgeneric, useNA = "always") # all are matched

# create a variable to be able to sum up the number of AWaRe antibiotics per patient
dbnanoro$access <- 0
dbnanoro$access[dbnanoro$Category=="access"] <- 1
dbnanoro$watch <- 0
dbnanoro$watch[dbnanoro$Category=="watch"] <- 1
dbnanoro$reserve <- 0
dbnanoro$reserve[dbnanoro$Category=="reserve"] <- 1

# create a variable for type of provider
table(dbnanoro$ActeurdeSanté2, useNA = "always")
table(dbnanoro$ActeurdeSanté_Visité, useNA = "always")
table(dbnanoro$OrdonnanceFormelle_Dispo, useNA = "always")
dbnanoro$prescribed[dbnanoro$OrdonnanceFormelle_Dispo==0] <- "selfmedication"
dbnanoro$prescribed[dbnanoro$OrdonnanceFormelle_Dispo==1] <- "prescribed"
table(dbnanoro$StrutureSanté_Visité, useNA = "always")
table(dbnanoro$ServiceStructFormel_Visité, dbnanoro$StrutureSanté_Visité, useNA = "always")
dbnanoro$providertype[dbnanoro$StrutureSanté_Visité==1] <- "hospital"
dbnanoro$providertype[dbnanoro$StrutureSanté_Visité==2] <- "healthcentre"
dbnanoro$providertype[dbnanoro$StrutureSanté_Visité==1&dbnanoro$ServiceStructFormel_Visité==1] <- "healthcentre"
dbnanoro$providertype[dbnanoro$StrutureSanté_Visité==2&dbnanoro$ServiceStructFormel_Visité==1] <- "healthcentre"
dbnanoro$providertype[dbnanoro$StrutureSanté_Visité==3&dbnanoro$ServiceStructFormel_Visité==1] <- "healthcentre"
dbnanoro$providertype[dbnanoro$StrutureSanté_Visité==3&dbnanoro$ActeurdeSanté2==3] <- "healthcentre"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==6&is.na(dbnanoro$providertype)&dbnanoro$prescribed=="prescribed"] <- "healthcentre"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==7&is.na(dbnanoro$providertype)&dbnanoro$prescribed=="prescribed"] <- "healthcentre"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==5&is.na(dbnanoro$providertype)&dbnanoro$prescribed=="prescribed"] <- "healthcentre"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==6&dbnanoro$prescribed=="selfmedication"] <- "selfmedication_pharmacy"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==7&dbnanoro$prescribed=="selfmedication"] <- "selfmedication_pharmacy"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==5&dbnanoro$prescribed=="selfmedication"] <- "selfmedication_pharmacy"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==8] <- "selfmedication_pharmacy"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==9] <- "selfmedication_informalprovider"
dbnanoro$providertype[dbnanoro$ActeurdeSanté2==10] <- "trad healer"
table(dbnanoro$providertype, useNA = "always")
round(prop.table(table(dbnanoro$providertype, useNA = "always"))*100,2)

# create a variable for aire de santé
dbnanoro$as[dbnanoro$Residence_Patients==1] <- "nanoro"
dbnanoro$as[dbnanoro$Residence_Patients==2] <- "nanoro"
dbnanoro$as[dbnanoro$Residence_Patients==3] <- "nanoro"
dbnanoro$as[dbnanoro$Residence_Patients==4] <- "nanoro"
dbnanoro$as[dbnanoro$Residence_Patients==5] <- "nanoro"
dbnanoro$as[dbnanoro$Residence_Patients==6] <- "nazoanga"
dbnanoro$as[dbnanoro$Residence_Patients==7] <- "nazoanga"
dbnanoro$as[dbnanoro$Residence_Patients==8] <- "nazoanga"
dbnanoro$as[dbnanoro$Residence_Patients==9] <- "nazoanga"

# assign clinical presentations from the WHO AWaRe book to syndroms/symptoms presented by patients
dbnanoro$Plaintes_Autres <- tolower(dbnanoro$Plaintes_Autres)
dbnanoro$clinicalpresentation[grepl(" 2", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "bronchitis"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé==2] <- "bronchitis"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="2 13"] <- "bronchitis"
dbnanoro$clinicalpresentation[grepl(" 3", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "pharyngitis"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé==3] <- "pharyngitis"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé==4] <- "pharyngitis"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé==5] <- "otitis media"
dbnanoro$clinicalpresentation[grepl("6", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "bronchitis"
dbnanoro$clinicalpresentation[grepl("9", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "typhoid"
dbnanoro$clinicalpresentation[grepl("10", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "dengue"
dbnanoro$clinicalpresentation[grepl("11", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "UTI"
dbnanoro$clinicalpresentation[grepl("8", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "infectious diarrhoea"
dbnanoro$clinicalpresentation[grepl("7", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "pneumonia"
dbnanoro$clinicalpresentation[grepl("12", dbnanoro$DiagnosticSpecifiq_Précisé)==T] <- "sepsis"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1"] <- "malaria"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1"] <- "malaria"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 13"] <- "malaria"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 5"] <- "otitis media"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 13"&dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION CUTANEE"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 13"&dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="FURONCLES"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 13"&dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="DERMATHOSE"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 13"&dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="CANDIDOSE INFECTEE"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Précisé=="1 13"&dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION CUTANEE"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="IST"] <- "STI"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION GENITALE"] <- "STI"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION GENITAL"] <- "STI"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="INFECTION SEXUELLEMENT"] <- "STI"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="INFECTION SEXUELLE"] <- "STI"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION DE LA PEAU"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="FURONCLE AVEC COLECTION DU PUS"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="FIEVRE TYPHOIDE"] <- "typhoid"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="FURONCLE"] <- "other"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="DREMATOSE"] <- "other"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="DERMATOSE"] <- "other"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="DERMATHOSE"] <- "other"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="DIARRHEE"] <- "infectious diarrhoea"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="BOURBOUILLE"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION CUTANEE"] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="CARIE DENTAIRE"] <- "dental infection"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="CARRIE DENTAIRE"] <- "dental infection"
dbnanoro$clinicalpresentation[grepl("TUBERCULO",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "tuberculosis"
dbnanoro$clinicalpresentation[grepl("ULCER",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T&(grepl("GASTR",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T|grepl("ESTOM",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T)] <- "gastric ulcer"
dbnanoro$clinicalpresentation[grepl("ULCER",dbnanoro$DiagnosticSpecifiq_Precisé_Autr==T) & dbnanoro$Plaintes=="5"] <- "gastric ulcer"
dbnanoro$clinicalpresentation[grepl("PLAI",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "wound"
dbnanoro$clinicalpresentation[grepl("SAIGNEMENT",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "wound"
dbnanoro$clinicalpresentation[grepl("BLESSURE",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "wound"
dbnanoro$clinicalpresentation[grepl("FRACTURE OUVERT",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "wound"
dbnanoro$clinicalpresentation[grepl("TENSIO",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("HYPERGENSION",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("HYPERGENSION",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("HYPERTENDU",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("HYPPERTENDU",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("HYPERGLYCEMIE",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("EPILEPSIE",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("CARDIO",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("MYCOS",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T|grepl("CANDI",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "fungal infection"
dbnanoro$clinicalpresentation[grepl("CANDIDOSE",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T|grepl("CANDI",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "fungal infection"
dbnanoro$clinicalpresentation[grepl("ASTHMATIQUE",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="AFFECTION RESPIRATOIRE"] <- "COPD exacerbation"
dbnanoro$clinicalpresentation[grepl("DOULEUR",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("ARTHROS",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("ARTROS",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[dbnanoro$Plaintes_Autres=="douleur generalise" & dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="INFECTION"] <- "pain"
dbnanoro$clinicalpresentation[grepl("MAL ",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("MAUX ",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("MIGRAINE",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("MAUX ",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("LOMBALG",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="MAL DE DENT"] <- "dental infection"
dbnanoro$clinicalpresentation[grepl("HEMOR",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[grepl("DIABET",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "NCD"
dbnanoro$clinicalpresentation[grepl("DOULOUR",dbnanoro$DiagnosticSpecifiq_Precisé_Autr)==T] <- "pain"
dbnanoro$clinicalpresentation[dbnanoro$DiagnosticSpecifiq_Precisé_Autr=="INFECTION"&dbnanoro$DiagnosticSpecifiq_Précisé=="13"] <- "pain" # dbnanoro$Plaintes_Autres was "douleur generalise"
# for those without specific diagnosis, we deduct it from the recorded symptoms
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="2"] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&(dbnanoro$Plaintes=="1"|dbnanoro$Plaintes=="1 11"|dbnanoro$Plaintes=="1 12"|dbnanoro$Plaintes=="1 11 12")&dbnanoro$TDRPalu_Resultats!=1] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&(dbnanoro$Plaintes=="1"|dbnanoro$Plaintes=="1 11"|dbnanoro$Plaintes=="1 12"|dbnanoro$Plaintes=="1 11 12")&dbnanoro$TDRPalu_Resultats==1] <- "malaria"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&(dbnanoro$Plaintes=="1"|dbnanoro$Plaintes=="1 11"|dbnanoro$Plaintes=="1 12"|dbnanoro$Plaintes=="1 11 12")&is.na(dbnanoro$TDRPalu_Resultats)] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("plai",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("blaisure",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("blessure",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("blesse",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("bouton sur",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("boutton sur",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("bouttons sur",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("eruption cutane",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("eruption sur la peau",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("ulcere",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("brulure",dbnanoro$Plaintes_Autres)==T&grepl("1 ",dbnanoro$Plaintes)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("furoncle",dbnanoro$Plaintes_Autres)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("abces",dbnanoro$Plaintes_Autres)==T] <- "skin soft tissue infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("angine",dbnanoro$Plaintes_Autres)==T] <- "pharyngitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation) & grepl("13",dbnanoro$Plaintes)==T & grepl("palu",dbnanoro$Plaintes_Autres)==T] <- "malaria"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("8",dbnanoro$Plaintes)==T & grepl("2 ",dbnanoro$Plaintes)==F & grepl(" 3 ",dbnanoro$Plaintes)==F]<- "infectious diarrhoea"  
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("8", dbnanoro$Plaintes)==F&(grepl(" 2 ",dbnanoro$Plaintes)==T|grepl("3 ", dbnanoro$Plaintes)==T|grepl("1 3",dbnanoro$Plaintes)==T|grepl("2 3", dbnanoro$Plaintes)==T)] <- "bronchitis"# the goal here is to have the main presentation that led to AB use, not to have a comprehensive diagnostic distribution. So removed: &(is.na(dbnanoro$TDRPalu_Resultats)|dbnanoro$TDRPalu_Resultats!=1)
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("8", dbnanoro$Plaintes)==F&grepl("4", dbnanoro$Plaintes)==T] <- "pharyngitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&(grepl(" 8", dbnanoro$Plaintes)==T|grepl("8 ", dbnanoro$Plaintes)==T)&(is.na(dbnanoro$TDRPalu_Resultats)|dbnanoro$TDRPalu_Resultats!=1)] <- "infectious diarrhoea"
# lots of "plaie" remaining, but never without other symptoms indicating skin infection
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("mycos",dbnanoro$Plaintes_Autres)==T|grepl("candi",dbnanoro$Plaintes_Autres)==T|grepl("verdatre",dbnanoro$Plaintes_Autres)==T] <- "fungal infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("infection urinaire",dbnanoro$Plaintes_Autres)==T] <- "UTI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("probleme urinaire",dbnanoro$Plaintes_Autres)==T] <- "UTI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("urinaire",dbnanoro$Plaintes_Autres)==T] <- "UTI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("1 13",dbnanoro$Plaintes)==T&(is.na(dbnanoro$TDRPalu_Resultats)| dbnanoro$TDRPalu_Resultats==2)] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("1 12 13",dbnanoro$Plaintes)==T&(is.na(dbnanoro$TDRPalu_Resultats)| dbnanoro$TDRPalu_Resultats==2)] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation) & grepl("1 13",dbnanoro$Plaintes)==T & dbnanoro$TDRPalu_Resultats==1] <- "malaria"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation) & grepl("2 ",dbnanoro$Plaintes)==F & grepl("3 ",dbnanoro$Plaintes)==F & grepl("13",dbnanoro$Plaintes)==F & dbnanoro$TDRPalu_Resultats==1] <- "malaria"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes %in% c("8","1 8","1 8 11","1 8 12","8 12", "8 11","8 13","8 9", "1 8 9")] <- "infectious diarrhoea"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("plai",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("blaisure",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("blessure",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("blesse",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("bouton sur",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("boutton sur",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("bouttons sur",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("eruption cutane",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("eruption sur la peau",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("ulcere",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("brulure",dbnanoro$Plaintes_Autres)==T] <- "wound"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("cardio",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("hypertens",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("hyper tens",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("coeur",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("cœur",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("diabete",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("cardiopa",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("tension",dbnanoro$Plaintes_Autres)==T] <- "NCD"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("epilepsie",dbnanoro$Plaintes_Autres)==T] <- "NCD"
# froid is a sign that cannot be attributed to a specific syndrome or disease
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("le sexe qui gratte",dbnanoro$Plaintes_Autres)==T] <- "STI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("ecoulement uretral",dbnanoro$Plaintes_Autres)==T] <- "STI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("douleur quand il pisse",dbnanoro$Plaintes_Autres)==T] <- "STI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("eccoulement vaginale",dbnanoro$Plaintes_Autres)==T] <- "STI"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("hepatomegalie",dbnanoro$Plaintes_Autres)==T] <- "hepatomegalie"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("fievre typhoide",dbnanoro$Plaintes_Autres)==T] <- "typhoid"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("palu",dbnanoro$Plaintes_Autres)==T] <- "malaria"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("mal de dent",dbnanoro$Plaintes_Autres)==T] <- "dental infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("maux de dent",dbnanoro$Plaintes_Autres)==T] <- "dental infection"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("maux d oreille",dbnanoro$Plaintes_Autres)==T] <- "otitis media"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("maux d'oreille",dbnanoro$Plaintes_Autres)==T] <- "otitis media"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("epigastrique",dbnanoro$Plaintes_Autres)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("5",dbnanoro$Plaintes)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("9",dbnanoro$Plaintes)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("10",dbnanoro$Plaintes)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("12",dbnanoro$Plaintes)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("vertige",dbnanoro$Plaintes_Autres)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("vomit",dbnanoro$Plaintes_Autres)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("nause",dbnanoro$Plaintes_Autres)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("ventre",dbnanoro$Plaintes_Autres)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("remonte gazeux",dbnanoro$Plaintes_Autres)==T] <- "other gastrointestinal complaints"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("douleur de la gorge",dbnanoro$Plaintes_Autres)==T] <- "pharyngitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("enflure de la gorge",dbnanoro$Plaintes_Autres)==T] <- "pharyngitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("12",dbnanoro$Plaintes)==T] <- "sepsis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("3 ",dbnanoro$Plaintes)==T] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl(" 3",dbnanoro$Plaintes)==T] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="3"] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("2 ",dbnanoro$Plaintes)==T] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl(" 2",dbnanoro$Plaintes)==T] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="2"] <- "bronchitis"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$TDRPalu_Resultats==1] <- "malaria"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="6"] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="6 7"] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="6 11"] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="1 6"] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="1 6 7"] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&dbnanoro$Plaintes=="1 7"] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("1 ", dbnanoro$Plaintes)==T] <- "unexplained fever"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("douleur",dbnanoro$Plaintes_Autres)==T] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("mal au",dbnanoro$Plaintes_Autres)==T] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("mal de",dbnanoro$Plaintes_Autres)==T] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("arthrose",dbnanoro$Plaintes_Autres)==T] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("artrose",dbnanoro$Plaintes_Autres)==T] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&grepl("maux",dbnanoro$Plaintes_Autres)==T] <- "pain"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&(dbnanoro$DiagnosticSpecifiq_Précisé=="13"|dbnanoro$Plaintes=="13"|dbnanoro$Plaintes=="7 13"|dbnanoro$Plaintes=="7")] <- "other"
dbnanoro$clinicalpresentation[is.na(dbnanoro$clinicalpresentation)&(dbnanoro$Plaintes=="11"|dbnanoro$Plaintes=="7 11")] <- "other"
# dbnanoro$clinicalpresentation[dbnanoro$clinicalpresentation=="pain"] <- "other" # bring presentations together that have no importance in this analysis
dbnanoro$clinicalpresentation[dbnanoro$clinicalpresentation=="gastric ulcer"] <- "other"


# create a database with one observation per patient
ab_patient <- dbnanoro %>%
  group_by(PARENT_KEY) %>%
  summarise(access = sum(access), watch = sum(watch), reserve = sum(reserve))
patientdata <- dbnanoro %>% # a bit simplified by removing some variables
  select(PARENT_KEY, providertype, as, ActeurdeSanté2, clinicalpresentation, ageyears, Sexe, Residence_Patients, Autre_Residence_Patients, Niveau_Education_Patients, NiveauEducation_Accomp,  
         Hospi_Observation, Hospi_Observation_DateEntrée, Plaintes, Plaintes_Autres, TDR_Palu_Dispo, 
         TDRPalu_Resultats, AutreExamLabo_Dispo, DiagnosticSpecifiq_Dispo, DiagnosticSpecifiq_Précisé, DiagnosticSpecifiq_Precisé_Autr,
         OrdonnanceFormelle_Dispo, PrescriptioOrale_NonMedical, Pourquoi_cetActeurdeSanté, Pourquoi_cetActeurdeSanté_Autre, achatMedicnbr_medicamentAchat, 
         Nom_mdts_pris_Maison)
# remove duplicates per patient (one observation per medicine used, so several lines now per patient)
dups = which(duplicated(patientdata%>%select('PARENT_KEY', 'ageyears','Sexe','Residence_Patients')))
patientdata <- patientdata %>% filter(!row.names(patientdata) %in% dups)
# merge simplified patient data with AB data
patientnanoro <- merge(patientdata,ab_patient, by = "PARENT_KEY", all = T)

# remove visits without any complaint or illness
patientnanoro <- patientnanoro %>% filter(is.na(Plaintes_Autres)==T|(Plaintes_Autres!="aucun"&Plaintes_Autres!="0"))

#### export databases ####
# anonimyze dataframe and remove unnecessary raw data for the clinical presentation analysis
patientnanoroshort <- patientnanoro %>% select(as, providertype, ActeurdeSanté2, clinicalpresentation, access, watch, reserve)

# antibiotic database
write.table(dbnanoro, file = 'dbnanoro.txt')
# patient database
write.table(patientnanoro, file = 'patientnanoro.txt')
write.table(patientnanoroshort, file = 'patientnanoroshort.txt')

#### import cleaned patient database ####
patientnanoro <- read.csv("patientnanoroshort.txt", sep="")

# variable for antibiotic use
patientnanoro$antibioticuse <- "no"
patientnanoro$antibioticuse[patientnanoro$access>0|patientnanoro$watch>0|patientnanoro$reserve>0] <- "yes"
patientnanoro$antibioticusenum <- 0
patientnanoro$antibioticusenum[patientnanoro$access>0|patientnanoro$watch>0|patientnanoro$reserve>0] <- 1

# variable for watch use
patientnanoro$watchuse <- "no"
patientnanoro$watchuse[patientnanoro$watch>0] <- "yes"
patientnanoro$watchusenum <- 0
patientnanoro$watchusenum[patientnanoro$watch>0] <- 1

#### Supp Table 1. Visits per provider ####
# number of interviews per providertype per aire de santé
table(patientnanoro$ActeurdeSanté2, patientnanoro$as)

#### Supp Table 2. (Watch) AB prevalence per visit/patient, by provider & by clinical presentation in primary care ####
patientnanoro_primary <- patientnanoro %>%
  filter(providertype!="hospital" & providertype!="trad healer")
patientnanoro_primary$providertype_simpl <- "selfmedication"
patientnanoro_primary$providertype_simpl[patientnanoro_primary$providertype=="healthcentre"] <- "healthcentre"

# AB use prevalence by provider & by clinical presentation
ABuse_byclinicalpresentation_nanoro <- patientnanoro_primary %>%
  group_by(providertype_simpl, clinicalpresentation) %>%
  summarise(total = n(), usedAB = sum(antibioticusenum), usedWatchAB = sum(watchusenum))
ABuse_byclinicalpresentation_nanoro$pctAB <- round(ABuse_byclinicalpresentation_nanoro$usedAB/ABuse_byclinicalpresentation_nanoro$total,3)*100
ABuse_byclinicalpresentation_nanoro$pctWatch <- round(ABuse_byclinicalpresentation_nanoro$usedWatchAB/ABuse_byclinicalpresentation_nanoro$total,3)*100
ABuse_byclinicalpresentation_nanoro
ABuse_byclinicalpresentation_nanoro$site <- "Nanoro, Burkina Faso"

# export
write.table(ABuse_byclinicalpresentation_nanoro, "ABuse_byclinicalpresentation_nanoro.txt")


#### KISANTU, DR CONGO - only Kisantu b/c for Kimpese symptoms missing ####
# # select part of the total DRC database
# dbkisantu <- db %>% 
#   filter(district=="Kisantu" & (providertype=="healthcentre"|providertype=="privateclinic"|providertype=="privatepharmacy")) %>%
#   select(KEY, interviewdate, providertype, illness, illness_spec, illness_other, symptoms, symptoms_other, diag_test, diag_test_other, diag_spec, diag_spec_other, abgeneric, Class, Category)
# # save the Kisantu primary care dataframe 
# write.table(dbkisantu, "dbkisantu.txt")
#### import & clean database (antibiotic use already cleaned; 1 observation per patient or antibiotic used) ####
dbkisantu <- read.csv("dbkisantu.txt", sep="")

# rename providertype to only 2 categories
dbkisantu$providertype_simpl <- "healthcentre"
dbkisantu$providertype_simpl[dbkisantu$providertype=="privatepharmacy"] <- "selfmedication"

# create a variable to be able to sum up the number of AWaRe antibiotics per patient
dbkisantu$access <- 0
dbkisantu$access[dbkisantu$Category=="access"] <- 1
dbkisantu$watch <- 0
dbkisantu$watch[dbkisantu$Category=="watch"] <- 1
dbkisantu$reserve <- 0
dbkisantu$reserve[dbkisantu$Category=="reserve"] <- 1

# clean clinical signs & symptoms
table(dbkisantu$illness, useNA = "always") #2105 acute illness episodes out of 2263
dbkisantu$illness_other <- tolower(dbkisantu$illness_other)
dbkisantu$diag_spec_other <- tolower(dbkisantu$diag_spec_other)
dbkisantu$illness_spec[grepl("vomisse", dbkisantu$illness_other)==TRUE] <- "nausea_vomiting"
dbkisantu$illness_spec[grepl("plaie", dbkisantu$illness_other)==TRUE] <- "wound"
dbkisantu$illness_spec[grepl("saign", dbkisantu$illness_other)==TRUE] <- "wound"
dbkisantu$illness_spec[grepl("tumef", dbkisantu$illness_other)==TRUE] <- "swelling"
dbkisantu$illness_spec[grepl("tumã©faction", dbkisantu$illness_other)==TRUE] <- "swelling"
dbkisantu$illness_spec[grepl("diarr", dbkisantu$illness_other)==TRUE] <- "infectious diarrhoea"
dbkisantu$illness_spec[grepl("toux", dbkisantu$illness_other)==TRUE] <- "cough"
dbkisantu$illness_spec[grepl("rhunorrhã", dbkisantu$illness_other)==TRUE] <- "bronchitis"
dbkisantu$illness_spec[grepl("lombalgie", dbkisantu$illness_other)==TRUE] <- "back pain"
dbkisantu$illness_spec[grepl("douleur abdo", dbkisantu$illness_other)==TRUE] <- "abdominal pain"
dbkisantu$illness_spec[grepl("hypogastralgie", dbkisantu$illness_other)==TRUE] <- "abdominal pain"
dbkisantu$illness_spec[grepl("myctalgie", dbkisantu$illness_other)==TRUE] <- "urinary pain"
dbkisantu$illness_spec[grepl("mictalg", dbkisantu$illness_other)==TRUE] <- "urinary pain"
dbkisantu$illness_spec[grepl("douleu", dbkisantu$illness_other)==TRUE & dbkisantu$illness_spec=="other"] <- "pain"
dbkisantu$illness_spec[grepl("fiã¨vre", dbkisantu$illness_other)==TRUE] <- "fever"
dbkisantu$illness_spec[grepl("fievre", dbkisantu$illness_other)==TRUE] <- "fever"
dbkisantu$illness_spec[grepl("eruption cut", dbkisantu$illness_other)==TRUE] <- "skin soft tissue infection"
dbkisantu$illness_spec[grepl("pruri", dbkisantu$illness_other)==TRUE] <- "skin soft tissue infection"
dbkisantu$illness_spec[grepl("eruptions cut", dbkisantu$illness_other)==TRUE] <- "skin soft tissue infection"
dbkisantu$illness_spec[grepl(" ã‰ruption cutanã©e", dbkisantu$illness_other)==TRUE] <- "skin soft tissue infection"
dbkisantu$illness_spec[grepl(" abces", dbkisantu$illness_other)==TRUE] <- "skin soft tissue infection"
dbkisantu$illness_spec[grepl(" ã‰ruption cutanã©e", dbkisantu$illness_other)==TRUE] <- "skin soft tissue infection"
dbkisantu$illness_spec[grepl("rhume", dbkisantu$illness_other)==TRUE] <- "bronchitis"
dbkisantu$illness_spec[grepl("rhume", dbkisantu$illness_other)==TRUE] <- "bronchitis"
dbkisantu$illness_spec[grepl("accident moto", dbkisantu$illness_other)==TRUE] <- "trauma"
dbkisantu$illness_spec[grepl("solution de cont", dbkisantu$illness_other)==TRUE] <- "wound"
dbkisantu$illness_spec[grepl("selles glairosanguinolante", dbkisantu$illness_other)==TRUE] <- "bloody diarrhoea/dysentery"
dbkisantu$illness_spec[grepl("amibiase", dbkisantu$illness_other)==TRUE] <- "parasitic infection"
dbkisantu$illness_spec[grepl("angine", dbkisantu$illness_other)==TRUE] <- "pharyngitis"
dbkisantu$illness_spec[grepl("anã©mie", dbkisantu$illness_other)==TRUE] <- "anaemia"
dbkisantu$illness_spec[grepl("anorexie", dbkisantu$illness_other)==TRUE] <- "anorexia"
dbkisantu$illness_spec[grepl("apgar deprime", dbkisantu$illness_other)==TRUE] <- "unknown cause"
dbkisantu$illness_spec[grepl("appendicite sub aigue", dbkisantu$illness_other)==TRUE] <- "appendicitis"
dbkisantu$illness_spec[grepl("artralgie", dbkisantu$illness_other)==TRUE] <- "pain"
dbkisantu$illness_spec[grepl("arthralgie", dbkisantu$illness_other)==TRUE] <- "pain"
dbkisantu$illness_spec[grepl("brã»lure", dbkisantu$illness_other)==TRUE] <- "wound"
dbkisantu$illness_spec[grepl("brulure", dbkisantu$illness_other)==TRUE] <- "wound"
dbkisantu$illness_spec[grepl("rhumatism", dbkisantu$illness_other)==TRUE] <- "pain"
dbkisantu$illness_spec[grepl("colique abdominale", dbkisantu$illness_other)==TRUE] <- "abdominal pain"
dbkisantu$illness_spec[grepl("convulsion", dbkisantu$illness_other)==TRUE] <- "convulsions"
dbkisantu$illness_spec[grepl("courbature", dbkisantu$illness_other)==TRUE] <- "pain"

# assign clinical presentations from the WHO AWaRe book to syndroms/symptoms presented by patients
dbkisantu$clinicalpresentation[dbkisantu$diag_spec=="bronchitis"] <- "bronchitis"
dbkisantu$clinicalpresentation[dbkisantu$diag_spec=="bronchiolitis"] <- "bronchitis"
dbkisantu$clinicalpresentation[dbkisantu$diag_spec=="malaria"] <- "malaria"
dbkisantu$clinicalpresentation[dbkisantu$diag_spec=="gastroenteritis"] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[dbkisantu$diag_spec=="pneumonia"] <- "pneumonia"
dbkisantu$clinicalpresentation[dbkisantu$diag_spec=="typhoid"] <- "typhoid"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="bronchitis"] <- "bronchitis"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="cough"] <- "bronchitis"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="skin soft tissue infection"] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="pharyngitis"] <- "pharyngitis"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="infectious diarrhoea"] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="bloody diarrhoea/dysentery"] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="malaria"] <- "malaria"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="pneumonia"] <- "pneumonia"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="trauma"] <- "wound"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="wound"] <- "wound"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="pain"] <- "pain"
dbkisantu$clinicalpresentation[dbkisantu$illness_spec=="headache"&grepl("fever", dbkisantu$symptoms)==F] <- "pain"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("sepsis", dbkisantu$diag_spec_other)==T&grepl("fever", dbkisantu$symptoms)==T] <- "sepsis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("diarrhoea", dbkisantu$symptoms)==T&grepl("fever", dbkisantu$symptoms)==T] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("diarrhoea", dbkisantu$symptoms)==T&grepl("vomiting", dbkisantu$symptoms)==T] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("diarrhoea", dbkisantu$symptoms)==T&grepl("abdo_pain", dbkisantu$symptoms)==T] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("diarrhoea", dbkisantu$symptoms)==T&grepl("nausea", dbkisantu$symptoms)==T] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("abdo_pain", dbkisantu$symptoms)==T&grepl("vomiting", dbkisantu$symptoms)==T&grepl("fever", dbkisantu$symptoms)==T] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&dbkisantu$symptoms=="diarrhoea"] <- "infectious diarrhoea"
dbkisantu$clinicalpresentation[grepl("Carrie dentaire", dbkisantu$symptoms_other)==T|grepl("Carident", dbkisantu$symptoms_other)==T|grepl("carie", dbkisantu$diag_spec_other)==T|grepl("carident", dbkisantu$diag_spec_other)==T] <- "dental infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("Carrie dentaire", dbkisantu$symptoms_other)==T] <- "dental infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("palu", dbkisantu$diag_spec_other)==T] <- "malaria"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("staphylo", dbkisantu$diag_spec_other)==T] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("abcã¨s", dbkisantu$diag_spec_other)==T] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("abces", dbkisantu$diag_spec_other)==T] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("streptoc", dbkisantu$diag_spec_other)==T] <- "pneumonia"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("pneumopathie a investiguã©", dbkisantu$diag_spec_other)==T] <- "pneumonia"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("grippe", dbkisantu$diag_spec_other)==T] <- "bronchitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("grippal", dbkisantu$diag_spec_other)==T] <- "bronchitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("rhume", dbkisantu$diag_spec_other)==T] <- "bronchitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("angine", dbkisantu$diag_spec_other)==T] <- "pharyngitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("pharyng", dbkisantu$diag_spec_other)==T] <- "pharyngitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("syndrome infectieux", dbkisantu$diag_spec_other)==T&grepl("abdo_pain",dbkisantu$symptoms)==F] <- "unexplained fever"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("syndrã´me infectieux", dbkisantu$diag_spec_other)==T&grepl("abdo_pain",dbkisantu$symptoms)==F] <- "unexplained fever"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("sexuelle", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infections urogenit", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection urogã©nital", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection urogenital", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection uro genital", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infectiongenital", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection uro-geni", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection gã©nitale", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("abces gã©nital", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("ist", dbkisantu$diag_spec_other)==T] <- "STI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&dbkisantu$diag_spec_other=="ulcã¨re"] <- "wound" # no other sympt
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&dbkisantu$diag_spec_other=="traumatisme crã¢nien"] <- "wound" # no other sympt
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&dbkisantu$diag_spec_other=="plaie infectã©e"] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&dbkisantu$diag_spec_other=="plaie et gonflement du visage"] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("plaie", dbkisantu$diag_spec_other)==T&grepl("fever", dbkisantu$symptoms)==F] <- "wound" # no other sympt
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("plaie", dbkisantu$diag_spec_other)==T&grepl("fever", dbkisantu$symptoms)==T] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&dbkisantu$diag_spec_other=="plaie superficielle"] <- "wound" # no other sympt
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection urina", dbkisantu$diag_spec_other)==T] <- "UTI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infections urina", dbkisantu$diag_spec_other)==T] <- "UTI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("infection urun", dbkisantu$diag_spec_other)==T] <- "UTI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("information urinaire", dbkisantu$diag_spec_other)==T] <- "UTI"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("uruli", dbkisantu$illness_spec)==T] <- "buruli ulcer"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("uruli", dbkisantu$diag_spec_other)==T] <- "buruli ulcer"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("mbas", dbkisantu$diag_spec_other)==T] <- "buruli ulcer"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("paldism", dbkisantu$diag_spec_other)==T] <- "malaria"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("paldusm", dbkisantu$diag_spec_other)==T] <- "malaria"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("otite", dbkisantu$diag_spec_other)==T] <- "otitis media"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("mycos", dbkisantu$diag_spec_other)==T] <- "fungal infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("levuros", dbkisantu$diag_spec_other)==T] <- "fungal infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fiã¨vre thyp", dbkisantu$diag_spec_other)==T] <- "typhoid"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fiã¨vre typhoã¯de", dbkisantu$diag_spec_other)==T] <- "typhoid"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("cough", dbkisantu$symptoms)==T&grepl("fever", dbkisantu$symptoms)==T] <- "bronchitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("cough", dbkisantu$symptoms)==T&grepl("headache", dbkisantu$symptoms)==T] <- "bronchitis"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("wound", dbkisantu$symptoms)==T&grepl("fever", dbkisantu$symptoms)==T] <- "skin soft tissue infection"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("tension", dbkisantu$diag_spec_other)==T] <- "NCD"
# do another check of remaining diag_spec_other, then replace remaning fever without malaria by unexplained fever & GI complaints
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fever", dbkisantu$symptoms)==T&grepl("diarrhoea", dbkisantu$symptoms)==F&dbkisantu$diag_spec!="malaria"] <- "unexplained fever"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fever", dbkisantu$symptoms)==F&grepl("vomiting", dbkisantu$symptoms)==T&dbkisantu$diag_spec!="malaria"] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fever", dbkisantu$symptoms)==F&grepl("abdo_pain", dbkisantu$symptoms)==T&dbkisantu$diag_spec!="malaria"] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fever", dbkisantu$symptoms)==F&grepl("abdo_pain", dbkisantu$symptoms)==T&dbkisantu$diag_spec!="malaria"] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("abdominal p", dbkisantu$illness_spec)==T] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("vomissement", dbkisantu$symptoms_other)==T] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("ouleur abdo", dbkisantu$symptoms_other)==T] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("vomit", dbkisantu$illness_spec)==T] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&(grepl("epigastr", dbkisantu$illness_other)==T|grepl("epigastr", dbkisantu$symptoms_other)==T)] <- "other GI complaints"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("fever", dbkisantu$symptoms)==F&grepl("plaie", dbkisantu$symptoms_other)==T&dbkisantu$diag_spec!="malaria"] <- "wound"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("chronic", dbkisantu$illness)==T] <- "NCD"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&(grepl("noillness", dbkisantu$illness)==T|grepl("nosymptoms", dbkisantu$symptoms)==T)] <- "no illness"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&(grepl("rash", dbkisantu$illness_spec)==T|grepl("rash", dbkisantu$symptoms)==T)] <- "other"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&(grepl("dermatose", dbkisantu$diag_spec_other)==T|grepl("dermati", dbkisantu$diag_spec_other)==T)] <- "other"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&(grepl("skinprob", dbkisantu$illness_spec)==T|grepl("Eruption cut", dbkisantu$symptoms_other)==T)] <- "other"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&grepl("Plaie", dbkisantu$symptoms_other)==T] <- "wound"
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)==T&(grepl("ouleur", dbkisantu$symptoms_other)==T|grepl("arthrit", dbkisantu$diag_spec_other)==T)] <- "pain"
missingscp <- dbkisantu %>% filter(is.na(clinicalpresentation)) # checked them one by one. some fatigue, some myalgia, some other, not worth specific category
dbkisantu$clinicalpresentation[is.na(dbkisantu$clinicalpresentation)] <- "other"
# summary table
table(dbkisantu$clinicalpresentation,dbkisantu$providertype_simpl, useNA = "always")
# classify buruli ulcers as wounds of skin infections, depending on whether other symptoms pointing at systemic infection 
dbkisantu$clinicalpresentation[dbkisantu$clinicalpresentation=="buruli ulcer"] <- "wound"
dbkisantu$clinicalpresentation[dbkisantu$clinicalpresentation=="buruli ulcer"&grepl("fever",dbkisantu$symptoms)==T] <- "skin soft tissue infection"

# create a database with a summary of AWaRe antibiotic use per patient
ab_patient_kisantu <- dbkisantu %>%
  group_by(KEY) %>%
  summarise(access = sum(access), watch = sum(watch), reserve = sum(reserve))

# create simplified patient data without antibiotic data and remove duplicates
patientkisantu <- select(dbkisantu, -abgeneric, -Class, -Category, -access, -watch, -reserve)
# remove duplicates per patient (one observation per medicine used, so several lines now per patient)
dups = which(duplicated(patientkisantu%>%select('KEY')))
patientkisantu <- patientkisantu %>% filter(!row.names(patientkisantu) %in% dups)

# merge with AB summary per patient data 
patientkisantu <- merge(patientkisantu,ab_patient_kisantu, by = "KEY", all.x = T)

# variable for antibiotic use
patientkisantu$antibioticuse <- "no"
patientkisantu$antibioticuse[patientkisantu$access>0|patientkisantu$watch>0|patientkisantu$reserve>0] <- "yes"
patientkisantu$antibioticusenum <- 0
patientkisantu$antibioticusenum[patientkisantu$access>0|patientkisantu$watch>0|patientkisantu$reserve>0] <- 1

# variable for watch use
patientkisantu$watchuse <- "no"
patientkisantu$watchuse[patientkisantu$watch>0] <- "yes"
patientkisantu$watchusenum <- 0
patientkisantu$watchusenum[patientkisantu$watch>0] <- 1

#### Supp Table 2. (Watch) AB prevalence per visit/patient, by provider & by clinical presentation in primary care ####
# AB use prevalence by provider & by clinical presentation
ABuse_byclinicalpresentation_kisantu <- patientkisantu %>%
  group_by(providertype_simpl, clinicalpresentation) %>%
  summarise(total = n(), usedAB = sum(antibioticusenum), usedWatchAB = sum(watchusenum))
ABuse_byclinicalpresentation_kisantu$pctAB <- round(ABuse_byclinicalpresentation_kisantu$usedAB/ABuse_byclinicalpresentation_kisantu$total,3)*100
ABuse_byclinicalpresentation_kisantu$pctWatch <- round(ABuse_byclinicalpresentation_kisantu$usedWatchAB/ABuse_byclinicalpresentation_kisantu$total,3)*100
ABuse_byclinicalpresentation_kisantu$site <- "Kisantu, DR Congo"
# export
write.table(ABuse_byclinicalpresentation_kisantu, "ABuse_byclinicalpresentation_kisantu.txt")

#### BOTH STUDY SITES COMBINED to make Supp Figure with distribution of clinical presentations and corresponding antibiotic use ####
# merge data from both countries
tabletwosites <- rbind(ABuse_byclinicalpresentation_nanoro, ABuse_byclinicalpresentation_kisantu)
# single variable for facet
tabletwosites$provider_site <- paste(tabletwosites$providertype_simpl, tabletwosites$site)
# rename clinical presentation
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="pneumonia"] <-"community-acquired pneumonia"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="skin soft tissue infection"] <- "skin/soft tissue infection"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="UTI"] <- "lower urinary tract infection"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="infectious diarrhoea"] <- "infectious gastroenteritis"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="other gastrointestinal complaints"] <- "other gastrointestinal symptoms"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="other GI complaints"] <- "other gastrointestinal symptoms"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="STI"] <- "sexually transmitted infection"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="NCD"] <- "non-communicable disease"
tabletwosites$clinicalpresentation[tabletwosites$clinicalpresentation=="wound"] <- "wound without signs of infection"
# make a long table with AWaRe by clinical presentation
tabletwosites$watch <- tabletwosites$usedWatchAB
tabletwosites$access <- tabletwosites$total - tabletwosites$watch
tabletwosites$noAB <- tabletwosites$total - tabletwosites$usedAB
# convert the dataframe from wide to long format
tabletwosites$pres_provider_site <- paste(tabletwosites$clinicalpresentation,";",tabletwosites$provider_site)
tabletwosites_small <- tabletwosites %>% select(pres_provider_site, noAB, access, watch)
write.table(tabletwosites_small, "tabletwosites_small.txt") # Oddly, I could not remove the providerype column, so had to do it manually ouside R
tabletwosites_small <- read_excel("tabletwosites_small.xlsx") # import the df again without column providertype
tabletwosites_long <- pivot_longer(tabletwosites_small, cols = -pres_provider_site, names_to = "AWaRe", values_to = "n")
# break the single identifying column again in two
tabletwosites_long <- separate(tabletwosites_long, pres_provider_site, into = c("clinicalpresentation", "provider_site"), sep = ";")

# create a bar chart with proportions of clinical presentations
clinpresdistribution_supposed <- ggplot(tabletwosites, aes(x=factor(provider_site, levels = c("selfmedication Kisantu, DR Congo", "healthcentre Kisantu, DR Congo", "selfmedication Nanoro, Burkina Faso", "healthcentre Nanoro, Burkina Faso" )), 
                                                           y=total, fill=factor(clinicalpresentation, 
                                                                                levels = c("other", "pain","wound without signs of infection", "non-communicable disease","fungal infection","malaria","unexplained fever","sexually transmitted infection",
                                                                                           "other gastrointestinal symptoms","sepsis","typhoid","infectious gastroenteritis**", "lower urinary tract infection","skin/soft tissue infection",
                                                                                           "community-acquired pneumonia","dental infection","pharyngitis","otitis media","bronchitis")))) +
  geom_bar(position = "fill",stat = "identity", color = "black") +
  # coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL,ncol=1)) +
  theme(axis.text.x = element_text (angle = 90)) +
  scale_fill_manual(values=c("white", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "white","white", "lightgrey", "yellow","yellow","yellow", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "lightgrey"))
clinpresdistribution
ggsave(clinpresdistribution, filename = "clinpresdistribution.jpg",  width = 5, height = 7, bg = "white")

# bar chart with actual AB use per presentation
clinpresdistribution_supposed <- ggplot(tabletwosites, aes(x=factor(provider_site, levels = c("selfmedication Kisantu, DR Congo", "healthcentre Kisantu, DR Congo", "selfmedication Nanoro, Burkina Faso", "healthcentre Nanoro, Burkina Faso" )), 
                                                           y=total, fill=factor(clinicalpresentation, 
                                                                                levels = c("other", "pain","wound without signs of infection", "non-communicable disease","fungal infection","malaria","unexplained fever","sexually transmitted infection",
                                                                                           "other gastrointestinal symptoms","sepsis","typhoid","infectious gastroenteritis**", "lower urinary tract infection","skin/soft tissue infection",
                                                                                           "community-acquired pneumonia","dental infection","pharyngitis","otitis media","bronchitis")))) +
  geom_bar(position = "fill",stat = "identity", color = "black") +
  # coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="",y="") +
  guides(fill=guide_legend(title=NULL,ncol=1)) +
  theme(axis.text.x = element_text (angle = 90)) +
  scale_fill_manual(values=c("white", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "white","white", "lightgrey", "yellow","yellow","yellow", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "lightgrey"))
clinpresdistribution


