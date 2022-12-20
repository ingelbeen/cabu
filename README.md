# cabu
Community-wide antibiotic use (CABU) studies in the DR Congo and Burkina Faso. This repository contains the study protocol, questionnaires, data collected for this study and analysis scripts.
- The zip file contains all raw data for both districts in DR Congo, of the patient exit interviews in Kisantu and Kimpese and of healthcare utilisation household surveys in Kimpese. For healthcare utilisation in Kisantu, data of the SETA study was used, which will be published elsewhere
- The cleaned patient exit survey databases are dbkisantu.txt (de-identified patient with antibiotic use database, Kisantu, DR Congo, Nov 2019) and patientnanoro.txt (anonymized patient with anitibiotic use database, Nanoro, Burkina Faso, Nov-Dec 2021)
- The questionnaires are in .xlsx format, allowing importation in Open Data Kit
- 3 analysis .R scripts: 
  * the HCU script cleans and analyses the household survey data on healthcare utilisation
  * the CABU script cleans and analyses the patient exit surveys on antibiotic use following a healthcare visit, weighs this using the output of the HCU file
  
  (published http://doi.org/10.1016/j.cmi.2022.04.002) 
  * antibiotic use by clinical presentation, to estimate the percentage of (Watch) antibiotic use that could be prevented when applying the WHO AWaRe antibiotic book
  
  (submitted LID)
