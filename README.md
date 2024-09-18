# Burden_estimation_public  
  
This is a colloborative project between the South African Centre for Epidemiological Modelling and Analysis (SACEMA) and the National Institute for Communicable Diseases (NICD) aimed at constructing the burden pyramid for COVID-19 during the 5 waves in South Africa.  
 
## Title of project  
The burden of SARS-CoV-2 Infection and Severe Illness in South Africa March 2020-August 2022: A synthesis of epidemiological data  
  
## Team members  
 - Stefano Tempia (Center for Respiratory Diseases and Meningitis, National Institute for Communicable Diseases), tempias@who.int
 - Cheryl Cohen (Center for Respiratory Diseases and Meningitis, National Institute for Communicable Diseases), cherylc@nicd.ac.za
 - Juliet Pulliam (SACEMA)  
 - Larisse Bolton (SACEMA), lbolton@sun.ac.za  
 - Sibongile Walaza (Center for Respiratory Diseases and Meningitis, National Institute for Communicable Diseases), sibongilew@nicd.ac.za

## Data contributors/custodians 
 - Waasila Jassat (DATCOV data custodian), waasilaj@genesis-analytics.com
 - Kaiyuan Sun (PHIRST-C attack rates custodian), kaiyuan.sun@nih.gov
 - Debbie Bradshaw and Rob Dorrington (Excess deaths data custodians), rob.dorrington@uct.ac.za
 - Nicole Wolter (HUS data), nicolew@nicd.ac.za

## Collaborators  
 - Jackie Kleynhans (Center for Respiratory Diseases and Meningitis, National Institute for Communicable Diseases)
 - Neil Martinson (Perinatal HIV Research Unit, University of the Witwatersrand, Johannesburg, South Africa and Johns Hopkins University Center for TB Research)
 - Anne von Gottberg (Center for Respiratory Diseases and Meningitis, National Institute for Communicable Diseases) 
   
## Overall objectives  
To estimate the national infection and disease burden of SARS-CoV-2 in South Africa during the first 5 waves 
### Specific objectives  
- To estimate within the following syndromes: Non-severe, non-fatal illness (asymptomatic and mild illness), Severe Illness (hospitalisation) and Fatal illnesss  
- To estimate within the following categories: Medically attended and non-medically attended  
- To estimate within the decided age groups  

## Data sources  
1. Individual-level SARS-CoV-2 public and private hospital admissions and subsequent in-hospital deaths from the DATCOV national surveillance programme for SARS-CoV-2 hospitalisations    
2. South African Medical Research Council (SAMRC) estimates of excess deaths by sex and 5-year age bands   
3. Healthcare utilisation surveys (HUS) conducted in three communities in three provinces  
4. Age-stratified infection attack rates by SARS-CoV-2 wave obtained from the Prospective Household study of SARS-CoV-2, Influenza and Respiratory Syncytial virus community burden, Transmission dynamics and viral interaction in South Africa – Coronavirus disease 2019 (PHIRST-C) cohort  
5. Mid-year population estimates
  
## Software  
* RStudio 2023.06.0+421 for Windows  
* R Statistical software version 4.2.3 
### Dependencies  
- tidyverse (2.0.0)
- lubridate (1.9.3)   
- openxlsx (4.2.5.2)      
- stringr (1.5.1)  
- janitor (2.2.0)  
- data.table (1.15.4) 
### Scripts  
- CovidSourceCountry.R: This contains all the necessary inputs and function calls for all analysis
- CountryPopulation.R: Generate country population estimates overall, stratified by age and gender. (**inputs:** data_covid.RDS, Country projection by population group, sex and age (2002-2022)_LB.xlsx)
- CovidWaves.R: This script determines when waves and peaks are found and interpolates population estimates (**inputs:** data_datcov.RDS, data_covid.RDS, Total_excess.xlsx, Blood_draws.xlsx, **outputs:** peaks definitions and wave definitions)
- PopulationEstimates.R, PopulationEstimatesAdmissions.R, PopulationEstimatesDeaths.R: estimate the population at risk at peak of admissions and deaths (**inputs:** peaks definitions and wave definitions overall, admissions and deaths)
- WaveEstimates.R: This script extracts the hospitalisations and deaths per wave of infection
- MedAttendBurden.R: This script calculates the medically attended burden per level of severity (**inputs:** admission and death data per wave)
- SAMRC_excess.R: This script adjusts excess deaths according to assumptions and percentage attributable to COVID-19 and combines it with corresponding DATCOV deaths (**input:** SAMRC_excess.xlsx, wave death data, percentage attributable to COVID-19)
- NonMedBurden.R: This script calculates the non-medically attended burden per level of severity (**inputs:** medically attended burden, healthcare utilisation, excess and datcov deaths combined)
- InfectionRates.R: In this script the infection related estimates are generated, such as the IFR and number of infections (**inputs:** medically and non-medically attended burden estimates, PHIRST_AT.xlsx)
- confidence_intervals_strat.R: This script calculates the bootstrapped confidence intervals for medically and non-medically attended burden estimates
- BurdenBoot.R: Generate bootstrapped admission and death data per wave (**inputs:** DATCOV admission and death data)
- BurdenCI.R: Generate bootstrapped estimates for medically and non-medically attended burden estimates.  Included are the scripts for: MedAttendBurdenCItotal.R, MedAttendBurdenCI.R, NonMedBurdenCI.R, NonMedBurdenCIstrat.R
- BurdenPlotsUpd.R: Generates figures for medically and non-medically attended severe and fatal illness

  **For any technical queries regarding the analysis code or for comments and suggestions, please contact Larisse Bolton at lbolton@sun.ac.za**

