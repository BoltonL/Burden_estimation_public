#This script calculated the medically attended burden per level of severity: mild, severe, fatal

medically_burden_ci <- function(data_in_strat1,data_in_strat2,k,x_strat,y_strat){
  
  datcov_select_admit <- data_in_strat1
  datcov_select_death <- data_in_strat2
  wave_pop_est_strat_admit_age <- x_strat
  wave_pop_est_strat_death_age <- y_strat
  
      ##STRATIFIED BY AGE ONLY
      ##SEVERE HOSPITALISED
      # Number of patients admitted (total)
  patients_admit_age <- datcov_select_admit#datcov_select[datcov_select$AdmissionReason != "NA",]
  n_patients_admit_age <- patients_admit_age %>% #group by age and count
    group_by(age.group) %>%
    count(name = "age.patients.admit1")
  patients_known_admit_age <- patients_admit_age %>%
    filter(!is.na(AdmissionReason))
  n_patients_known_admit_strat_age <- patients_known_admit_age %>%
    group_by(age.group) %>%
    count(name = "age.patients.admit.known1")
  # Number of patients admitted for COVID-19
  patients_covid_admit_age <- patients_admit_age %>%
    filter(AdmissionReason ==  "COVID-19 symptoms")
  n_patients_covid_admit_age <- patients_covid_admit_age %>% #group by age and count
    group_by(age.group) %>%
    count(name = "age.covid.patients.admit1")
  admissions_pre_age <- left_join(x = n_patients_admit_age, y = n_patients_known_admit_strat_age, by = "age.group")
  admissions_age <- left_join(x = admissions_pre_age, y = n_patients_covid_admit_age, by = "age.group")
  admissions_age <- admissions_age %>%
    mutate(prop.covid.admit_age = age.covid.patients.admit1/age.patients.admit.known1)#proportion of hospitalisations due to covid
  # Number of COVID deaths in hospital (total - irrespective of diagnosis)
  deaths_admit_age <- datcov_select_death #DIED = COVID-19 DEATH 
  n_deaths_admit_age <- deaths_admit_age %>% #group by age and count
    group_by(age.group) %>%
    count(name = "age.covid.deaths1")
  #Incidence rate for COVID-19 hospitalisation
  incidence_severe_age <- left_join(x = admissions_age, y = n_deaths_admit_age, by = "age.group")
  incidence_severe_age <- incidence_severe_age %>%
    mutate(age.covid.deaths1 = ifelse(is.na(age.covid.deaths1),0,age.covid.deaths1),
           nr.admit.ndcovid_age = age.patients.admit1 - age.covid.deaths1, #remove all covid deaths from admissions - mutually exclusive
           nr.covid_age = prop.covid.admit_age * nr.admit.ndcovid_age) #now calculate what nr of covid hospitalisations would be
  
  incidence_severe_age <- left_join(x = incidence_severe_age, y = wave_pop_est_strat_admit_age[,c("age.group","age_pop_estimate_at_peak")], by = "age.group") #add in population estimates
  incidence_severe_age <- incidence_severe_age %>%
    mutate(incidence_hosp_age = (nr.covid_age/age_pop_estimate_at_peak)*100000) %>%
    mutate(age.group = factor(age.group, levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                                                    "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")))
  ## FATAL IN HOSPITAL
  # case fatality ratio from those that were COVID-19 admissions:
  fatality_covid_age <- datcov_select_death %>% #number of COVID-19 deaths from COVID-19 admissions
    filter(AdmissionReason ==  "COVID-19 symptoms") %>%
    group_by(age.group) %>%
    count(Discharge.Status, name = "fatalities1")
  
  hosp_fatality_covid_age <- full_join(x = fatality_covid_age, y = n_patients_covid_admit_age, by = "age.group")
  hosp_fatality_covid_age <- hosp_fatality_covid_age %>%  #case fatality ratio
    arrange(age.group) %>%
    mutate(Discharge.Status = as.character(Discharge.Status)) %>%
    mutate(Discharge.Status = ifelse(is.na(Discharge.Status),"Died",Discharge.Status)) %>%
    mutate(fatalities1 = ifelse(is.na(fatalities1),0,fatalities1)) %>%
    mutate(cfratio_age = fatalities1/age.covid.patients.admit1)
  
  # Proportion of COVID deaths from COVID hospitalisations
  covid_deaths_age <- left_join(x =incidence_severe_age, y = hosp_fatality_covid_age, by = "age.group") 
  covid_deaths_age <- select(covid_deaths_age,-Discharge.Status, -age.covid.patients.admit1.y)
  covid_deaths_age <- left_join(x = covid_deaths_age, y =wave_pop_est_strat_death_age[,c("age.group","age_pop_estimate_at_peak")], by = "age.group") #add in population estimates
  names(covid_deaths_age)[names(covid_deaths_age) == "age_pop_estimate_at_peak.x"] <- "age_pop_estimate_at_admit_age_peak"
  names(covid_deaths_age)[names(covid_deaths_age) == "age_pop_estimate_at_peak.y"] <- "age_pop_estimate_at_death_age_peak"
  covid_deaths_age <- covid_deaths_age %>%
    mutate(totals_age = nr.covid_age/(1-cfratio_age),
           fatals_age = totals_age - nr.covid_age)
  
  incidence_total_age <- covid_deaths_age[!is.na(covid_deaths_age$age.group),]
  incidence_total_age$age.group <- droplevels(incidence_total_age$age.group)
  
  incidence_total_age <- incidence_total_age %>%
    mutate(incidence_fatal_age = (fatals_age/age_pop_estimate_at_death_age_peak) *100000)
      
    return(incidence_total_age)
  }