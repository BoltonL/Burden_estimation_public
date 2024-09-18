#This script calculated the medically attended burden per level of severity: severe, fatal
medically_burden_country <- function(wave_in_admit,wave_in_death){

  wave_datcov_data_admit <- wave_in_admit[[1]] # wave data for admissions
  wave_datcov_data_admit <- lapply(wave_datcov_data_admit, function(y){
    y %>% 
      rename(Sex = BirthSex) %>%
      mutate(Sex = fct_explicit_na(Sex, na_level = NA_character_)) %>%
      filter(Sex== "Male" | Sex == "Female") %>%
      mutate(Sex = factor(Sex, levels = c("Female","Male"))) 
  })
  wave_pop_peaks_admit <- wave_in_admit[[2]] #population estimate at peaks stratified by age and sex
  wave_pop_peaks_admit_age <- lapply(wave_in_admit[[2]], function(a1){ #population estimate at peaks stratified by age only
    
  a1 %>% 
    group_by(age.group) %>%
    summarise(age_pop_estimate_at_peak = sum(age_pop_estimate_at_peak))
  })
  wave_pop_peaks_overall_admit <- wave_in_admit[[3]] #population estimate at peaks overall for admissions
  
  wave_datcov_data_death <- wave_in_death[[1]] # wave data for deaths
  wave_datcov_data_death <- lapply(wave_datcov_data_death, function(y){
    y %>% 
      rename(Sex = BirthSex) %>%
      mutate(Sex = fct_explicit_na(Sex, na_level = NA_character_)) %>%
      filter(Sex== "Male" | Sex == "Female") %>%
      mutate(Sex = factor(Sex, levels = c("Female","Male"))) 
  })
  wave_pop_peaks_death <- wave_in_death[[2]] #population estimate at peaks stratified by age and sex
  wave_pop_peaks_death_age <- lapply(wave_in_death[[2]], function(a2){ #population estimate at peaks stratified by age only
    
    a2 %>% 
      group_by(age.group) %>%
      summarise(age_pop_estimate_at_peak = sum(age_pop_estimate_at_peak))
  })
  wave_pop_peaks_overall_death <- wave_in_death[[3]] #population estimate at peaks overall for deaths
  
incidence_total_list_strat <- list()
incidence_total_list_strat_age <- list()
incidence_total_list_overall <- data.frame()
  
for (k in 1:length(wave_datcov_data_admit)){#for every wave period
datcov_select_admit <- wave_datcov_data_admit[[k]]
datcov_select_admit$Sex <- droplevels(datcov_select_admit$Sex)
wave_pop_est_strat_admit <- wave_pop_peaks_admit[[k]]
wave_pop_est_strat_admit_age <- wave_pop_peaks_admit_age[[k]]
wave_pop_est_overall_admit <- wave_pop_peaks_overall_admit[k,"age_pop_estimate_at_peak"]

datcov_select_death <- wave_datcov_data_death[[k]]
datcov_select_death$Sex <- droplevels(datcov_select_death$Sex)
wave_pop_est_strat_death <- wave_pop_peaks_death[[k]]
wave_pop_est_strat_death_age <- wave_pop_peaks_death_age[[k]]
wave_pop_est_overall_death <- wave_pop_peaks_overall_death[k,"age_pop_estimate_at_peak"]

##STRATIFIED BY AGE AND SEX
##SEVERE HOSPITALISED
# Number of patients admitted (total)
patients_admit <- datcov_select_admit
n_patients_admit <- patients_admit %>% #group by age and count
  group_by(Sex,age.group) %>%
  count(name = "age.patients.admit")
patients_known_admit <- patients_admit %>%
  filter(!is.na(AdmissionReason))
n_patients_known_admit_strat <- patients_known_admit %>%
  group_by(Sex,age.group) %>%
  count(name = "age.patients.admit.known")
# Number of patients admitted for COVID-19
patients_covid_admit <- patients_admit %>%
  filter(AdmissionReason ==  "COVID-19 symptoms")
n_patients_covid_admit <- patients_covid_admit %>% #group by age and count
  group_by(Sex,age.group) %>%
  count(name = "age.covid.patients.admit")
admissions_pre <- left_join(x = n_patients_admit, y = n_patients_known_admit_strat, by = c("Sex","age.group"))
admissions <- left_join(x = admissions_pre, y = n_patients_covid_admit, by = c("Sex","age.group"))
admissions <- admissions %>%
  mutate(prop.covid.admit = age.covid.patients.admit/age.patients.admit.known)#proportion of hospitalisations due to covid
# Number of COVID deaths in hospital (total - irrespective of diagnosis)
deaths_admit <- datcov_select_death #DIED = COVID-19 DEATH 
n_deaths_admit <- deaths_admit %>% #group by age and count
  group_by(Sex,age.group) %>%
  count(name = "age.covid.deaths")
#Incidence rate for COVID-19 hospitalisation
incidence_severe <- left_join(x = admissions, y = n_deaths_admit, by = c("Sex","age.group"))
incidence_severe <- incidence_severe %>%
  mutate(age.covid.deaths = ifelse(is.na(age.covid.deaths),0,age.covid.deaths),
        nr.admit.ndcovid = age.patients.admit - age.covid.deaths, #remove all covid deaths from admissions - mutually exclusive
         nr.covid = prop.covid.admit * nr.admit.ndcovid) #now calculate what nr of covid hospitalisations would be

incidence_severe <- left_join(x = incidence_severe, y = wave_pop_est_strat_admit[,c("Sex","age.group","age_pop_estimate_at_peak")], by = c("Sex","age.group")) #add in population estimates
incidence_severe <- incidence_severe %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_peak)*100000) %>%
  mutate(age.group = factor(age.group, levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                                                  "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")))
## FATAL IN HOSPITAL
# case fatality ratio from those that were COVID-19 admissions:
fatality_covid <- datcov_select_death %>% #number of COVID-19 deaths from COVID-19 admissions
  filter(AdmissionReason ==  "COVID-19 symptoms") %>%
  group_by(Sex,age.group) %>%
  count(Discharge.Status, name = "fatalities")

hosp_fatality_covid <- full_join(x = fatality_covid, y = n_patients_covid_admit, by = c("Sex","age.group"))
hosp_fatality_covid <- hosp_fatality_covid %>%  #case fatality ratio
  arrange(Sex, age.group) %>%
  mutate(Discharge.Status = as.character(Discharge.Status)) %>%
  mutate(Discharge.Status = ifelse(is.na(Discharge.Status),"Died",Discharge.Status)) %>%
  mutate(fatalities = ifelse(is.na(fatalities),0,fatalities)) %>%
  mutate(cfratio = fatalities/age.covid.patients.admit)

# Proportion of COVID deaths from COVID hospitalisations
covid_deaths <- left_join(x =incidence_severe, y = hosp_fatality_covid, by = c("Sex","age.group")) 
covid_deaths <- select(covid_deaths,-Discharge.Status, -age.covid.patients.admit.y)
covid_deaths <- left_join(x = covid_deaths, y =wave_pop_est_strat_death[,c("Sex","age.group","age_pop_estimate_at_peak")], by = c("Sex","age.group")) #add in population estimates
names(covid_deaths)[names(covid_deaths) == "age_pop_estimate_at_peak.x"] <- "age_pop_estimate_at_admit_peak"
names(covid_deaths)[names(covid_deaths) == "age_pop_estimate_at_peak.y"] <- "age_pop_estimate_at_death_peak"
covid_deaths <- covid_deaths %>%
  mutate(totals = nr.covid/(1-cfratio),
         fatals = totals - nr.covid)
  
incidence_total <- covid_deaths[!is.na(covid_deaths$age.group),]
incidence_total$age.group <- droplevels(incidence_total$age.group)

incidence_total <- incidence_total %>%
  mutate(incidence_fatal = (fatals/age_pop_estimate_at_death_peak) *100000)


incidence_total_list_strat[[k]] <- incidence_total

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


incidence_total_list_strat_age[[k]] <- incidence_total_age



##UNSTRATIFIED
##SEVERE HOSPITALISED
# Number of patients admitted (total)
patients_admit <- datcov_select_admit#datcov_select[datcov_select$AdmissionReason != "NA",]
n_patients_admit_total <- patients_admit %>% 
  count(name = "patients.admit.total")
patients_known_admit <- patients_admit %>%
  filter(!is.na(AdmissionReason))
n_patients_known_admit <- patients_known_admit %>%
  count(name = "patients.admit.known")
# Number of patients admitted for COVID-19
patients_covid_admit_total <- patients_admit %>%
  filter(AdmissionReason ==  "COVID-19 symptoms")
n_patients_covid_admit_total <- patients_covid_admit_total %>% 
  count(name = "covid.patients.admit.total")
admissions_total <- bind_cols(n_patients_admit_total,n_patients_known_admit,n_patients_covid_admit_total)
admissions_total <- admissions_total %>%
  mutate(prop.covid.admit.total = covid.patients.admit.total/patients.admit.known)
# Number of COVID deaths in hospital (total)
deaths_admit_total <- datcov_select_death
n_deaths_admit_total <- deaths_admit_total %>% 
  count(name = "covid.deaths.total")
#Incidence rate for COVID-19 hospitalisation
incidence_severe_total <- bind_cols(admissions_total, n_deaths_admit_total)
incidence_severe_total <- incidence_severe_total %>%
  mutate(nr.admit.ndcovid.total = patients.admit.total - covid.deaths.total, #remove all covid deaths from admissions - mutually exclusive
         nr.covid.total = prop.covid.admit.total * nr.admit.ndcovid.total) #now calculate what nr of hospitalisations would be

incidence_severe_total <- bind_cols(incidence_severe_total, wave_pop_est_overall_admit) #add in population estimates
names(incidence_severe_total)[ncol(incidence_severe_total)] <- "pop_estimate_at_peak_admit"
incidence_severe_total <- incidence_severe_total %>%
  mutate(incidence_hosp_total = (nr.covid.total/pop_estimate_at_peak_admit)*100000)

## FATAL IN HOSPITAL
# case fatality ratio from those that were COVID-19 admissions:
fatality_covid_total <- datcov_select_death %>% #number of COVID-19 deaths from COVID-19 admissions
  filter(AdmissionReason ==  "COVID-19 symptoms") %>%
  count(Discharge.Status, name = "fatalities.total") 

hosp_fatality_covid_total <- bind_cols(fatality_covid_total$fatalities.total, n_patients_covid_admit_total)
names(hosp_fatality_covid_total)[1] <- "fatalities.total"
hosp_fatality_covid_total <- hosp_fatality_covid_total %>%  #case fatality ratio
  mutate(cfratio.total = fatalities.total/covid.patients.admit.total)
# Proportion of COVID deaths from COVID hospitalisations
covid_deaths_total <- bind_cols(incidence_severe_total, hosp_fatality_covid_total) 
covid_deaths_total <- select(covid_deaths_total,-covid.patients.admit.total...3)
names(covid_deaths_total)[names(covid_deaths_total) == "covid.patients.admit.total...11"] <- "covid.patients.admit.total"
covid_deaths_total <- bind_cols(covid_deaths_total, wave_pop_est_overall_death)
names(covid_deaths_total)[names(covid_deaths_total) == "...12"] <- "pop_estimate_at_peak_death"


covid_deaths_total <- covid_deaths_total %>%
  mutate(totals.overall = nr.covid.total/(1-cfratio.total),
         fatals.overall = totals.overall - nr.covid.total)

incidence_total_overall <- covid_deaths_total %>%
  mutate(incidence_fatal_total = (fatals.overall/pop_estimate_at_peak_death) *100000)
incidence_total_overall <- bind_cols(k, incidence_total_overall)
names(incidence_total_overall)[1] <- "wave.nr"

incidence_total_list_overall <- rbind.data.frame(incidence_total_list_overall, incidence_total_overall, row.names = NULL)
}

incidence_med_burden <- list(incidence_total_list_strat,
                             incidence_total_list_strat_age,
                             incidence_total_list_overall)
return(incidence_med_burden)
}