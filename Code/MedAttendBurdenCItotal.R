#This script calculated the medically attended burden per level of severity: mild, severe, fatal

medically_burden_ci_total <- function(data_in1,data_in2,k,x,y){
  
    #k <- 1
    datcov_select_admit <- data_in1
    datcov_select_death <- data_in2
    wave_pop_est_overall_admit <- x
    wave_pop_est_overall_death <- y
    
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
    
    incidence_total_overall_boot <- covid_deaths_total %>%
      mutate(incidence_fatal_total = (fatals.overall/pop_estimate_at_peak_death) *100000)
    incidence_total_overall_boot <- bind_cols(k, incidence_total_overall_boot)
    names(incidence_total_overall_boot)[1] <- "wave.nr"
    
   
  
  return(incidence_total_overall_boot)
}