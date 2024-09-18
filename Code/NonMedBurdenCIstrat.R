non_medically_burden_ci_strat <- function(med_data_strat_in1,excess_strat_in, HUS_severe){
  
  incidence_med_country_age <- med_data_strat_in1
  
  excess_in_age <- excess_strat_in
  excess_set <- left_join(x = incidence_med_country_age, y = excess_in_age[,c("age.group","covid_attrib_deaths")], by = "age.group")
  
        total_severe_age <-  excess_set %>%
      mutate(incidence_non_hosp =  (incidence_hosp_age/ (sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp_age)
    
    
    non_fatal_incidence_age <- total_severe_age %>%
      mutate(incidence_non_fatal = ((covid_attrib_deaths - fatals_age)/age_pop_estimate_at_death_age_peak)*100000) %>%
      mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
      mutate(srif.ratio = incidence_non_fatal/(incidence_non_fatal + incidence_non_hosp))
    
return(non_fatal_incidence_age)
  
}