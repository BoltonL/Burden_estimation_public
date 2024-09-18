non_medically_burden_ci <- function(med_data_in1,excess_in, HUS_severe){
  
  #Countrywide
  incidence_med_country_total <- as.data.frame(med_data_in1)
  
  excess_in_total <- excess_in
  excess_set <- as.character(c(names(excess_in_total)[2], replicate(excess_in_total$covid_attrib_deaths, n = ncol(incidence_med_country_total)-1)))
  
  incidence_med_country_total_upd <- rbind.data.frame(incidence_med_country_total,
                                                      excess_set, row.names = NULL)
  
  incidence_med_country_total_upd <- incidence_med_country_total_upd %>%
    mutate(across(rep1:rep1000,as.numeric))
  incidence_med_country_total_upd <- transpose(incidence_med_country_total_upd, make.names = "Measures")
  
  #COUNTRYWIDE
  ##UNSTRATIFIED
  
   incidence_med_country_total_complete <- incidence_med_country_total_upd %>%
    mutate(incidence_non_hosp_total = (incidence_hosp_total/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp_total,
           incidence_non_fatal = ((covid_attrib_deaths - fatals.overall)/pop_estimate_at_peak_death)*100000,
           srif.ratio = incidence_non_fatal/(incidence_non_hosp_total + incidence_non_fatal))
  
  return(incidence_med_country_total_complete)
  
}