non_medically_burden <- function(med_attended_burden_country,HUS_severe, excess_in){

  #Countrywide
  incidence_med_country_total <- med_attended_burden_country[[3]]
  incidence_med_country_strat <- med_attended_burden_country[[1]]
  incidence_med_country_age <- med_attended_burden_country[[2]]
  excess_in_strat <- excess_in[[1]] #excess deaths stratified by sex, age
  excess_in_total <- excess_in[[2]] #total national excess deaths
  excess_in_age <- excess_in[[3]] #excess deaths stratified by age
  
#COUNTRYWIDE
##UNSTRATIFIED
incidence_med_country_total$wave.nr <- as.factor(incidence_med_country_total$wave.nr)
incidence_med_country_total_pre <- left_join(x = incidence_med_country_total, y = excess_in_total, by = "wave.nr")
incidence_med_country_total <- incidence_med_country_total_pre %>%
  mutate(incidence_non_hosp_total = (incidence_hosp_total/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp_total,
         incidence_non_fatal = ((covid_attrib_deaths - fatals.overall)/pop_estimate_at_peak_death)*100000,
         srif.ratio = incidence_non_fatal/(incidence_non_fatal +incidence_non_hosp_total))

##STRATIFIED
non_med_incidence_strat <- list()
non_med_incidence_age <- list()
  for (kk in 1:length(incidence_med_country_strat)){
 
incidence_med_attend_severe_strat <- incidence_med_country_strat[[kk]]$incidence_hosp
incidence_med_attend_severe_age <- incidence_med_country_age[[kk]]$incidence_hosp_age

total_severe <-  incidence_med_attend_severe_strat/ (sum(HUS_severe$Count)/sum(HUS_severe$Total))
total_severe_age <-  incidence_med_attend_severe_age/ (sum(HUS_severe$Count)/sum(HUS_severe$Total))

non_severe <- total_severe - incidence_med_attend_severe_strat
non_severe_age <- total_severe_age - incidence_med_attend_severe_age

non_fatal_incidence_strat <- excess_in_strat %>%
  mutate(incidence_non_fatal = ((covid_attrib_value - fatals)/age_pop_estimate_at_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal))

non_fatal_incidence_age <- excess_in_age %>%
  mutate(incidence_non_fatal = ((covid_attrib_deaths - fatals_age)/age_pop_estimate_at_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal))

non_med_incidence_strat[[kk]] <- cbind.data.frame(incidence_med_country_strat[[kk]],
                                                  non_severe,
                                                  row.names = NULL)
non_med_incidence_strat[[kk]] <- left_join(x = non_med_incidence_strat[[kk]], 
                                           y = non_fatal_incidence_strat[which(non_fatal_incidence_strat$wave.nr == kk),c("Sex","age.group","covid_attrib_value","incidence_non_fatal")],
                                           by = c("Sex","age.group"))
non_med_incidence_age[[kk]] <- cbind.data.frame(incidence_med_country_age[[kk]],
                                                non_severe_age, row.names = NULL)
non_med_incidence_age[[kk]] <- left_join(x = non_med_incidence_age[[kk]], 
                                           y = non_fatal_incidence_age[which(non_fatal_incidence_age$wave.nr == kk),c("age.group","covid_attrib_deaths","incidence_non_fatal")],
                                           by = "age.group")
names(non_med_incidence_strat[[kk]])[ncol(non_med_incidence_strat[[kk]])-2] <- "incidence_non_hosp"
names(non_med_incidence_strat[[kk]])[names(non_med_incidence_strat[[kk]]) == "covid_attrib_value"] <- "excess_deaths"

non_med_incidence_strat[[kk]] <- non_med_incidence_strat[[kk]] %>%
  mutate(srif.ratio = incidence_non_fatal/(incidence_non_fatal + incidence_non_hosp))

names(non_med_incidence_age[[kk]])[ncol(non_med_incidence_age[[kk]])-2] <- "incidence_non_hosp_age"
names(non_med_incidence_age[[kk]])[names(non_med_incidence_age[[kk]]) == "covid_attrib_deaths"] <- "excess_deaths"

non_med_incidence_age[[kk]] <- non_med_incidence_age[[kk]] %>%
  mutate(srif.ratio = incidence_non_fatal/(incidence_non_fatal + incidence_non_hosp_age))

  }


non_med_incidence <- list(incidence_med_country_total,
                          non_med_incidence_strat,
                          non_med_incidence_age)
                          # non_med_incidence_prov_total,
                          # non_med_incidence_prov_age,
                          # non_med_incidence_prov_sex)
return(non_med_incidence)
  
}