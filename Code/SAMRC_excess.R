SAMRC_excess <- function(excess_deaths_females,excess_deaths_males,wave_in_death, attributable){

excess_deaths_females2 <- excess_deaths_females %>%
  pivot_longer(cols = 'wave_1':'wave_5',names_to = "wave.nr") %>%
  mutate(wave.nr = unlist(str_extract_all(wave.nr, "[0-9]+"))) %>%
  mutate(wave.nr = as.factor(wave.nr),
         age.group = as.factor(age.group))
excess_deaths_females2$age.group <- recode_factor(excess_deaths_females2$age.group,
                                                  '0' = '0-4',
                                                  '5' = '5-9',
                                                  '10' = '10-14',
                                                  '15' = '15-19',
                                                  '20' = '20-24',
                                                  '25' = '25-29',
                                                  '30' = '30-34',
                                                  '35' = '35-39',
                                                  '40' = '40-44',
                                                  '45' = '45-49',
                                                  '50' = '50-54',
                                                  '55' = '55-59',
                                                  '60' = '60-64',
                                                  '65' = '65-69',
                                                  '70' = '70-74',
                                                  '75' = '75-79',
                                                  '80' = '80+')
excess_deaths_females2$Sex <- "Female"
excess_deaths_males2 <- excess_deaths_males %>%
  pivot_longer(cols = 'wave_1':'wave_5',names_to = "wave.nr") %>%
  mutate(wave.nr = unlist(str_extract_all(wave.nr, "[0-9]+"))) %>%
  mutate(wave.nr = as.factor(wave.nr),
         age.group = as.factor(age.group))
excess_deaths_males2$age.group <- recode_factor(excess_deaths_males2$age.group,
                                                '0' = '0-4',
                                                '5' = '5-9',
                                                '10' = '10-14',
                                                '15' = '15-19',
                                                '20' = '20-24',
                                                '25' = '25-29',
                                                '30' = '30-34',
                                                '35' = '35-39',
                                                '40' = '40-44',
                                                '45' = '45-49',
                                                '50' = '50-54',
                                                '55' = '55-59',
                                                '60' = '60-64',
                                                '65' = '65-69',
                                                '70' = '70-74',
                                                '75' = '75-79',
                                                '80' = '80+')
excess_deaths_males2$Sex <- "Male"
excess_deaths <- rbind.data.frame(excess_deaths_females2,excess_deaths_males2, row.names = NULL)
excess_deaths <- excess_deaths %>%
  select(wave.nr, Sex,age.group,value) %>%
  mutate(Sex = as.factor(Sex)) %>%
  arrange(wave.nr,Sex,age.group)

excess_deaths_age <- excess_deaths %>%
  select(wave.nr, Sex,age.group,value) %>%
  mutate(Sex = as.factor(Sex)) %>%
  arrange(wave.nr,Sex,age.group)


wave_datcov_data_death <- wave_in_death[[1]] # countrylevel datcov
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
wave_pop_peaks_overall_death <- wave_in_death[[3]] #population estimate at peaks overall

covid_datcov_deaths_strat <- data.frame()
covid_datcov_deaths_age <- data.frame()
for (k in 1:length(wave_datcov_data_death)){#for every wave period
#  k <- 1
datcov_select_death <- wave_datcov_data_death[[k]]
datcov_select_death$Sex <- droplevels(datcov_select_death$Sex)
wave_pop_est_strat_death <- wave_pop_peaks_death[[k]]
wave_pop_est_strat_death_age <- wave_pop_peaks_death_age[[k]]
wave_pop_est_overall_death <- wave_pop_peaks_overall_death[k,"age_pop_estimate_at_peak"]

fatality_covid <- datcov_select_death %>% #number of COVID-19 deaths from COVID-19 admissions
  filter(AdmissionReason ==  "COVID-19 symptoms") %>%
  group_by(Sex,age.group) %>%
  count(Discharge.Status, name = "fatalities")
covid_deaths_strat <- left_join(x = fatality_covid, y =wave_pop_est_strat_death[,c("Sex","age.group","age_pop_estimate_at_peak")], by = c("Sex","age.group")) #add in population estimates
covid_deaths_strat <- covid_deaths_strat %>%
  mutate(wave.nr = k) %>%
  select(wave.nr, Sex, age.group,fatalities,age_pop_estimate_at_peak)%>%
  mutate(wave.nr = as.factor(wave.nr),
         Sex = as.factor(Sex),
         age.group = as.factor(age.group))

covid_datcov_deaths_strat <- rbind.data.frame(covid_datcov_deaths_strat, covid_deaths_strat, row.names = NULL)

fatality_covid_age <- datcov_select_death %>% #number of COVID-19 deaths from COVID-19 admissions
  filter(AdmissionReason ==  "COVID-19 symptoms") %>%
  group_by(age.group) %>%
  count(Discharge.Status, name = "fatalities")
covid_deaths_age <- left_join(x = fatality_covid_age, y =wave_pop_est_strat_death_age[,c("age.group","age_pop_estimate_at_peak")], by = "age.group") #add in population estimates
covid_deaths_age <- covid_deaths_age %>%
  mutate(wave.nr = k) %>%
  select(wave.nr, age.group,fatalities,age_pop_estimate_at_peak)%>%
  mutate(wave.nr = as.factor(wave.nr),
         age.group = as.factor(age.group))

covid_datcov_deaths_age <- rbind.data.frame(covid_datcov_deaths_age, covid_deaths_age, row.names = NULL)

}


datcov_excess_strat <- left_join(x = covid_datcov_deaths_strat, y = excess_deaths, by = c("wave.nr","Sex","age.group"))
datcov_excess_strat <- datcov_excess_strat %>%
  mutate(value = as.numeric(value)) %>%
  mutate(covid_attrib_value = floor(attributable*value)) %>%
  mutate(covid_attrib_value = ifelse(covid_attrib_value < 0, fatalities,covid_attrib_value))
datcov_excess_total <- datcov_excess_strat %>%
  group_by(wave.nr) %>%
  summarise(covid_attrib_deaths = sum(covid_attrib_value)) %>%
  ungroup()
datcov_excess_age <- datcov_excess_strat %>%
  group_by(wave.nr, age.group) %>%
  summarise(covid_attrib_deaths = sum(covid_attrib_value),
            fatalities = sum(fatalities), .groups = 'keep') %>%
  ungroup()

datcov_excess_age <- left_join(x = datcov_excess_age, y = covid_datcov_deaths_age[,c("wave.nr","age.group","age_pop_estimate_at_peak")], by = c("wave.nr","age.group"))
datcov_excess_strat$covid_attrib_value <- floor(datcov_excess_strat$covid_attrib_value)
datcov_excess_age$covid_attrib_deaths <- floor(datcov_excess_age$covid_attrib_deaths)
datcov_excess_total$covid_attrib_deaths <- floor(datcov_excess_total$covid_attrib_deaths)
  
datcov_excess_list <- list(datcov_excess_strat,
                           datcov_excess_total,
                           datcov_excess_age)
return(datcov_excess_list)





}  
