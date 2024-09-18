library(tidyverse)
library(lubridate)
library(openxlsx)
library(data.table)
library(stringr)
library(janitor)


#-------------------------------------------------------------------------------------------------------------
clean_datcov <- 'datcov_LB.RDS'
clean_cases <- 'covid_cases.RDS'
country_est <- 'Country projection by population group, sex and age (2002-2022)_LB.xlsx'
country_est_sheet <- 4
provinces <- c("North West","Western Cape","KwaZulu-Natal")


data_datcov <- readRDS(file = file.path("./Data","data_datcov.RDS"))
data_covid <- readRDS(file = file.path("./Data","data_covid.RDS"))
#--------------------------------------------------------------------------------------------------------------------------------
#2. DEFINE WAVE PEAKS, CUTS AND INTERPOLATE POPULATION ESTIMATES
#country wide population estimates stratified by age and gender, and then overall
data_dir <- './Data'
source("./Code/CountryPopulation.R")
interp_pop <- country_estimates(data_dir, data_covid, country_est, country_est_sheet)

#Peak cases, admissions and deaths(countrywide)
additional <- "Total_excess.xlsx"
blood_draws <- read.xlsx(file.path(data_dir,"Blood_draws.xlsx"))
blood_draws <- blood_draws %>%
  mutate(Start = convert_to_date(Start),
         Finish = convert_to_date(Finish)) %>%
  mutate(Start_epiweek = epiweek(Start),
         Finish_epiweek = epiweek(Finish),
         epiyear = epiyear(Start)) %>%
  filter(Draw %in% c(2,5,9,10,11)) %>%
  mutate(Draw = as.factor(Draw),
         Start_epiweek = as.factor(Start_epiweek),
         Finish_epiweek = as.factor(Finish_epiweek),
         epiyear = as.factor(epiyear))
  
source('./Code/CovidWaves.R')
covid_waves_script <- covid_waves_def(data_dir, data_datcov, data_covid, additional, blood_draws)
peaks_def <- covid_waves_script[[1]] # peaks based on COVID case data
peaks_admission_def <- covid_waves_script[[2]]
peaks_death_def <- covid_waves_script[[3]]
waves_def <- covid_waves_script[[4]] # nadir based on COVID case data
waves_admissions_def <- covid_waves_script[[5]]
waves_deaths_def <- covid_waves_script[[6]]
combined_defs <- covid_waves_script[[7]]
covid_datcov <- covid_waves_script[[8]]
covid_datcov$epiweek <- as.factor(covid_datcov$epiweek)
covid_datcov$epiyear <- as.factor(covid_datcov$epiyear)
# covid_datcov <- covid_datcov %>%
#   group_by(epiyear,epiweek) %>%
#   arrange(Adm.Date.AdmissionDate) %>%
#   ungroup()

#-------------------------------------------------------------------------------------------------------------
#3. ESTIMATE POPULATION AT RISK AT PEAK
#countrywide population stratified and overall
source('./Code/PopulationEstimates.R')
source('./Code/PopulationEstimatesAdmissions.R')
# source('./Code/PopulationEstimatesCOVIDAdmissions.R')
source('./Code/PopulationEstimatesDeaths.R')
pop_at_risk <- estimate_population(peaks = peaks_def, interp_pop,waves = waves_def)
pop_at_risk_admit <- estimate_population_admissions(peaks =  peaks_admission_def, interp_pop,waves = waves_admissions_def)
pop_at_risk_deaths <- estimate_population_deaths(peaks =  peaks_death_def, interp_pop,waves = waves_deaths_def)
# strat_pop_at_risk <- pop_at_risk[[1]] #population at risk stratified by age and sex
# overall_pop_at_risk <- pop_at_risk[[2]] #population at risk overall countrywide

#-------------------------------------------------------------------------------------------------------------
#4. EXTRACT HOSPITALISATIONS PER WAVE
#countrywide overall and stratified by sex and age
source('./Code/WaveEstimates.R')
source('./Code/WaveEstimatesOld.R')

wave_data_old <- estimate_waves_check(pop_at_risk,covid_datcov) 
wave_data <- estimate_waves(pop = pop_at_risk, data_in =covid_datcov) #wave data for cases
wave_data_admit <- estimate_waves(pop = pop_at_risk_admit, data_in = covid_datcov) #wave data for admissions
covid_death <- covid_datcov %>%
  filter(Discharge.Status == "Died")

wave_data_death <- estimate_waves(pop = pop_at_risk_deaths, data_in = covid_death) #wave data for in hospital deaths

# lapply(wave_data[[1]], function(r){
#   r1 <- r %>%
#     mutate(AdmissionReason = as.character(AdmissionReason)) %>%
#     mutate(AdmissionReason = fct_explicit_na(AdmissionReason, na_level = "NA"))
#   print(levels(r1$AdmissionReason))
#   table(r1$AdmissionReason)/nrow(r1)
# })
#Duration of cases waves
# wave_duration_cases <- lapply(wave_data_cases[[1]], function(x){
#   cases_waves1 <- x[1]$colldate1
#   cases_waves2 <- x[2]$colldate1
#   cases_duration <- time_length(interval(start = cases_waves1,end = cases_waves2), unit = "weeks")
#   })
# 
# 
# #Duration of total admissions waves
# wave_duration_admissions <- lapply(wave_data_admit[[1]], function(x){
#     cases_waves1 <- x[1]$Adm.Date.AdmissionDate
#     cases_waves2 <- x[2]$Adm.Date.AdmissionDate
#     admit_duration <- time_length(interval(start = cases_waves1,end = cases_waves2), unit = "weeks")
#   })
# 
# #Duration of covid admissions waves
# wave_duration_admissions_covid <- lapply(wave_data_covid_admit[[1]], function(x){
#   cases_waves1 <- x[1]$Adm.Date.AdmissionDate
#   cases_waves2 <- x[2]$Adm.Date.AdmissionDate
#   admit_duration <- time_length(interval(start = cases_waves1,end = cases_waves2), unit = "days")
# })
# 
# #Duration of deaths waves
# wave_duration_deaths <- lapply(wave_data_death[[1]], function(x){
#   cases_waves1 <- x[1]$Adm.Date.AdmissionDate
#   cases_waves2 <- x[2]$Adm.Date.AdmissionDate
#   admit_duration <- time_length(interval(start = cases_waves1,end = cases_waves2), unit = "weeks")
# })

#------------------------------------------------------------------------------------------------------------
#5. MEDICALLY ATTENDED SEVERE AND FATAL
#This script calculates the medically attended burden per level of severity: severe, fatal
##Countrywide estimates (per 100 000 population)
source('./Code/MedAttendBurden.R')
med_attended_burden_country <- medically_burden_country(wave_in_admit = wave_data_admit, wave_in_death = wave_data_death)

med_attended_burden_country_table_total <- med_attended_burden_country[[3]] %>%
    select(wave.nr, incidence_hosp_total, fatalities.total, fatals.overall,incidence_fatal_total, cfratio.total)
med_attended_burden_country_table_strat <- lapply(med_attended_burden_country[[1]], function(m1){
   m1 %>%
  select(Sex, age.group,incidence_hosp, fatalities, fatals, incidence_fatal, cfratio)
})
med_attended_burden_country_table_age <- lapply(med_attended_burden_country[[2]], function(m1){
  m1 %>%
    select(age.group,incidence_hosp_age, fatalities1, fatals_age, incidence_fatal_age, cfratio_age)
})
med_attended_burden_country_table_strat_out <- data.frame()
med_attended_burden_country_table_age_out <- data.frame()
for (m1 in 1:length(med_attended_burden_country_table_strat)){

  med_strat <- med_attended_burden_country_table_strat[[m1]] %>%
    mutate(wave.nr = m1) %>%
    select(wave.nr, Sex, age.group, incidence_hosp, fatalities, fatals, incidence_fatal, cfratio) %>%
    mutate(wave.nr = as.factor(wave.nr))
  med_strat_age <- med_attended_burden_country_table_age[[m1]] %>%
    mutate(wave.nr = m1) %>%
    select(wave.nr, age.group, incidence_hosp_age, fatalities1, fatals_age, incidence_fatal_age, cfratio_age) %>%
    mutate(wave.nr = as.factor(wave.nr))
  med_attended_burden_country_table_age_out <- rbind.data.frame(med_attended_burden_country_table_age_out,
                                                                  med_strat_age, row.names = NULL)
  med_attended_burden_country_table_strat_out <- rbind.data.frame(med_attended_burden_country_table_strat_out,
                                                                med_strat, row.names = NULL)
}

#------------------------------------------------------------------------------------------------------------
#6. NON-MEDICALLY ATTENDED SEVERE 

#Health seeking behaviour from Health utilization survey
#Source: Wolter, N., et al. Healthcare utilization during the first two waves of the COVID-19 epidemic in South Africa: 
#a cross-sectional household survey.
HUS_severe <- bind_cols(provinces,c(13,27,62),c(19,48,76))
names(HUS_severe) <- c("Province","Count","Total")
HUS_severe$prop <- HUS_severe$Count/HUS_severe$Total
HUS_mild <- bind_cols(provinces,c(20,8,34),c(72,12,68))
names(HUS_mild) <- names(HUS_severe)[1:3]
HUS_mild$prop <- HUS_mild$Count/HUS_mild$Total
excess_deaths_females <- read.xlsx('./Data/SAMRC_excess.xlsx', sheet = 1)
excess_deaths_males <- read.xlsx('./Data/SAMRC_excess.xlsx', sheet = 2)
perc.covid <- 0.85 #weekly report for week 18
source('./Code/SAMRC_excess.R')
phirst <- read.xlsx('./Data/PHIRST_AT.xlsx', sheet = 4)
datcov_excess_deaths <- SAMRC_excess(excess_deaths_females,excess_deaths_males,wave_in_death = wave_data_death, attributable = perc.covid)
datcov_excess_deaths[[1]] <- left_join(x = datcov_excess_deaths[[1]], 
                                       y = med_attended_burden_country_table_strat_out[,c("wave.nr","Sex","age.group","fatals")], 
                                       by = c("wave.nr","Sex","age.group"))
datcov_excess_deaths[[3]] <- left_join(x = datcov_excess_deaths[[3]], 
                                       y = med_attended_burden_country_table_age_out[,c("wave.nr","age.group","fatals_age")], 
                                       by = c("wave.nr","age.group"))

source('./Code/NonMedBurden.R')
non_attend_burden <- non_medically_burden(med_attended_burden_country, HUS_severe, excess_in = datcov_excess_deaths)


non_med_attended_burden_country_table_total <- non_attend_burden[[1]] %>%
  select(wave.nr, pop_estimate_at_peak_admit, incidence_hosp_total, fatalities.total, fatals.overall, incidence_fatal_total, cfratio.total, 
         incidence_non_hosp_total, incidence_non_fatal,srif.ratio) 

non_med_attended_burden_country_table_strat <- lapply(non_attend_burden[[2]], function(m1){
  m1 %>%
    select(Sex, age.group,age_pop_estimate_at_admit_peak,incidence_hosp, fatalities, fatals,incidence_fatal, cfratio, incidence_non_hosp, incidence_non_fatal,srif.ratio)
})
non_med_attended_burden_country_table_age <- lapply(non_attend_burden[[3]], function(m1){
  m1 %>%
    select(age.group,age_pop_estimate_at_admit_age_peak,incidence_hosp_age, fatalities1,fatals_age, incidence_fatal_age, cfratio_age,incidence_non_hosp_age, incidence_non_fatal,srif.ratio) 
})
non_med_attended_burden_country_table_strat_out <- data.frame()
non_med_attended_burden_country_table_age_out <- data.frame()
for (m1 in 1:length(non_med_attended_burden_country_table_strat)){
  
  non_med_strat <- non_med_attended_burden_country_table_strat[[m1]] %>%
    mutate(wave.nr = m1) %>%
    select(wave.nr, Sex, age.group,age_pop_estimate_at_admit_peak, incidence_hosp,fatalities, fatals, incidence_fatal, cfratio, 
           incidence_non_hosp,incidence_non_fatal,srif.ratio)
  non_med_strat_age <- non_med_attended_burden_country_table_age[[m1]] %>%
    mutate(wave.nr = m1) %>%
    select(wave.nr, age.group, age_pop_estimate_at_admit_age_peak,incidence_hosp_age,fatalities1,fatals_age, incidence_fatal_age, cfratio_age, 
           incidence_non_hosp_age, incidence_non_fatal,srif.ratio)
  non_med_attended_burden_country_table_age_out <- rbind.data.frame(non_med_attended_burden_country_table_age_out,
                                                                non_med_strat_age, row.names = NULL)
  non_med_attended_burden_country_table_strat_out <- rbind.data.frame(non_med_attended_burden_country_table_strat_out,
                                                                  non_med_strat, row.names = NULL)
}

# get national estimates
wave_total_estimates <- lapply(non_attend_burden[[3]], function(w1){
  hfr_wave <- sum(w1$fatals_age)/(sum(w1$nr.covid_age) + sum(w1$fatals_age))  *100
  incidence_hosp_wave <-  sum(w1$nr.covid_age)/sum(w1$age_pop_estimate_at_admit_age_peak) *100000
  incidence_fatal_wave <- sum(w1$fatals_age)/sum(w1$age_pop_estimate_at_death_age_peak) *100000
  incidence_non_hosp_wave <- (incidence_hosp_wave/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp_wave
  incidence_non_fatal_wave <- ((sum(w1$excess_deaths) - sum(w1$fatals_age))/sum(w1$age_pop_estimate_at_death_age_peak))*100000
  srif_wave <- incidence_non_fatal_wave/(incidence_non_fatal_wave + incidence_non_hosp_wave)
  
  wave_total_estimates <- list(hfr_wave,
                               incidence_hosp_wave,
                               incidence_fatal_wave,
                               incidence_non_hosp_wave,
                               incidence_non_fatal_wave,
                               srif_wave)
  
})
#plot hospitalisation-fatality ratio for age groups
non_med_attended_burden_country_table_age_out$wave.nr <- factor(non_med_attended_burden_country_table_age_out$wave.nr,
                                                                labels = c("Wave 1","Wave 2","Wave 3","Wave 4","Wave 5"))
non_med_attended_burden_country_table_age_out$cfratio_age <- non_med_attended_burden_country_table_age_out$cfratio_age*100
non_med_attended_burden_country_table_age_out$srif.ratio <- non_med_attended_burden_country_table_age_out$srif.ratio*100
non_med_attended_burden_country_table_age_out$total_severe <- non_med_attended_burden_country_table_age_out$incidence_hosp_age +
  non_med_attended_burden_country_table_age_out$incidence_non_hosp_age
non_med_attended_burden_country_table_age_out$total_fatal <- non_med_attended_burden_country_table_age_out$incidence_fatal_age +
  non_med_attended_burden_country_table_age_out$incidence_non_fatal


data_dir_ci <- './Output/CI'
data_dir_ci_2 <- 'G:/results'

#Calculate the infection rates
source('./Code/InfectionRates.R')
#Determine estimates for national using national population estimates
infection_table_out <- infection_rates(ma_country = non_med_attended_burden_country_table_total,
                                       ma_age = non_attend_burden[[3]],
                attack_rate = phirst)
infections_rates_country_table <- infection_table_out[[1]]

infections_rates_age_table <- infection_table_out[[2]]
infections_rates_attack <- infection_table_out[[3]]
infection_rates_age_ci <- phirst %>%
  filter(age_cat != "All") %>%
  mutate(attack.rate = attack.rate*100000) %>%
         mutate(age_cat = as.character(age_cat)) %>%
           mutate(age_cat = ifelse(age_cat == "00-04","0-4",age_cat)) %>%
           mutate(age_cat = ifelse(age_cat == "05-09","5-9",age_cat)) %>%
           mutate(age_cat = as.factor(age_cat)) 
         names(infection_rates_age_ci)[names(infection_rates_age_ci) == "age_cat"] <- "age.group"
         
infections_rates_age_table$wave.nr <- factor(infections_rates_age_table$wave.nr,
                                                                labels = c("1","2","3","4","5"))


source('./Code/confidence_intervals_strat.R')
confidence_intervals_out <- conf_int_strat(directory = data_dir_ci, directory_upd = data_dir_ci_2, attack = infections_rates_attack)
hfr_ci <- confidence_intervals_out[[1]]
non_med_attended_burden_country_table_age_out_upd <- left_join(x = non_med_attended_burden_country_table_age_out[,c("wave.nr","age.group","cfratio_age")],
                                                               y = hfr_ci[,c("wave.nr","age.group","CI_low","CI_high")],
                                                               by = c("wave.nr","age.group"))
hfr1 <- ggplot(non_med_attended_burden_country_table_age_out_upd, aes(x = age.group, group = 1)) + 
  geom_point(aes(y = cfratio_age),color = "aquamarine4", size = 1) +
  geom_line(aes(y = cfratio_age), color = "aquamarine4") +
  geom_errorbar(aes(ymin = CI_low,ymax = CI_high), width = 0.2, color = "red") +
  facet_wrap(facets = vars(wave.nr), nrow = 1, ncol = 5) +
  labs(x = "Age (years)", y = "Hospitalisation-fatality \n ratio (%)",
       title = "Age-specific hospitalisation-fatality ratio for first five SARS-CoV-2 waves in South Africa") + 
  theme_bw() + theme(text = element_text(family = "sans", size = 12), axis.text.x = element_text(angle = 90)) + 
  scale_x_discrete(expand = expansion(mult = c(0,0.05))) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,65)) 

hfr2 <-ggplot(non_med_attended_burden_country_table_age_out,aes(x = age.group, y = cfratio_age, group = wave.nr)) +  
  # geom_point(aes(color = wave.nr), size = 3) +
  geom_line(aes(color = wave.nr))+
  labs(x = "Age (years)", y = "Hospitalisation-fatality \n ratio (%)", 
       title = "Age-specific hospitalisation-fatality ratio for first five SARS-CoV-2 waves in South Africa",
       color = "Wave number") + 
  theme_bw() + theme(text = element_text(family = "sans", size = 12),
                     legend.text = element_text(size = 12)) + 
  scale_x_discrete(expand = expansion(mult = c(0,0.05))) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,60)) +
  guides(color = guide_legend(override.aes = list(linewidth = 3)))


ifr_ci <-confidence_intervals_out[[4]] %>%
  select(age.group,CI_low_ifr,CI_high_ifr)
ifr_waves <- cbind.data.frame(rep(seq(1,5,1), each = length(unique(ifr_ci$age.group))),
                              ifr_ci, row.names = NULL)
names(ifr_waves)[1] <- "wave.nr"
ifr_waves$wave.nr <- as.factor(ifr_waves$wave.nr)

infections_table_ifr <- left_join(x = infections_rates_age_table, 
                                  y = ifr_waves[,c("wave.nr","age.group","CI_low_ifr","CI_high_ifr")],
                                  by = c("wave.nr","age.group"))



ifr1 <- ggplot(infections_table_ifr, aes(x = age.group, group = 1)) + 
  geom_point(aes(y = infection_rate), color = "aquamarine4", size = 1) + 
  geom_line(aes(y = infection_rate), color = "aquamarine4")+
  geom_errorbar(aes(ymin = CI_low_ifr,ymax = CI_high_ifr), width = 0.2, color = "red") +
  facet_wrap(facets = vars(wave.nr), nrow = 1, ncol = 5) +
  labs(x = "Age (years)", y = "Infection-fatality \n ratio (%)", 
       title = "Age-specific infection-fatality ratio for first five SARS-CoV-2 waves in South Africa") + 
  theme_bw() + theme(text = element_text(family = "sans", size = 12), 
                     axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,21)) +
  scale_x_discrete(expand = expansion(mult = c(0,0.05))) 

ifr2 <- ggplot(infections_table_ifr,aes(x = age.group, y = infection_rate, group = wave.nr)) +  
  #geom_point(aes(color = wave.nr), size = 3) +
  geom_line(aes(color = wave.nr))+
  labs(x = "Age (years)", y = "Infection-fatality \n ratio (%)", 
      title = "Age-specific infection-fatality ratio for first five SARS-CoV-2 waves in South Africa",
       color = "Wave number") + 
  theme_bw() + theme(text = element_text(family = "sans", size = 12),
                     legend.text = element_text(size = 12)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,11)) +
  scale_x_discrete(expand = expansion(mult = c(0,0.05))) +
  guides(color = guide_legend(override.aes = list(linewidth = 3)))
  
figure.ifr <- ggpubr::ggarrange(hfr1,ifr1,ncol =1, nrow = 2, labels = c("A","B"), vjust = 3, common.legend = TRUE, legend = "bottom")
figure.hfr <- ggpubr::ggarrange(hfr2,ifr2,ncol =1, nrow = 2, labels = c("A","B"), vjust = 3, common.legend = TRUE, legend = "bottom")
plot(figure.hfr)
plot(figure.ifr)
# burden_country_total_unadjusted <- non_attend_burden_unadjusted[[1]]
# burden_country_strat_unadjusted <- non_attend_burden_unadjusted[[2]]
#-----------------------------------------------------------------------------------------------------------------

# prop.refer <- med_attended_mild_burden_pre[[1]][2]
# hospital_sri <- med_attended_mild_burden_pre[[2]]
# outpatient_ili <- med_attended_mild_burden_pre[[3]]



#------------------------------------------------------------------------------------------------------------

#7. CONFIDENCE INTERVALS

##Update wave_data
wave_datcov_data_admit <- wave_data_admit[[1]] # wave data for admissions
wave_datcov_data_admit <- lapply(wave_datcov_data_admit, function(y){
  y %>% 
    rename(Sex = BirthSex) %>%
    mutate(Sex = fct_explicit_na(Sex, na_level = NA_character_)) %>%
    filter(Sex== "Male" | Sex == "Female") %>%
    mutate(Sex = factor(Sex, levels = c("Female","Male"))) 
})
wave_pop_peaks_admit_age <- lapply(wave_in_admit[[2]], function(a1){ #population estimate at peaks stratified by age only
  
  a1 %>% 
    group_by(age.group) %>%
    summarise(age_pop_estimate_at_peak = sum(age_pop_estimate_at_peak))
})

wave_datcov_data_death <- wave_data_death[[1]] # wave data for deaths
wave_datcov_data_death <- lapply(wave_datcov_data_death, function(y){
  y %>% 
    rename(Sex = BirthSex) %>%
    mutate(Sex = fct_explicit_na(Sex, na_level = NA_character_)) %>%
    filter(Sex== "Male" | Sex == "Female") %>%
    mutate(Sex = factor(Sex, levels = c("Female","Male"))) 
})
wave_pop_peaks_death_age <- lapply(wave_in_death[[2]], function(a2){ #population estimate at peaks stratified by age only
  
  a2 %>% 
    group_by(age.group) %>%
    summarise(age_pop_estimate_at_peak = sum(age_pop_estimate_at_peak))
})

#a. generate bootstrap samples of wave_data (DATCOV data)
# source('./Code/BurdenBoot.R')
#input = wave data for admissions refactored and wave data for deaths refactored
bootstrap_samples <- burden_bootstrap_data(data_admit_in = wave_datcov_data_admit,data_death_in = wave_datcov_data_death)

#output:
bootstrap_samples_admit <- readRDS(file.path('./Data/wave_bootstrap_data_admit.RDS')) #bootstrapped wave data for admissions
bootstrap_samples_death <- readRDS(file.path('./Data/wave_bootstrap_data_death.RDS')) #bootstrapped wave data for deaths


#----------------------------------------------------------------------------------------------------------------------------------------------------------
