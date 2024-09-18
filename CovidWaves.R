#This script determines when waves and peaks are found and interpolates population estimates.

covid_waves_def <- function(data_dir, data_datcov, data_covid,additional,blood_draws){


#------------------------------------------------------------------------------------------------------------------------------------  
 ##COUNTRYWIDE ESTIMATES 
#Determine which hospitalisations were COVID-19 related 
  ##Total hospital admissions irrespective of admission reason
covid_datcov <- data_datcov %>%
   mutate(epiyear = epiyear(Adm.Date.AdmissionDate),  #extract year of admission for covid
         epiweek = epiweek(Adm.Date.AdmissionDate)) #extract epi week of admission for covid
    
    ##Overall - no stratification
  covid_datcov_admissions_total <- covid_datcov %>%
  group_by(epiyear,epiweek) %>%  #determine the number of hospital admissions per epiweek per year
  count()
  covid_datcov_admissions_total$epiyear <- as.factor(covid_datcov_admissions_total$epiyear)
  covid_datcov_admissions_total$epiweek <- as.factor(covid_datcov_admissions_total$epiweek)
names(covid_datcov_admissions_total)[ncol(covid_datcov_admissions_total)] <- "Admissions"

#covid-specific admissions known
covid_admit <- covid_datcov %>%
  filter(AdmissionReason == "COVID-19 symptoms") 
covid_admit_total <- covid_admit %>%
  group_by(epiyear,epiweek) %>%  #determine the number of hospital admissions per epiweek per year
  count()
covid_admit_total$epiyear <- as.factor(covid_admit_total$epiyear)
covid_admit_total$epiweek <- as.factor(covid_admit_total$epiweek)
names(covid_admit_total)[ncol(covid_admit_total)] <- "Admissions"

#in-hospital deaths
covid_death <- covid_datcov %>%
  filter(Discharge.Status == "Died") %>%
  group_by(epiyear,epiweek) %>%
  count()
covid_death$epiyear <- as.factor(covid_death$epiyear)
covid_death$epiweek <- as.factor(covid_death$epiweek)
names(covid_death)[ncol(covid_death)] <- "Deaths"

 ##Stratified by age and sex
# covid_datcov_admissions_strat <- covid_datcov %>%
#   group_by(age.group,BirthSex,epiyear,epiweek) %>%  #determine the number of hospital admissions per epiweek per year
#   count()
# covid_datcov_admissions_strat$epiyear <- as.factor(covid_datcov_admissions_strat$epiyear)
# covid_datcov_admissions_strat$epiweek <- as.factor(covid_datcov_admissions_strat$epiweek)
# names(covid_datcov_admissions_strat)[ncol(covid_datcov_admissions_strat)] <- "Admissions"
# 
# covid_datcov <- covid_datcov %>%
#   mutate(AdmissionReason = ifelse(!str_detect(AdmissionReason,"[A-Z]+"),NA,AdmissionReason)) %>%
#   mutate(AdmissionReason = fct_explicit_na(AdmissionReason, na_level = "NA")) 
# # %>%
# #   mutate(Province = factor(Province, levels = province_input))

###All national hospitalisations with no particulars on which covid related
#data_cov_prov <- readRDS(file = "./Data/data_cov_prov_total.RDS") #read in data extract


#Determine the case count by year and epi week
##Overall - no stratification
total_epiweek_est <- data_covid %>% 
  group_by(epiyear,epiweek) %>%  # determine the number of covid cases per year and epiweek
  count() 
total_epiweek_est$epiyear <- as.factor(total_epiweek_est$epiyear)
total_epiweek_est$epiweek <- as.factor(total_epiweek_est$epiweek)
names(total_epiweek_est)[ncol(total_epiweek_est)] <- "cases"

##Stratified by age and sex
# names(data_covid)[2] <- "BirthSex"
# total_epiweek_strat <- data_covid %>% 
#   group_by(age.group,BirthSex,epiyear,epiweek) %>%  # determine the number of covid cases per year and epiweek
#   count() 
# total_epiweek_strat$epiyear <- as.factor(total_epiweek_strat$epiyear)
# total_epiweek_strat$epiweek <- as.factor(total_epiweek_strat$epiweek)
# names(total_epiweek_strat)[ncol(total_epiweek_strat)] <- "cases"

###Plot the number of COVID cases and respective hospitalisations and deaths
total_admit <- full_join(x = covid_datcov_admissions_total, y = covid_admit_total, by = c("epiyear","epiweek")) #combine all hospitalisations
names(total_admit)[names(total_admit) == "Admissions.x"] <- "Total_admit"
names(total_admit)[names(total_admit) == "Admissions.y"] <- "Covid_admit"
total_admit <- total_admit %>%
  mutate(Covid_admit = ifelse(is.na(Covid_admit),0,Covid_admit))
total_plot <- full_join(x = total_epiweek_est, y = total_admit, by = c("epiyear","epiweek"))  #combine the covid cases and total admissions by year and epi week
total_plot <- select(total_plot, c("epiweek","epiyear","cases","Covid_admit","Total_admit"))
total_plot <- full_join(x = total_plot, y = covid_death, by = c("epiyear","epiweek")) #combine cases, hospitalisations and deaths
# total_plot_strat <- full_join(x = total_epiweek_strat, y = covid_datcov_admissions_strat, by = c("age.group", "BirthSex","epiyear","epiweek"))  #combine the datasets by year and epi week
# total_plot_strat <- select(total_plot_strat, c("age.group", "BirthSex","epiweek","epiyear","cases","Admissions"))
excess_deaths <- read.xlsx(xlsxFile = file.path(data_dir,additional))

excess_deaths_upd <- excess_deaths %>%
  mutate(Week_starting = convert_to_date(Week_starting)) %>%
  mutate(Count = round(Count,0)) %>%
  mutate(epiweek = epiweek(Week_starting),
         epiyear = epiyear(Week_starting)) %>%
  mutate(epiweek = as.factor(epiweek),
         epiyear = as.factor(epiyear)) %>%
  filter(Week_starting <= ymd("2022-09-18"))
names(excess_deaths_upd)[names(excess_deaths_upd) == "Count"] <- "Counts"
total_plot_upd <- full_join(x = total_plot, y = excess_deaths_upd[,c("Counts","epiweek","epiyear")], by = c("epiyear","epiweek"))
names(total_plot_upd)[names(total_plot_upd) == "Counts"] <- "Excess_death"


total_plot_upd_excess <- total_plot_upd %>%
  pivot_longer(cols = cases:Excess_death,
               names_to = "Number",
               values_to = "Counts") %>%
  mutate(Number = factor(Number, levels = c("cases","Total_admit","Covid_admit","Deaths","Excess_death")))


# covid1 <- ggplot(total_plot,
#        aes(x = epiweek)) +    
#   geom_col(aes(y = cases, fill = "Confirmed COVID-19 Cases")) +
#     geom_col(aes(y = Admissions, fill = "COVID-19 related hospital admissions")) +
#     scale_x_discrete(name = "Epidemiological week",
#                labels = total_plot$epiweek,
#                breaks = total_plot$epiweek) +             
#   scale_y_continuous(
#     expand  = c(0,0),  #remove excess space
#     limits = c(0,max(total_plot$cases)),  #define the limits for the y axis
#     n.breaks = 10)+                      # remove excess space below x-axis
#   facet_grid(~epiyear,        # facet on year (of Date class column)
#              space="free_x",                
#              scales="free_x",               # x-axes adapt to data range (not "fixed")
#              switch="x") +                  # facet labels (year) on bottom
#   theme_bw() +
#   theme(strip.placement = "outside",                  # facet label placement
#         strip.background = element_blank(),         # no facet lable background
#         panel.grid.minor.x = element_blank(),          
#         panel.border = element_blank(),             # no border for facet panel
#         panel.spacing=unit(0,"cm"),
#         text = element_text(family = "serif", size = 16),
#         axis.text.x = element_text(size = 8),
#         legend.text = element_text(size = 16),
#         legend.position = "bottom") +
#   labs(title = "South Africa COVID-19 confirmed cases and hospitalisations",
#        y = "Number",
#        caption = "Data source: NICD",
#        fill = element_blank()) +
#   scale_fill_manual(values = c("pink","purple")) 
# print(covid1)
##------------------------------------------------------------------------------
##determining wave cut-offs based on peaks and lowest cases around peaks

#Peak cases
peak_cases_prep <- total_plot_upd %>%  #this is the dataset that contains all cases, hospitalisations and deaths
  group_by(epiyear,epiweek) %>%
  summarise(total.cases = sum(cases, na.rm = FALSE), .groups = "keep") %>%
  ungroup()
peak_cases <- peak_cases_prep[peaks(peak_cases_prep$total.cases, span = 11) == TRUE,] ##span is set to 11 to ensure that the utmost peak is selected
peak_cases$peak_nr <- seq(from = 1, to = nrow(peak_cases), by = 1)

#Peak total hospitalisations - full datcov
peak_admission_prep <- total_plot_upd %>%
  group_by(epiyear,epiweek) %>%
  summarise(total.admit = sum(Total_admit, na.rm = TRUE), .groups = "keep") %>%
  ungroup()
peak_admissions <- peak_admission_prep[peaks(peak_admission_prep$total.admit, span = 11) == TRUE,]
peak_admissions$peak_nr <- seq(from = 1, to = nrow(peak_admissions), by = 1)

#Peak COVID hospitalisations
peak_admission_prep_covid <- total_plot_upd %>%
  group_by(epiyear,epiweek) %>%
  summarise(covid.admit = sum(Covid_admit, na.rm = TRUE), .groups = "keep") %>%
  ungroup()
peak_admissions_covid <- peak_admission_prep_covid[peaks(peak_admission_prep_covid$covid.admit, span = 11) == TRUE,]
peak_admissions_covid$peak_nr <- seq(from = 1, to = nrow(peak_admissions_covid), by = 1)

#Peak in-hospital deaths
peak_deaths_prep <- total_plot_upd %>%
  group_by(epiyear,epiweek) %>%
  summarise(total.death = sum(Deaths, na.rm = TRUE), .groups = "keep") %>%
  ungroup()
peak_deaths <- peak_deaths_prep[peaks(peak_deaths_prep$total.death, span = 11) == TRUE,] ##span is set to 11 to ensure that the utmost peak is selected
peak_deaths$peak_nr <- seq(from = 1, to = nrow(peak_deaths), by = 1)

#Peak excess deaths
peak_excess_deaths_prep <- total_plot_upd %>%
  group_by(epiyear,epiweek) %>%
  summarise(total.excess = sum(Excess_death, na.rm = TRUE), .groups = "keep") %>%
  ungroup()
peak_excess_deaths <- peak_excess_deaths_prep[peaks(peak_excess_deaths_prep$total.excess, span = 11) == TRUE,] ##span is set to 11 to ensure that the utmost peak is selected
peak_excess_deaths$peak_nr <- seq(from = 1, to = nrow(peak_excess_deaths), by = 1)

#determine between peak points - that is the minimum number of cases/hospitalisations/deaths between peaks (Nadir)
#Nadir of cases
waves_set <- data.frame()
for (i in 1:(nrow(peak_cases)-1)){
interwave_time <- select(peak_cases[i:(i+1), ], epiyear:total.cases)
interwave_cases_1 <- which(peak_cases_prep$epiweek == interwave_time$epiweek[1] &  #which cases fell in the period between the peaks
                             peak_cases_prep$epiyear == interwave_time$epiyear[1]) 
interwave_cases_2 <- which(peak_cases_prep$epiweek == interwave_time$epiweek[2] &
                             peak_cases_prep$epiyear == interwave_time$epiyear[2])
interwave_total <- peak_cases_prep[interwave_cases_1:interwave_cases_2,]
waves <- subset(interwave_total,interwave_total$total.cases ==min(interwave_total$total.cases)) #determine the minimum
waves_set <- bind_rows(waves_set,waves)

}


prefirst1 <- which(as.numeric(peak_cases_prep$epiweek) == as.numeric(peak_cases$epiweek[1]) &  # before the first peak
                    as.numeric(peak_cases_prep$epiyear) == as.numeric(peak_cases$epiyear[1]))
end_wave <- which(as.numeric(peak_cases_prep$epiweek) == as.numeric(peak_cases$epiweek[nrow(peak_cases)]) &  # after the last peak
                    as.numeric(peak_cases_prep$epiyear) == as.numeric(peak_cases$epiyear[nrow(peak_cases)]))
prefirst_wave <- peak_cases_prep[1:prefirst1,]
ending_wave <- peak_cases_prep[end_wave:nrow(peak_cases_prep),]
waves_first <- slice_min(.data = as.data.frame(prefirst_wave), order_by = total.cases, with_ties = FALSE)
waves_ending <- as.data.frame(ending_wave[ending_wave$epiweek == 32,])
waves_set <- bind_rows(waves_set,waves_first,waves_ending)
waves_set <- arrange(waves_set, epiyear,epiweek)  #collect together all the information about the wave cuts

#Nadir of total hospitalisations
waves_set_admit <- data.frame()
for (i in 1:(nrow(peak_admissions)-1)){
  interwave_time <- select(peak_admissions[i:(i+1), ], epiyear:total.admit)
  interwave_cases_1 <- which(peak_admission_prep$epiweek == interwave_time$epiweek[1] &  #which  fell in the period between the peaks
                               peak_admission_prep$epiyear == interwave_time$epiyear[1]) 
  interwave_cases_2 <- which(peak_admission_prep$epiweek == interwave_time$epiweek[2] &
                               peak_admission_prep$epiyear == interwave_time$epiyear[2])
  interwave_total <- peak_admission_prep[interwave_cases_1:interwave_cases_2,]
  waves <- subset(interwave_total,interwave_total$total.admit ==min(interwave_total$total.admit)) #determine the minimum
  waves_set_admit <- bind_rows(waves_set_admit,waves)
  
}


prefirst1admit <- which(as.numeric(peak_admission_prep$epiweek) == as.numeric(peak_admissions$epiweek[1]) &  # before the first peak
                     as.numeric(peak_admission_prep$epiyear) == as.numeric(peak_admissions$epiyear[1]))
end_wave_admit <- which(as.numeric(peak_admission_prep$epiweek) == as.numeric(peak_admissions$epiweek[nrow(peak_admissions)]) &  # after the last peak
                    as.numeric(peak_admission_prep$epiyear) == as.numeric(peak_admissions$epiyear[nrow(peak_admissions)]))
prefirst_wave_admit <- peak_admission_prep[1:prefirst1admit,]
ending_wave_admit <- peak_admission_prep[end_wave_admit:nrow(peak_admission_prep),]
waves_first_admit <- slice_min(.data = as.data.frame(prefirst_wave_admit), order_by = total.admit, with_ties = FALSE)
waves_ending_admit <- as.data.frame(ending_wave_admit[ending_wave_admit$epiweek == 32,])
waves_set_admit <- bind_rows(waves_set_admit,waves_first_admit,waves_ending_admit)
waves_set_admit <- arrange(waves_set_admit, epiyear,epiweek)  #collect together all the information about the wave cuts
 

#Nadir of COVID admissions
waves_set_covid_admit <- data.frame()
for (i in 1:(nrow(peak_admissions_covid)-1)){
  interwave_time <- select(peak_admissions_covid[i:(i+1), ], epiyear:covid.admit)
  interwave_cases_1 <- which(peak_admission_prep_covid$epiweek == interwave_time$epiweek[1] &  #which cases fell in the period between the peaks
                               peak_admission_prep_covid$epiyear == interwave_time$epiyear[1]) 
  interwave_cases_2 <- which(peak_admission_prep_covid$epiweek == interwave_time$epiweek[2] &
                               peak_admission_prep_covid$epiyear == interwave_time$epiyear[2])
  interwave_total <- peak_admission_prep_covid[interwave_cases_1:interwave_cases_2,]
  waves <- subset(interwave_total,interwave_total$covid.admit ==min(interwave_total$covid.admit)) #determine the minimum
  waves_set_covid_admit <- bind_rows(waves_set_covid_admit,waves)
  
}


prefirst1covid_admit <- which(as.numeric(peak_admission_prep_covid$epiweek) == as.numeric(peak_admissions_covid$epiweek[1]) &  # before the first peak
                          as.numeric(peak_admission_prep_covid$epiyear) == as.numeric(peak_admissions_covid$epiyear[1]))
end_wave_covid_admit <- which(as.numeric(peak_admission_prep_covid$epiweek) == as.numeric(peak_admissions_covid$epiweek[nrow(peak_admissions_covid)]) &  # after the last peak
                          as.numeric(peak_admission_prep_covid$epiyear) == as.numeric(peak_admissions_covid$epiyear[nrow(peak_admissions_covid)]))
prefirst_wave_covid_admit <- peak_admission_prep_covid[1:prefirst1covid_admit,]
ending_wave_covid_admit <- peak_admission_prep_covid[end_wave_covid_admit:nrow(peak_admission_prep_covid),]
waves_first_covid_admit <- slice_min(.data = as.data.frame(prefirst_wave_covid_admit), order_by = covid.admit, with_ties = FALSE)
waves_ending_covid_admit <- as.data.frame(ending_wave_covid_admit[ending_wave_covid_admit$epiweek == 32,])
waves_set_covid_admit <- bind_rows(waves_set_covid_admit,waves_first_covid_admit,waves_ending_covid_admit)
waves_set_covid_admit <- arrange(waves_set_covid_admit, epiyear,epiweek)  #collect together all the information about the wave cuts

#Nadir of deaths
waves_set_deaths <- data.frame()
for (i in 1:(nrow(peak_deaths)-1)){
  interwave_time <- select(peak_deaths[i:(i+1), ], epiyear:total.death)
  interwave_cases_1 <- which(peak_deaths_prep$epiweek == interwave_time$epiweek[1] &  #which cases fell in the period between the peaks
                               peak_deaths_prep$epiyear == interwave_time$epiyear[1]) 
  interwave_cases_2 <- which(peak_deaths_prep$epiweek == interwave_time$epiweek[2] &
                               peak_deaths_prep$epiyear == interwave_time$epiyear[2])
  interwave_total <- peak_deaths_prep[interwave_cases_1:interwave_cases_2,]
  if (i != 2){
  waves <- subset(interwave_total,interwave_total$total.death ==min(interwave_total$total.death)) #determine the minimum
  } else {
    wave3 <- subset(interwave_total,interwave_total$total.death ==min(interwave_total$total.death))$epiweek
    wave3_upd <- varhandle::unfactor(wave3) + 1
    waves <- subset(interwave_total,interwave_total$epiweek == wave3_upd)
  }
  waves_set_deaths <- bind_rows(waves_set_deaths,waves)
  
}

  deaths_pre <- waves_set_deaths %>%
    filter(duplicated(total.death))
  deaths_pre2 <- waves_set_deaths %>%
    filter(total.death != deaths_pre$total.death)
  waves_set_deaths <- bind_rows(deaths_pre,deaths_pre2)
  


prefirst1covid_death <- which(as.numeric(peak_deaths_prep$epiweek) == as.numeric(peak_deaths$epiweek[1]) &  # before the first peak
                                as.numeric(peak_deaths_prep$epiyear) == as.numeric(peak_deaths$epiyear[1]))
end_wave_covid_death <- which(as.numeric(peak_deaths_prep$epiweek) == as.numeric(peak_deaths$epiweek[nrow(peak_deaths)]) &  # after the last peak
                                as.numeric(peak_deaths_prep$epiyear) == as.numeric(peak_deaths$epiyear[nrow(peak_deaths)]))
prefirst_wave_covid_death <- peak_deaths_prep[1:prefirst1covid_death,]
ending_wave_covid_death <- peak_deaths_prep[end_wave_covid_death:nrow(peak_deaths_prep),]
waves_first_covid_death <- slice_min(.data = as.data.frame(prefirst_wave_covid_death), order_by = total.death, with_ties = FALSE)
waves_ending_covid_death <- as.data.frame(ending_wave_covid_death[ending_wave_covid_death$epiweek == 32,])
waves_set_deaths <- bind_rows(waves_set_deaths,waves_first_covid_death,waves_ending_covid_death)
waves_set_deaths <- arrange(waves_set_deaths, epiyear,epiweek)  #collect together all the information about the wave cuts

#Nadir of excess deaths
waves_set_excess_deaths <- data.frame()
for (i in 1:(nrow(peak_excess_deaths)-1)){
  interwave_time <- select(peak_excess_deaths[i:(i+1), ], epiyear:total.excess)
  interwave_cases_1 <- which(peak_excess_deaths_prep$epiweek == interwave_time$epiweek[1] &  #which cases fell in the period between the peaks
                               peak_excess_deaths_prep$epiyear == interwave_time$epiyear[1]) 
  interwave_cases_2 <- which(peak_excess_deaths_prep$epiweek == interwave_time$epiweek[2] &
                               peak_excess_deaths_prep$epiyear == interwave_time$epiyear[2])
  interwave_total <- peak_excess_deaths_prep[interwave_cases_1:interwave_cases_2,]
  waves <- subset(interwave_total,interwave_total$total.excess ==min(interwave_total$total.excess)) #determine the minimum
  waves_set_excess_deaths <- bind_rows(waves_set_excess_deaths,waves)
  
}


prefirst1_excess <- which(as.numeric(peak_excess_deaths_prep$epiweek) == as.numeric(peak_excess_deaths$epiweek[1]) &  # before the first peak
                                as.numeric(peak_excess_deaths_prep$epiyear) == as.numeric(peak_excess_deaths$epiyear[1]))
end_wave_excess <- which(as.numeric(peak_excess_deaths_prep$epiweek) == as.numeric(peak_excess_deaths$epiweek[nrow(peak_excess_deaths)]) &  # after the last peak
                                as.numeric(peak_excess_deaths_prep$epiyear) == as.numeric(peak_excess_deaths$epiyear[nrow(peak_excess_deaths)]))
prefirst_wave_excess <- peak_excess_deaths_prep[1:prefirst1_excess,]
ending_wave_excess <- peak_excess_deaths_prep[end_wave_excess:nrow(peak_excess_deaths_prep),]
waves_first_excess <- slice_min(.data = as.data.frame(prefirst_wave_excess), order_by = total.excess, with_ties = FALSE)
waves_ending_excess <- as.data.frame(ending_wave_excess[ending_wave_excess$epiweek == 32,])
waves_set_excess_deaths <- bind_rows(waves_set_excess_deaths,waves_first_excess,waves_ending_excess)
waves_set_excess_deaths <- arrange(waves_set_excess_deaths, epiyear,epiweek)  #collect together all the information about the wave cuts



total_plot_upd_excess$Number <- recode_factor(total_plot_upd_excess$Number,
                                       "cases" = "Cases",
                                       "Total_admit" = "Admissions",
                                       "Covid_admit" = "COVID-19 admissions",
                                       "Deaths" = "Deaths",
                                       "Excess_death" = "Excess deaths")
total_plot_upd_excess <- total_plot_upd_excess %>%
  filter(Number != "COVID-19 admissions", Number != "Excess deaths")

covid2 <- ggplot() +
  geom_col(data = total_plot_upd_excess, aes(x = epiweek, y = Counts, fill = Number), position = position_dodge() ) +
  # geom_col(aes(y = Admissions, fill = "COVID-19 related hospital admissions")) +
  #geom_line(data = covid_incidence, aes(x = epiweek, y = incidence*b, group = 1),na.rm = TRUE, size = 1.1, color = "darkorchid") +
  geom_point(data = peak_cases, aes( x = epiweek, y = total.cases), size = 4, color = "grey") +
  geom_segment(data = waves_set, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of cases"), linewidth = 1.2) +
  geom_point(data = peak_admissions, aes( x = epiweek, y = total.admit), size = 4, color = "aquamarine4") +
  geom_segment(data = waves_set_admit, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of admissions"), linewidth = 1.2) +
  #geom_point(data = peak_admissions_covid, aes( x = epiweek, y = covid.admit), size = 2, color = "purple4",) +
  #geom_segment(data = waves_set_covid_admit, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of COVID-19 admissions"), size = 1.2) +
  geom_point(data = peak_deaths, aes( x = epiweek, y = total.death), size = 4, color = "red4") +
  geom_segment(data = waves_set_deaths, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of deaths"), linewidth = 1.2) +
  #geom_point(data = peak_excess_deaths, aes( x = epiweek, y = total.excess), size = 2, color = "orange3") +
  #geom_segment(data = waves_set_excess_deaths, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of excess deaths"), size = 1.2) +
  geom_rect(data = blood_draws, aes(xmin = Start_epiweek, xmax = Finish_epiweek, ymin = 0 , ymax = max(peak_cases$total.cases)), fill = "pink",alpha = 0.5) +
  scale_x_discrete(name = "Epidemiological week",
                   labels = as.character(c(seq(10,53,3),seq(1,53,3),seq(1,38,3))),
                   breaks = as.character(c(seq(10,53,3),seq(1,53,3),seq(1,38,3)))) +             # remove excess space
  scale_y_continuous(name = "Number",
    expand  = c(0,0), # remove excess space below x-axis
    limits = c(0,155000),
    n.breaks = 10) +
    # sec.axis = sec_axis(trans = ~./b, name = "Incidence/100 000 population at risk"))+
  facet_grid(~epiyear,        # facet on year (of Date class column)
             space="free_x",
             scales="free_x",               # x-axes adapt to data range (not "fixed")
             switch="x") +                  # facet labels (year) on bottom
  theme_classic() +
  theme(strip.placement = "outside",                  # facet label placement
        strip.background = element_blank(),         # no facet lable background
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),             # no border for facet panel
        panel.spacing=unit(0,"cm"),
        text = element_text(family = "sans", size = 18),
        axis.text.y.left = element_text(size = 16),
        axis.title.y.left = element_text(size = 18),
        # axis.text.y.right = element_text(size = 16, color = "darkorchid"),
        # axis.title.y.right = element_text(size = 16, color = "darkorchid"),
        axis.text.x = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.position = "right",
        strip.text.x = element_text(size = 16)) +
  labs(#title = "South Africa COVID-19 confirmed cases, hospitalisations and in-hospital deaths",
       #caption = "Data source: NICD",
       color = element_blank(),
       fill =  element_blank())  +
  scale_color_manual(values = c("aquamarine4","black","red4")) +
  scale_fill_manual(values = c("grey","aquamarine4","red4"))
#source: https://epirhandbook.com/en/epidemic-curves.html#multi-level-date-labels
#https://stackoverflow.com/questions/9109156/ggplot-combining-two-plots-from-different-data-frames
print(covid2)

covid3 <- ggplot() +
  geom_col(data = total_plot_upd_excess[total_plot_upd_excess$Number == "Cases",], aes(x = epiweek, y = Counts), position = position_dodge(), fill = "grey" ) +
  # geom_col(aes(y = Admissions, fill = "COVID-19 related hospital admissions")) +
  #geom_line(data = covid_incidence, aes(x = epiweek, y = incidence*b, group = 1),na.rm = TRUE, size = 1.1, color = "darkorchid") +
 # geom_point(data = peak_cases, aes( x = epiweek, y = total.cases), size = 2, color = "grey") +
  # geom_segment(data = waves_set, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of cases"), size = 1.2) +
  # geom_point(data = peak_admissions, aes( x = epiweek, y = total.admit), size = 2, color = "aquamarine4") +
  # geom_segment(data = waves_set_admit, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of total admissions"), size = 1.2) +
  #geom_point(data = peak_admissions_covid, aes( x = epiweek, y = covid.admit), size = 2, color = "purple4",) +
  #geom_segment(data = waves_set_covid_admit, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of COVID-19 admissions"), size = 1.2) +
  # geom_point(data = peak_deaths, aes( x = epiweek, y = total.death), size = 2, color = "red4") +
  # geom_segment(data = waves_set_deaths, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of deaths"), size = 1.2) +
  #geom_point(data = peak_excess_deaths, aes( x = epiweek, y = total.excess), size = 2, color = "orange3") +
  #geom_segment(data = waves_set_excess_deaths, aes(x = epiweek, xend = epiweek,y = 0, yend = max(peak_cases$total.cases), color = "Nadir of excess deaths"), size = 1.2) +
  #geom_ribbon(data = covid_incidence, aes(x = epiweek, y = incidence*b, xmin = epiweek, xmax = epiweek, ymin = 0, ymax = max(incidence*b,na.rm = TRUE), fill = "wave"),alpha = 0.5) +
  scale_x_discrete(name = "Epidemiological week",
                   labels = as.character(c(seq(10,53,3),seq(1,53,3),seq(1,38,3))),
                   breaks = as.character(c(seq(10,53,3),seq(1,53,3),seq(1,38,3)))) +             # remove excess space
  scale_y_continuous(name = "Number of confirmed cases",
                     expand  = c(0,0), # remove excess space below x-axis
                     limits = c(0,155000),
                     n.breaks = 10) +
  # sec.axis = sec_axis(trans = ~./b, name = "Incidence/100 000 population at risk"))+
  facet_grid(~epiyear,        # facet on year (of Date class column)
             space="free_x",
             scales="free_x",               # x-axes adapt to data range (not "fixed")
             switch="x") +                  # facet labels (year) on bottom
  theme_classic() +
  theme(strip.placement = "outside",                  # facet label placement
        strip.background = element_blank(),         # no facet lable background
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),             # no border for facet panel
        panel.spacing=unit(0,"cm"),
        text = element_text(family = "sans", size = 18),
        axis.text.y.left = element_text(size = 16),
        axis.title.y.left = element_text(size = 18),
        # axis.text.y.right = element_text(size = 16, color = "darkorchid"),
        # axis.title.y.right = element_text(size = 16, color = "darkorchid"),
        axis.text.x = element_text(size = 16),
        strip.text.x = element_text(size = 16))
        # legend.text = element_text(size = 14),
        # legend.position = "right") +
  # labs(title = "South Africa COVID-19 confirmed cases",
  #      caption = "Data source: NICD")
       # color = element_blank(),
       # fill =  element_blank())  
    # scale_fill_manual(values = c("grey","aquamarine4","red4"))
#source: https://epirhandbook.com/en/epidemic-curves.html#multi-level-date-labels
#https://stackoverflow.com/questions/9109156/ggplot-combining-two-plots-from-different-data-frames
print(covid3)


combine_peaks <- peak_cases %>%
  mutate(Type = 0) %>%
  mutate(Type = "COVID-19 cases") %>%
  select(peak_nr,epiweek,epiyear,Type,total.cases)
names(combine_peaks)[names(combine_peaks) == "total.cases"] <- "Counts"

peak_admissions_add <- peak_admissions %>% 
  mutate(Type = 0)  %>%
  mutate(Type = "All hospital admissions") %>%
  select(peak_nr,epiweek,epiyear,Type,total.admit)
names(peak_admissions_add)[names(peak_admissions_add) == "total.admit"] <- "Counts"
# peak_covid_admissions_add <- peak_admissions_covid %>% 
#   mutate(Type = 0)  %>%
#   mutate(Type = "COVID-19 hospital admissions") %>%
#   select(peak_nr,epiweek,epiyear,Type,covid.admit)
# names(peak_covid_admissions_add)[names(peak_covid_admissions_add) == "covid.admit"] <- "Counts"
peak_deaths_add <- peak_deaths %>% 
  mutate(Type = 0)  %>%
  mutate(Type = "In-hospital deaths") %>%
  select(peak_nr,epiweek,epiyear,Type,total.death)
names(peak_deaths_add)[names(peak_deaths_add) == "total.death"] <- "Counts"
peak_excess_deaths_add <- peak_excess_deaths %>% 
  mutate(Type = 0)  %>%
  mutate(Type = "Excess deaths") %>%
  select(peak_nr,epiweek,epiyear,Type,total.excess)
names(peak_excess_deaths_add)[names(peak_excess_deaths_add) == "total.excess"] <- "Counts"

combine_peaks <- rbind.data.frame(combine_peaks,
                                  peak_admissions_add,
                                 # peak_covid_admissions_add, 
                                  peak_deaths_add,
                                  peak_excess_deaths_add, row.names = NULL)
combine_peaks <- combine_peaks %>%
  arrange(peak_nr)
combine_peaks_dates <- left_join(x=combine_peaks, y = excess_deaths_upd[,c("Week_starting","epiweek","epiyear")], by = c("epiyear","epiweek"))

waves_list <- list(waves_set,waves_set_admit,waves_set_deaths,waves_set_excess_deaths)
start_epiweek <- ymd(min(data_covid$colldate1) - days(1))
waves_combine_dates <- lapply(waves_list,function(p){
 new1 <- left_join(x = p,  y = excess_deaths_upd[,c("Week_starting","epiweek","epiyear")], by = c("epiyear","epiweek")) 
 new2 <- new1 %>%
   mutate(Week_starting = if_else(is.na(Week_starting),start_epiweek,Week_starting))
})

combine_waves <- waves_combine_dates[[1]] %>%
  mutate(Type = 0) %>%
  mutate(Type = "COVID-19 cases") %>%
  select(Week_starting,epiweek,epiyear,Type,total.cases)
names(combine_waves)[names(combine_waves) == "total.cases"] <- "Counts"

wave_admissions_add <- waves_combine_dates[[2]] %>% 
  mutate(Type = 0)  %>%
  mutate(Type = "All hospital admissions") %>%
  select(Week_starting,epiweek,epiyear,Type,total.admit)
names(wave_admissions_add)[names(wave_admissions_add) == "total.admit"] <- "Counts"
# wave_covid_admissions_add <- waves_combine_dates[[3]] %>% 
#   mutate(Type = 0)  %>%
#   mutate(Type = "COVID-19 hospital admissions") %>%
#   select(Week_starting,epiweek,epiyear,Type,covid.admit)
# names(wave_covid_admissions_add)[names(wave_covid_admissions_add) == "covid.admit"] <- "Counts"
wave_deaths_add <- waves_combine_dates[[3]] %>% 
  mutate(Type = 0)  %>%
  mutate(Type = "In-hospital deaths") %>%
  select(Week_starting,epiweek,epiyear,Type,total.death)
names(wave_deaths_add)[names(wave_deaths_add) == "total.death"] <- "Counts"
wave_excess_deaths_add <- waves_combine_dates[[4]] %>% 
  mutate(Type = 0)  %>%
  mutate(Type = "Excess deaths") %>%
  select(Week_starting,epiweek,epiyear,Type,total.excess)
names(wave_excess_deaths_add)[names(wave_excess_deaths_add) == "total.excess"] <- "Counts"

combine_waves <- rbind.data.frame(combine_waves,
                                   wave_admissions_add,
#                                   wave_covid_admissions_add, 
                                   wave_deaths_add, row.names = NULL)
#                                   wave_excess_deaths_add, row.names = NULL)

# compare_waves <- list(combine_peaks_dates,combine_waves)
# names(compare_waves) <- c("peaks","waves")
#write.xlsx(compare_waves,"./Output/compare_waves.xlsx")

# for (kk in 1:nrow(waves_set)){
#   wave_duration_set <- rep(0, times = nrow(waves_set)-1)
#   wave_duration_set[kk] <- waves_set
#   
# }
# covid3 <- ggplot() +    
#   geom_col(data = covid_incidence, aes(x = epiweek, y = incidence, fill = wave)) +
#   scale_x_discrete(name = "Epidemiological week",
#                    labels = covid_incidence$epiweek,
#                    breaks = covid_incidence$epiweek) +             # remove excess space
#   scale_y_continuous(name = "Incidence / 100 000 population at risk",
#                      expand  = c(0,0), # remove excess space below x-axis
#                      limits = c(0,max(covid_incidence$incidence)),
#                      n.breaks = 10)+                      
#   facet_grid(~epiyear,        # facet on year (of Date class column)
#              space="free_x",                
#              scales="free_x",               # x-axes adapt to data range (not "fixed")
#              switch="x") +                  # facet labels (year) on bottom
#   theme_bw() +
#   theme(strip.placement = "outside",                  # facet label placement
#         strip.background = element_blank(),         # no facet lable background
#         panel.grid.minor.x = element_blank(),          
#         panel.border = element_blank(),             # no border for facet panel
#         panel.spacing=unit(0,"cm"),
#         text = element_text(family = "serif", size = 16),
#         axis.text.x = element_text(size = 8),
#         legend.text = element_text(size = 16),
#         legend.position = "bottom") +
#   labs(title = "South Africa COVID-19 incidence per 100 000 population at risk",
#        caption = "Data source: NICD",
#        color = element_blank(),
#        fill = element_blank())  +
#   scale_color_manual(values = c("deepskyblue1")) +
#   scale_fill_manual(values = c("aquamarine4","darkolivegreen1"))
# print(covid3)

covid_waves_script <- list(peak_cases,
                           peak_admissions,
                           peak_deaths,
                           waves_combine_dates[[1]],
                           waves_combine_dates[[2]],
                           waves_combine_dates[[3]],
                           combine_waves,
                           covid_datcov)
                          # covid_datcov, #covid admissions in total
                          # covid_datcov_admissions_total, #covid admissions per epiweek and year overall
                           #covid_datcov_admissions_strat) #covid admissions per epiweek and year by strata
                           # peak_cases_province, waves_set_province,
                           # covid_datcov_province,
                           # covid_datcov_province_admissions_total, #covid admissions per provice, epiweek and year overall
                           # covid_datcov_province_admissions_age, #covid admissions per provice, epiweek and year by age
                           # covid_datcov_province_admissions_sex) #covid admissions per provice, epiweek and year by sex
#output: data on when peak in cases happended, the epiweeks for waves, interpolated population estimates,
#datcov data to later find hospital admissions within waves

}




