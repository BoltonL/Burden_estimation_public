estimate_waves <- function(pop, data_in){

  strat_pop_at_risk <- pop[[1]]
  overall_pop_at_risk <- pop[[2]]
  
 
  #Stratified
waves_datcov_strat <- list()
wave_pop_est_list_strat <- list()
wave_dates_overall_strat <- list()
for (k in 1:(length(unique(strat_pop_at_risk$peak_nr)))){#for every wave period
  # define waves
   # k <- 1
  wave_points <- strat_pop_at_risk %>%  #pick out the particular peaks to define wave under consideration
    filter(peak_nr == unique(strat_pop_at_risk$peak_nr)[k])
  wave_start <- slice_head(wave_points,n = length(levels(strat_pop_at_risk$Age))*length(levels(strat_pop_at_risk$Sex))) #define start of the wave
  wave_end <- slice_tail(wave_points, n = length(levels(strat_pop_at_risk$Age))*length(levels(strat_pop_at_risk$Sex))) #define end of the wave
  wave_pop_est <- wave_points[!duplicated(wave_points$age_pop_estimate_at_peak),c("Sex","Age","age_pop_estimate_at_peak")] #define the population estimate at peak
  names(wave_pop_est)[2] <- "age.group"
      datcov_wave_start <- unique(wave_start$Week_starting)
  # datcov_wave_start <- covid_datcov %>% #find starting dates of admissions for the wave
  #   filter(epiweek == unique(wave_start$wave_start_epiweek), 
  #          epiyear == unique(wave_start$wave_start_epiyear)) %>%
  #   #arrange(Adm.Date.AdmissionDate) %>%
  #   slice_min(order_by = Adm.Date.AdmissionDate,n = 1, with_ties = FALSE) %>%
  #   select(Adm.Date.AdmissionDate)
  
    # datcov_wave_start <- covid_datcov %>% #find starting dates of admissions for the wave
    #   filter(epiweek == unique(wave_start$wave_start_epiweek), 
    #          epiyear == unique(wave_start$wave_start_epiyear)) %>%
    #   #arrange(Adm.Date.AdmissionDate) %>%
    #   slice_max(order_by = Adm.Date.AdmissionDate,n = 1, with_ties = FALSE) %>%
    #   select(Adm.Date.AdmissionDate)
 
  datcov_wave_end <- unique(wave_end$Week_starting + days(6))
  # datcov_wave_end <- covid_datcov %>% #find ending dates of admissions for the wave
  #   filter(epiweek == unique(wave_end$wave_start_epiweek), 
  #          epiyear == unique(wave_end$wave_start_epiyear)) %>%
  #   #arrange(Adm.Date.AdmissionDate) %>%
  #   slice_max(order_by = Adm.Date.AdmissionDate,n = 1, with_ties = FALSE) %>%
  #   select(Adm.Date.AdmissionDate)
  # datcov_select <- covid_datcov %>% #extract admissions that fall within the wave period
  #   filter(Adm.Date.AdmissionDate %within% interval(start = datcov_wave_start$Adm.Date.AdmissionDate, 
  #                                          end = datcov_wave_end$Adm.Date.AdmissionDate))
  datcov_select <- data_in %>% #extract admissions that fall within the wave period
    filter(Adm.Date.AdmissionDate %within% interval(start = datcov_wave_start, end = datcov_wave_end))
  wave_dates_strat <- c(datcov_wave_start,datcov_wave_end)
  wave_dates_overall_strat[[k]] <- wave_dates_strat
  waves_datcov_strat[[k]] <- datcov_select
  wave_pop_est_list_strat[[k]] <- wave_pop_est
}

#Unstratified
waves_datcov_overall <- list()
wave_pop_est_overall <- data.frame()
wave_dates_overall <- list()
for (k in 1:(length(unique(overall_pop_at_risk$peak_nr)))){#for every wave period
#k <- 2
  # define waves
  wave_points <- overall_pop_at_risk %>%  #pick out the particular peaks to define wave under consideration
    filter(peak_nr == unique(overall_pop_at_risk$peak_nr)[k])
  wave_start <- slice_head(wave_points,n = 1) #define start of the wave
  wave_end <- slice_tail(wave_points, n = 1) #define end of the wave
  wave_pop_est <- wave_points[!duplicated(wave_points$age_pop_estimate_at_peak),c("age_pop_estimate_at_peak")] #define the population estimate at peak
      datcov_wave_start <- wave_start$Week_starting
  # datcov_wave_start <- covid_datcov %>% #find starting dates of admissions for the wave
  #   filter(epiweek == unique(wave_start$wave_start_epiweek), 
  #          epiyear == unique(wave_start$wave_start_epiyear)) %>%
  #   #arrange(Adm.Date.AdmissionDate) %>%
  #   slice_min(order_by = Adm.Date.AdmissionDate,n = 1, with_ties = FALSE) %>%
  #   select(Adm.Date.AdmissionDate)
    datcov_wave_end <- wave_end$Week_starting + days(6)
  # datcov_wave_end <- covid_datcov %>% #find ending dates of admissions for the wave
  #   filter(epiweek == unique(wave_end$wave_start_epiweek), 
  #          epiyear == unique(wave_end$wave_start_epiyear)) %>%
  #   #arrange(Adm.Date.AdmissionDate) %>%
  #   slice_max(order_by = Adm.Date.AdmissionDate,n = 1, with_ties = FALSE) %>%
  #   select(Adm.Date.AdmissionDate)
  
  datcov_select <- data_in %>% #extract admissions that fall within the wave period
    filter(Adm.Date.AdmissionDate %within% interval(start = datcov_wave_start, end = datcov_wave_end))
    # filter(Adm.Date.AdmissionDate %within% interval(start = datcov_wave_start$Adm.Date.AdmissionDate, 
    #                                                 end = datcov_wave_end$Adm.Date.AdmissionDate))
  wave_dates <- c(datcov_wave_start,datcov_wave_end)
  wave_dates_overall[[k]] <- wave_dates
  waves_datcov_overall[[k]] <- datcov_select
  wave_pop_est_overall <- rbind.data.frame(wave_pop_est_overall, wave_pop_est, row.names = NULL)
}
  names(wave_pop_est_overall) <- "age_pop_estimate_at_peak"
 

  
  
  
  
  estimates <- list( waves_datcov_strat,
                   wave_pop_est_list_strat,
                   wave_pop_est_overall,
                   wave_dates_overall)
 return(estimates)
}