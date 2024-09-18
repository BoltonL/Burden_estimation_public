med_compartment_ci <- function(boot_samples_admit,boot_samples_death,
                               wave_data_admit_peaks,wave_data_death_peaks,
                               wave_data_admit_in,wave_data_death_in,
                               HUS_severe,HUS_mild){
#input: bootstrapped wave data for admissions, bootstrapped wave data for deaths
    #medically attended severe and fatal
source('./Code/MedAttendBurdenCItotal.R')
source('./Code/MedAttendBurdenCI.R')
   #non medically attended severe and mild
source('./Code/NonMedBurdenCI.R')
source('./Code/NonMedBurdenCIstrat.R')
  
  

med_attend_boot_list <- list()
med_attend_ci_list <- list()
non_med_attend_ci_list <- list()
ci <- list()
ci_dat <- list()
med_attend_boot_strat_dat <- data.frame()
#for (k in 1:length(boot_samples_admit)){#for every wave
k <- 5
  peaks_total_admit <-  wave_data_admit[[3]][k,"age_pop_estimate_at_peak"]#wave peak
  peaks_total_death <-  wave_data_death[[3]][k,"age_pop_estimate_at_peak"]#wave peak
  peaks_strat_admit <- wave_data_admit_peaks_age[[k]]
  peaks_strat_death <- wave_data_death_peaks_age[[k]]
  # wave_sample_admit <- boot_samples_admit#bootstrap_samples[[i]]
  # wave_sample_death <- boot_samples_death
  
  
  #unstratified -#generates bootstrapped estimates of medically attended
  med_attend_boot <- mapply(function(data_in1,data_in2,k,x,y) 
    medically_burden_ci_total(data_in1,data_in2,k,x,y),
                            bootstrap_samples_admit,bootstrap_samples_death,
    MoreArgs = list(k = k, x = peaks_total_admit, y = peaks_total_death))#medically attended severe and fatal incidence per 100 000
  med_attend_boot <- as.data.frame(med_attend_boot)
  names(med_attend_boot) <- str_replace(names(med_attend_boot), "[A-Z]","rep")
  med_attend_boot_dat <- cbind.data.frame(row.names(med_attend_boot),med_attend_boot,row.names = NULL)
  names(med_attend_boot_dat)[1] <- "Measures"
  
  med_attend_boot_list[[k]] <- med_attend_boot_dat
  
  med_attend_ci <- med_attend_boot_dat %>%
    filter(Measures == "incidence_hosp_total"|
           Measures == "cfratio.total"|
           Measures == "incidence_fatal_total") %>%
    mutate(across(rep1:rep1000, as.numeric)) %>%
    rowwise() %>%
    mutate(CI_low = quantile(c_across(rep1:rep1000),0.025),
           CI_high = quantile(c_across(rep1:rep1000),0.975)) 
  med_attend_ci <- as.data.frame(med_attend_ci)
  
  med_attend_ci_list[[k]] <- med_attend_ci %>%
    select(Measures, CI_low,CI_high)
  
  #age stratified medically attended
  med_attend_boot_strat <- mapply(function(data_in_strat1,data_in_strat2,k,x_strat,y_strat) 
    medically_burden_ci(data_in_strat1,data_in_strat2,k,x_strat,y_strat),
    bootstrap_samples_admit,bootstrap_samples_death,
    MoreArgs = list(k = k, x_strat = peaks_strat_admit, y_strat = peaks_strat_death))#medically attended severe and fatal incidence per 100 000
  med_attend_boot_strat_dat <- cbind.data.frame(unlist(med_attend_boot_strat[1,]),
                                                unlist(med_attend_boot_strat[2,]),
                                                row.names = NULL)
  
  for (j in 3:nrow(med_attend_boot_strat)){
  med_attend_boot_strat_dat <- cbind.data.frame(med_attend_boot_strat_dat, 
                                                unlist(med_attend_boot_strat[j,]),
                                                row.names = NULL)
  
  }
  names(med_attend_boot_strat_dat) <- row.names(med_attend_boot_strat)
  rep_nrs <- rep(1:1000, each = length(unique(med_attend_boot_strat_dat$age.group)))
  med_attend_boot_strat_dat <- cbind.data.frame(rep_nrs, med_attend_boot_strat_dat, row.names = NULL)
  
  med_attend_boot_strat_dat_measures <- med_attend_boot_strat_dat %>%
    select(rep_nrs, age.group, incidence_hosp_age,cfratio_age,incidence_fatal_age) %>%
    group_by(age.group) %>%
    summarise(CI_low_hosp = quantile(incidence_hosp_age,0.025),
              CI_high_hosp = quantile(incidence_hosp_age,0.975),
              CI_low_fatal = quantile(incidence_fatal_age,0.025),
              CI_high_fatal = quantile(incidence_fatal_age,0.975),
              CI_low_hf = quantile(cfratio_age,0.025),
              CI_high_hf = quantile(cfratio_age,0.975)) %>% 
    ungroup()
  
 #unstratified non medically attended
  non_med_attend_boot_dat <- non_medically_burden_ci(med_data_in1 = med_attend_boot_dat,
                          excess_in = subset(datcov_excess_deaths[[2]],wave.nr == k),
                          HUS_severe)
  
  non_med_attend_ci <- transpose(non_med_attend_boot_dat, keep.names = "Measures")
  names(non_med_attend_ci)[2:ncol(non_med_attend_ci)] <- str_replace(names(non_med_attend_ci)[2:ncol(non_med_attend_ci)], "[A-Z]","rep")
  non_med_attend_ci <- non_med_attend_ci %>%
    filter(Measures == "incidence_non_hosp_total"|
             Measures == "incidence_non_fatal"|
           Measures == "srif.ratio") %>%
  rowwise() %>%
    mutate(CI_low = quantile(c_across(rep1:rep1000),0.025),
           CI_high = quantile(c_across(rep1:rep1000),0.975)) 
  non_med_attend_ci <- as.data.frame(non_med_attend_ci)
  
  non_med_attend_ci_list[[k]] <- non_med_attend_ci %>%
    select(Measures, CI_low,CI_high)
  
  #age stratified non medically attended
  k <- 1
  non_med_attend_boot_strat_dat <- non_medically_burden_ci_strat(med_data_strat_in1 = med_attend_boot_strat_dat,
                                                     excess_strat_in = subset(datcov_excess_deaths[[3]],wave.nr == k),
                                                     HUS_severe)
  
  
  non_med_attend_boot_strat_dat_measures <- non_med_attend_boot_strat_dat %>%
    select(rep_nrs, age.group, incidence_non_hosp,srif.ratio,incidence_non_fatal) %>%
    group_by(age.group) %>%
    summarise(CI_low_hosp = quantile(incidence_non_hosp,0.025),
              CI_high_hosp = quantile(incidence_non_hosp,0.975),
              CI_low_fatal = quantile(incidence_non_fatal,0.025),
              CI_high_fatal = quantile(incidence_non_fatal,0.975),
              CI_low_sf = quantile(srif.ratio,0.025),
              CI_high_hsf = quantile(srif.ratio,0.975)) %>% 
    ungroup()  



}