country_estimates <- function(data_dir, data_covid, country_est, country_est_sheet){


#Obtain population numbers for 2019 to 2022 by age and sex  
population_est <- read.xlsx(xlsxFile = file.path(data_dir,country_est), sheet = country_est_sheet)  #source: statsSA
population_est_19_22 <- population_est %>%
  select(Sex,Age, `2019`,`2020`,`2021`,`2022`) %>%  #only considering March 2019 till July 2022
  mutate(Sex = replace_na(Sex, "Total")) %>%
  mutate(Age = factor(Age,levels =
                        c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                          "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")),
         Sex = as.factor(Sex)) %>%
  filter(Sex != "Total")
population_est_19_22$Sex <- droplevels(population_est_19_22$Sex)

population_est_19_22_nrs <- population_est_19_22 %>%
  gather(key = "Est.date",value = "Est.pop", `2019`:`2022`)

#Obtain total countrywide population for 2019 to 2022 
population_est_19_22_total <- population_est %>%
  select(Sex,`2019`,`2020`,`2021`,`2022`) %>%
  mutate(Sex = replace_na(Sex, "Total")) %>%
  filter(Sex == "Total")

population_est_19_22_nrs_total <- population_est_19_22_total %>%
  gather(key = "Est.date",value = "Est.pop", `2019`:`2022`)

#Determine population change
## Stratified by sex and age
population_increase <- t(as.data.frame(rep(0, times = ncol(population_est_19_22)-3)))
row.names(population_increase) <- NULL

population_prelim_total <- data.frame()
for (w in 1:length(levels(population_est_19_22$Sex))){
  population_est_prelim <- population_est_19_22 %>%
    filter(Sex == levels(population_est_19_22$Sex)[w])
  for (k in 1:length(levels(population_est_prelim$Age))){
    for (g in 3:(ncol(population_est_prelim)-1)){
      prop.change <- (population_est_prelim[k,(g+1)] - population_est_prelim[k,g])/population_est_prelim[k,g]
      population_increase[[g-2]] <- prop.change
    }
    population_growth <- bind_cols(levels(population_est_prelim$Sex)[w],
                                   levels(population_est_prelim$Age)[k],
                                   population_increase)
    names(population_growth) <- c("Sex","Age",paste0("change.pop_",names(population_est_prelim)[4:ncol(population_est_prelim)]))
    population_prelim_total <- rbind.data.frame(population_prelim_total, population_growth, row.names = NULL)
  }
}

##Unstratified
population_increase_total <- t(as.data.frame(rep(0, times = ncol(population_est_19_22_total)-3)))
row.names(population_increase_total) <- NULL

population_prelim_total_est <- data.frame()
for (g in 2:(ncol(population_est_19_22_total)-1)){
      prop.change.total <- (population_est_19_22_total[1,(g+1)] - population_est_19_22_total[1,g])/population_est_19_22_total[1,g]
      population_increase_total[[g-1]] <- prop.change.total
    }
    
    names(population_increase_total) <- c(paste0("change.pop_",names(population_est_19_22_total)[3:ncol(population_est_19_22_total)]))
    population_prelim_total_est <- as.data.frame(t(population_increase_total))


##Determine dates for population estimates
year_dates_start <- min(data_covid$test_date)
year_dates_int <- ymd("2019-07-01") #generate dates to associate population estimates with epiweeks
epi_dates_origin <- ymd("2020-01-01")
year_dates <- c(year_dates_int,year_dates_int + dyears(1), year_dates_int + dyears(2), year_dates_int + dyears(3))
year_dates <- as.Date(year_dates)
epiweeks_dates <- c(epi_dates_origin,epi_dates_origin + dyears(1) + ddays(1), epi_dates_origin + dyears(2), epi_dates_origin + dyears(3))
epiweeks <- epiweek(epiweeks_dates)[2:length(epiweeks_dates)]

#Determine population estimates by epiweek 
## Stratified by age and sex
population_interp <- population_prelim_total %>%
  gather(key = "Est.date", value = "prop.pop.change", change.pop_2020:change.pop_2022) %>%
  mutate(Est.date = str_extract_all(Est.date,"[0-9]+")) %>%
  mutate(Age = factor(Age,levels = 
                        c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                          "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")),
         Sex = as.factor(Sex))

for (ii in 1:length(year_dates)){
  a1 <- as.character(year(year_dates[ii]))
  population_est_19_22_nrs <- population_est_19_22_nrs %>%
    mutate(Est.date = ifelse(str_detect(Est.date,a1), as.character(year_dates[ii]), Est.date))
  population_est_19_22_nrs_total <- population_est_19_22_nrs_total %>%
    mutate(Est.date = ifelse(str_detect(Est.date,a1), as.character(year_dates[ii]), Est.date))
}
population_est_19_22_nrs$Est.date <- ymd(population_est_19_22_nrs$Est.date)

nr_weeks_vec <- rep(0,length(epiweeks)) 
weeks_total_years <- data.frame()
for (i in 1:(length(year_dates)-1)){ #make sure how many weeks are included within the year for every mid year estimate
  for (kk in 1:length(levels(population_interp$Sex))){
      for (jj in 1:length(levels(population_interp$Age))){
      nr_weeks <- floor(time_length(interval(start = year_dates[i], end = year_dates[i+1]), unit = "weeks"))
      nr_weeks_vec[i] <- nr_weeks
      
      week_dates <- seq.Date(from = year_dates[i], to = (year_dates[i+1] + ddays(1)), by = "weeks") #generate a sequence of dates
      week_increase_prep <- population_interp %>%
        filter(Sex == levels(population_interp$Sex)[kk],
               Age == levels(population_interp$Age)[jj],
               str_detect(Est.date,as.character(year(year_dates[i+1])))) %>%
        select(prop.pop.change)
      week_increase <- rep(week_increase_prep$prop.pop.change/nr_weeks, times = length(week_dates)) #assume a constant increase per week
      week_epi <- epiweek(week_dates)
      weeks_total <- bind_cols(rep(levels(population_interp$Sex)[kk], times = length(week_dates)),
                               rep(levels(population_interp$Age)[jj], times = length(week_dates)),
                               week_dates,week_increase,week_epi)
      weeks_total_years <- bind_rows(weeks_total_years,weeks_total)
      
    }
  }
}
names(weeks_total_years) <- c("Sex","Age","Est.date","Prop.change_week","epiweek")
weeks_total_years <- weeks_total_years %>%
  filter(Est.date != year_dates[2] , Est.date != year_dates[3], Est.date != year_dates[4]) #remove superfluous dates

population_interp_epi <- left_join(x = weeks_total_years,population_est_19_22_nrs, by = c("Sex","Age","Est.date"))
population_interp_epi$Age <- factor(population_interp_epi$Age,levels = 
                                      c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44",
                                        "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"))
population_interp_epi$Sex <- as.factor(population_interp_epi$Sex)

population_estimates <- data.frame()
for (q in 1:length(levels(population_interp_epi$Sex))){
  for (p in 1:length(levels(population_interp_epi$Age))){
    subpopulation <- population_interp_epi %>%
      filter(Age == levels(population_interp_epi$Age)[p],
             Sex == levels(population_interp_epi$Sex)[q])
    for (pp in 1:(nrow(subpopulation)-1)){ #calculate the assumed weekly change in population (assuming that percentage change is constant)
      week_row <- subpopulation[pp,]
      pop.change <- week_row$Est.pop * week_row$Prop.change_week #population change = starting population estimate * prop. change
      new.pop <- week_row$Est.pop + pop.change
      subpopulation[pp+1,"Est.pop"] <- new.pop
    }
    
    population_estimates <- rbind.data.frame(population_estimates,subpopulation, row.names = NULL)
  }
}

population_estimates$epiyear <- as.factor(year(population_estimates$Est.date))
population_estimates$epiweek <- as.factor(population_estimates$epiweek)

##Unstratified
population_interp_total <- population_prelim_total_est %>%
  gather(key = "Est.date", value = "prop.pop.change", change.pop_2020:change.pop_2022) %>%
  mutate(Est.date = str_extract_all(Est.date,"[0-9]+")) 
population_est_19_22_nrs_total$Est.date <- ymd(population_est_19_22_nrs_total$Est.date)

nr_weeks_vec_total <- rep(0,length(epiweeks)) 
weeks_total_years_est <- data.frame()
for (i in 1:(length(year_dates)-1)){ #make sure how many weeks are included within the year for every mid year estimate
 
      nr_weeks <- floor(time_length(interval(start = year_dates[i], end = year_dates[i+1]), unit = "weeks"))
      nr_weeks_vec_total[i] <- nr_weeks
      
      week_dates <- seq.Date(from = year_dates[i], to = (year_dates[i+1] + ddays(1)), by = "weeks") #generate a sequence of dates
      week_increase_prep_total <- population_interp_total %>%
        filter(str_detect(Est.date,as.character(year(year_dates[i+1])))) %>%
        select(prop.pop.change)
      week_increase <- rep(week_increase_prep_total$prop.pop.change/nr_weeks, times = length(week_dates)) #assume a constant increase per week
      week_epi <- epiweek(week_dates)
      weeks_total <- bind_cols(week_dates,week_increase,week_epi)
      weeks_total_years_est <- bind_rows(weeks_total_years_est,weeks_total)
      
    }
 
names(weeks_total_years_est) <- c("Est.date","Prop.change_week","epiweek")
weeks_total_years_est<- weeks_total_years_est %>%
  filter(Est.date != year_dates[2] , Est.date != year_dates[3], Est.date != year_dates[4]) #remove superfluous dates

population_interp_epi_total <- left_join(x = weeks_total_years_est,population_est_19_22_nrs_total, by = c("Est.date"))
population_interp_epi_total <- select(population_interp_epi_total, -Sex)

population_estimates_total <- data.frame()
    for (pp in 1:(nrow(population_interp_epi_total)-1)){ #calculate the assumed weekly change in population (assuming that percentage change is constant)
      week_row <- population_interp_epi_total[pp,]
      pop.change <- week_row$Est.pop * week_row$Prop.change_week #population change = starting population estimate * prop. change
      new.pop <- week_row$Est.pop + pop.change
      population_interp_epi_total[pp+1,"Est.pop"] <- new.pop
    }
    
    population_estimates_total <- rbind.data.frame(population_estimates_total,population_interp_epi_total, row.names = NULL)
  

population_estimates_total$epiyear <- as.factor(year(population_estimates_total$Est.date))
population_estimates_total$epiweek <- as.factor(population_estimates_total$epiweek)

population_estimates_combine <- list(population_estimates_total, population_estimates)
return(population_estimates_combine)
}
