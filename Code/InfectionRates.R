# Copyright (c) 2024 Larisse Bolton
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

infection_rates <- function(ma_country,ma_age,attack_rate){

  incidence_country_total <- ma_country
    incidence_country_age <- ma_age
  
  
  
  infection_ar_total <- attack_rate %>%
    filter(age_cat == "All")
  
  infection_ar_age <- attack_rate %>%
    filter(age_cat != "All") %>%
    mutate(age_cat = as.character(age_cat)) %>%
    mutate(age_cat = ifelse(age_cat == "00-04","0-4",age_cat)) %>% #rename age categories for attack rate to be similar to dataset
    mutate(age_cat = ifelse(age_cat == "05-09","5-9",age_cat)) %>%
    mutate(age_cat = as.factor(age_cat))
  names(infection_ar_age)[names(infection_ar_age) == "age_cat"] <- "age.group"
  
   incidence_country_age_upd <- lapply(incidence_country_age, function(i1){ #collapse older age groups for comparable groups to excess deaths
 
    i2 <- i1 %>%
      filter(age.group == "65-69"| age.group == "70-74"| age.group == "75-79"| age.group == "80+") %>%
      summarise(nr.covid = sum(nr.covid_age),
                age_pop_estimate_at_admit_peak = sum(age_pop_estimate_at_admit_age_peak),
                fatals = sum(fatals_age),
                fatalities = sum(fatalities1),
                age_pop_estimate_at_death_peak = sum(age_pop_estimate_at_death_age_peak),
                excess_deaths = sum(excess_deaths)) %>%
      mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
             incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
             incidence_non_severe = (incidence_hosp/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp,
             incidence_non_fatal = ((excess_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
      mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
      mutate(age.group = "65+") %>%
      select(age.group,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,excess_deaths,
             incidence_non_severe, incidence_non_fatal)

    i3 <- i1 %>%
      filter(age.group != "65-69", age.group != "70-74", age.group != "75-79", age.group != "80+") %>%
      select(age.group,nr.covid_age,age_pop_estimate_at_admit_age_peak,incidence_hosp_age,fatalities1, fatals_age, age_pop_estimate_at_death_age_peak,
             incidence_fatal_age,excess_deaths,
             incidence_non_hosp_age, incidence_non_fatal)
    names(i3) <- names(i2)
    i4 <- rbind.data.frame(i3,i2,row.names = NULL)
    
    i4 <- i4 %>%
      mutate(incidence_severe_total = incidence_hosp + incidence_non_severe, #calculate the total severe illness incidence = sum of hospitalisation and out of hospital severe
             incidence_fatal_total = incidence_fatal + incidence_non_fatal) #calculate the total fatal illness incidence = sum of in and out of hospital deaths incidence
    return(i4)
   })

infection_ar_age$wave.nr <- as.factor(infection_ar_age$wave.nr)
infection_ar_total$wave.nr <- as.factor(infection_ar_total$wave.nr)

infections_country_age_out <- data.frame()
for (j in 1:length(incidence_country_age_upd)){
  infections_country_age <- incidence_country_age_upd[[j]] %>%
    mutate(wave.nr = j) 
  # %>%
  #   select(wave.nr, age.group,age_pop_estimate_at_admit_peak, incidence_hosp, incidence_fatal, 
  #          incidence_non_severe, incidence_non_fatal,incidence_severe_total,incidence_fatal_total)
  infections_country_age_out <- rbind.data.frame(infections_country_age_out,
                                                 infections_country_age, row.names = NULL)
}

infections_country_age_out$wave.nr <- as.factor(infections_country_age_out$wave.nr)

infections_country <- left_join(x = incidence_country_total, y = infection_ar_total[,c("wave.nr","attack.rate")], by = "wave.nr") #join attack rate dataset to what has already been calculated
infections_country$total_severe <- infections_country$incidence_hosp_total + infections_country$incidence_non_hosp_total
infections_country$total_deaths <- infections_country$incidence_fatal_total + infections_country$incidence_non_fatal
infections_country$infection_incidence <- (infections_country$attack.rate *100000) - infections_country$total_severe - infections_country$total_deaths
infections_country$infection_rate <- ((infections_country$total_deaths)/(infections_country$attack.rate *100000))*100 #(per 100 000) infection fatality rate as percentage
infections_country$infections_nr <- (infections_country$attack.rate*(infections_country$pop_estimate_at_peak_admit)) #total number of infections

infections_country_age_out_upd <- left_join(x = infections_country_age_out, y = infection_ar_age[,c("age.group","wave.nr","attack.rate")], by = c("age.group","wave.nr"))
infections_country_age_out_upd$infection_incidence <- (infections_country_age_out_upd$attack.rate *100000) - infections_country_age_out_upd$incidence_severe_total - infections_country_age_out_upd$incidence_fatal_total
infections_country_age_out_upd$infection_rate <- (infections_country_age_out_upd$incidence_fatal_total)/(infections_country_age_out_upd$attack.rate *100000)*100 #per 100 000
infections_country_age_out_upd$infections_nr <- (infections_country_age_out_upd$attack.rate*(infections_country_age_out_upd$age_pop_estimate_at_admit_peak))

infections_out <- list(infections_country, infections_country_age_out_upd,
                       infection_ar_age)
return(infections_out)


}