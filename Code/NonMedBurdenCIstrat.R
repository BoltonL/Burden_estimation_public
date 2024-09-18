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