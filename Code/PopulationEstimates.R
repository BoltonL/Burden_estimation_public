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

#This script calculated the interpolated population estimates for the epiweek around the peak
estimate_population <- function(peaks, interp_pop,waves){

overall_population_estimate <- interp_pop[[1]] #overall
stratified_population_estimate <- interp_pop[[2]] #stratified by age and sex

##population Stratified by age and sex
peaks_def_estimate <- data.frame()

for (i in 1:nrow(peaks)){
  peak_epiweek <- peaks[i,"epiweek"]
  peak_epiyear <- peaks[i,"epiyear"]
  for (jj in 1:length(levels(stratified_population_estimate$Sex))){
  for (j in 1:length(levels(stratified_population_estimate$Age))){
  peak_estimate <- stratified_population_estimate %>%
    filter(Sex == levels(stratified_population_estimate$Sex)[jj],
           Age == levels(stratified_population_estimate$Age)[j]) %>%
    filter(epiweek == varhandle::unfactor(peak_epiweek$epiweek), 
           epiyear == varhandle::unfactor(peak_epiyear$epiyear)) %>%
    select(Sex,Age,epiweek, epiyear, Est.pop)
  peaks_def_estimate <- rbind.data.frame(peaks_def_estimate, peak_estimate, row.names = NULL)
  }
  }
}

peaks_def_estimatev2 <- left_join(x = peaks_def_estimate, y = peaks, by = c("epiyear","epiweek"))

rows = seq(from = 2,to = nrow(waves)-1)
times = 2
wave_cuts <- waves %>%
  mutate(epiweek = varhandle::unfactor(epiweek) + 1) %>%
  mutate(epiweek = as.factor(epiweek)) %>%
  mutate(Week_starting = Week_starting + days(7))
row.names(wave_cuts) <- seq(from = 1, to = nrow(wave_cuts))
wave_cuts <- bind_rows(waves,
                       wave_cuts[2:(nrow(wave_cuts)-1),])
wave_cuts <- wave_cuts %>%
  arrange(epiyear,epiweek) %>%
  mutate(peak_nr = 0)
  
wave_cuts[c(1,2),"peak_nr"] <- 1
wave_cuts[c(nrow(wave_cuts),(nrow(wave_cuts)-1)),"peak_nr"] <- nrow(wave_cuts)/2
wave_cuts[wave_cuts$peak_nr == 0, "peak_nr"] <- rep(seq(from = 2, to = ((nrow(wave_cuts)/2)-1)), each = 2)

waves_estimate_def <- left_join(x = wave_cuts, y = peaks_def_estimatev2, by = "peak_nr")  
waves_estimate_def <- rename(waves_estimate_def, c(
  'wave_start_epiweek' = "epiweek.x",
  'wave_start_epiyear' = 'epiyear.x',
  'wave_start_total_cases' = 'total.cases.x',
  'peak_nr' = 'peak_nr',
  'wave_peak_epiweek' = 'epiweek.y',
  'wave_peak_epiyear' = 'epiyear.y',
  'wave_peak_total_cases' = 'total.cases.y',
  'age_pop_estimate_at_peak' = 'Est.pop'
))  
 
##Unstratified
peaks_def_estimate_total <- data.frame()

for (i in 1:nrow(peaks)){
  peak_epiweek <- peaks[i,"epiweek"]
  peak_epiyear <- peaks[i,"epiyear"]
  peak_estimate_total <- overall_population_estimate %>%
        filter(epiweek == varhandle::unfactor(peak_epiweek$epiweek), 
               epiyear == varhandle::unfactor(peak_epiyear$epiyear)) %>%
        select(epiweek, epiyear, Est.pop)
      peaks_def_estimate_total <- rbind.data.frame(peaks_def_estimate_total, peak_estimate_total, row.names = NULL)
    }
  
peaks_def_estimate_totalv2 <- left_join(x = peaks_def_estimate_total, y = peaks, by = c("epiyear","epiweek"))

rows = seq(from = 2,to = nrow(waves)-1)
times = 2
wave_cuts <- waves %>%
  mutate(epiweek = varhandle::unfactor(epiweek) + 1) %>%
  mutate(epiweek = as.factor(epiweek)) %>%
  mutate(Week_starting = Week_starting + days(7))
row.names(wave_cuts) <- seq(from = 1, to = nrow(wave_cuts))
wave_cuts <- bind_rows(waves,
                       wave_cuts[2:(nrow(wave_cuts)-1),])
wave_cuts <- wave_cuts %>%
  arrange(epiyear,epiweek) %>%
  mutate(peak_nr = 0)

wave_cuts[c(1,2),"peak_nr"] <- 1
wave_cuts[c(nrow(wave_cuts),(nrow(wave_cuts)-1)),"peak_nr"] <- nrow(wave_cuts)/2
wave_cuts[wave_cuts$peak_nr == 0, "peak_nr"] <- rep(seq(from = 2, to = ((nrow(wave_cuts)/2)-1)), each = 2)

waves_estimate_def_total <- left_join(x = wave_cuts, y = peaks_def_estimate_totalv2, by = "peak_nr")  
waves_estimate_def_total <- rename(waves_estimate_def_total, c(
  'wave_start_epiweek' = "epiweek.x",
  'wave_start_epiyear' = 'epiyear.x',
  'wave_start_total_cases' = 'total.cases.x',
  'peak_nr' = 'peak_nr',
  'wave_peak_epiweek' = 'epiweek.y',
  'wave_peak_epiyear' = 'epiyear.y',
  'wave_peak_total_cases' = 'total.cases.y',
  'age_pop_estimate_at_peak' = 'Est.pop'
))  



wave_population <- list(waves_estimate_def, waves_estimate_def_total)
return(wave_population)  
  
  
}