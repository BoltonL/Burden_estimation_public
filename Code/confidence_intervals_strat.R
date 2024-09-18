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

conf_int_strat <- function(directory,directory_upd, attack){

#medically attended
med_attend_wave_1 <- 'med_attend_strat_ci_1.RDS'
med_attend_wave_2 <- 'med_attend_strat_ci_2.RDS'
med_attend_wave_3 <- 'med_attend_strat_ci_3.RDS'
med_attend_wave_4 <- 'med_attend_strat_ci_4.RDS'
med_attend_wave_5 <- 'med_attend_strat_ci_5.RDS'

med_attend_wave_1_df <- readRDS(file.path(directory,med_attend_wave_1))
med_attend_wave_2_df <- readRDS(file.path(directory,med_attend_wave_2))
med_attend_wave_3_df <- readRDS(file.path(directory,med_attend_wave_3))
med_attend_wave_4_df <- readRDS(file.path(directory,med_attend_wave_4))
med_attend_wave_5_df <- readRDS(file.path(directory,med_attend_wave_5))

med_attend_ci_1 <- med_attend_wave_1_df %>%
  pivot_longer(cols = CI_low_hosp:CI_high_hf, names_to = "Headings", values_to = "CI_pt") %>%
  mutate(Measures = str_extract(Headings,"[a-z]+$"),
         CI = str_remove(Headings,"_[a-z]+$")) %>%
  select(-Headings) %>%
  pivot_wider(names_from = "CI",values_from = "CI_pt")
med_attend_ci_1 <- cbind.data.frame(rep("Wave 1",times = nrow(med_attend_ci_1)),
                                    med_attend_ci_1, row.names = NULL)
names(med_attend_ci_1)[1] <- 'wave.nr'

med_attend_ci_2 <- med_attend_wave_2_df %>%
  pivot_longer(cols = CI_low_hosp:CI_high_hf, names_to = "Headings", values_to = "CI_pt") %>%
  mutate(Measures = str_extract(Headings,"[a-z]+$"),
         CI = str_remove(Headings,"_[a-z]+$")) %>%
  select(-Headings) %>%
  pivot_wider(names_from = "CI",values_from = "CI_pt")
med_attend_ci_2 <- cbind.data.frame(rep("Wave 2",times = nrow(med_attend_ci_2)),
                                    med_attend_ci_2, row.names = NULL)
names(med_attend_ci_2)[1] <- 'wave.nr'

med_attend_ci_3 <- med_attend_wave_3_df %>%
  pivot_longer(cols = CI_low_hosp:CI_high_hf, names_to = "Headings", values_to = "CI_pt") %>%
  mutate(Measures = str_extract(Headings,"[a-z]+$"),
         CI = str_remove(Headings,"_[a-z]+$")) %>%
  select(-Headings) %>%
  pivot_wider(names_from = "CI",values_from = "CI_pt")
med_attend_ci_3 <- cbind.data.frame(rep("Wave 3",times = nrow(med_attend_ci_3)),
                                    med_attend_ci_3, row.names = NULL)
names(med_attend_ci_3)[1] <- 'wave.nr'

med_attend_ci_4 <- med_attend_wave_4_df %>%
  pivot_longer(cols = CI_low_hosp:CI_high_hf, names_to = "Headings", values_to = "CI_pt") %>%
  mutate(Measures = str_extract(Headings,"[a-z]+$"),
         CI = str_remove(Headings,"_[a-z]+$")) %>%
  select(-Headings) %>%
  pivot_wider(names_from = "CI",values_from = "CI_pt")
med_attend_ci_4 <- cbind.data.frame(rep("Wave 4",times = nrow(med_attend_ci_4)),
                                    med_attend_ci_4, row.names = NULL)
names(med_attend_ci_4)[1] <- 'wave.nr'

med_attend_ci_5 <- med_attend_wave_5_df %>%
  pivot_longer(cols = CI_low_hosp:CI_high_hf, names_to = "Headings", values_to = "CI_pt") %>%
  mutate(Measures = str_extract(Headings,"[a-z]+$"),
         CI = str_remove(Headings,"_[a-z]+$")) %>%
  select(-Headings) %>%
  pivot_wider(names_from = "CI",values_from = "CI_pt")
med_attend_ci_5 <- cbind.data.frame(rep("Wave 5",times = nrow(med_attend_ci_5)),
                                    med_attend_ci_5, row.names = NULL)
names(med_attend_ci_5)[1] <- 'wave.nr'

med_attend_ci <- rbind.data.frame(med_attend_ci_1,
                                  med_attend_ci_2,
                                  med_attend_ci_3,
                                  med_attend_ci_4,
                                  med_attend_ci_5,row.names = NULL)
med_attend_ci <- med_attend_ci %>%
  mutate(Measures = str_replace(Measures, "hf","cfratio_age")) %>%
  mutate(Measures = str_replace(Measures, "hosp","incidence_hosp_age")) %>%
  mutate(Measures = str_replace(Measures, "fatal","incidence_fatal_age")) %>%
  mutate(Measures = as.factor(Measures),
         wave.nr = as.factor(wave.nr))
med_attend_ci_hfr <- med_attend_ci %>%
 filter(Measures == "cfratio_age") %>%
  mutate(CI_low = CI_low*100,
         CI_high = CI_high*100)

##Non_medically attended
# non_med_attend_wave_1 <- 'non_med_attend_boot_strat_ci_1_upd.RDS'#'non_med_attend_strat_ci_1.RDS'
# non_med_attend_wave_2 <- 'non_med_attend_boot_strat_2_ci_upd.RDS'#'non_med_attend_strat_ci_2.RDS'
# non_med_attend_wave_3 <- 'non_med_attend_boot_strat_3_ci_upd.RDS'#'non_med_attend_strat_ci_3.RDS'
# non_med_attend_wave_4 <- 'non_med_attend_boot_strat_4_ci_upd.RDS'#'non_med_attend_strat_ci_4.RDS'
# non_med_attend_wave_5 <- 'non_med_attend_boot_strat_5_ci_upd.RDS'#'non_med_attend_strat_ci_5.RDS'
# 
# non_med_attend_wave_1_df <- readRDS(file.path(directory,non_med_attend_wave_1))
# names(non_med_attend_wave_1_df)[ncol(non_med_attend_wave_1_df)] <- "CI_high_sf"
# non_med_attend_wave_2_df <- readRDS(file.path(directory,non_med_attend_wave_2))
# names(non_med_attend_wave_2_df)[ncol(non_med_attend_wave_2_df)] <- "CI_high_sf"
# non_med_attend_wave_3_df <- readRDS(file.path(directory,non_med_attend_wave_3))
# names(non_med_attend_wave_3_df)[ncol(non_med_attend_wave_3_df)] <- "CI_high_sf"
# non_med_attend_wave_4_df <- readRDS(file.path(directory,non_med_attend_wave_4))
# names(non_med_attend_wave_4_df)[ncol(non_med_attend_wave_4_df)] <- "CI_high_sf"
# non_med_attend_wave_5_df <- readRDS(file.path(directory,non_med_attend_wave_5))
# names(non_med_attend_wave_5_df)[ncol(non_med_attend_wave_5_df)] <- "CI_high_sf"
# 
# non_med_attend_ci_1 <- non_med_attend_wave_1_df %>%
#   pivot_longer(cols = CI_low_hosp:CI_high_sf, names_to = "Headings", values_to = "CI_pt") %>%
#   mutate(Measures = str_extract(Headings,"[a-z]+$"),
#          CI = str_remove(Headings,"_[a-z]+$")) %>%
#   select(-Headings) %>%
#   pivot_wider(names_from = "CI",values_from = "CI_pt")
# non_med_attend_ci_1 <- cbind.data.frame(rep("Wave 1",times = nrow(non_med_attend_ci_1)),
#                                     non_med_attend_ci_1, row.names = NULL)
# names(non_med_attend_ci_1)[1] <- 'wave.nr'
# 
# non_med_attend_ci_2 <- non_med_attend_wave_2_df %>%
#   pivot_longer(cols = CI_low_hosp:CI_high_sf, names_to = "Headings", values_to = "CI_pt") %>%
#   mutate(Measures = str_extract(Headings,"[a-z]+$"),
#          CI = str_remove(Headings,"_[a-z]+$")) %>%
#   select(-Headings) %>%
#   pivot_wider(names_from = "CI",values_from = "CI_pt")
# non_med_attend_ci_2 <- cbind.data.frame(rep("Wave 2",times = nrow(non_med_attend_ci_2)),
#                                     non_med_attend_ci_2, row.names = NULL)
# names(non_med_attend_ci_2)[1] <- 'wave.nr'
# 
# non_med_attend_ci_3 <- non_med_attend_wave_3_df %>%
#   pivot_longer(cols = CI_low_hosp:CI_high_sf, names_to = "Headings", values_to = "CI_pt") %>%
#   mutate(Measures = str_extract(Headings,"[a-z]+$"),
#          CI = str_remove(Headings,"_[a-z]+$")) %>%
#   select(-Headings) %>%
#   pivot_wider(names_from = "CI",values_from = "CI_pt")
# non_med_attend_ci_3 <- cbind.data.frame(rep("Wave 3",times = nrow(non_med_attend_ci_3)),
#                                     non_med_attend_ci_3, row.names = NULL)
# names(non_med_attend_ci_3)[1] <- 'wave.nr'
# 
# non_med_attend_ci_4 <- non_med_attend_wave_4_df %>%
#   pivot_longer(cols = CI_low_hosp:CI_high_sf, names_to = "Headings", values_to = "CI_pt") %>%
#   mutate(Measures = str_extract(Headings,"[a-z]+$"),
#          CI = str_remove(Headings,"_[a-z]+$")) %>%
#   select(-Headings) %>%
#   pivot_wider(names_from = "CI",values_from = "CI_pt")
# non_med_attend_ci_4 <- cbind.data.frame(rep("Wave 4",times = nrow(non_med_attend_ci_4)),
#                                     non_med_attend_ci_4, row.names = NULL)
# names(non_med_attend_ci_4)[1] <- 'wave.nr'
# 
# non_med_attend_ci_5 <- non_med_attend_wave_5_df %>%
#   pivot_longer(cols = CI_low_hosp:CI_high_sf, names_to = "Headings", values_to = "CI_pt") %>%
#   mutate(Measures = str_extract(Headings,"[a-z]+$"),
#          CI = str_remove(Headings,"_[a-z]+$")) %>%
#   select(-Headings) %>%
#   pivot_wider(names_from = "CI",values_from = "CI_pt")
# non_med_attend_ci_5 <- cbind.data.frame(rep("Wave 5",times = nrow(non_med_attend_ci_5)),
#                                     non_med_attend_ci_5, row.names = NULL)
# names(non_med_attend_ci_5)[1] <- 'wave.nr'
# 
# non_med_attend_ci <- rbind.data.frame(#non_med_attend_ci_1,
#                                   non_med_attend_ci_2,
#                                   non_med_attend_ci_3,
#                                   non_med_attend_ci_4,
#                                   non_med_attend_ci_5,row.names = NULL)
# non_med_attend_ci_1 <- non_med_attend_ci_1 %>%
#   mutate(Measures = str_replace(Measures, "sf","srif.ratio")) %>%
#   mutate(Measures = str_replace(Measures, "hosp","incidence_non_hosp_age")) %>%
#   mutate(Measures = str_replace(Measures, "fatal","incidence_non_fatal")) %>%
#   mutate(Measures = as.factor(Measures),
#          wave.nr = as.factor(wave.nr))
# 
# non_med_attend_ci_sfr_1 <- non_med_attend_ci_1 %>%
#   filter(Measures == "srif.ratio") %>%
#   mutate(CI_low = CI_low*100,
#          CI_high = CI_high*100)

#total
non_med_attend_wave_1_dat <- 'non_med_attend_boot_strat_dat_1_upd.RDS'#'non_med_attend_boot_strat_dat_1.RDS'
non_med_attend_wave_2_dat <- 'non_med_attend_boot_strat_2_upd.RDS'#'non_med_attend_boot_strat_dat_2.RDS'
non_med_attend_wave_3_dat <- 'non_med_attend_boot_strat_3_upd.RDS'#'non_med_attend_boot_strat_dat_3.RDS'
non_med_attend_wave_4_dat <- 'non_med_attend_boot_strat_4_upd.RDS'#'non_med_attend_boot_strat_dat_4.RDS'
non_med_attend_wave_5_dat <- 'non_med_attend_boot_strat_5_upd.RDS'#'non_med_attend_boot_strat_dat_5.RDS'
set.seed(1234)

#generate draws of hus from binomial distribution
hus_r_draw <- rbinom(n = 1000, size = sum(HUS_severe$Total), prob = sum(HUS_severe$Count)/sum(HUS_severe$Total))/sum(HUS_severe$Total)
#assume HUS remains unchanged irrespective of age strata or wave
hus_r_draw_df <- cbind.data.frame(seq(1,1000,1), hus_r_draw, row.names = NULL)
names(hus_r_draw_df) <- c("rep_nrs","hus_draw")

# 1

##reorder the age groups to coincide with PHIRST-C
non_med_attend_wave_1_dat_df <- readRDS(file.path(directory_upd,non_med_attend_wave_1_dat))
non_med_attend_wave_1_dat_dfi2 <- non_med_attend_wave_1_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group == "65-69"| age.group == "70-74"| age.group == "75-79"| age.group == "80+") %>%
  summarise(nr.covid = sum(nr.covid_age),
            age.covid.patients = sum(age.covid.patients.admit1.x),
            age_pop_estimate_at_admit_peak = sum(age_pop_estimate_at_admit_age_peak),
            fatals = sum(fatals_age),
            fatalities = sum(fatalities1),
            age_pop_estimate_at_death_peak = sum(age_pop_estimate_at_death_age_peak),
            covid_attrib_deaths = sum(covid_attrib_deaths)) %>%
    mutate(age.group = "65+") %>%
  mutate(rep_nrs = row.names(.))  %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,fatalities, fatals, age_pop_estimate_at_death_peak,
         covid_attrib_deaths) %>%
  ungroup()

non_med_attend_wave_1_dat_dfi3 <- non_med_attend_wave_1_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group != "65-69", age.group != "70-74", age.group != "75-79", age.group != "80+") %>%
  select(rep_nrs, age.group,age.covid.patients.admit1.x,nr.covid_age,age_pop_estimate_at_admit_age_peak,fatalities1, fatals_age, age_pop_estimate_at_death_age_peak,
         covid_attrib_deaths) %>%
  ungroup()

names(non_med_attend_wave_1_dat_dfi3) <- names(non_med_attend_wave_1_dat_dfi2)
non_med_attend_wave_1_dat_dfi4 <- rbind.data.frame(non_med_attend_wave_1_dat_dfi3,non_med_attend_wave_1_dat_dfi2,row.names = NULL)

##Add in HUS draws
non_med_attend_wave_1_dat_dfi4$rep_nrs <- as.numeric(non_med_attend_wave_1_dat_dfi4$rep_nrs)
non_med_attend_wave_1_dat_dfi4 <- left_join(x = non_med_attend_wave_1_dat_dfi4, y = hus_r_draw_df, by = "rep_nrs")
non_med_attend_wave_1_dat_dfi4_upd <- non_med_attend_wave_1_dat_dfi4 %>%
mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,  #hospitalisation incidence
       incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,   #in-hospital death incidence
       incidence_non_severe = (incidence_hosp/hus_draw) - incidence_hosp,  #severe illness outside hospital incidence
       incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%  #outside hospital death incidence
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,hus_draw,
         incidence_non_severe, covid_attrib_deaths, incidence_non_fatal)

non_med_attend_wave_1_dat_df_upd <- non_med_attend_wave_1_dat_dfi4_upd %>%
  arrange(rep_nrs,age.group) %>%  #by age group
  mutate(total_severe_inc = incidence_hosp + incidence_non_severe,  #total severe illness incidence
         total_fatal_inc = incidence_fatal + incidence_non_fatal)   #total incidence of death

non_med_attend_wave_1_dat_df_totals <- non_med_attend_wave_1_dat_df_upd %>%
  group_by(rep_nrs) %>%  #generate national estimates by adding across age groups
  mutate(nr_covid_total = sum(nr.covid),
         age_covid_patients_total = sum(age.covid.patients),
         population_admit_total = sum(age_pop_estimate_at_admit_peak),
         population_death_total = sum(age_pop_estimate_at_death_peak),
         fatalities_total = sum(fatalities),
         fatals_total = sum(fatals),
         covid_attrib_deaths_total = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp_total = (nr_covid_total/population_admit_total)*100000,
         incidence_fatal_total = (fatals_total/population_death_total)*100000,
         incidence_non_severe_total = (incidence_hosp_total/hus_draw) - incidence_hosp_total,
         incidence_non_fatal_total = ((covid_attrib_deaths_total - fatals_total)/population_death_total)*100000,
         hfr_total = fatals_total/(fatals_total + nr_covid_total),
         srif_total = incidence_non_fatal_total/(incidence_non_fatal_total + incidence_non_severe_total),
         total_severe_wave = incidence_hosp_total + incidence_non_severe_total,
         total_fatal_wave = incidence_fatal_total + incidence_non_fatal_total) %>%
  filter(!duplicated(rep_nrs)) %>%
  select(-(age.group:incidence_non_fatal)) %>%
  ungroup()



#age specific confidence intervals
non_med_attend_total_1_ci <- non_med_attend_wave_1_dat_df_upd %>%
  group_by(age.group) %>% 
  summarise(CI_low_severe = quantile(total_severe_inc,0.025),
            CI_high_severe = quantile(total_severe_inc,0.975),
            CI_low_fatal = quantile(total_fatal_inc, 0.025),
            CI_high_fatal = quantile(total_fatal_inc,0.975)) %>%
  ungroup()

#choose constraint for attack rate to be the maximum of the upper CI's for the severe and fatal illness
attack_constraint <- max(non_med_attend_total_1_ci$CI_high_severe + non_med_attend_total_1_ci$CI_high_fatal)

attack_1 <- attack[attack$wave.nr == 1,]
attack_1_N <- sum(attack_1$N)
attack_1_n <- sum(attack_1$n)

attack_1_upd <- data.frame()

#While the attack rate drawn results in an incidence greater than the constraint, keep going.  But if condition fails, keep 
#drawing attack rate until condition passes
attack_rate_draws <- function(ar_rate, ages,threshold_def){
  repeat {
  attack_rep <- rbinom(n = 1, size = ar_rate[ages,"N"], prob = ar_rate[ages,"attack.rate"])/ar_rate[ages,"N"]
  sim_attack <- attack_rep*100000
 if ((sim_attack*100000) > threshold_def){
  return(attack_rep)
   break
 } 
  }
}
 

for (i in 1:nrow(attack_1)){  
new_vec <- replicate(n= 1000, attack_rate_draws(ar_rate = attack_1,ages = i,threshold = attack_constraint))
attack_vec <-cbind.data.frame(c(1:1000),attack_1[i,c("age.group")],
                              new_vec, row.names = NULL)
attack_1_upd <- rbind.data.frame(attack_1_upd,attack_vec, row.names = NULL) 
  
}

#wave attack rate
wave_attack <- replicate(n= 1000, rbinom(n = 1, size = attack_1_N, prob = attack_1_n/attack_1_N)/attack_1_N) #use total sample and total successes
wave_attack_df <- cbind.data.frame(c(1:1000), wave_attack, row.names = NULL)
names(wave_attack_df)[1] <- "rep_nrs"

names(attack_1_upd) <- c("rep_nrs","age.group","attack_rate_draw")

non_med_attend_wave_1_dat_df_upd2 <- left_join(x = non_med_attend_wave_1_dat_df_upd, y = attack_1_upd, by = c("age.group","rep_nrs"))
non_med_attend_total_1 <- non_med_attend_wave_1_dat_df_upd2 %>% #age group level
  mutate(infection_incidence = (attack_rate_draw*100000) - (total_severe_inc + total_fatal_inc),
         ifr = (total_fatal_inc/(attack_rate_draw*100000))*100,
         nr_infections = attack_rate_draw*age_pop_estimate_at_admit_peak)

non_med_attend_total_1a <- left_join(x = non_med_attend_wave_1_dat_df_totals, y = wave_attack_df, by = "rep_nrs")
non_med_attend_total_1a$ifr <- (non_med_attend_total_1a$total_fatal_wave/(non_med_attend_total_1a$wave_attack*100000))*100
non_med_attend_total_1a$infection_incidence <- (non_med_attend_total_1a$wave_attack*100000) - (non_med_attend_total_1a$total_severe_wave + non_med_attend_total_1a$total_fatal_wave)
 
#national level estimates i.e. summing over the age groups
non_med_attend_total_1_ci_total <- non_med_attend_total_1a %>%
  summarise(CI_low_severe = quantile(total_severe_wave,0.025),
            CI_high_severe = quantile(total_severe_wave,0.975),
            CI_low_fatal = quantile(total_fatal_wave, 0.025),
            CI_high_fatal = quantile(total_fatal_wave,0.975),
            CI_low_inc_hosp = quantile(incidence_hosp_total,0.025),
            CI_high_inc_hosp = quantile(incidence_hosp_total,0.975),
            CI_low_inc_fatal = quantile(incidence_fatal_total,0.025),
            CI_high_inc_fatal = quantile(incidence_fatal_total,0.975),
            CI_low_inc_non_severe = quantile(incidence_non_severe_total,0.025),
            CI_high_inc_non_severe = quantile(incidence_non_severe_total,0.975),
            CI_low_inc_non_fatal = quantile(incidence_non_fatal_total,0.025),
            CI_high_inc_non_fatal = quantile(incidence_non_fatal_total,0.975),
            CI_low_hfr_total = quantile(hfr_total,0.025),
            CI_high_hfr_fatal = quantile(hfr_total,0.975),
            CI_low_srif_total = quantile(srif_total,0.025),
            CI_high_srif_total = quantile(srif_total,0.975),
            CI_low_attack_rate = quantile(wave_attack,0.025),
            CI_high_attack_rate = quantile(wave_attack, 0.975),
            I_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975)) %>%
  ungroup()

non_med_attend_total_1_ci_add <- non_med_attend_total_1 %>%
  group_by(age.group) %>% #age specific confidence intervals
  summarise(CI_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975),
            CI_low_attack = quantile(attack_rate_draw,0.025),
            CI_high_attack = quantile(attack_rate_draw,0.975)) %>%
  ungroup()




non_med_attend_total_1_ci <- cbind.data.frame(rep("Wave 1", times = nrow(non_med_attend_total_1_ci)),
                                              non_med_attend_total_1_ci, row.names = NULL)
names(non_med_attend_total_1_ci)[1] <- "wave.nr"

non_med_attend_total_1_ci_wave <- cbind.data.frame("Wave 1",
                                                   non_med_attend_total_1_ci_total, row.names = NULL)
names(non_med_attend_total_1_ci_wave)[1] <- "wave_nr"


#2
non_med_attend_wave_2_dat_df <- readRDS(file.path(directory_upd,non_med_attend_wave_2_dat))
non_med_attend_wave_2_dat_dfi2 <- non_med_attend_wave_2_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group == "65-69"| age.group == "70-74"| age.group == "75-79"| age.group == "80+") %>%
  summarise(nr.covid = sum(nr.covid_age),
            age.covid.patients = sum(age.covid.patients.admit1.x),
            age_pop_estimate_at_admit_peak = sum(age_pop_estimate_at_admit_age_peak),
            fatals = sum(fatals_age),
            fatalities = sum(fatalities1),
            age_pop_estimate_at_death_peak = sum(age_pop_estimate_at_death_age_peak),
            covid_attrib_deaths = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  mutate(age.group = "65+") %>%
  mutate(rep_nrs = row.names(.)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,covid_attrib_deaths,
         incidence_non_severe, incidence_non_fatal) %>%
  ungroup()

non_med_attend_wave_2_dat_dfi3 <- non_med_attend_wave_2_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group != "65-69", age.group != "70-74", age.group != "75-79", age.group != "80+") %>%
  select(rep_nrs, age.group,age.covid.patients.admit1.x,nr.covid_age,age_pop_estimate_at_admit_age_peak,incidence_hosp_age,fatalities1, fatals_age, age_pop_estimate_at_death_age_peak,
         incidence_fatal_age,covid_attrib_deaths, incidence_non_hosp, incidence_non_fatal) %>%
  ungroup()

names(non_med_attend_wave_2_dat_dfi3) <- names(non_med_attend_wave_2_dat_dfi2)
non_med_attend_wave_2_dat_dfi4 <- rbind.data.frame(non_med_attend_wave_2_dat_dfi3,non_med_attend_wave_2_dat_dfi2,row.names = NULL)
non_med_attend_wave_2_dat_dfi4$rep_nrs <- as.numeric(non_med_attend_wave_2_dat_dfi4$rep_nrs)
non_med_attend_wave_2_dat_dfi4 <- left_join(x = non_med_attend_wave_2_dat_dfi4, y = hus_r_draw_df, by = "rep_nrs")
non_med_attend_wave_2_dat_dfi4_upd <- non_med_attend_wave_2_dat_dfi4 %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/hus_draw) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,hus_draw,
         incidence_non_severe, covid_attrib_deaths, incidence_non_fatal)

non_med_attend_wave_2_dat_df_upd <- non_med_attend_wave_2_dat_dfi4_upd %>%
  arrange(rep_nrs,age.group) %>%
  mutate(total_severe_inc = incidence_hosp + incidence_non_severe,
         total_fatal_inc = incidence_fatal + incidence_non_fatal)

non_med_attend_wave_2_dat_df_totals <- non_med_attend_wave_2_dat_df_upd %>%
  group_by(rep_nrs) %>%
  mutate(nr_covid_total = sum(nr.covid),
         age_covid_patients_total = sum(age.covid.patients),
         population_admit_total = sum(age_pop_estimate_at_admit_peak),
         population_death_total = sum(age_pop_estimate_at_death_peak),
         fatalities_total = sum(fatalities),
         fatals_total = sum(fatals),
         covid_attrib_deaths_total = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp_total = (nr_covid_total/population_admit_total)*100000,
         incidence_fatal_total = (fatals_total/population_death_total)*100000,
         incidence_non_severe_total = (incidence_hosp_total/hus_draw) - incidence_hosp_total,
         incidence_non_fatal_total = ((covid_attrib_deaths_total - fatals_total)/population_death_total)*100000,
         hfr_total = fatals_total/(fatals_total + nr_covid_total),
         srif_total = incidence_non_fatal_total/(incidence_non_fatal_total + incidence_non_severe_total),
         total_severe_wave = incidence_hosp_total + incidence_non_severe_total,
         total_fatal_wave = incidence_fatal_total + incidence_non_fatal_total) %>%
  filter(!duplicated(rep_nrs)) %>%
  select(-(age.group:incidence_non_fatal)) %>%
  ungroup()



non_med_attend_total_2_ci <- non_med_attend_wave_2_dat_df_upd %>%
  group_by(age.group) %>%
  summarise(CI_low_severe = quantile(total_severe_inc,0.025),
            CI_high_severe = quantile(total_severe_inc,0.975),
            CI_low_fatal = quantile(total_fatal_inc, 0.025),
            CI_high_fatal = quantile(total_fatal_inc,0.975)) %>%
  ungroup()

attack_constraint <- max(non_med_attend_total_2_ci$CI_high_severe + non_med_attend_total_2_ci$CI_high_fatal)

attack_2 <- attack[attack$wave.nr == 2,]
attack_2_N <- sum(attack_2$N)
attack_2_n <- sum(attack_2$n)

attack_2_upd <- data.frame()

for (i in 1:nrow(attack_2)){  
  new_vec <- replicate(n= 1000, attack_rate_draws(ar_rate = attack_2,ages = i,threshold = attack_constraint))
  attack_vec <-cbind.data.frame(c(1:1000),attack_2[i,c("age.group")],
                                new_vec, row.names = NULL)
  attack_2_upd <- rbind.data.frame(attack_2_upd,attack_vec, row.names = NULL) 
  
}

names(attack_2_upd) <- c("rep_nrs","age.group","attack_rate_draw")

wave_attack_2 <- replicate(n= 1000, rbinom(n = 1, size = attack_2_N, prob = attack_2_n/attack_2_N)/attack_2_N)
wave_attack_2_df <- cbind.data.frame(c(1:1000), wave_attack_2, row.names = NULL)
names(wave_attack_2_df)[1] <- "rep_nrs"

non_med_attend_wave_2_dat_df_upd2 <- left_join(x = non_med_attend_wave_2_dat_df_upd, y = attack_2_upd, by = c("age.group","rep_nrs"))
non_med_attend_total_2 <- non_med_attend_wave_2_dat_df_upd2 %>%
  mutate(infection_incidence = (attack_rate_draw*100000) - (total_severe_inc + total_fatal_inc),
         ifr = (total_fatal_inc/(attack_rate_draw*100000))*100,
         nr_infections = attack_rate_draw*age_pop_estimate_at_admit_peak)

non_med_attend_total_2a <- left_join(x = non_med_attend_wave_2_dat_df_totals, y = wave_attack_2_df, by = "rep_nrs")
non_med_attend_total_2a$ifr <- (non_med_attend_total_2a$total_fatal_wave/(non_med_attend_total_2a$wave_attack_2*100000))*100
non_med_attend_total_2a$infection_incidence <- (non_med_attend_total_2a$wave_attack_2*100000) - (non_med_attend_total_2a$total_severe_wave + non_med_attend_total_2a$total_fatal_wave)


non_med_attend_total_2_ci_total <- non_med_attend_total_2a %>%
  summarise(CI_low_severe = quantile(total_severe_wave,0.025),
            CI_high_severe = quantile(total_severe_wave,0.975),
            CI_low_fatal = quantile(total_fatal_wave, 0.025),
            CI_high_fatal = quantile(total_fatal_wave,0.975),
            CI_low_inc_hosp = quantile(incidence_hosp_total,0.025),
            CI_high_inc_hosp = quantile(incidence_hosp_total,0.975),
            CI_low_inc_fatal = quantile(incidence_fatal_total,0.025),
            CI_high_inc_fatal = quantile(incidence_fatal_total,0.975),
            CI_low_inc_non_severe = quantile(incidence_non_severe_total,0.025),
            CI_high_inc_non_severe = quantile(incidence_non_severe_total,0.975),
            CI_low_inc_non_fatal = quantile(incidence_non_fatal_total,0.025),
            CI_high_inc_non_fatal = quantile(incidence_non_fatal_total,0.975),
            CI_low_hfr_total = quantile(hfr_total,0.025),
            CI_high_hfr_fatal = quantile(hfr_total,0.975),
            CI_low_srif_total = quantile(srif_total,0.025),
            CI_high_srif_total = quantile(srif_total,0.975),
            CI_low_attack_rate = quantile(wave_attack_2,0.025),
            CI_high_attack_rate = quantile(wave_attack_2, 0.975),
            I_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975)) %>%
  ungroup()

non_med_attend_total_2_ci_add <- non_med_attend_total_2 %>%
  group_by(age.group) %>%
  summarise(CI_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975),
            CI_low_attack = quantile(attack_rate_draw,0.025),
            CI_high_attack = quantile(attack_rate_draw,0.975)) %>%
  ungroup()

non_med_attend_total_2_ci <- cbind.data.frame(rep("Wave 2", times = nrow(non_med_attend_total_2_ci)),
                                              non_med_attend_total_2_ci, row.names = NULL)
names(non_med_attend_total_2_ci)[1] <- "wave.nr"

non_med_attend_total_2_ci_wave <- cbind.data.frame("Wave 2",
                                                   non_med_attend_total_2_ci_total, row.names = NULL)
names(non_med_attend_total_2_ci_wave)[1] <- "wave_nr"


#3
non_med_attend_wave_3_dat_df <- readRDS(file.path(directory_upd,non_med_attend_wave_3_dat))
non_med_attend_wave_3_dat_dfi2 <- non_med_attend_wave_3_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group == "65-69"| age.group == "70-74"| age.group == "75-79"| age.group == "80+") %>%
  summarise(nr.covid = sum(nr.covid_age),
            age.covid.patients = sum(age.covid.patients.admit1.x),
            age_pop_estimate_at_admit_peak = sum(age_pop_estimate_at_admit_age_peak),
            fatals = sum(fatals_age),
            fatalities = sum(fatalities1),
            age_pop_estimate_at_death_peak = sum(age_pop_estimate_at_death_age_peak),
            covid_attrib_deaths = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  mutate(age.group = "65+") %>%
  mutate(rep_nrs = row.names(.)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,covid_attrib_deaths,
         incidence_non_severe, incidence_non_fatal) %>%
  ungroup()

non_med_attend_wave_3_dat_dfi3 <- non_med_attend_wave_3_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group != "65-69", age.group != "70-74", age.group != "75-79", age.group != "80+") %>%
  select(rep_nrs, age.group,age.covid.patients.admit1.x,nr.covid_age,age_pop_estimate_at_admit_age_peak,incidence_hosp_age,fatalities1, fatals_age, age_pop_estimate_at_death_age_peak,
         incidence_fatal_age,covid_attrib_deaths, incidence_non_hosp, incidence_non_fatal) %>%
  ungroup()

names(non_med_attend_wave_3_dat_dfi3) <- names(non_med_attend_wave_3_dat_dfi2)
non_med_attend_wave_3_dat_dfi4 <- rbind.data.frame(non_med_attend_wave_3_dat_dfi3,non_med_attend_wave_3_dat_dfi2,row.names = NULL)
non_med_attend_wave_3_dat_dfi4$rep_nrs <- as.numeric(non_med_attend_wave_3_dat_dfi4$rep_nrs)
non_med_attend_wave_3_dat_dfi4 <- left_join(x = non_med_attend_wave_3_dat_dfi4, y = hus_r_draw_df, by = "rep_nrs")
non_med_attend_wave_3_dat_dfi4_upd <- non_med_attend_wave_3_dat_dfi4 %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/hus_draw) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,hus_draw,
         incidence_non_severe, covid_attrib_deaths, incidence_non_fatal)

non_med_attend_wave_3_dat_df_upd <- non_med_attend_wave_3_dat_dfi4_upd %>%
  arrange(rep_nrs,age.group) %>%
  mutate(total_severe_inc = incidence_hosp + incidence_non_severe,
         total_fatal_inc = incidence_fatal + incidence_non_fatal)

non_med_attend_wave_3_dat_df_totals <- non_med_attend_wave_3_dat_df_upd %>%
  group_by(rep_nrs) %>%
  mutate(nr_covid_total = sum(nr.covid),
         age_covid_patients_total = sum(age.covid.patients),
         population_admit_total = sum(age_pop_estimate_at_admit_peak),
         population_death_total = sum(age_pop_estimate_at_death_peak),
         fatalities_total = sum(fatalities),
         fatals_total = sum(fatals),
         covid_attrib_deaths_total = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp_total = (nr_covid_total/population_admit_total)*100000,
         incidence_fatal_total = (fatals_total/population_death_total)*100000,
         incidence_non_severe_total = (incidence_hosp_total/hus_draw) - incidence_hosp_total,
         incidence_non_fatal_total = ((covid_attrib_deaths_total - fatals_total)/population_death_total)*100000,
         hfr_total = fatals_total/(fatals_total + nr_covid_total),
         srif_total = incidence_non_fatal_total/(incidence_non_fatal_total + incidence_non_severe_total),
         total_severe_wave = incidence_hosp_total + incidence_non_severe_total,
         total_fatal_wave = incidence_fatal_total + incidence_non_fatal_total) %>%
  filter(!duplicated(rep_nrs)) %>%
  select(-(age.group:incidence_non_fatal)) %>%
  ungroup()


non_med_attend_total_3_ci <- non_med_attend_wave_3_dat_df_upd %>%
  group_by(age.group) %>%
  summarise(CI_low_severe = quantile(total_severe_inc,0.025),
            CI_high_severe = quantile(total_severe_inc,0.975),
            CI_low_fatal = quantile(total_fatal_inc, 0.025),
            CI_high_fatal = quantile(total_fatal_inc,0.975)) %>%
  ungroup()

attack_constraint <- max(non_med_attend_total_3_ci$CI_high_severe + non_med_attend_total_3_ci$CI_high_fatal)

attack_3 <- attack[attack$wave.nr == 3,]
attack_3_N <- sum(attack_3$N)
attack_3_n <- sum(attack_3$n)

attack_3_upd <- data.frame()

for (i in 1:nrow(attack_3)){  
  new_vec <- replicate(n= 1000, attack_rate_draws(ar_rate = attack_3,ages = i,threshold = attack_constraint))
  attack_vec <-cbind.data.frame(c(1:1000),attack_3[i,c("age.group")],
                                new_vec, row.names = NULL)
  attack_3_upd <- rbind.data.frame(attack_3_upd,attack_vec, row.names = NULL) 
  
}

names(attack_3_upd) <- c("rep_nrs","age.group","attack_rate_draw")
wave_attack_3 <- replicate(n= 1000, rbinom(n = 1, size = attack_3_N, prob = attack_3_n/attack_3_N)/attack_3_N)
wave_attack_3_df <- cbind.data.frame(c(1:1000), wave_attack_3, row.names = NULL)
names(wave_attack_3_df)[1] <- "rep_nrs"

non_med_attend_wave_3_dat_df_upd2 <- left_join(x = non_med_attend_wave_3_dat_df_upd, y = attack_3_upd, by = c("age.group","rep_nrs"))
non_med_attend_total_3 <- non_med_attend_wave_3_dat_df_upd2 %>%
  mutate(infection_incidence = (attack_rate_draw*100000) - (total_severe_inc + total_fatal_inc),
         ifr = (total_fatal_inc/(attack_rate_draw*100000))*100,
         nr_infections = attack_rate_draw*age_pop_estimate_at_admit_peak)

non_med_attend_total_3a <- left_join(x = non_med_attend_wave_3_dat_df_totals, y = wave_attack_3_df, by = "rep_nrs")
non_med_attend_total_3a$ifr <- (non_med_attend_total_3a$total_fatal_wave/(non_med_attend_total_3a$wave_attack_3*100000))*100
non_med_attend_total_3a$infection_incidence <- (non_med_attend_total_3a$wave_attack_3*100000) - (non_med_attend_total_3a$total_severe_wave + non_med_attend_total_3a$total_fatal_wave)

non_med_attend_total_3_ci_total <- non_med_attend_total_3a %>%
  summarise(CI_low_severe = quantile(total_severe_wave,0.025),
            CI_high_severe = quantile(total_severe_wave,0.975),
            CI_low_fatal = quantile(total_fatal_wave, 0.025),
            CI_high_fatal = quantile(total_fatal_wave,0.975),
            CI_low_inc_hosp = quantile(incidence_hosp_total,0.025),
            CI_high_inc_hosp = quantile(incidence_hosp_total,0.975),
            CI_low_inc_fatal = quantile(incidence_fatal_total,0.025),
            CI_high_inc_fatal = quantile(incidence_fatal_total,0.975),
            CI_low_inc_non_severe = quantile(incidence_non_severe_total,0.025),
            CI_high_inc_non_severe = quantile(incidence_non_severe_total,0.975),
            CI_low_inc_non_fatal = quantile(incidence_non_fatal_total,0.025),
            CI_high_inc_non_fatal = quantile(incidence_non_fatal_total,0.975),
            CI_low_hfr_total = quantile(hfr_total,0.025),
            CI_high_hfr_fatal = quantile(hfr_total,0.975),
            CI_low_srif_total = quantile(srif_total,0.025),
            CI_high_srif_total = quantile(srif_total,0.975),
            CI_low_attack_rate = quantile(wave_attack_3,0.025),
            CI_high_attack_rate = quantile(wave_attack_3, 0.975),
            I_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975)) %>%
  ungroup()

non_med_attend_total_3_ci_add <- non_med_attend_total_3 %>%
  group_by(age.group) %>%
  summarise(CI_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975),
            CI_low_attack = quantile(attack_rate_draw,0.025),
            CI_high_attack = quantile(attack_rate_draw,0.975)) %>%
  ungroup()

non_med_attend_total_3_ci <- cbind.data.frame(rep("Wave 3", times = nrow(non_med_attend_total_3_ci)),
                                              non_med_attend_total_3_ci, row.names = NULL)
names(non_med_attend_total_3_ci)[1] <- "wave.nr"

non_med_attend_total_3_ci_wave <- cbind.data.frame("Wave 3",
                                                   non_med_attend_total_3_ci_total, row.names = NULL)
names(non_med_attend_total_3_ci_wave)[1] <- "wave_nr"

#4
non_med_attend_wave_4_dat_df <- readRDS(file.path(directory_upd,non_med_attend_wave_4_dat))
non_med_attend_wave_4_dat_dfi2 <- non_med_attend_wave_4_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group == "65-69"| age.group == "70-74"| age.group == "75-79"| age.group == "80+") %>%
  summarise(nr.covid = sum(nr.covid_age),
            age.covid.patients = sum(age.covid.patients.admit1.x),
            age_pop_estimate_at_admit_peak = sum(age_pop_estimate_at_admit_age_peak),
            fatals = sum(fatals_age),
            fatalities = sum(fatalities1),
            age_pop_estimate_at_death_peak = sum(age_pop_estimate_at_death_age_peak),
            covid_attrib_deaths = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  mutate(age.group = "65+") %>%
  mutate(rep_nrs = row.names(.)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,covid_attrib_deaths,
         incidence_non_severe, incidence_non_fatal) %>%
  ungroup()

non_med_attend_wave_4_dat_dfi3 <- non_med_attend_wave_4_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group != "65-69", age.group != "70-74", age.group != "75-79", age.group != "80+") %>%
  select(rep_nrs, age.group,age.covid.patients.admit1.x,nr.covid_age,age_pop_estimate_at_admit_age_peak,incidence_hosp_age,fatalities1, fatals_age, age_pop_estimate_at_death_age_peak,
         incidence_fatal_age,covid_attrib_deaths, incidence_non_hosp, incidence_non_fatal) %>%
  ungroup()

names(non_med_attend_wave_4_dat_dfi3) <- names(non_med_attend_wave_4_dat_dfi2)
non_med_attend_wave_4_dat_dfi4 <- rbind.data.frame(non_med_attend_wave_4_dat_dfi3,non_med_attend_wave_4_dat_dfi2,row.names = NULL)
non_med_attend_wave_4_dat_dfi4$rep_nrs <- as.numeric(non_med_attend_wave_4_dat_dfi4$rep_nrs)
non_med_attend_wave_4_dat_dfi4 <- left_join(x = non_med_attend_wave_4_dat_dfi4, y = hus_r_draw_df, by = "rep_nrs")
non_med_attend_wave_4_dat_dfi4_upd <- non_med_attend_wave_4_dat_dfi4 %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/hus_draw) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,hus_draw,
         incidence_non_severe, covid_attrib_deaths, incidence_non_fatal)

non_med_attend_wave_4_dat_df_upd <- non_med_attend_wave_4_dat_dfi4_upd %>%
  arrange(rep_nrs,age.group) %>%
  mutate(total_severe_inc = incidence_hosp + incidence_non_severe,
         total_fatal_inc = incidence_fatal + incidence_non_fatal)

non_med_attend_wave_4_dat_df_totals <- non_med_attend_wave_4_dat_df_upd %>%
  group_by(rep_nrs) %>%
  mutate(nr_covid_total = sum(nr.covid),
         age_covid_patients_total = sum(age.covid.patients),
         population_admit_total = sum(age_pop_estimate_at_admit_peak),
         population_death_total = sum(age_pop_estimate_at_death_peak),
         fatalities_total = sum(fatalities),
         fatals_total = sum(fatals),
         covid_attrib_deaths_total = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp_total = (nr_covid_total/population_admit_total)*100000,
         incidence_fatal_total = (fatals_total/population_death_total)*100000,
         incidence_non_severe_total = (incidence_hosp_total/hus_draw) - incidence_hosp_total,
         incidence_non_fatal_total = ((covid_attrib_deaths_total - fatals_total)/population_death_total)*100000,
         hfr_total = fatals_total/(fatals_total + nr_covid_total),
         srif_total = incidence_non_fatal_total/(incidence_non_fatal_total + incidence_non_severe_total),
         total_severe_wave = incidence_hosp_total + incidence_non_severe_total,
         total_fatal_wave = incidence_fatal_total + incidence_non_fatal_total) %>%
  filter(!duplicated(rep_nrs)) %>%
  select(-(age.group:incidence_non_fatal)) %>%
  ungroup()

non_med_attend_total_4_ci <- non_med_attend_wave_4_dat_df_upd %>%
  group_by(age.group) %>%
  summarise(CI_low_severe = quantile(total_severe_inc,0.025),
            CI_high_severe = quantile(total_severe_inc,0.975),
            CI_low_fatal = quantile(total_fatal_inc, 0.025),
            CI_high_fatal = quantile(total_fatal_inc,0.975)) %>%
  ungroup()

attack_constraint <- max(non_med_attend_total_4_ci$CI_high_severe + non_med_attend_total_4_ci$CI_high_fatal)

attack_4 <- attack[attack$wave.nr == 4,]
attack_4_N <- sum(attack_4$N)
attack_4_n <- sum(attack_4$n)

attack_4_upd <- data.frame()


for (i in 1:nrow(attack_4)){  
  new_vec <- replicate(n= 1000, attack_rate_draws(ar_rate = attack_4,ages = i,threshold = attack_constraint))
  attack_vec <-cbind.data.frame(c(1:1000),attack_4[i,c("age.group")],
                                new_vec, row.names = NULL)
  attack_4_upd <- rbind.data.frame(attack_4_upd,attack_vec, row.names = NULL) 
  
}

names(attack_4_upd) <- c("rep_nrs","age.group","attack_rate_draw")
wave_attack_4 <- replicate(n= 1000, rbinom(n = 1, size = attack_4_N, prob = attack_4_n/attack_4_N)/attack_4_N)
wave_attack_4_df <- cbind.data.frame(c(1:1000), wave_attack_4, row.names = NULL)
names(wave_attack_4_df)[1] <- "rep_nrs"

non_med_attend_wave_4_dat_df_upd2 <- left_join(x = non_med_attend_wave_4_dat_df_upd, y = attack_4_upd, by = c("age.group","rep_nrs"))
non_med_attend_total_4 <- non_med_attend_wave_4_dat_df_upd2 %>%
  mutate(infection_incidence = (attack_rate_draw*100000) - (total_severe_inc + total_fatal_inc),
         ifr = (total_fatal_inc/(attack_rate_draw*100000))*100,
         nr_infections = attack_rate_draw*age_pop_estimate_at_admit_peak)

non_med_attend_total_4a <- left_join(x = non_med_attend_wave_4_dat_df_totals, y = wave_attack_4_df, by = "rep_nrs")
non_med_attend_total_4a$ifr <- (non_med_attend_total_4a$total_fatal_wave/(non_med_attend_total_4a$wave_attack_4*100000))*100
non_med_attend_total_4a$infection_incidence <- (non_med_attend_total_4a$wave_attack_4*100000) - (non_med_attend_total_4a$total_severe_wave + non_med_attend_total_4a$total_fatal_wave)

non_med_attend_total_4_ci_total <- non_med_attend_total_4a %>%
  summarise(CI_low_severe = quantile(total_severe_wave,0.025),
            CI_high_severe = quantile(total_severe_wave,0.975),
            CI_low_fatal = quantile(total_fatal_wave, 0.025),
            CI_high_fatal = quantile(total_fatal_wave,0.975),
            CI_low_inc_hosp = quantile(incidence_hosp_total,0.025),
            CI_high_inc_hosp = quantile(incidence_hosp_total,0.975),
            CI_low_inc_fatal = quantile(incidence_fatal_total,0.025),
            CI_high_inc_fatal = quantile(incidence_fatal_total,0.975),
            CI_low_inc_non_severe = quantile(incidence_non_severe_total,0.025),
            CI_high_inc_non_severe = quantile(incidence_non_severe_total,0.975),
            CI_low_inc_non_fatal = quantile(incidence_non_fatal_total,0.025),
            CI_high_inc_non_fatal = quantile(incidence_non_fatal_total,0.975),
            CI_low_hfr_total = quantile(hfr_total,0.025),
            CI_high_hfr_fatal = quantile(hfr_total,0.975),
            CI_low_srif_total = quantile(srif_total,0.025),
            CI_high_srif_total = quantile(srif_total,0.975),
            CI_low_attack_rate = quantile(wave_attack_4,0.025),
            CI_high_attack_rate = quantile(wave_attack_4, 0.975),
            I_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975)) %>%
  ungroup()

non_med_attend_total_4_ci_add <- non_med_attend_total_4 %>%
  group_by(age.group) %>%
  summarise(CI_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975),
            CI_low_attack = quantile(attack_rate_draw,0.025),
            CI_high_attack = quantile(attack_rate_draw,0.975)) %>%
  ungroup()

non_med_attend_total_4_ci <- cbind.data.frame(rep("Wave 4", times = nrow(non_med_attend_total_4_ci)),
                                              non_med_attend_total_4_ci, row.names = NULL)
names(non_med_attend_total_4_ci)[1] <- "wave.nr"

non_med_attend_total_4_ci_wave <- cbind.data.frame("Wave 4",
                                                   non_med_attend_total_4_ci_total, row.names = NULL)
names(non_med_attend_total_4_ci_wave)[1] <- "wave_nr"

#5
non_med_attend_wave_5_dat_df <- readRDS(file.path(directory_upd,non_med_attend_wave_5_dat))
non_med_attend_wave_5_dat_dfi2 <- non_med_attend_wave_5_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group == "65-69"| age.group == "70-74"| age.group == "75-79"| age.group == "80+") %>%
  summarise(nr.covid = sum(nr.covid_age),
            age.covid.patients = sum(age.covid.patients.admit1.x),
            age_pop_estimate_at_admit_peak = sum(age_pop_estimate_at_admit_age_peak),
            fatals = sum(fatals_age),
            fatalities = sum(fatalities1),
            age_pop_estimate_at_death_peak = sum(age_pop_estimate_at_death_age_peak),
            covid_attrib_deaths = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/(sum(HUS_severe$Count)/sum(HUS_severe$Total))) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  mutate(age.group = "65+") %>%
  mutate(rep_nrs = row.names(.)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,covid_attrib_deaths,
         incidence_non_severe, incidence_non_fatal) %>%
  ungroup()

non_med_attend_wave_5_dat_dfi3 <- non_med_attend_wave_5_dat_df %>%
  group_by(rep_nrs) %>%
  filter(age.group != "65-69", age.group != "70-74", age.group != "75-79", age.group != "80+") %>%
  select(rep_nrs, age.group,age.covid.patients.admit1.x,nr.covid_age,age_pop_estimate_at_admit_age_peak,incidence_hosp_age,fatalities1, fatals_age, age_pop_estimate_at_death_age_peak,
         incidence_fatal_age,covid_attrib_deaths, incidence_non_hosp, incidence_non_fatal) %>%
  ungroup()

names(non_med_attend_wave_5_dat_dfi3) <- names(non_med_attend_wave_5_dat_dfi2)
non_med_attend_wave_5_dat_dfi4 <- rbind.data.frame(non_med_attend_wave_5_dat_dfi3,non_med_attend_wave_5_dat_dfi2,row.names = NULL)
non_med_attend_wave_5_dat_dfi4$rep_nrs <- as.numeric(non_med_attend_wave_5_dat_dfi4$rep_nrs)
non_med_attend_wave_5_dat_dfi4 <- left_join(x = non_med_attend_wave_5_dat_dfi4, y = hus_r_draw_df, by = "rep_nrs")
non_med_attend_wave_5_dat_dfi4_upd <- non_med_attend_wave_5_dat_dfi4 %>%
  mutate(incidence_hosp = (nr.covid/age_pop_estimate_at_admit_peak)*100000,
         incidence_fatal = (fatals/age_pop_estimate_at_death_peak)*100000,
         incidence_non_severe = (incidence_hosp/hus_draw) - incidence_hosp,
         incidence_non_fatal = ((covid_attrib_deaths - fatals)/age_pop_estimate_at_death_peak)*100000) %>%
  mutate(incidence_non_fatal = ifelse(incidence_non_fatal < 0, 0, incidence_non_fatal)) %>%
  select(rep_nrs, age.group,age.covid.patients,nr.covid,age_pop_estimate_at_admit_peak,incidence_hosp,fatalities, fatals, age_pop_estimate_at_death_peak,incidence_fatal,hus_draw,
         incidence_non_severe, covid_attrib_deaths, incidence_non_fatal)

non_med_attend_wave_5_dat_df_upd <- non_med_attend_wave_5_dat_dfi4_upd %>%
  arrange(rep_nrs,age.group) %>%
  mutate(total_severe_inc = incidence_hosp + incidence_non_severe,
         total_fatal_inc = incidence_fatal + incidence_non_fatal)

non_med_attend_wave_5_dat_df_totals <- non_med_attend_wave_5_dat_df_upd %>%
  group_by(rep_nrs) %>%
  mutate(nr_covid_total = sum(nr.covid),
         age_covid_patients_total = sum(age.covid.patients),
         population_admit_total = sum(age_pop_estimate_at_admit_peak),
         population_death_total = sum(age_pop_estimate_at_death_peak),
         fatalities_total = sum(fatalities),
         fatals_total = sum(fatals),
         covid_attrib_deaths_total = sum(covid_attrib_deaths)) %>%
  mutate(incidence_hosp_total = (nr_covid_total/population_admit_total)*100000,
         incidence_fatal_total = (fatals_total/population_death_total)*100000,
         incidence_non_severe_total = (incidence_hosp_total/hus_draw) - incidence_hosp_total,
         incidence_non_fatal_total = ((covid_attrib_deaths_total - fatals_total)/population_death_total)*100000,
         hfr_total = fatals_total/(fatals_total + nr_covid_total),
         srif_total = incidence_non_fatal_total/(incidence_non_fatal_total + incidence_non_severe_total),
         total_severe_wave = incidence_hosp_total + incidence_non_severe_total,
         total_fatal_wave = incidence_fatal_total + incidence_non_fatal_total) %>%
  filter(!duplicated(rep_nrs)) %>%
  select(-(age.group:incidence_non_fatal)) %>%
  ungroup()

non_med_attend_total_5_ci <- non_med_attend_wave_5_dat_df_upd %>%
  group_by(age.group) %>%
  summarise(CI_low_severe = quantile(total_severe_inc,0.025),
            CI_high_severe = quantile(total_severe_inc,0.975),
            CI_low_fatal = quantile(total_fatal_inc, 0.025),
            CI_high_fatal = quantile(total_fatal_inc,0.975)) %>%
  ungroup()

attack_constraint <- max(non_med_attend_total_5_ci$CI_high_severe + non_med_attend_total_5_ci$CI_high_fatal)

attack_5 <- attack[attack$wave.nr == 5,]
attack_5_N <- sum(attack_5$N)
attack_5_n <- sum(attack_5$n)

attack_5_upd <- data.frame()


for (i in 1:nrow(attack_5)){  
  new_vec <- replicate(n= 1000, attack_rate_draws(ar_rate = attack_5,ages = i,threshold = attack_constraint))
  attack_vec <-cbind.data.frame(c(1:1000),attack_5[i,c("age.group")],
                                new_vec, row.names = NULL)
  attack_5_upd <- rbind.data.frame(attack_5_upd,attack_vec, row.names = NULL) 
  
}

names(attack_5_upd) <- c("rep_nrs","age.group","attack_rate_draw")
wave_attack_5 <- replicate(n= 1000, rbinom(n = 1, size = attack_5_N, prob = attack_5_n/attack_5_N)/attack_5_N)
wave_attack_5_df <- cbind.data.frame(c(1:1000), wave_attack_5, row.names = NULL)
names(wave_attack_5_df)[1] <- "rep_nrs"

non_med_attend_wave_5_dat_df_upd2 <- left_join(x = non_med_attend_wave_5_dat_df_upd, y = attack_5_upd, by = c("age.group","rep_nrs"))
non_med_attend_total_5 <- non_med_attend_wave_5_dat_df_upd2 %>%
  mutate(infection_incidence = (attack_rate_draw*100000) - (total_severe_inc + total_fatal_inc),
         ifr = (total_fatal_inc/(attack_rate_draw*100000))*100,
         nr_infections = attack_rate_draw*age_pop_estimate_at_admit_peak)

non_med_attend_total_5a <- left_join(x = non_med_attend_wave_5_dat_df_totals, y = wave_attack_5_df, by = "rep_nrs")
non_med_attend_total_5a$ifr <- (non_med_attend_total_5a$total_fatal_wave/(non_med_attend_total_5a$wave_attack_5*100000))*100
non_med_attend_total_5a$infection_incidence <- (non_med_attend_total_5a$wave_attack_5*100000) - (non_med_attend_total_5a$total_severe_wave + non_med_attend_total_5a$total_fatal_wave)

non_med_attend_total_5_ci_total <- non_med_attend_total_5a %>%
  summarise(CI_low_severe = quantile(total_severe_wave,0.025),
            CI_high_severe = quantile(total_severe_wave,0.975),
            CI_low_fatal = quantile(total_fatal_wave, 0.025),
            CI_high_fatal = quantile(total_fatal_wave,0.975),
            CI_low_inc_hosp = quantile(incidence_hosp_total,0.025),
            CI_high_inc_hosp = quantile(incidence_hosp_total,0.975),
            CI_low_inc_fatal = quantile(incidence_fatal_total,0.025),
            CI_high_inc_fatal = quantile(incidence_fatal_total,0.975),
            CI_low_inc_non_severe = quantile(incidence_non_severe_total,0.025),
            CI_high_inc_non_severe = quantile(incidence_non_severe_total,0.975),
            CI_low_inc_non_fatal = quantile(incidence_non_fatal_total,0.025),
            CI_high_inc_non_fatal = quantile(incidence_non_fatal_total,0.975),
            CI_low_hfr_total = quantile(hfr_total,0.025),
            CI_high_hfr_fatal = quantile(hfr_total,0.975),
            CI_low_srif_total = quantile(srif_total,0.025),
            CI_high_srif_total = quantile(srif_total,0.975),
            CI_low_attack_rate = quantile(wave_attack_5,0.025),
            CI_high_attack_rate = quantile(wave_attack_5, 0.975),
            I_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975)) %>%
  ungroup()

non_med_attend_total_5_ci_add <- non_med_attend_total_5 %>%
  group_by(age.group) %>%
  summarise(CI_low_incrate = quantile(infection_incidence,0.025),
            CI_high_incrate = quantile(infection_incidence,0.975),
            CI_low_ifr = quantile(ifr,0.025),
            CI_high_ifr = quantile(ifr,0.975),
            CI_low_attack = quantile(attack_rate_draw,0.025),
            CI_high_attack = quantile(attack_rate_draw,0.975)) %>%
  ungroup()

non_med_attend_total_5_ci <- cbind.data.frame(rep("Wave 5", times = nrow(non_med_attend_total_5_ci)),
                                              non_med_attend_total_5_ci, row.names = NULL)
names(non_med_attend_total_5_ci)[1] <- "wave.nr"

non_med_attend_total_5_ci_wave <- cbind.data.frame("Wave 5",
                                                   non_med_attend_total_5_ci_total, row.names = NULL)
names(non_med_attend_total_5_ci_wave)[1] <- "wave_nr"



non_med_attend_total_ci <- rbind.data.frame(non_med_attend_total_1_ci,
                                            non_med_attend_total_2_ci,
                                      non_med_attend_total_3_ci,
                                      non_med_attend_total_4_ci,
                                      non_med_attend_total_5_ci,row.names = NULL)
non_med_attend_total_ci_add <- rbind.data.frame(non_med_attend_total_1_ci_add,
                                            non_med_attend_total_2_ci_add,
                                            non_med_attend_total_3_ci_add,
                                            non_med_attend_total_4_ci_add,
                                            non_med_attend_total_5_ci_add,row.names = NULL)
non_med_attend_total_wave_ci <- rbind.data.frame(non_med_attend_total_1_ci_wave,
                                                 non_med_attend_total_2_ci_wave,
                                                 non_med_attend_total_3_ci_wave,
                                                 non_med_attend_total_4_ci_wave,
                                                 non_med_attend_total_5_ci_wave, row.names = NULL)



ci_combine <- list(med_attend_ci_hfr,
                   med_attend_ci,
                   # non_med_attend_ci,
                   non_med_attend_total_ci,
                   non_med_attend_total_ci_add,
                   non_med_attend_total_wave_ci)
return(ci_combine)

}