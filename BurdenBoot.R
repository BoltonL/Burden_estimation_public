burden_bootstrap_data <- function(data_admit_in,data_death_in){
set.seed(12345)
wave_bootstrap_fn <- function(wave_nr,wave_data_in){
  #sample from datcov data unique patient ids per wave
  wave_data_in[[wave_nr]][sample(wave_data_in[[wave_nr]]$row.index, 
                                 length(wave_data_in[[wave_nr]]$row.index), 
                                 replace = TRUE),]
  
  
}

#generate 1000 bootstrap samples
input_set_admit <- lapply(data_admit_in, function(t1){
  t1 %>%
    mutate(row.index = seq(1,nrow(t1)))
})

wave_bootstrap_data_admit <- lapply(1:5,function(wave_nr,wave_data_in){
  replicate_nr <- 1000
  replicate(n = replicate_nr, wave_bootstrap_fn(wave_nr,wave_data_in = input_set_admit), simplify = FALSE)
})

input_set_death <- lapply(data_death_in, function(t1){
  t1 %>%
    mutate(row.index = seq(1,nrow(t1)))
})

wave_bootstrap_data_death <- lapply(1:5,function(wave_nr,wave_data_in){
  replicate_nr <- 1000
  replicate(n = replicate_nr, wave_bootstrap_fn(wave_nr,wave_data_in = input_set_death), simplify = FALSE)
})

saveRDS(wave_bootstrap_data_admit,file = file.path("./Data","wave_bootstrap_data_admit.RDS"))
saveRDS(wave_bootstrap_data_death,file = file.path("./Data","wave_bootstrap_data_death.RDS"))


}