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