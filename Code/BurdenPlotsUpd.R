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

burden_plots_upd <- function(data_in1, data_in2){

  data_in1 <- non_med_attended_burden_country_table_age_out  
  data_in2 <- non_med_attended_burden_country_table_total
  
 x.max.left <- max(data_in1$incidence_hosp_age)
 x.max.right <- max(data_in1$incidence_fatal_age)
 
 x.max.left.nm <- max(data_in1$incidence_non_hosp_age)
 x.max.right.nm <- max(data_in1$incidence_non_fatal)
 
 
  for (i in 1:length(unique(data_in1$wave.nr))){
    data_in_pre <- data_in1[str_extract(data_in1$wave.nr, "[0-9]+") == i,]
    #medically attended
  plotrix::pyramid.plot(data_in_pre$incidence_hosp_age,data_in_pre$incidence_fatal_age,
                                                   labels = data_in_pre$age.group,
                                                   top.labels = c("Severe non-fatal","Age (years)","Fatal"),
                                                   gap = x.max.left/8,
                                                   lxcol = "black",
                                                   rxcol = "grey",
                                                   laxlab = seq(0,x.max.left, 50),
                                                   raxlab = seq(0,x.max.right,50),
                                                   show.values = FALSE,
                                                   ndig = 0,
                                                   unit = "Incidence / 100 000 population at risk",
                                                   main = paste0("Wave ",i,": Medically attended"))
  #Non medically attended
  # if (i== 1){
  #   divisor <- 10
  # }else if (i == 2){
  #   divisor <- 12
  # } else if (i ==3){
  #   divisor <- 10
  # } else {
  #   divisor <- 10
  # }

  plotrix::pyramid.plot(data_in_pre$incidence_non_hosp_age,data_in_pre$incidence_non_fatal, 
                        labels = data_in_pre$age.group,
                        top.labels = c("Severe non-fatal","Age (years)","Fatal"),
                        gap = x.max.right.nm/10,
                        lxcol = "black", 
                        rxcol = "grey",
                        laxlab = seq(0,x.max.right.nm, 100),
                        raxlab = seq(0,x.max.right.nm,200),
                        show.values = FALSE,
                        ndig = 0,
                        unit = "Incidence / 100 000 population at risk",
                        main = paste0("Wave ",i,": Non-medically attended"))
    
  }
  }
  
  
  
  
  
  
  
  
  
  
}