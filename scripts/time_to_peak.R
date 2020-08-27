#time to peak delF response for first pulse
#open PlotGCaMP_multi csv file
time_to_peak <- function(genotype, #mandatory genotype, should be text
                                 pulse_length = 60, # time of pulse-loop in seconds 
                                 stim_length = 10, # time of stimulus in seconds
                                 pre_pulse = 30) # time before each pulse in seconds 
  {
  
  library(tidyverse)
  library(magrittr)
  
  filename <- file.choose()
  data <- read_csv(filename)
  
  ###### read in data and divide by pulse length #####
  data <- data %>%
    mutate(pulse_num = ceiling(time/pulse_length),
           pulse_time = time - (pulse_num - 1)*pulse_length)
  
  ###### Get the time of the max delF after pulse initiation, but before #####
  ######      removal
  
  peak_rows <-data %>%
    group_by(genotype, animal, animal_num, pulse_num) %>%
    filter(pulse_num < 2) %>%
    filter(pulse_time > (pre_pulse-2) & pulse_time < (pre_pulse + stim_length - 0.5)) %>%
    select(-signal, -MeanGCaMP, -MeanBackground, -fitted) %>% #get rid of some useless columns
    mutate(row_number = row_number()) %>% # first add a row_number column to use as index
    mutate(peak = which.max(delF)) %>% # gives the index (row number) of max value
    filter(row_number == peak)
  
  write_csv(peak_rows, file.path(dirname(filename), 
                                        glue::glue({genotype}, "_time_to_peak.csv")))
  print(file.path(dirname(filename), glue::glue({genotype}, "_time_to_peak.csv")))
  
}

