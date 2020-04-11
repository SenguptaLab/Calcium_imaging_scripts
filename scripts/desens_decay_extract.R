#script to extract values for each pulse and copy to csv file: max > 40 seconds
#also determines t1/2 for each calcium trace
#open plotGCaMP_multi csv raw file
#on plots - dashed lines = fit
desens_decay_extract <- function(genotype, #mandatory genotype, should be text
                                 pulse_length = 60, # time of pulse in seconds
                                 n_pulses = 5) {

library(tidyverse)
library(magrittr)
pulse_length <- 60
n_pulses <- 5
  
filename <- file.choose()
data <- read_csv(filename)

###### read in data and divide by pulse length #####
data <- data %>%
  mutate(pulse_num = ceiling(time/pulse_length),
         pulse_time = time - (pulse_num - 1)*pulse_length)

###### Get the time of the max delF after pulse initiation, but before #####
######      removal
postPulse_data <- data %>%
  group_by(genotype, animal, animal_num, pulse_num) %>%
  filter(pulse_time > 28 & pulse_time < 40.25) %>%
  select(-signal, -MeanGCaMP, -MeanBackground, -fitted) %>% #get rid of some useless columns
  mutate(row_number = row_number()) # first add a row_number column to use as index
  
peak_rows <-data %>%
  group_by(genotype, animal, animal_num, pulse_num) %>%
  filter(pulse_time > 28 & pulse_time < 40.25) %>%
  summarize(peak = which.max(delF)) # gives the index (row number) of max value
  
##### now filter each pulse to take only times > than peak #####
decay_data <- full_join(postPulse_data, peak_rows) %>%
  filter(row_number > peak) %>%
  group_by(genotype, animal, animal_num, pulse_num) %>%
  mutate(new_row_number = row_number(),
         decay_time = new_row_number*0.25) 

#### plot result for pulse
decay_data %>%
  filter(pulse_num < (n_pulses + 1)) %>%
  ggplot(aes(x = decay_time, y = delF)) +
  geom_line(aes(group = animal, colour = animal_num)) + 
  facet_wrap(~pulse_num) +
  guides(color = guide_legend("Pulse number"))

# in this model k = exp(lrc) and y(t) = c + y0*exp(-k*t)
# so ln(delF0 / .5 * delF0) = k * t(1/2), so ln(2)/k = t(1/2)
# final function is ln(2)/exp(lrc) = t(1/2)

half_time_getter <- function(fit) {
    lrc <- fit %>% broom::tidy() %>%
    filter(term == "lrc") %>%
    select(estimate) %>%
    as.numeric()
    half_time <- log(2)/exp(lrc)
    return(half_time)
}

p.val_getter <- function(fit) {
  p.value <- fit %>% broom::tidy() %>%
    filter(term == "lrc") %>%
    select(p.value) %>%
    as.numeric()
  return(p.value)
}

#### fit nls to the data ####
decay_data_fit <- decay_data %>%
  filter(pulse_num < n_pulses + 1) %>%
  group_by(animal, animal_num, pulse_num) %>%
  nest() %>%
  mutate(fit = map(data, ~try(nls(delF ~ SSasymp(decay_time, Asym, R0, lrc),
                              data = .x))))

#### Get half-time, p.value and predictions from fits ####
decay_data_fit %<>%
  mutate(fit_result = 
           case_when(
             class(unlist(fit)) == "character" ~ "error",
             TRUE ~ "fit"))

fit_results <- decay_data_fit %>% 
  select(animal, animal_num, pulse_num, fit_result)

decay_data_fit %<>%
  filter(fit_result == "fit") %>%
  mutate(half_time = map(fit, half_time_getter) %>% unlist(),
         p.value = map(fit, p.val_getter) %>% unlist(),
         predictions = map(fit, function(x) {predict(x)}))

plot <- decay_data_fit %>%
  unnest(cols = c(data, predictions)) %>%
  ggplot(aes(x = decay_time, y = delF)) +
  geom_line(aes(group = animal, colour = animal_num)) + 
  geom_line(aes(group = animal, 
                colour = animal_num, y = predictions), 
            lty = 2) +
  facet_wrap(~pulse_num) +
  guides(color = guide_legend("Pulse number"))
print(plot)

fit_results <- full_join(decay_data_fit, fit_results) %>%
  select(animal, animal_num, pulse_num, fit_result, half_time, p.value)

write_csv(decay_data, file.path(dirname(filename), glue::glue({genotype},"_decay_data.csv")))

write_csv(fit_results, 
          file.path(dirname(filename),
                    glue::glue({genotype},
                               "_desens_decay_extract.csv")))

ggsave(plot, filename = file.path(dirname(filename),
                                  glue::glue({genotype},
                                             "_desens_decay_plots.png")),
       width = 11, height = 8.5, units = "in")

message("results written to:")
print(file.path(dirname(filename), 
                glue::glue({genotype}, 
                           "_desens_decay_extract.csv")))

}

desens_decay_extract(genotype = "grk1")
