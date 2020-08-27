#a function to plot delta F / F for each pulse normalized to each baseline prior to the pulse overlaid
#open raw .csv PlotCaMP_multi file
#generates a .csv file and a plot 
habit_baseline_pulses <- function(genotype, pulse_dur = 60, write_data = TRUE, n_pulses = 6) {
  
  library(tidyverse)
  library(magrittr)
  theme_set(theme_classic())
  filename <- file.choose()
  dataset <- read_csv(filename)
  # Choose your dataset (*.csv)
  
  dataset %<>%
    mutate(pulse_num = ceiling(time/pulse_dur),
           pulse_time = time - (pulse_num - 1)*pulse_dur)
  
  ###### baseline correct pre-pulse #####
  dataset <- dataset %>%
    filter(pulse_time > 25 & pulse_time < 29) %>% # get just pre-pulse values
    group_by(genotype, pulse_num, animal) %>% # group by pulse (and potentially other things to add here)
    summarise(baseline = mean(delF)) %>%
    full_join(., dataset) %>%
    mutate(corrected_delF = delF - baseline) %>%
    filter(pulse_num <= n_pulses)
  
  plot <- dataset %>%
    ggplot(aes(x = pulse_time, y = corrected_delF)) +
    annotate("rect", xmin = 30, xmax = 40, ymin = -.05, ymax = 0.6,
             alpha = .05, colour = "grey") +
    stat_summary(geom = "ribbon", fun.data = "mean_se",
                 alpha = 0.1, aes(group = pulse_num, fill = pulse_num)) +
    stat_summary(aes(group = pulse_num, colour = pulse_num), geom = "line", fun.y = "mean") +
    coord_cartesian(xlim = c(25,45)) +
    viridis::scale_color_viridis() +
    viridis::scale_fill_viridis() +
    labs(x = "time (s)", y = paste0("Delta F / F0")) +
    guides(colour = guide_legend("Pulse number"),
           fill = guide_legend("Pulse number"))
  
  print(plot)
  
  if(write_data) {
    write_csv(dataset, file.path(dirname(filename),
                                 glue::glue({genotype}, "_habit_baseline_pulses.csv")))
    message("Filtered results written to:")
    print(file.path(dirname(filename), glue::glue({genotype}, "_habit_baseline_pulses.csv")))
  }
  
}
