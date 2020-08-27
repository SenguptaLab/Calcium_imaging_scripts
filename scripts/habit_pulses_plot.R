#a function to plot delta F / F for each pulse overlaid 
#open raw .csv PlotCaMP_multi file
habit_pulses_plot <- function(genotype, pulse_dur = 60, write_data = TRUE, n_pulses = 6) {
  
  library(tidyverse)
  library(magrittr)
  theme_set(theme_classic())
  filename <- file.choose()
  dataset <- read_csv(filename)
  # Choose your dataset (*.csv)
  
  dataset %<>%
    mutate(pulse_num = ceiling(time/pulse_dur),
           pulse_time = time - (pulse_num - 1)*pulse_dur)
  
  dataset <- dataset %>%
    ggplot(aes(x = pulse_time, y = delF)) +
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
  
  print(dataset)
  
}


