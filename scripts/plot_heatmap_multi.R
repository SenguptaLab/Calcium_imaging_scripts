#a function to make new adjusted heatmaps- across MULTIPLE DAYS of imaging (where animal numbers may be the same/overlapping)
#open raw .csv PlotCaMP_multi file and generate a new heat map with different parameters
plot_heatmap_multi <- function(data,
                               heatmap_limits = "auto", 
                               endPulse = 59.5,
                               use_existing = FALSE,
                               ...) {
  library(tidyverse)
  library(magrittr)
  library(scales)
  
  if(missing(data)) {  
    data <- read_csv(file.choose()) %>%
      mutate(animal_num = as.factor(animal_num))
  } else {
    data <- data
  }
  #full_join(data, plot_order) %>%
  #unnest(cols=c()) %>%
  
  if(!is.numeric(heatmap_limits)) { # using auto calc unless a numeric vector input
    breaks <- round(
      data %>% unnest(cols = c()) %$% quantile(delF, c(0.05, 0.5, 0.99)),
      2
    )
    labels <- as.character(breaks)
    limits <- breaks[c(1,3)]
  } else {
    breaks <- heatmap_limits
    labels <- as.character(breaks)
    limits <- breaks[c(1,3)]
  }
  
  labels <- as.character(breaks)
  limits <- breaks[c(1,3)]
  
  plot_order <- data %>% 
    group_by(animal) %>%
    summarise(maxD = MF.matR::max_delta(delF, end = endPulse)) %>%
    arrange(maxD)
  
  full_join(data, plot_order, cols = c("animal", "maxD")) %>% 
    filter(time < 60.25) %>%
    ggplot(aes(x = time, y = fct_reorder(factor(animal), maxD))) +
    geom_tile(aes(fill = signal)) +
    scale_fill_viridis_c(option = "magma",
                         breaks = breaks,
                         labels = labels,
                         limits = limits,
                         oob =squish) +
    theme_classic() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.text.y = element_blank()) +
    labs(y = "Animal number")
}
