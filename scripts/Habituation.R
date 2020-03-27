##to analyze habituation across multiple pulses - gives peak delF per pulse
habituation <- function(genotype, end = 30, ...) {
  #first get a directory path to use for saving:
  message("choose your habituation data file")
  filename <- file.choose()
  
  data <- read_csv(filename)
  
  data <- data %>%
    mutate(pulse_num = ceiling(time/60),
           pulse_time = time - (pulse_num - 1)*60)
  
  peakPulses <- data %>%
    group_by(genotype, animal, animal_num, pulse_num) %>%
    summarize(peak_delF = max_delta(delF, end = end)) # max of 20 frames after 'end' of the pre-pulse window, minus mean of 10 frames prior
  
  p <- peakPulses %>% ggplot(aes(x = pulse_num, y = peak_delF)) + geom_point()
  print(p)
  #write_csv(peakPulses, glue::glue(dirname(filename), "/", {genotype}, "_habituation.csv"))
  write_csv(peakPulses, file.path(dirname(filename), 
                                  glue::glue({genotype}, "_habituation.csv")))
  print(file.path(dirname(filename), glue::glue({genotype}, "_habituation.csv")))
}