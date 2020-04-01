###how to analyze only the second pulse when imaging two pulses (i.e. 1M Glycerol experiments) - creates csv file
corrected_secondpulse <- function(genotype) {
  #first get a directory path to use for saving:
  message("choose your csv data file")  
  filename <- file.choose()
  
  data <- read_csv(filename)
  
  pulse2 <- data %>%   
    unnest() %>%   
    filter(time > 90, time < 181)    #filter for data between 90 - 181 (second pulse)

  baselines <- pulse2 %>%
    filter(time > 100, time < 119) %>%
    group_by(genotype) %>%
    summarize(meanDelf = mean(delF)) #assigning the mean of values 100 - 119 to "meanDelf"
  
  # join  pulse2 and baselines data
  corrected_pulse2 <- full_join(pulse2, baselines) %>%
    mutate(correct_delF = delF - meanDelf) #make a new column for del - meanDelf = this substracts the mean F for values 100 - 119 from ALL F-F0/F0 values

  write_csv(corrected_pulse2, file.path(dirname(filename), 
                                  glue::glue({genotype}, "_correctedpulse2.csv")))
  print(file.path(dirname(filename), glue::glue({genotype}, "_correctedpulse2.csv")))
}
