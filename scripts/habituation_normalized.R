##to analyze habituation across multiple pulses, normalized to the largest response 
#open habituation.csv file
habituation_normalized <- function(genotype){
  message("choose your habituation data file")
  filename <- file.choose()
  
  data <- read_csv(filename)
  
  data2 <- data %>%
    group_by(animal_num) %>%
    summarize(pulsepeak_delF = max(peak_delF))
  
  data3 <- dplyr::full_join(data, data2) %>% mutate(normalized_delF = peak_delF/pulsepeak_delF)
  
  write_csv(data3, file.path(dirname(filename), 
                                        glue::glue({genotype}, "_normalizedhabituation.csv")))
  print(file.path(dirname(filename), glue::glue({genotype}, "_normalizedhabituation.csv")))
  
}

habituation_normalized("wildtype")
