#script to determine max peak delF value(max value throughout first 60s), and normalize delF values to this max value 
#used to plot desensitization within a 60s experiment
#open .csv file provided by PlotGCaMP_multi
desensitization_normalized <- function(genotype){
  message("choose your .csv data file")
  filename <- file.choose()
  
  data <- read_csv(filename)

  filtered60s <- data %>% unnest %>% filter(time < 60.25)
  
  desensnormalized <- filtered60s %>%
    group_by(animal_num) %>%
    mutate(normalized = delF/max(delF))
  
  write_csv(desensnormalized, file.path(dirname(filename), 
                             glue::glue({genotype}, "_desensnormalized.csv")))
  print(file.path(dirname(filename), glue::glue({genotype}, "_desensnormalized.csv")))
  
}
