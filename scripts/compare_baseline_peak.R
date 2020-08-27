#calculate max delF 0-60s
#calculate baseline_meanGCaMP 0s-25s
##this generates csv with baseline_meanGCaMP calculated from mean_GCaMP values from 0s to 25s (before odor stimulus) and max(delF) during 60 sec stimulus 

compare_baseline_peak <- function(genotype){
  message("choose your .csv data file") #open plotGCaMP_multi file
  filename <- file.choose()
  
  data <- read_csv(filename)
  
  peak_pulse <- data %>%
    group_by(genotype, animal, animal_num) %>%
    filter(time <= 60) %>% 
    select(-signal, -MeanBackground, -fitted) %>% #get rid of some useless columns
    mutate(peak_GCaMP = max(delF)) #max delF in 60sec duration
  
  baseline_peak <- peak_pulse %>%
    group_by(genotype, animal, animal_num) %>%
    filter(time <= 25) %>% 
    mutate(baseline_meanGCaMP = mean(MeanGCaMP)) %>% #baseline mean in 0-25sec
    filter(time < 0.5) #removes excess timepoints
  
  write_csv(baseline_peak, file.path(dirname(filename), 
                                        glue::glue({genotype}, "_compare_baseline_peak.csv")))
  print(file.path(dirname(filename), glue::glue({genotype}, "_compare_baseline_peak.csv")))
  
}

compare_baseline_peak(genotype = "wt") #open PlotCaMP_multi csv file
compare_baseline_peak(genotype = "mutant") #open PlotGCaMP_multi csv file

plotgcamp_wt <- read_csv(file.choose()) #open compare_baseline_peak csv file
plotgcamp_mutant <- read_csv(file.choose()) #open compare_baseline_peak csv file

plotgcamp_all <- rbind(plotgcamp_wt, plotgcamp_mutant)

plotgcamp_all %>%
  group_by(genotype) %>%
  ggplot(aes(x = baseline_meanGCaMP, y = peak_GCaMP, color = genotype)) + 
  geom_point() + 
  theme_classic() 


