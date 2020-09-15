##name/combine your data analyzed by PlotGCaMP_multi
alldata <- rbind(genotype1$data, genotype2$data)

unnested <- unnest(alldata)

##to plot just the second pulse! For a 30-30-30sec x 2 (180sec) experiment, with two 30 second pulses
##Specifically made to analyze ASH second pulse, as the first pulse is often noisy
##need to center on pulse for the SECOND pulse (see script below)

pulse2 <- alldata %>%
  unnest() %>%
  filter(time > 90, time < 181) 
pulse2 %>%
  ggplot(aes(x = time, y = delF, group = genotype)) +
  stat_summary(geom = "line", fun.y = mean, aes(colour = genotype)) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2, aes(fill = genotype))

baselines <- pulse2 %>%
  filter(time > 100, time < 119) %>%
  group_by(genotype) %>%
  summarize(meanDelf = mean(delF))
  
# join  pulse2 and baselines
corrected_pulse2 <- full_join(pulse2, baselines) %>%
  mutate(correct_delF = delF - meanDelf)

corrected_pulse2 %>%
  ggplot(aes(x = time, y = correct_delF, group = genotype)) +
  stat_summary(geom = "line", fun.y = mean, aes(colour = genotype)) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2, aes(fill = genotype)) +
  geom_segment(aes(x =120, xend = 150, y = .65, yend = .65), colour = "black", size = .75) + 
  theme_classic()

#### set wd first! - but should use relative paths!!!!
write_csv(corrected_pulse2, "secondpulse_correctedfitvalues.csv")
