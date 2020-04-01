###how to analyze only the second pulse when imaging two pulses (i.e. 1M Glycerol experiments)
pulse2 <- alldata %>%   #alldata incorporates multi genotypes, renamed to pulse2
  unnest() %>%   #unnest the data
  filter(time > 90, time < 181)    #filter for data between 90 - 181 (second pulse)
pulse2 %>%
  ggplot(aes(x = time, y = delF, group = genotype)) +
  stat_summary(geom = "line", fun.y = mean, aes(colour = genotype)) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2, aes(fill = genotype))

baselines <- pulse2 %>%
  filter(time > 100, time < 119) %>%
  group_by(genotype) %>%
  summarize(meanDelf = mean(delF)) #assigning the mean of values 100 - 119 to "meanDelf"

# join  pulse2 and baselines data
corrected_pulse2 <- full_join(pulse2, baselines) %>%
  mutate(correct_delF = delF - meanDelf) #make a new column for del - meanDelf = this substracts the mean F for values 100 - 119 from ALL F-F0/F0 values

corrected_pulse2 %>% #now let's graph it
  ggplot(aes(x = time, y = correct_delF, group = genotype)) +
  stat_summary(geom = "line", fun.y = mean, aes(colour = genotype)) +
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2, aes(fill = genotype)) +
  geom_segment(aes(x =120, xend = 150, y = 1, yend = 1), colour = "black", size = .75) + 
  theme_classic()