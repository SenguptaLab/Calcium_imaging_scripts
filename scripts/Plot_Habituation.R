##plotting normalized habituation curves- mean and standard error of max pulse peak for each pulse:
plot_habituation <- function(data){
  data %>%
    group_by(pulse_num, genotype) %>%
    summarise(mean_normalized_delF = mean(normalized_delF),
              n = n(),
              sem.low = mean_normalized_delF - sd(normalized_delF) / n()^0.5, #this is for standard error
              sem.high = mean_normalized_delF + sd(normalized_delF) / n()^0.5) %>%
    ggplot(aes(x = pulse_num, y = mean_normalized_delF)) +
    geom_line(aes(colour = genotype), alpha = 1) + #mean line
    geom_ribbon(aes(ymin = sem.low, ymax = sem.high, fill = genotype), alpha = 0.2) +
    coord_cartesian(ylim = c(0, 1)) +
    coord_cartesian(xlim = c(1, 10)) +
    scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) + #for 10 pulses
    theme_classic()
}

