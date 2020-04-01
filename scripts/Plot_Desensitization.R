#script to plot normalized delF values for desensitization - mean and SEM
plot_desensitization <- function(dataset){
  dataset %>%
    group_by(time, genotype) %>%
    summarise(meanDelF = mean(normalized),
              n = n(),
              sem.low = meanDelF - sd(normalized) / n()^0.5, #this is for standard error
              sem.high = meanDelF + sd(normalized) / n()^0.5) %>%
    ggplot(aes(x = time, y = meanDelF)) +
    geom_line(aes(colour = genotype), alpha = 1) + #mean line
    geom_ribbon(aes(ymin = sem.low, ymax = sem.high, fill = genotype), alpha = 0.2) + #labels the SEM with slightly less dimmer shading
    coord_cartesian(xlim = c(20, 45)) +
    coord_cartesian(ylim = c(-.50, 1.2)) +
    geom_segment(aes(x = 30, xend = 40, y = 1.05, yend = 1.05), colour = "black", size = .5) + #this labels when the cue was on
    scale_x_continuous(name="time", limits=c(25, 45)) +
    scale_y_continuous(breaks=c(0, .5, 1)) +
    theme_classic()
}

