##script to plot mean and SEM of GCaMP responses 
plot_sem <- function(dataset) {
  dataset %>%
    unnest(cols = c()) %>%
    ggplot(aes(x = time, y = delF)) +
    stat_summary(geom = "line", fun.y = mean) +
    stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.2) +
    aes(group = genotype, color = genotype) +
    theme_classic()
}


