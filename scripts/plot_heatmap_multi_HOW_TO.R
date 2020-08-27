#how to use plot_heatmap_multi to generate heatmap across multiple days of imaging

#first, open all of your raw .csv files and assign them names. here is an example:

wt_day1 <- read_csv(file.choose()) #open plotGCaMP_multi generated .csv file #1 
wt_day2 <- read_csv(file.choose()) #open plotGCaMP_multi generated .csv file #2 
wt_day3 <- read_csv(file.choose()) #open plotGCaMP_multi generated .csv file #3 

all_wt_data <- rbind(wt_day1, wt_day2, wt_day3) #combine this data altogether

#then - set your working directory to the folder you want this combined .csv file (combining data across all of your days) to appear in
  ##Session > Set Working Directory > Choose Directory

#you will then generate a .csv file combined the three datasets. file name is in ".csv" 

write_csv(all_wt_data, "all_wt_data.csv")

#now you're ready to go! use the plot_heatmap_multi function after running the entire function in R to save it to your environment
#open your combined .csv file when prompted

plot_heatmap_multi(heatmap_limits = c(-.1,0,0.5),
             endPulse = 29.5)

#done!