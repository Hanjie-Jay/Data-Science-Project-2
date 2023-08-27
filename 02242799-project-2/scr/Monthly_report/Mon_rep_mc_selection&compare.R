# This is the script used to filter the earthquake data that only included last 12 month

#==================================================================
# For reproduce a different month report date, only need to change the global_variables.R file
#==================================================================

library(tidyverse)
library(lubridate)

here::i_am("scr/Monthly_report/Mon_rep_mc_selection&compare.R")

library(here)
library(ggmap)

# Read the global variables into the environment
source(here::here("scr","global_variables.R"))
source(here::here("scr","Monthly_report","Mon_rep_data_preprocess.R"))

# Load the filtered dataset
filter_data_name <- paste0("last_12_months_", file_name)
earthquake_tbl_filtered <- readr::read_csv(here::here("data","derived", filter_data_name))


#==================================================================
#==================================================================
# Select the m_c value via maximum curvature
#==================================================================

# Calculate the magnitude frequency distribution
mag_freq_dist <- earthquake_tbl_filtered %>%
  group_by(mag) %>%
  summarize(mag_count = n()) %>%
  arrange(mag)

# Calculate the curvature for each magnitude
mag_curvature <- mag_freq_dist  %>%
  mutate(cum_count = cumsum(mag_count)) %>%
  mutate(curv = c(0, diff(cum_count, differences = 2), 0))

# Allocate the magnitude value m_c that maximise the curvature
mc <- mag_curvature[which.max(abs(mag_curvature$curv)), "mag"] %>%
  pull()

# Plot the histogram of the magnitudes
end_date <- ymd(gsub("_induced-earthquakes.csv", "", file_name))
start_date <- end_date %m-% months(12)
mag_hist_title <- paste0("Earthquake Magnitude Histogram of last 12 months (From ",
                         start_date," to ",end_date,")")
magnitude_histogram <- ggplot(earthquake_tbl_filtered, aes(x = mag)) +
  geom_histogram(binwidth = 0.05) +
  geom_vline(xintercept = mc, linetype = "dashed", color = "red") +
  labs(title = mag_hist_title,
       x = "Magnitude",
       y = "Frequency",
      caption = paste("Red dashed line indicates m_c =", mc)) +
  theme(plot.title = element_text(size = 7),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size = 5))
ggsave(here::here('outputs','mag_hist.png'), plot=magnitude_histogram, device='png',
       height=4, width=8, units='in', dpi=650)

# Calculate the proportion that the magnitute is above the mc value we use
earthquake_filtered_above_mc <- earthquake_tbl_filtered %>% 
  filter(mag > mc)
above_mc_proportion <- nrow(earthquake_filtered_above_mc) / nrow(earthquake_tbl_filtered)
cat("The proportion of the earthquake that magnitude above mc is:", above_mc_proportion)


#==================================================================
# Verify our m_c allocation via comparing to the cdf of exponential distribution
#==================================================================

# Create dataset that above mc
mg_filtered_above_mc <- earthquake_filtered_above_mc %>%
  pull(mag)

# Calculate the empirical CDF of the observed magnitudes that above mc
observed_ecdf <- ecdf(mg_filtered_above_mc)

# Compute the exponential distribution parameter β and the expected CDF
beta <- mean(mg_filtered_above_mc - mc) # Use MLE formula of β
expected_cdf <- function(x) {
  pexp(x - mc, rate = 1 / beta) # subtract mc here to suit the beta
}

# Perform the Kolmogorov-Smirnov test to see the goodness-of-fit
ks_test <- ks.test(mg_filtered_above_mc, expected_cdf)
print(round(ks_test$p.value, 4))
ks_test_p_value <- round(ks_test$p.value, 4)

# Plot the observed ECDF and the expected CDF in single graph to compare
com_cdf_title <- "Comparing the observed ECDF and the expected CDF with our selected mc"
com_cdf_plot <- ggplot(data.frame(mag = mg_filtered_above_mc), aes(x = mag)) +
  stat_ecdf(aes(y = ..y.., color = "Observed")) +
  stat_function(fun = expected_cdf, aes(color = "Expected")) +
  labs(title = com_cdf_title,
       x = "Magnitude", 
       y = "Cumulative Probability", 
       color = "Distribution") +
  theme(plot.title = element_text(size = 7),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5),
        plot.caption = element_text(size = 4))
ggsave(here::here('outputs','com_cdf_plot.png'), plot=com_cdf_plot, device='png',
       height=5, width=7, units='in', dpi=650)



#==================================================================
#==================================================================
# Compare the earthquake activity in last month to last 11 months
#==================================================================

# Prepare the data
earthquake_last_month <- earthquake_tbl_filtered %>% 
  filter(date >= end_date %m-% months(1))
earthquake_previous_11_months <- earthquake_tbl_filtered %>% 
  filter(date < end_date %m-% months(1))

#==========number of earthquakes==========
# Calculate the total number of earthquakes in each period and visualise via bar chart
count_earthquake_last_month <- nrow(earthquake_last_month)
avg_count_earthquake_previous_11_months <- nrow(earthquake_previous_11_months)/11

# Visualise the earthquake counts using bar charts
Last_Month <- paste0(end_date %m-% months(1), ' to ', end_date)
Previous_11_Months <-  paste0('Average from ', start_date , ' to ', end_date %m-% months(1))
com_count_title <- paste0("Earthquake Counts Comparison (From ",start_date," to ",end_date,")")
earthquake_counts <- data.frame(period = c(Last_Month, Previous_11_Months),
                     earthquake_count = c(count_earthquake_last_month, 
                                          avg_count_earthquake_previous_11_months))
com_count_bar <- ggplot(earthquake_counts, aes(x = period, y = earthquake_count)) +
  geom_bar(stat = "identity") +
  labs(title = com_count_title,
       x = "Period",
       y = "Count") +
  theme(plot.title = element_text(size = 6),
        axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 4),
        plot.caption = element_text(size = 4))
ggsave(here::here('outputs','com_count_bar.png'), plot=com_count_bar, device='png',
       height=5, width=7, units='in', dpi=650)


#==========earthquake locations==========
# Visualise the earthquake locations using scatterplots
Last_Month_p <- paste0('From ', end_date %m-% months(1), ' to ', end_date)
Previous_11_Months_p <- paste0('From ', start_date , ' to ', end_date %m-% months(1))
com_loc_title <- paste0("Earthquake Locations Comparison (From ",start_date," to ",end_date,")")
bbox <- make_bbox(earthquake_tbl_filtered$lon, earthquake_tbl_filtered$lat, 
                  f = c(1.25, 0.25)) # Define bounding box
mymap_stamen <- get_stamenmap(bbox, zoom = 10, maptype = "terrain") # Fetch the map
com_loc_plot <- ggmap(mymap_stamen) +
  geom_point(data = earthquake_tbl_filtered, 
             aes(x = lon, y = lat, color = factor(if_else(date >= end_date %m-% months(1), 
                                                          Last_Month_p, Previous_11_Months_p)))) +
  labs(title = com_loc_title,
       x = "Longitude",
       y = "Latitude",
       color = "Period") +
  theme(plot.title = element_text(size = 7),
        axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5),
        plot.caption = element_text(size = 4),
        legend.position = "bottom")
ggsave(here::here('outputs','com_loc_plot.png'), plot=com_loc_plot, device='png',
       height=5, width=7, units='in', dpi=650)


#==========earthquake magnitudes==========
# Calculate earthquake magnitudes in each period and visualise using box plots
mag_summary_last_month <- summary(earthquake_last_month$mag)
mag_summary_previous_11_months <- summary(earthquake_previous_11_months$mag)
print(mag_summary_last_month)
print(mag_summary_previous_11_months)

# Transpose the summary statistics and create a matrix
mag_summary_matrix <- t(matrix(c(mag_summary_last_month, mag_summary_previous_11_months), ncol = 2))

# Create a data frame with the summary statistics
mag_summary_table <- t(data.frame(
  Statistic = names(mag_summary_last_month),
  `Last_Month` = round(mag_summary_matrix[1, ], 4),
  `Previous_11_Months` = round(mag_summary_matrix[2,], 4)
))

# Visualise the distribution of magnitudes using box plots for each period
com_mag_title <- paste0("Earthquake Magnitude Comparison (From ",start_date," to ",end_date,")")
com_mag_box <- ggplot(earthquake_tbl_filtered, 
                      aes(x = factor(if_else(date >= end_date  %m-% months(1), 
                                                                      Last_Month_p, 
                                                                      Previous_11_Months_p)), 
                          y = mag)) +
  geom_boxplot() +
  labs(title = com_mag_title,
       x = "Period",
       y = "Magnitude") +
  theme(plot.title = element_text(size = 6),
        axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5),
        plot.caption = element_text(size = 5))
ggsave(here::here('outputs','com_mag_box.png'), plot=com_mag_box, device='png',
       height=5, width=7, units='in', dpi=650)
