# This is the script used to preform simulation study to assess which 
# approaches is the best in terms of computation time

library(tidyverse)
library(microbenchmark)
library(profvis)

here::i_am("scr/Technical_guidance_on_MCMC/simulation_study_with_visualisation.R")

library(here)

#==================================================================
#==================================================================
# Define the four different pre-allocation approaches as functions
#==================================================================

## data frame row-wise
data_frame_row_wise <- function(m, p) {
  df <- data.frame(matrix(NA, nrow=m, ncol=p))
  for (i in 1:m) {
    df[i,] <- 1:p
  }
}

## data frame column-wise
data_frame_column_wise <- function(m, p) {
  df <- data.frame(matrix(NA, nrow=p, ncol=m))
  for (i in 1:m) {
    df[,i] <- 1:p
  }
  df <- t(df)
  df <- as.data.frame(df)
}

## matrix row-wise
matrix_row_wise <- function(m, p) {
  matrixs <- matrix(NA, nrow=m, ncol=p)
  for (i in 1:m) {
    matrixs[i,] <- 1:p
  }
  df <- as.data.frame(matrixs)
}

## matrix column-wise
matrix_column_wise <- function(m, p) {
  matrixs <- matrix(NA, nrow=p, ncol=m)
  for (i in 1:m) {
      matrixs[,i] <- 1:p
  }
  matrixs <- t(matrixs)
  df <- as.data.frame(matrixs)
}



#==================================================================
#==================================================================
# PRIMARY ANALYSIS USING SMALL VALUE OF m
#==================================================================

#==================================================================
# Measure computation time for each approach and plot the graph
#==================================================================

# Set parameters for the simulation study 1 to select a range of method
# For REPRODUCTION, change the parameter here
m_val <-  1e4
p_val <-  10
iter_num <- 10L
unit <- "ms"

# Measure computation time for each approach using the microbenchmark library
simu_results <- microbenchmark(
  data_frame_row_wise(m_val, p_val),
  data_frame_column_wise(m_val, p_val),
  matrix_row_wise(m_val, p_val),
  matrix_column_wise(m_val, p_val),
  times = iter_num,
  unit = unit
)

# Store the summary table for computation time of each approach
summary_simu_results <- summary(simu_results)
summary_simu_results_df <- as.data.frame(summary_simu_results)
summary_simu_results_df$Method <- c("data_frame_row_wise",
                                   "data_frame_column_wise",
                                   "matrix_row_wise",
                                   "matrix_column_wise")
summary_simu_results_df <- summary_simu_results_df[, c(ncol(summary_simu_results_df), 
                                                       2:(ncol(summary_simu_results_df)-1))]
summary_simu_results_df <- summary_simu_results_df %>%
  mutate(across(min:max, round, 4))

# Create new data frame with only Method and mean columns and reshape the data to long format
mean_median_results_df <- summary_simu_results_df[, c("Method", "mean", "median")]
mean_median_long <- mean_median_results_df %>%
  gather(key = "Statistic", value = "Value", -Method)

# Create the bar plot for the mean and median statistics
mmp_title <- paste0("Mean and Median Computation Time by Methods (m=",m_val,' ,p=',p_val, ')')
mmp_y_lab <- paste0("Mean time (", unit,")")
mean_median_plot <- ggplot(mean_median_long, aes(x = Method, y = Value, fill = Statistic))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = mmp_title,
       x = "Method",
       y = mmp_y_lab)+
  facet_wrap(~Method,scales="free")+
  theme(plot.title = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 4),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size = 5),
        strip.text = element_text(size = 6),
        strip.background = element_rect(fill = "lightgray", size = 1.25),
        legend.position = "top")
ggsave(here::here('outputs','mean_median_plot.png'), plot=mean_median_plot, 
       device='png',height=4, width=8, units='in', dpi=650)

# Display the best two methods
sorted_mean_median_results_df <- mean_median_results_df[order(mean_median_results_df$mean),]
best_two_methods <- sorted_mean_median_results_df[1:2,]
cat("The best two efficient methods are:", best_two_methods$Method, "\n")



#==================================================================
# Find the composition of computation time of each part of code using the profvis library
#==================================================================

# Dataframe row-wise
profvis::profvis(
  {
    m <-  1e4
    p <- 10
    df <- data.frame(matrix(NA, nrow=m, ncol=p))
    for (i in 1:m) {
      df[i,] <- 1:p
    }
  }, prof_output=here::here("outputs","profvis_data_frame_row_wise.csv") # store the output
)
profvis_data_frame_row_wise <- summaryRprof(here::here("outputs","profvis_data_frame_row_wise.csv"))$by.total

# Dataframe Column-wise
profvis::profvis(
  {
    m <-  1e4
    p <- 10
    df <- data.frame(matrix(NA, nrow=p, ncol=m))
    for (i in 1:m) {
      df[,i] <- 1:p
    }
    df <- t(df)
    df <- as.data.frame(df)
  }, prof_output=here::here("outputs","profvis_data_frame_column_wise.csv") # store the output
)
profvis_data_frame_column_wise <- summaryRprof(here::here("outputs","profvis_data_frame_column_wise.csv"))$by.total

# Matrix row-wise
profvis::profvis(
  {
    m <-  1e5
    p <- 10
    matrixs <- matrix(NA, nrow=m, ncol=p)
    for (i in 1:m) {
      matrixs[i,] <- 1:p
    }
    df <- as.data.frame(matrixs)
  }, prof_output=here::here("outputs","profvis_matrix_row_wise.csv") # store the output
)
profvis_matrix_row_wise <- summaryRprof(here::here("outputs","profvis_matrix_row_wise.csv"))$by.total

# Matrix column-wise
profvis::profvis(
  {
    m <-  1e5
    p <- 10
    matrixs <- matrix(NA, nrow=p, ncol=m)
    for (i in 1:m) {
      matrixs[,i] <- 1:p
    }
    matrixs <- t(matrixs)
    df <- as.data.frame(matrixs)
  }, prof_output=here::here("outputs","profvis_matrix_column_wise.csv") # store the output
)
profvis_matrix_column_wise <- summaryRprof(here::here("outputs","profvis_matrix_column_wise.csv"))$by.total



#==================================================================
#==================================================================
# FURTHER ANALYSIS FOR DIFFERENT VALUE OF m
#==================================================================

#==================================================================
# Use the best two methods evaluate the training time of a different values of m and p
#==================================================================

# Set parameters for the simulation study; For reproduction, change the parameter here
m_val_2 <-  1e8
p_val_2 <-  10
iter_num_2 <- 3L
unit_2 <- "s"

# Measure computation time for each approach using the microbenchmark library
simu_results_2 <- microbenchmark(
  matrix_row_wise(m_val_2, p_val_2),
  matrix_column_wise(m_val_2, p_val_2),
  times = iter_num_2,
  unit = unit_2
)

# Store the summary table for computation time of each approach
summary_simu_results_2 <- summary(simu_results_2)
summary_simu_results_df_2 <- as.data.frame(summary_simu_results_2)
summary_simu_results_df_2$Method <- c("matrix_row_wise",
                                       "matrix_column_wise")
summary_simu_results_df_2 <- summary_simu_results_df_2[, c(ncol(summary_simu_results_df_2), 
                                                           2:(ncol(summary_simu_results_df_2)-1))]

# Create new data frame with only Method and mean columns and reshape the data to long format
mean_median_results_df_2 <- summary_simu_results_df_2[, c("Method", "mean", "median")]
mean_median_long_2 <- mean_median_results_df_2 %>%
  gather(key = "Statistic", value = "Value", -Method)

# Create the bar plot for the mean and median statistics
mmp_title_2 <- paste0("Mean and Median Computation Time by Methods (m=",m_val_2,' ,p=',p_val_2, ')')
mmp_y_lab_2 <- paste0("Mean time (", unit_2,")")
mean_median_plot_2 <- ggplot(mean_median_long_2, aes(x = Method, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = mmp_title_2,
       x = "Method",
       y = mmp_y_lab_2) +
  theme(plot.title = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        plot.caption = element_text(size = 5),
        legend.position = "top")
ggsave(here::here('outputs','mean_median_plot_2.png'), plot=mean_median_plot_2, 
       device='png',height=4, width=8, units='in', dpi=650)


#==================================================================
#==================================================================
# Based on the results, recommend the most efficient approach for the team
#==================================================================

best_approach <- mean_median_results_df_2[which.min(mean_median_results_df_2$mean), "Method"]
cat("The most efficient approach is:", best_approach, "\n")
