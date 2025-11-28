# load packages

library(tidyverse)
library(MASS)
library(Hmisc)
library(gridExtra)
library(kableExtra)

# pre data for visualised conditions

set.seed(1234)

my_rs <- seq(0.2, 0.99, length.out = 45)

plot_no <- 1:45

correlations <- tibble(plot_no, my_rs)

# define function for creating slopes for alt condition

slope_function <- function(my_desired_r) {
  set.seed(1234)
  
  my_sample_size = 128
  
  mean_variable_1 = 0
  sd_variable_1 = 1
  
  mean_variable_2 = 0
  sd_variable_2 = 1
  
  mu <- c(mean_variable_1, mean_variable_2) 
  
  myr <- my_desired_r * sqrt(sd_variable_1) * sqrt(sd_variable_2)
  
  mysigma <- matrix(c(sd_variable_1, myr, myr, sd_variable_2), 2, 2) 
  
  corr_data = as_tibble(mvrnorm(my_sample_size, mu, mysigma, empirical = TRUE))
  
  corr_model <- lm(V2 ~ V1, data = corr_data)
  
  my_residuals <- abs(residuals(corr_model))
  
  data_with_resid <- round(cbind(corr_data, my_residuals), 2)
  
  slopes <- data_with_resid %>%
    mutate(slope_inverted = (1 + (0.25)^ my_residuals)-1) %>%
    mutate(slope_inverted_floored = pmax(0.2,(1+(0.25)^my_residuals)-1)) %>%
    mutate(typical = 0.033) %>%
    mutate(standard_alpha = 1)
  
  return(slopes)
}

# define plotting function for alt and standard scatter conditions

plot_function <- function(slopes, my_desired_r, letter, size_value, opacity_value) {
  
  p <- ggplot(slopes, aes(x = V1, y = V2)) +
    scale_size_identity() +
    scale_alpha_identity() +
    geom_point(aes(size =  4*(size_value+0.2), alpha = opacity_value), shape = 16) +  
    labs(x = "", y = "") +
    theme_classic() +
    theme(axis.text = element_blank()) +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) +
    theme(legend.position = "None")
  
  ggsave(p,filename=paste0(counter, letter, ".png"),
         device = "png",
         bg = "white",
         path = "all_plots",
         units = "px",
         width = 1000,
         height = 1000,
  )
  
  return(p)
  
}

# Create plots

counter = 1

for(value in my_rs) {
  
  slopes <- slope_function(value)
  slopeI <- (slopes$slope_inverted)
  slopeI_floored <- (slopes$slope_inverted_floored)
  
  typical <- (slopes$typical)
  standard_alpha <- (slopes$standard_alpha)
  
  plot_function(slopes, value, "Alt", slopeI, slopeI_floored)
  plot_function(slopes, value, "Norm", typical, standard_alpha)
  
  if (counter > 0) {
    counter = counter + 1
  }
}

# function to reduce correlation dataframes for use in tabular condition

quantile_reduce <- function(df, n) {
  probs <- seq(0, 1, length.out = n + 1)
  breaks <- unique(quantile(df$V1, probs = probs, na.rm = TRUE))
  
  bins <- cut(df$V1, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  agg <- aggregate(cbind(V1, V2) ~ bins, data = cbind(df, bins = bins), FUN = mean)
  agg[, c("V1", "V2")]
}

# Loop to create and render tables

counter = 1

for (value in my_rs) {
  
  slopes <- slope_function(value)
  
  table_df <- quantile_reduce(slopes, n = 10)
  
  kable(table_df) %>%
    save_kable()
  

}


