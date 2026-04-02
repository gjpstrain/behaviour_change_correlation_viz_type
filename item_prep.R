# load packages

library(tidyverse)
library(MASS)
library(Hmisc)
library(gridExtra)
library(kableExtra)
library(webshot2)
library(magick)
library(glue)
library(gtools)
library(grid)

# pre data for visualised conditions

set.seed(1234)

my_rs <- seq(0.2, 0.99, length.out = 10)

plot_no <- 1:10

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

plot_function <- function(slopes, my_desired_r, letter, size_value, opacity_value) {
  
  text_coords <- list(
    list(label = "Less plain Diet", x = -0.1, y = -0.1, hjust = 0, vjust = 0, rot = 0, size = 11),
    list(label = "More plain Diet", x = 0.95, y = -0.1, hjust = 1, vjust = 0, rot = 0, size = 11),
    list(label = "Fewer Cancer\nDiagnoses", x = -0.05, y = 0, hjust = 0, vjust = 0.5, rot = 90, size = 11),
    list(label = "More Cancer\nDiagnoses", x = -0.05, y = 0.6, hjust = 0, vjust = 0.5, rot = 90, size = 11)
  )
  
  text_grobs <- do.call(gList, lapply(text_coords, function(tc) {
    textGrob(tc$label, 
             x = tc$x, y = tc$y, 
             hjust = tc$hjust, vjust = tc$vjust,
             rot = tc$rot,
             gp = gpar(fontsize = tc$size))
  }))
  
  # Wrap in gTree
  text_grob <- gTree(children = text_grobs)
  
  p <- ggplot(slopes, aes(x = V1, y = V2)) +
    scale_size_identity() +
    scale_alpha_identity() +
    geom_point(aes(size = 4*(size_value+0.2), alpha = opacity_value), shape = 16) +  
    labs(x = "", y = "") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 1, 1), "cm"),
          plot.background = element_rect(colour = "black", fill = NA, size = 1)) +
    annotation_custom(
      grob = text_grob,
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    coord_cartesian(clip = "off")
  
  ggsave(p, filename = paste0(counter, letter, ".png"),
         device = "png",
         bg = "white",
         path = "stimuli/raw_plots",
         units = "px",
         width = 1000,
         height = 1000)
  
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

## Part 2: Creating Experimental csv

# grab images and add directory for psychopy exp

raw_plots_alt <- mixedsort(list.files(path = "stimuli/raw_plots", pattern = "Alt", full.names = T)) %>%
  paste0("exp_resources/", .)

raw_plots_standard <- mixedsort(list.files(path = "stimuli/raw_plots", pattern = "Norm", full.names = T)) %>%
  paste0("exp_resources/", .)

# shuffle images for pseudorandomisation

left_images <- c(images_alt[1:15], images_standard[16:30], images_tables[31:45])

middle_images <- c(images_tables[1:15], images_alt[16:30], images_standard[31:45])

right_images <- c(images_standard[1:15], images_tables[16:30], images_alt[31:45])

# create other vectors

colour <- rep("black", times = 45)

instructions <- rep("Please complete the charity assignment. Make sure you use all your cash.",
                    times = 45)

trial_no <- 1:45

# cbind exp dataframe together

exp_data <- cbind(trial_no, left_images, middle_images, right_images,
                  colour, instructions) %>% as_data_frame() %>%
  mutate(trial_no = as.numeric(trial_no))

# use exp df to create and append AC df

exp_data <- exp_data %>%
  bind_rows(
    exp_data %>%
      slice_tail(n = 4) %>%
      mutate(
        colour = "red",
        trial_no = trial_no + 4,
        instructions = "Please ignore the plots and assign all money to the central charity."
      )
  )

write_csv(exp_data, "behaviour_change_donation_exp_data.csv")
