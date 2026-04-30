################################################################################

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
library(magick)

################################################################################

# pre data for visualised conditions

set.seed(1234)

my_rs <- c(0.2, 0.5, 0.8)

plot_no <- 1:3

correlations <- tibble(plot_no, my_rs)

################################################################################

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

plot_function <- function(slopes, my_desired_r, letter, size_value, opacity_value,
                          label_y, topic) {
  
  text_coords <- list(
    list(label = "Lower Donation", x = -0.1, y = -0.1, hjust = -0.2, vjust = 0, rot = 0, size = 11),
    list(label = "Higher Donation", x = 0.95, y = -0.1, hjust = 1, vjust = 0, rot = 0, size = 11),
    list(label = label_y, x = -0.1, y = 0.25, hjust = 0, vjust = 0.5, rot = 90, size = 11)
  )
  
  text_grobs <- do.call(gList, lapply(text_coords, function(tc) {
    textGrob(tc$label, 
             x = tc$x, y = tc$y, 
             hjust = tc$hjust, vjust = tc$vjust,
             rot = tc$rot,
             gp = gpar(fontsize = tc$size))
  }))
  
  text_grob <- gTree(children = text_grobs)
  
  arrow <- segmentsGrob(
    x0 = unit(-0.05, "npc"), y0 = unit(0, "npc"),
    x1 = unit(-0.05, "npc"), y1 = unit(0.95, "npc"),
    arrow = arrow(length = unit(2, "mm")),
    gp = gpar(col = "black")
  )
  
  arrow2 <- segmentsGrob(
    x0 = unit(-0.05, "npc"), y0 = unit(0, "npc"),
    x1 = unit(0.95, "npc"), y1 = unit(0, "npc"),
    arrow = arrow(length = unit(2, "mm")),
    gp = gpar(col = "black")
  )
  
  p <- ggplot(slopes, aes(x = V1, y = V2)) +
    scale_size_identity() +
    scale_alpha_identity() +
    geom_point(aes(size = 4*(size_value+0.2), alpha = opacity_value), shape = 16) +  
    labs(x = "", y = "") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 1, 1), "cm"),
          plot.background = element_rect(colour = "black", fill = NA, size = 1)) +
    annotation_custom(grob = text_grob,  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(grob = arrow, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(grob = arrow2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(
      grob = text_grob,
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    coord_cartesian(clip = "off")
  
  ggsave(p, filename = paste0(counter,
                              "_",
                              letter,
                              "_",
                              topic,
                              ".png"),
         device = "png",
         bg = "white",
         path = "stimuli/raw_plots",
         units = "px",
         width = 1000,
         height = 1000)
  
  return(p)
}

################################################################################

# Create plots

counter = 1

for(value in my_rs) {
  
  slopes <- slope_function(value)
  slopeI <- (slopes$slope_inverted)
  slopeI_floored <- (slopes$slope_inverted_floored)
  
  typical <- (slopes$typical)
  standard_alpha <- (slopes$standard_alpha)
  
  # Great British Dog Foundation
  
  plot_function(slopes, value, "Alt", slopeI, slopeI_floored,
                "Dogs Rescued", "dog_rescue")
  plot_function(slopes, value, "Norm", typical, standard_alpha,
                "Dogs Rescued", "dog_rescue")
  
  # Action for a Greener World

  plot_function(slopes, value, "Alt", slopeI, slopeI_floored,
                "Trees Planted", "greener_world")
  plot_function(slopes, value, "Norm", typical, standard_alpha,
                "Trees Planted", "greener_world")

  # Good Samaritans - mental health helpline

  plot_function(slopes, value, "Alt", slopeI, slopeI_floored,
                "Phone Operators", "samaritans")
  plot_function(slopes, value, "Norm", typical, standard_alpha,
                "Phone Operators", "samaritans")
  
  if (counter > 0) {
    counter = counter + 1
  }
}

################################################################################

# insert created plots onto template backgrounds

box <- list(x = 270, y = 860, w = 1070, h = 1070)

numbers   <- c("1", "2", "3")
versions  <- c("Alt", "Norm")
charities <- c("greener_world", "samaritans", "dog_rescue")

output_dir <- "stimuli/final_plots/"

for (charity in charities) {
  template_path <- file.path("stimuli", "charity_templates", paste0(charity, ".png"))
  
  
  template <- image_read(template_path)
  
  for (version in versions) {
    for (number in numbers) {
      
      graph_path  <- file.path("stimuli", "raw_plots", paste0(number, "_", version, "_", charity, ".png"))
      output_path <- file.path(output_dir, paste0(number, "_", version, "_", charity, ".png"))
      
      graph         <- image_read(graph_path)
      graph_resized <- image_resize(graph, geometry_size_pixels(box$w, box$h,
                                                                preserve_aspect = FALSE))
      result        <- image_composite(template, graph_resized,
                                       offset = geometry_point(box$x, box$y))
      image_write(result, path = output_path, format = "png")
    }
  }
}

################################################################################

# create final experimental csv

plot_dir   <- "stimuli/final_plots"
output_csv <- "data/exp_trials.csv"

charities <- c("dog_rescue", "greener_world", "samaritans")

plot_types <- c("Norm", "Alt")

correlations <- c("1", "2", "3")

ext <- ".png"

infographics <- expand_grid(
  charity     = charities,
  plot_type   = plot_types,
  correlation = correlations
) %>%
  mutate(
    filename = paste0(correlation, "_", plot_type, "_", charity, ext),
    filepath = file.path(plot_dir, filename)
  )

pairs <- combn(nrow(infographics), 2, simplify = FALSE) %>%
  map_dfr(~ tibble(left_idx = .x[1], right_idx = .x[2])) %>%
  mutate(
    left_charity  = infographics$charity[left_idx],
    right_charity = infographics$charity[right_idx]
  ) %>%
  filter(left_charity != right_charity) %>% 
  mutate(
    left_path         = infographics$filepath[left_idx],
    right_path        = infographics$filepath[right_idx],
    left_plot_type    = infographics$plot_type[left_idx],
    right_plot_type   = infographics$plot_type[right_idx],
    left_correlation  = infographics$correlation[left_idx],
    right_correlation = infographics$correlation[right_idx],
    same_plot_type    = left_plot_type   == right_plot_type,
    same_correlation  = left_correlation == right_correlation,
    trial_id          = row_number()
  ) %>%
  dplyr::select(
    trial_id,
    left_charity,  left_plot_type,  left_correlation,  left_path,
    right_charity, right_plot_type, right_correlation, right_path,
    same_plot_type, same_correlation
  )

pairs <- pairs %>%
  mutate(
    left_path  = gsub("stimuli/final_plots", "exp_resources/stimuli", left_path,  fixed = TRUE),
    right_path = gsub("stimuli/final_plots", "exp_resources/stimuli", right_path, fixed = TRUE)
  ) # replace with final path for psychopy experiment

instructions <- rep("Please complete the charity assignment. Make sure you use all your cash.",
                    times = 108) # instructions

colour <- rep("black", times = 108) # instruction text colour

exp_data <- cbind(pairs, instructions, colour)

# make AC part of final csv

exp_data <- exp_data %>%
  bind_rows(
    exp_data %>%
      slice_tail(n = 6) %>%
      mutate(
        colour = "red",
        trial_id = trial_id + 6,
        instructions = "Please ignore the plots and assign all money to the right hand charity."
      )
  )

write_csv(exp_data, output_csv)    # write csv to file

##################################################################################
