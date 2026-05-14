library(tidyverse)

# read in csv file
# rename unique item no column for use in function

exp_data <- read_csv("data/pav_exp_data.csv")

# function to output passed dfs

check_responses <- function(anon) {

  just_columns <- anon %>%
    select(c("participant",
             "trial_id",
             "tb3.text")) %>%
    filter(trial_id %in% c(109, 110, 111,       # assign 100 to right
                           112, 113, 114)) %>%  # assign 100 to left (0 to right)
    mutate_all(~replace(., is.na(.), 0))        # replace non-responses with zero, as blank values are zero
  
  id <- unique(just_columns$participant)
  
  just_columns$answer = NULL
  
  new_df <- just_columns %>%
    mutate(answer = case_when(
      trial_id == 109 ~ 100,
      trial_id == 110 ~ 100,
      trial_id == 111 ~ 100,
      # then do zero responses
      trial_id == 112 ~ 0,
      trial_id == 113 ~ 0,
      trial_id == 114 ~ 0,
    )) %>%
    mutate(correct = case_when(
      trial_id < 112 ~ tb3.text == answer,
      trial_id > 111 ~ tb3.text == answer 
    )) %>%
    group_by(participant) %>%
    summarise(total_correct = sum(correct)) %>%
    arrange(-total_correct)
  
  new_df$passed <- new_df$total_correct >= 2
  
  write_csv(new_df,file.path("data", paste0(unique(anon$expName), "_passed.csv")))

}

# use function on data csv

check_responses(exp_data)
