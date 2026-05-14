library(tidyverse)

data <- read_csv("data/pav_exp_data.csv")

passed <- read.csv("data/donation_behaviour_passed.csv")

passed_yes <- passed %>%
  filter(passed == TRUE)

just_passed <- inner_join(data, passed_yes, by = "participant")

final_data <- just_passed %>%
  group_by(participant) %>%
  dplyr::mutate(ID = cur_group_id())

final_data$participant <- NULL

final_data <- final_data %>%
  rename(participant = ID)

write_csv(final_data, "data/exp_data_final.csv") 
