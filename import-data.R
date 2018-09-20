# n_repetions: 2 (3 mins)
# n_sessions: 8
# n_experiments: 2

library(tidyverse)
library(readr)

self_AE_1 <- read_csv("data/self_AE_1.csv")
object_AE_1 <- read_csv("data/object_AE_1.csv")

data_self <- self_AE_1 %>%
    select(-stimulus) %>%
    filter(trial_type == "image-keyboard-response") %>%
    select(-responses, -test_part, -trial_type, -trial_index, -internal_node_id)

data_self %>% group_by(angle) %>%
    summarise(N = length(age))


data_object <- object_AE_1 %>%
    select(-stimulus) %>%
    filter(trial_type == "image-keyboard-response") %>%
    select(-responses, -test_part, -trial_type, -trial_index, -internal_node_id)

data_object %>% group_by(angle) %>%
    summarise(N = length(age))

data <- bind_rows(data_self, data_object) %>%
    mutate(rt = rt/1000)

data %>% group_by(condition, angle) %>%
    summarise(meanrt = mean(rt))



