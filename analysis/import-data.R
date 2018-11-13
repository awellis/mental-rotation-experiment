# n_repetions: 4 (3 mins)
# n_sessions: 4
# n_experiments (conditions): 2

library(fs)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(stringr)
library(purrr)

## import csv files ----
import_csv_file <- function(csvfile) {
    csvfile <- read_csv(csvfile)
    dataset <- csvfile %>%
        select(-stimulus) %>%
        filter(trial_type == "image-keyboard-response") %>%
        select(-responses, -test_part, -trial_type, -trial_index, -internal_node_id) %>%
        mutate(correct = str_to_lower(correct))
    dataset
}


csvfiles <- dir_ls("././data", recursive = TRUE, glob = "*.csv")

DATASET <- csvfiles %>% map_df(import_csv_file)

DATASET <- DATASET %>%
    mutate(rt = as.numeric(rt)/1000,
           response = case_when(key_press == "70" ~ "left",
                                key_press == "74" ~ "right"),
           correct = if_else(correct == "true" | correct == "TRUE", 1, 0),
           correct_response = case_when(correct_response == "f" ~ "left",
                                        correct_response == "j" ~ "right")) %>%
    mutate_at(.funs = funs(as.factor),
              .vars = vars(subject, sex, handedness,
                           condition, response)) %>%
    mutate(rotation = as.ordered(angle)) %>%
    select(subject, session, condition, angle, rotation, rt,
           response, correct, correct_response, sex, age,
           handedness) %>%
    drop_na() %>%
    filter(rt > 0)

save(DATASET, file = "././data/DATASET.Rda")

