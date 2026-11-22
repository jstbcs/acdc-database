library(tidyverse)

## Faßbender et al. (2023)

## Eriksen Flanker Task

flanker <- read_table("FLANKER_RESULTS.txt")

flanker_new <- flanker %>%
  group_by(ID, Time) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>%
  transmute(
    subject = as.numeric(str_remove(ID, "GS")),
    block = if_else(
      trial <= 20,
      -999,
      1
      ),
    trial = if_else(block == -999, trial, trial - 20),
    within_identifier = Time,
    congruency = if_else(
      Comp == 0,
      "congruent",
      "incongruent"
      ),
    accuracy = case_when(
      Error %in% c(3, 4, "NaN") ~ NA_real_,
      Error == 0 ~ 1,
      TRUE ~ 0
      ),
    rt = case_when(
      RT == "NaN" ~ NA_real_,
      TRUE ~ as.numeric(RT) / 1000
      )
    )

write_csv(flanker_new, "faßbender_2023_flanker.csv")


## Simon Task

simon <- read_table("SIMON_RESULTS.txt")

simon_new <- simon %>%
  group_by(ID, Time) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>%
  transmute(
    subject = as.numeric(str_remove(ID, "GS")),
    block = if_else(
      trial <= 20,
      -999,
      1
      ),
    trial = if_else(
      block == -999,
      trial,
      trial - 20),
    within_identifier = Time,
    congruency = if_else(
      Comp == 0,
      "congruent",
      "incongruent"
      ),
    accuracy = case_when(
      Error %in% c(3, 4, "NaN") ~ NA_real_,
      Error == 0 ~ 1,
      TRUE ~ 0
      ),
    rt = case_when(
      RT == "NaN" ~ NA_real_,
      TRUE ~ as.numeric(RT) / 1000
      )
    )

write_csv(simon_new, "faßbender_2023_simon.csv")


## Stroop Task

stroop <- read_table("STROOP_RESULTS.txt")


stroop_new <- stroop %>%
  group_by(ID, Time) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>%
  transmute(
    subject = as.numeric(str_remove(ID, "GS")),
            block = if_else(
              trial <= 20,
              -999,
              1
              ),
    trial = if_else(block == -999, trial, trial - 20),
    within_identifier = Time,
    congruency = if_else(
      Comp == 0,
      "congruent",
      "incongruent"
      ),
    accuracy = case_when(
      Error %in% c(3, 4, "NaN") ~ NA_real_,
      Error == 0 ~ 1,
      TRUE ~ 0
      ),
    rt = case_when(
      RT == "NaN" ~ NA_real_,
      TRUE ~ as.numeric(RT) / 1000
      )
    )

write_csv(stroop_new, "faßbender_2023_stroop.csv")