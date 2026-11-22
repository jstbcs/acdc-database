library(tidyverse)

## Enkavi et al. (2019)
# https://github.com/IanEisenberg/Self_Regulation_Ontology/tree/master

# directory with data of the first time point
t1_dir <- "Self_Regulation_Ontology-master/Data/Complete_02-16-2019/Individual_Measures"

# directory with data of the second time point
t2_dir <- "Self_Regulation_Ontology-master/Data/Retest_02-16-2019/Individual_Measures"


## Local-global Task

t1_lg <- read_csv(
  file.path(t1_dir, "local_global_letter.csv.gz")
  )%>%
  mutate(within_identifier = "t1")

t2_lg <- read_csv(
  file.path(t2_dir, "local_global_letter.csv.gz")
  ) %>%
  mutate(within_identifier = "t2")

local_global_letter <- bind_rows(t1_lg, t2_lg) %>%
  transmute(
    subject = as.numeric(str_remove(worker_id, "s")),
    block = if_else(
      exp_stage == "practice",
      -999,
      1
      ),
    trial = trial_num + 1,
    within_identifier = within_identifier,
    congruency = as.character(conflict_condition),
    accuracy = correct,
    rt = if_else(
      rt == -1,
      NA_real_,
      rt / 1000)
  )

write_csv(local_global_letter, "local_global_letter.csv")


## Attentional Network Task

t1_atten_net <- read_csv(
  file.path(t1_dir, "attention_network_task.csv.gz")
) %>%
  mutate(
    within_identifier = case_when(
      cue == "nocue" ~ "t1_no_cue",
      cue == "double" ~ "t1_double_cue",
      cue == "spatial" ~ "t1_spatial_cue",
      cue == "center" ~ "t1_center_cue"
    )
  )

t2_atten_net <- read_csv(
  file.path(t2_dir, "attention_network_task.csv.gz")
) %>%
  mutate(
    within_identifier = case_when(
      cue == "nocue" ~ "t2_no_cue",
      cue == "double" ~ "t2_double_cue",
      cue == "spatial" ~ "t2_spatial_cue",
      cue == "center" ~ "t2_center_cue"
    )
  )

atten_net <- bind_rows(t1_atten_net, t2_atten_net) %>%
  transmute(
    subject = as.numeric(str_remove(worker_id, "s")),
    trial = (trial_num - 1) %% 48 + 1,
    block = if_else(
      exp_stage == "practice",
      -999,
      (trial_num - 1) %/% 48 + 1
    ),
    within_identifier = within_identifier,
    congruency = as.character(flanker_type),
    accuracy = correct,
    rt = if_else(rt == -1, NA_real_, rt / 1000)
  )

write_csv(atten_net, "attention_network.csv")