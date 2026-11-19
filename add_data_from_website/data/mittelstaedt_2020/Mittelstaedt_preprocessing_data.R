library(tidyverse)

## Mittelstädt et al. (2020)

## Experiment 1
exp1_data <- list.files(
  path = "RawData_Experiment 1",
  full.names = TRUE) %>%
  lapply(
    read_table,
    skip = 3,
    col_names = c(
      "Sub",
      "BlockNum",
      "Trial",
      "Trty",
      "Cor",
      "RT"
      )
    ) %>%
  bind_rows() %>%
  group_by(Sub) %>%
  mutate(
    order = if_else(
      first(BlockNum) == 1,
      "order_hard_first",
      "order_easy_first"
    )
  ) %>%
  ungroup()

exp1_data_new <- exp1_data %>%
  transmute(
    subject = Sub,
    block = case_when(
      BlockNum == 1 ~ -999,
      BlockNum == 2 ~ -998,
      TRUE ~ BlockNum - 2
    ),
    trial = Trial,
    within_identifier = if_else(
      BlockNum %% 2 == 0,
      "easy", # when BlockNum is even, the discriminability is high
      "hard" # when BlockNum is odd, the discriminability is low
    ),
    congruency = case_when(
      Trty %in% c(1, 3) ~ "congruent",
      Trty %in% c(2, 4) ~ "incongruent"),
    accuracy = Cor,
    rt = RT / 1000,
    order = order
  )

exp1_hard_first <- exp1_data_new %>%
  filter(order == "order_hard_first") %>%
  select(-order)

exp1_easy_first <- exp1_data_new %>%
  filter(order == "order_easy_first") %>%
  select(-order)

write_csv(exp1_hard_first, "mittelstaedt_2020_exp1_hard_first.csv")
write_csv(exp1_easy_first, "mittelstaedt_2020_exp1_easy_first.csv")


## Experiment 2
exp2_data <- list.files(
  path = "RawData_Experiment 2",
  full.names = TRUE) %>%
  lapply(
    read_table,
    skip = 3,
    col_names = c(
      "Sub",
      "BlockNum",
      "Trial",
      "Trty",
      "Cor",
      "RT"
    )
  ) %>%
  bind_rows() %>%
  group_by(Sub) %>%
  mutate(
    order = if_else(
      first(BlockNum) <= 7 ,
      "order_two_first",
      "order_four_first"
    )
  ) %>%
  ungroup()

exp2_data_new <- exp2_data %>%
  transmute(
    subject = Sub,
    block = case_when(
      BlockNum == 1 ~ -999,
      BlockNum == 8 ~ -998,
      TRUE ~ BlockNum
    ),
    trial = Trial,
    within_identifier = if_else(
      BlockNum %in% c(1, 2, 3, 4, 5, 6, 7),
      "two",
      "four"
    ),
    congruency = case_when(
      Trty %in% c(1, 2, 5, 6) ~ "congruent",
      Trty %in% c(3, 4, 7, 8) ~ "incongruent"),
    accuracy = Cor,
    rt = RT / 1000,
    order = order
  )

exp2_two_first <- exp2_data_new %>%
  filter(order == "order_two_first") %>%
  select(-order)

exp2_four_first <- exp2_data_new %>%
  filter(order == "order_four_first") %>%
  select(-order)

write_csv(exp2_two_first, "mittelstaedt_2020_exp2_simple_first.csv")
write_csv(exp2_four_first, "mittelstaedt_2020_exp2_difficult_first.csv")


## Experiment 3
exp3_data <- read_delim("RawData_Experiment 3.txt") %>%
  mutate(
    Subject = if_else( # there are two subjects with number 53, change one to 64 based on age
    Subject == 53 & Alter == 20,
    64,
    Subject
    ))

exp3_data_new <- exp3_data %>%
  transmute(
    subject = Subject,
    block = case_when(
      Block == 1 ~ -999,
      Block == 2 ~ -998,
      TRUE ~ Block - 2
    ),
    trial = TrialNr,
    within_identifier = if_else( 
      Cond == 1,
      "constant",
      "variable"
    ),
    congruency = if_else(
      Congru == 1,
      "congruent",
      "incongruent"
    ),
    accuracy = Accuracy,
    rt = StimSlide.RT / 1000
  )

write_csv(exp3_data_new, "mittelstaedt_2020_exp3.csv")