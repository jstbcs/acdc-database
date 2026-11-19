library(tidyverse)

## Experiment 1 in Ellinghaus et al. (2024)

Exp1 <- read.csv("E1Dat.csv")

Exp1_new <- Exp1 %>%
  transmute(
    subject = vpNum, 
    block = case_when(
      blockNum == 1 ~ -999,
      blockNum == 2 ~ -998,
      TRUE ~ blockNum - 2
    ),
    trial = trialNum,
    within_identifier = ratio,
    congruency = recode(
      compatibility,
      "comp" = "congruent",
      "incomp" = "incongruent"
      ),
    accuracy = case_when(
      corrCode == 1 ~ 1,
      corrCode == 2 ~ 0,
      corrCode %in% c(3, 4) ~ NA_real_
      ),
  rt = rt / 1000
  )

write_csv(Exp1_new, "ellinghaus_2024_exp1.csv")


## Experiment 2 in Ellinghaus et al. (2024)

Exp2 <- read.csv("E2Dat.csv")

Exp2_new <- Exp2 %>%
  transmute(
    subject = vpNum, 
    block = case_when(
      blockNum == 1 ~ -999,
      blockNum == 2 ~ -998,
      TRUE ~ blockNum - 2
    ),
    trial = trialNum,
    within_identifier = ratio,
    congruency = recode(
      compatibility,
      "comp" = "congruent",
      "incomp" = "incongruent"
      ),
    accuracy = case_when(
      corrCode == 1 ~ 1,
      corrCode == 2 ~ 0,
      corrCode %in% c(3, 4) ~ NA_real_
    ),
    rt = rt / 1000
  )

write_csv(Exp2_new, "ellinghaus_2024_exp2.csv")