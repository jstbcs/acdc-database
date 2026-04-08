test = readRDS("~/projects/research/acdc/acdc-database/add_data_from_website/test/reymermet_list.RData")

raw_data_study1 = test$study1$data1$observation_table # stroop, young
raw_data_study2 = test$study1$data3$observation_table # flanker young
raw_data_study3 = test$study1$data5$observation_table # stroop old
raw_data_study4 = test$study1$data7$observation_table # flanker old

raw_data_study1$within_identifier = ifelse(raw_data_study1$trial > 36, 1, 2)
raw_data_study2$within_identifier = ifelse(trial > 36, 1, 2)

write.csv(raw_data_study1, "~/../Downloads/raw_within.csv")
test <- raw_data_study1 %>% 
  mutate(
    congruency = case_when(
      congruency == 1 ~ "congruent",
      congruency == 2 ~ "incongruent",
      congruency == 3 ~ "neutral"
    )
  )
write.csv(test, "~/../Downloads/raw_within.csv")
