test = readRDS("~/projects/research/acdc/acdc-entry/adding_data/test/enkavi_list.RData")

raw_data_study1 = test$study1$data1$observation_table # stroop, young
raw_data_study2 = test$study1$data3$observation_table # flanker young
raw_data_study3 = test$study1$data5$observation_table # stroop old
raw_data_study4 = test$study1$data7$observation_table # flanker old

raw_data_study1$within_identifier = ifelse(trial > 36, 1, 2)
raw_data_study2$within_identifier = ifelse(trial > 36, 1, 2)

write.csv(raw_data_study1, "~/../Downloads/raw_1.csv")