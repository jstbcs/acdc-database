# PREPARE RAW DATA FOR INHIBITION DATA BASE 
library(dplyr)
library(data.table)
library(stringr)

# NOTE!! The dataset numbers in this script do not correspond to the 
# dataset IDs in the ACDC database 

########## Overview of datasets #################################### 
# - von Basitian et al.:        dataset1, dataset7, dataset10
# - Pratte et al. :             dataset2, dataset3, dataset8, dataset9
# - Rey-Mermet et al.:          dataset4, dataset5, dataset11, dataset12 
# - Hedge et al.:               dataset6, dataset13, dataset49, dataset50
# - Many labs data:             dataset14 - dataset34 
# - Whitehead et al.(2020):     dataset 35 - 40, dataset 46 - 48
# - Tang et al. (2022):         dataset 41
# - Chetverikov et al. (2017):  dataset 42
# - Stahl et al. (2014):        dataset 43 - 45
# - Enkavi et al. (2019):       dataset 51 - 52
# - Kucina et al. (2023):       dataset 53 - 58

########## Read in and format the datasets ########## 

# Dataset 1 (Von Bastian et al.) Stroop task 
dataset1 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/vonbastian_2015_evidence/LEF_stroop.csv", sep = ";") %>%
  mutate(congruency = ifelse(congruency == "congruent", 1, ifelse(congruency == "incongruent", 2, ifelse(congruency == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         datasetid = 1,
         subject = as.factor(ID),
         between = NA,
         within = NA,
         block = 1,
         rt = RT/1000) # rt data in seconds
ntrial <- length(dataset1 [dataset1$ID == dataset1$ID[1], 1])
nsub <- length(unique(dataset1$ID))
dataset1$trial <- rep(1:ntrial, nsub) # add subject and trial numbers
dataset1 <- dataset1 %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  

# Dataset 2 (Pratte et al.); color stroop
dataset2 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/pratte_2010_exploring/allsi2.dat.txt", sep = " ")
colnames(dataset2) <- c("exp", "subject", "blk", "trial", "color", "distract", "cond", "resp", "accuracy", "rt", "errorTotal", "unused")
dataset2 <- dataset2 %>% filter(exp == 1) %>% # keep Stroop task data
  mutate(datasetid = 2,
         block = blk+1,
         trial = trial+1,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         between = NA,
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 3 (Pratte et al.); spatial Stroop
dataset3 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/pratte_2010_exploring/allsi7.dat.txt", sep = " ")
colnames(dataset3) <- c("subject","blk","blktype","trial","word","location","cond","resp","accuracy","rt","errorTotal", "unused")
dataset3 <- dataset3 %>% filter(blktype == 1) %>% # keep Stroop task data
  mutate(datasetid = 3,
         block = (blk+2)/2,
         trial = trial,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         between = NA, 
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 4 (Rey-Mermet et al.); numStroop
dataset4 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/mermet_2018_should/numStroop.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset4 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset4 <- left_join(dataset4, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 4,
         subject =  sub - 100, 
         subject = as.factor(subject),
         accuracy = ifelse(acc < 97, acc, NA),     # 97 and 99 in raw data are excluded trials
         between = ageGroup,        
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) %>%
  group_split(between) 
dataset4a <- dataset4[[1]] # age group 1 only
dataset4b <- dataset4[[2]] # age group 3 only 


# Dataset 5 (Rey-Mermet et al.); colStroop
dataset5 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/mermet_2018_should/colStroop.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset5 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset5 <- left_join(dataset5, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 5,
         subject = as.factor(sub - 100),
         accuracy = ifelse(acc < 97, acc, NA),
         between = ageGroup,
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) %>%
  group_split(between) 
dataset5a <- dataset5[[1]] # age group 1 only
dataset5b <- dataset5[[2]] # age group 3 only 


# Dataset 6 (Hedge et al.); Stroop task study 1
study <- 1:2
idx <- list(matrix(c(rep(rep(c(1:5, 7:16, 18:36, 38:50), each = 2),2), # no data of participants 6, 17 and 37 (no second session)
                     rep(c(1,2), 94), rep(c("Stroop", "Flanker"), each = 94)), ncol = 3),
            matrix(c(rep(rep(c(1:27, 29:55, 57:62), each = 2),2),  # no data of participants 28 and 56 (no second session)
                     rep(c(1,2), 120), rep(c("Stroop", "Flanker"), each = 120)), ncol = 3))
urls <- list(vector(), vector())
hedge <- list(list(), list())
for(i in 1:length(study)){
  for(j in 1:nrow(idx[[i]])){
    urls[[i]][j] <- paste("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/hedge_2018_reliability/Study", paste(study[i]), "-",
                          idx[[i]][j,3], "/Study", paste(study[i]), "_P", idx[[i]][j,1], idx[[i]][j,3], idx[[i]][j,2], ".csv", sep = "")
    hedge[[i]][[j]] <- read.csv(urls[[i]][j]) %>% mutate(subject = paste(idx[[i]][j,1]),
                                                         session = paste(idx[[i]][j,2]),
                                                         study = paste(study[i]))
    colnames(hedge[[i]][[j]]) <- c("block", "trial", "direction", "cond", "accuracy", "rt", "participant", "session", "study")
  }
}
# combine data of study 1
hedge_data1 <- bind_rows(hedge[[1]]) %>%
  mutate(congruency = ifelse(cond == 0, 1, ifelse(cond == 2, 2, ifelse(cond == 1, 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(session == 1, block, block + 5),
         between = NA,
         within = as.numeric(session),
         subject = as.factor(as.numeric(study)*100 + as.numeric(participant))) # add subject numbers
dataset6 <- hedge_data1 %>% filter(direction == 0) %>% # keep Stroop task data of study 1
  mutate(datasetid = 6) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 7 (Von Bastian et al.); simon task 
dataset7 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/vonbastian_2015_evidence/LEF_simon.csv", sep = ";") %>%
  mutate(congruency = ifelse(congruency == "congruent", 1, ifelse(congruency == "incongruent", 2, ifelse(congruency == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         datasetid = 7,
         block = 1,
         subject = as.factor(ID),
         between = NA, 
         within = NA,
         rt = RT/1000) # rt data in seconds
ntrial <- length(dataset7[dataset7$ID == dataset7$ID[1], 1])
nsub <- length(unique(dataset7$ID))
dataset7$trial <- rep(1:ntrial, nsub) # add subject and trial numbers
dataset7 <- dataset7 %>% select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 8 (Pratte et al.); classic simon task
dataset8 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/pratte_2010_exploring/allsi2.dat.txt", sep = " ")
colnames(dataset8) <- c("exp", "subject", "blk", "trial", "color", "distract", "cond", "resp", "accuracy", "rt", "errorTotal", "unused")
dataset8 <- dataset8 %>% filter(exp == 0) %>% # keep classic Simon task data
  mutate(datasetid = 8,
         block = blk+1,
         trial = trial,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         between = NA, 
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 9 (Pratte et al.); lateral simon task
dataset9 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/pratte_2010_exploring/allsi7.dat.txt", sep = " ")
colnames(dataset9) <- c("subject","blk","blktype","trial","word","location","cond","resp","accuracy","rt","errorTotal", "unused")
dataset9 <- dataset9 %>% filter(blktype == 0) %>% # keep lateral Simon task data
  mutate(datasetid = 9,
         block = (blk+1)/2,
         trial = trial+1,
         subject = as.factor(subject),
         congruency = ifelse(cond == 1, 1, ifelse(cond == 0, 2, ifelse(cond == 2, 3, NA))),
         congruency = as.factor(congruency),
         between = NA, 
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 

# Dataset 10 (Von Bastian et al.); flanker task
dataset10 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/vonbastian_2015_evidence/LEF_flanker.csv", sep = ";") %>%
  mutate(congruency = ifelse(congruency == "congruent", 1, ifelse(congruency == "incongruent", 2, ifelse(congruency == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         datasetid = 10,
         block = 1,
         subject = as.factor(ID),
         between = NA,
         within = NA,
         rt = RT/1000) # rt data in seconds
ntrial <- length(dataset10[dataset10$ID == dataset10$ID[1], 1])
nsub <- length(unique(dataset10$ID))
dataset10$trial <- rep(1:ntrial, nsub) # add subject and trial numbers
dataset10 <- dataset10 %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  

# Dataset 11 (Rey-Mermet et al.); arrow flanker task
dataset11 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/mermet_2018_should/arrowFlanker.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset11 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset11 <- left_join(dataset11, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 11,
         subject = as.factor(sub - 100),
         accuracy = acc,
         between = ageGroup,   
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) %>%
  group_split(between) 
dataset11a <- dataset11[[1]] # age group 1 only
dataset11b <- dataset11[[2]] # age group 3 only 

# Dataset 12 (Rey-Mermet et al.); letter flanker task
dataset12 <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/mermet_2018_should/letFlanker.dat.txt", sep = " ") %>% mutate(id = row_number())
trialnumber <- dataset12 %>% group_by(sub, block) %>% mutate(trial = row_number()) %>% ungroup()
dataset12 <- left_join(dataset12, trialnumber, by = c("id", "sub", "ageGroup", "block", "trialType", "cond", "stim", "acc", "rt")) %>%
  mutate(congruency = ifelse(cond == "congruent", 1, ifelse(cond == "incongruent", 2, ifelse(cond == "neutral", 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(block == "practice", -999, substring(block ,nchar(block))),
         datasetid = 12,
         subject = as.factor(sub - 100),
         accuracy = acc,
         between = ageGroup,   
         within = NA) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)  %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) %>%
  group_split(between) 
dataset12a <- dataset12[[1]] # age group 1 only
dataset12b <- dataset12[[2]] # age group 3 only 


# Dataset 13 (Hedge et al.); flanker of study 1
dataset13 <- hedge_data1 %>% filter(direction != 0) %>% # keep flanker task data of study 1
  mutate(datasetid = 13) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 14-34 (Many Labs studies from https://osf.io/n8xa7/)
manylabs <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/ebersole_2016_many/StroopCleanSet.csv")
for(i in 1:21){
  assign(paste("dataset", i+13, sep = ""), 
         manylabs %>% filter(study_name == unique(manylabs$study_name)[i]) %>% 
           mutate(datasetid = i+13, 
                  subject = as.factor(session_id), 
                  block = block_number,
                  trial = trial_number+1, 
                  congruency = ifelse(congruent == "Congruent", 1, ifelse(congruent == "Incongruent", 2, NA)), 
                  congruency = as.factor(congruency),
                  accuracy = trial_error,
                  between = NA,
                  within = NA,
                  rt = trial_latency/1000) %>% 
           select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) %>%
           group_by(subject) %>%
           arrange(trial, .by_group = TRUE) %>%
           ungroup())
}


# Dataset 35 (Whitehead et al., 2020; FlankerExp2)
dataset35 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/FlankerExp2.csv") %>%
  mutate(
    datasetid = 35,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    block = BlockNum,
    between = NA,
    within = NA,
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 36 (Whitehead et al., 2020; FlankerExp3)
dataset36 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/FlankerExp3.csv") %>% 
  mutate(
    datasetid = 36,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = factor(Subject - 100),
    between = NA,
    within = NA,
    block = ifelse(PracExp == "Exp", 1, -999), 
    rt = StimSlideFlanker.RT / 1000,
    accuracy = StimSlideFlanker.ACC
  ) %>% 
  group_by(subject) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 37 (Whitehead et al., 2020; SimonExp2)
dataset37 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/SimonExp2.csv") %>%
  mutate(
    datasetid = 37,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA,
    block = BlockNum, 
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject, block) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 38 (Whitehead et al., 2020; SimonExp 3)
dataset38 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/SimonExp3.csv") %>%
  mutate(
    datasetid = 38,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA,
    block = ifelse(PracExp == "Exp", 1, -999), 
    rt = StimSlideSimon.RT / 1000,
    accuracy = StimSlideSimon.ACC) %>%
  # add trial number/ "Prac" (Note: group by blocks if there are several)
  group_by(subject) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 39 (Whitehead et al., 2020; StroopExp 2)
dataset39 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/StroopExp2.csv") %>% 
  mutate(
    datasetid = 39, 
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA, 
    block = BlockNum, 
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% 
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 40 (Whitehead et al., 2020; StroopExp 3)
dataset40 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/StroopExp3.csv") %>% 
  mutate(
    datasetid = 40,
    congruency = ifelse(Congruency == 0, 2, Congruency),
    congruency = as.factor(congruency),
    subject = as.factor(Subject - 100),
    between = NA, 
    within = NA, 
    block = ifelse(PracExp == "Exp", 1, -999),  
    rt = StimSlideStroop.RT / 1000,
    accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>% # group by block if existed
  mutate(trial = row_number()) %>% 
  ungroup() %>% # add trial column
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 41 (Snijder et al., 2022); data online at https://osf.io/evuhg
dataset41 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/tang_2022_dual/destroop-raw.csv")  %>% 
  mutate(itemType = ifelse(.$itemType == 2, 
                           "PC50",
                           ifelse(.$itemType == 1 & .$session != "baseline",
                                  "MI",
                                  "MC"
                           )
  )
  ) %>% 
  mutate(
    datasetid = 41,
    # create subject variable starting at 1
    subject = rep(seq_along(rle(ID)$lengths), times = rle(ID)$lengths),
    subject = as.factor(subject),
    block = case_when(
      phase == "test" & session == "baseline"  ~ 1,
      phase == "test" & session == "reactive"  ~ 2,
      phase == "test" & session == "proactive" ~ 3,
      phase == "retest" & session == "baseline"  ~ 4,
      phase == "retest" & session == "reactive"  ~ 5,
      phase == "retest" & session == "proactive" ~ 6),
    between = NA, 
    within = factor(interaction(phase, session, itemType)),   # baseline/ reactive/ proactive condition and test setting
    within = case_when(
      within == "test.baseline.MC" ~ 1, 
      within == "test.baseline.PC50" ~ 2, 
      within == "test.reactive.MC" ~ 3, 
      within == "test.reactive.PC50" ~ 4,
      within == "test.reactive.MI" ~ 5, 
      within == "test.proactive.PC50" ~ 6, 
      within == "test.proactive.MI" ~ 7, 
      within == "retest.baseline.MC" ~ 8, 
      within == "retest.baseline.PC50" ~ 9, 
      within == "retest.reactive.MC" ~ 10, 
      within == "retest.reactive.PC50" ~ 11, 
      within == "retest.reactive.MI" ~ 12, 
      within == "retest.proactive.PC50" ~ 13, 
      within == "retest.proactive.MI" ~ 14
    ),
    congruency = ifelse(grepl("incon", trialCode), 2, 1),
    accuracy = ACC,
    rt = RT / 1000)  %>%
  group_by(subject, block) %>%   # adding within each subject and within condition trial number
  mutate(
    trial = row_number()
  ) %>%
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 42 (Chetverikov et al., 2017); data online at https://osf.io/7rb48
dataset42 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/chetverikov_2017_blame/flanker_data.csv") %>% 
  mutate(
    datasetid = 42,
    subject = as.factor(uid),
    #block = lapply(dataset42, 1, function(i) as.numeric(strsplit(i, " ")[[1]][2])),
    block = gsub("Block ", " ", blockf),
    block = as.numeric(block),
    trial = trialN + 1, 
    between = NA, 
    within = NA, 
    congruency = ifelse(grepl("Incompatible", compf), 2, 1), 
    accuracy = corr) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

# Dataset 43: Stahl et al. (2014): Stroop task 
dataset43 <- read.delim("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/stahl_2014_behavioral/stroop.dat", header = FALSE, sep = " ") %>%
  select(-V14) 
colnames(dataset43) <- c("subj", "subj_code", "date", "time", "block", "trial_no", 
                         "trial_type", "condition", "color", "word", "exp_resp", 
                         "latency", "error")
dataset43 <- dataset43 %>%
  mutate(
    datasetid = 43,
    subject = rep(seq_along(rle(subj)$lengths), times = rle(subj)$lengths),
    subject = as.factor(subject),
    block = ifelse(grepl("tst", block), 
                   apply(dataset43["block"], 1, function(i) as.numeric(strsplit(i, "tst")[[1]][2])),
                   ifelse(grepl("mix", block) | grepl("ueb_", block),
                          -999,
                          block)),
    congruency = ifelse(condition == "con" | condition == "ident", 1, 
                        ifelse(condition == "incon", 2, 3)),
    congruency = as.factor(congruency),
    accuracy = error,
    between = NA, 
    within = NA,
    rt = latency / 1000) %>%
  group_by(subject, block) %>% # change trial number to include warm-ups
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 44: Stahl et al. (2014); Simon task
dataset44 <- read.delim("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/stahl_2014_behavioral/simon.dat", header = FALSE, sep = " ") 
colnames(dataset44) <- c("subj", "subjcode", "date", "time", "part", 
                         "trial_no", "trial_type", "response", "latency")
dataset44 <- dataset44 %>%
  mutate(
    datasetid = 44, 
    subject = rep(seq_along(rle(subj)$lengths), times = rle(subj)$lengths),
    subject = as.factor(subject),
    # block = part?,
    # trial = trial_no?,
    # congruency = trial_type? 
    # accuracy missing,  
    between = NA, 
    within = NA, 
    rt = latency / 1000
  )


# Dataset 45: Stahl et al. (2014); Flanker task
dataset45 <- read.delim("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/stahl_2014_behavioral/flanker.dat", header = FALSE, sep= " ") %>%
  select(-V14)
colnames(dataset45) <- c("subj", "subjcode", "date", "time", "block", 
                         "trial_no", "trial_type", "condition", "targ",
                         "dist", "exp_resp", "latency", "err")
dataset45 <- dataset45 %>%
  mutate(
    datasetid = 45, 
    subject = rep(seq_along(rle(subj)$lengths), times = rle(subj)$lengths),
    subject = as.factor(subject), 
    block = ifelse(grepl("tst", block), 
                   apply(dataset45["block"], 1, function(i) as.numeric(strsplit(i, "tst")[[1]][2])),
                   block),  # extract block number
    block = ifelse(grepl("mix", block)  | grepl("ueb_", block), 
                   -999,
                   block),   # encode practice block
    congruency = ifelse(condition == "congr" | condition == "ident", 1, 
                        ifelse(condition == "incon", 2, 3)),
    congruency = as.factor(congruency),
    accuracy = err,
    between = NA,
    within = NA, 
    rt = latency / 1000
  ) %>%
  group_by(subject, block) %>% # change trial number to include warm-ups
  mutate(trial = row_number()) %>% 
  ungroup() %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 46: Whitehead et al. (2020): Simon task from Experiment1
dataset46 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideSimon.RT != "NA") %>%  # choose simon task entries
  mutate(datasetid = 46, 
         congruency = ifelse(Congruency == 0, 2, Congruency),
         congruency = as.factor(congruency),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         between = NA, 
         within = NA,
         rt = StimSlideSimon.RT / 1000,
         accuracy = StimSlideSimon.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 47: Whitehead et al. (2020): Flanker task from Experiment1
dataset47 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideFlanker.RT != "NA") %>%  # choose Flanker task entries
  mutate(datasetid = 47, 
         congruency = ifelse(Congruency == 0, 2, Congruency),
         congruency = as.factor(congruency),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         between = NA, 
         within = NA,
         rt = StimSlideFlanker.RT / 1000,
         accuracy = StimSlideFlanker.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 


# Dataset 48: Whitehead et al. (2020): Stroop task from Experiment1
dataset48 <- data.table::fread("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/whitehead_2020/Experiment1.csv") %>% 
  filter(StimSlideStroop.RT != "NA") %>%  # choose Stroop task entries
  mutate(datasetid = 48, 
         congruency = ifelse(Congruency == 0, 2, Congruency),
         congruency = as.factor(congruency),
         subject = as.factor(Subject - 505),
         block = BlockNum, 
         between = NA, 
         within = NA,
         rt = StimSlideStroop.RT / 1000,
         accuracy = StimSlideStroop.ACC) %>%
  group_by(subject, block) %>%  # add trial number
  mutate(trial = row_number()) %>% 
  ungroup() %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset49 & 50: Hedge data of study 2
hedge_data2 <- bind_rows(hedge[[2]]) %>% # combine data of study2
  mutate(congruency = ifelse(cond == 0, 1, ifelse(cond == 2, 2, ifelse(cond == 1, 3, NA))),
         congruency = as.factor(congruency),
         block = ifelse(session == 1, block, block + 5),
         between = NA,
         within = as.numeric(session),
         subject = as.factor(as.numeric(study)*100 + as.numeric(participant))) # add subject numbers

dataset49 <- hedge_data2 %>% filter(direction == 0) %>% # keep Stroop task data of study 2
  mutate(datasetid = 49) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

dataset50 <- hedge_data2 %>% filter(direction != 0) %>% # keep flanker task data of study 2
  mutate(datasetid = 50) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 51: Enkavi et al.; stroop task 
# first wave: 522 participants
dataset51a <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/enkavi_2019_large/stroop.csv", sep = ",") 
trialnumber  <- dataset51a %>% group_by(worker_id, exp_stage) %>% mutate(trial = row_number()) %>%  ungroup() # code trial number
dataset51a <- dataset51a %>%
  mutate(
    datasetid = rep(51, nrow(dataset51a)), 
    subject = as.numeric(str_split_fixed(worker_id, fixed("s"), 2)[, 2]), 
    block = ifelse(exp_stage == "practice", -999, 1),
    trial = trialnumber$trial, 
    congruency = ifelse(condition == "congruent", 1, 2),
    between = NA, 
    within = 1, # first wave; test phase 
    accuracy = correct, 
    rt = rt / 1000
  ) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 
# retest 2nd wave: 151 participants 
dataset51b <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/enkavi_2019_large/stroop2.csv", sep = ",") 
trialnumber  <- dataset51b %>% group_by(worker_id, exp_stage) %>% mutate(trial = row_number()) %>%  ungroup() # code trial number
dataset51b <- dataset51b %>%
  mutate(
    datasetid = rep(51, nrow(dataset51b)), 
    subject =  as.numeric(str_split_fixed(worker_id, fixed("s"), 2)[, 2]),
    block = ifelse(exp_stage == "practice", -999, 1),
    trial = trialnumber$trial, 
    congruency = ifelse(condition == "congruent", 1, 2),
    between = NA, 
    within = 2, # second wave; retest phase
    accuracy = correct, 
    rt = rt / 1000
  ) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 
# merge both measurement points 
dataset51 <- rbind(dataset51a, dataset51b)


# Dataset 52: Enkavi et al.; simon task 
# first wave: 522 participants
dataset52a <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/enkavi_2019_large/simon.csv", sep = ",") 
dataset52a <- dataset52a %>%
  mutate(
    datasetid = rep(52, nrow(dataset52a)), 
    subject = as.numeric(str_split_fixed(worker_id, fixed("s"), 2)[, 2]),
    block = ifelse(exp_stage == "practice", -999, 1),
    trial = trial_num + 1, 
    congruency = ifelse(condition == "congruent", 1, 2),
    between = NA, 
    within = 1, # first wave; test phase 
    accuracy = correct, 
    rt = rt / 1000
  ) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt) 
#second wave: 148 participants
dataset52b <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/enkavi_2019_large/simon2.csv", sep = ",") 
dataset52b <- dataset52b %>%
  mutate(
    datasetid = rep(52, nrow(dataset52b)), 
    subject = as.numeric(str_split_fixed(worker_id, fixed("s"), 2)[, 2]),
    block = ifelse(exp_stage == "practice", -999, 1),
    trial = trial_num + 1, 
    congruency = ifelse(condition == "congruent", 1, 2),
    between = NA, 
    within = 2, # second wave, retest
    accuracy = correct, 
    rt = rt / 1000
  ) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)
# merge 
dataset52 <- rbind(dataset52a, dataset52b)

# Kucina et al. data -----------
kucina_tasks <- read.csv("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/kucina_2023_calibration/data4.csv") %>%
  group_split(experimentGroupID)

# Dataset 53: Kucina Stroop2 task 
#dataset53 <- as.data.frame(kucina_tasks[[3]]) %>% # TODO: find out why weird actionIDs!
 # mutate(subject = )

# Dataset 54: Kucina Simon2 task
dataset54 <- as.data.frame(kucina_tasks[[2]]) %>%
  mutate(
    datasetid = rep(54, nrow(.)),
    subject = userID, 
    block = ifelse(Session == 1, Block, # keep Block for first session
                   ifelse(Block == 0, 36, # Block 20 in session 2 is coded as 0, should be 36
                          Block + 16)), # Start counting from 16 for 2nd session
    trial = Trial, 
    congruency = ifelse(Conflict == "congruent", 1, 2), 
    between = NA, # between manipulation was task type
    within = Double + 1, 
    accuracy = ifelse(R == S, 1, 0),
    rt = RT) %>%  
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

# Dataset55: Kucina Stroopon task 
# Stroopon tasks: split into single trial and double trial task  
stroopon <- as.data.frame(kucina_tasks[[4]]) 
dataset55 <- data.frame(); dataset56 <- data.frame() # stroopon1 and 2 respectively
ids <- unique(stroopon$userID)
for(i in 1:length(ids)){ # for each participant
  if(length(unique(stroopon[stroopon$userID == ids[i], 4])) == 1){ # if no 2nd response
    dataset55 <- rbind(dataset55, stroopon[stroopon$userID == ids[i], ]) # add to stroopon1
  } else if(length(unique(stroopon[stroopon$userID == ids[i], 4])) == 2) { #if 2nd response
    dataset56 <- rbind(dataset56, stroopon[stroopon$userID == ids[i], ]) # add to stroopon2
  }
} 
dataset55 <- dataset55 %>%
  mutate(datasetid = 55, 
         subject = userID, 
         block = ifelse(Session == 1, Block, # keep Block number for first session
                        ifelse(Block == 0, 36, # Block 20 in session 2 is coded as 0, should be 36
                               Block + 16)), # Start counting from 16 for 2nd session 
         trial = Trial, 
         congruency = ifelse(Conflict == "congruent+congruent", 1, 2),
         between = NA, 
         within = NA,
         accuracy = ifelse(R == S, 1, 0),
         rt = RT) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

# Dataset56: Kucina Stroopon2 task 
dataset56 <- dataset56 %>%
  mutate(datasetid = 56, 
         subject = userID, 
         block = ifelse(Session == 1, Block, # keep Block number for first session
                        ifelse(Block == 0, 36, # Block 20 in session 2 is coded as 0, should be 36
                               Block + 16)), # Start counting from 16 for 2nd session 
         trial = Trial, 
         congruency = ifelse(Conflict == "congruent+congruent", 1, 2),
         between = NA, 
         within = Double + 1, 
         accuracy = ifelse(R == S, 1, 0),
         rt = RT 
  ) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

# Dataset 57: Kucina Flanker1 task 
flanker <- as.data.frame(kucina_tasks[[1]])
dataset57 <- data.frame(); dataset58 <- data.frame() #Flanker1 and 2 respectively
ids <- unique(flanker$userID)
for(i in 1:length(ids)){ # for each participant
  if(length(unique(flanker[flanker$userID == ids[i], 4])) == 1){ # if no 2nd response
    dataset57 <- rbind(dataset57, flanker[flanker$userID == ids[i], ]) # add to flanker1
  } else if(length(unique(flanker[flanker$userID == ids[i], 4])) == 2) { #if 2nd response
    dataset58 <- rbind(dataset58, flanker[flanker$userID == ids[i], ]) # add to flanker2
  }
}
dataset57 <- dataset57 %>%
  mutate(
    datasetid = 57, 
    subject = userID, 
    block = ifelse(Session == 1, Block, # keep Block number for first session
                   ifelse(Block == 0, 36, # Block 20 in session 2 is coded as 0, should be 36
                          Block + 16)), # Start counting from 16 for 2nd session ,
    trial = Trial, 
    congruency = ifelse(Conflict == "congruent", 1, 2), 
    between = NA,
    within = NA, 
    accuracy = ifelse(R == S, 1, 0), 
    rt = RT
  )  %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)


# Dataset 58: Kucina Flanker2 task 
dataset58 <- dataset58 %>%
  mutate(datasetid = 58, 
         subject = userID, 
         block = ifelse(Session == 1, Block, # keep Block number for first session
                        ifelse(Block == 0, 36, # Block 20 in session 2 is coded as 0, should be 36
                               Block + 16)), # Start counting from 16 for 2nd session 
         trial = Trial, 
         congruency = ifelse(Conflict == "congruent", 1, 2),
         between = NA, 
         within = Double + 1, 
         accuracy = ifelse(R == S, 1, 0), 
         rt = RT) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

# Löffler data ----------------- 

# flanker task 
participants <- seq(1, 151)[-c(4,34,103)]
data_colnames <- c("Subject", "Session","Block", "TrialNum", "TaskDescription", "CongruencyNum",
                   "Congruency", "Arrow", "ArrowDirection", "Flanker", "FlankerDirection",
                   "Fix", "ISI", "ITI", # presentation times of the fixation cross, the inter-stimulus-interval, and the inter-trial-intervall
                   "Accuracy", "RT","Response", "CorrResp")
data_colclasses <- c("integer", "integer", "integer","integer", "character","integer",
                     "character", "integer", "character", "integer", "character",
                     "numeric","numeric","numeric",
                     "integer", "numeric","character", "character")
df_flanker <- data.frame()
for(i in 1:length(participants)){
  filepath <-  paste0("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/loeffler_2022_common/Flanker_task/Erikson%20Flanker%20TaskFlanker_Task_S",
                      participants[i], "_Ses1exp.txt")
  dat <- read.table(filepath, col.names = data_colnames,
                    colClasses = data_colclasses)
  df_flanker  <- rbind(df_flanker,dat)
  rm(dat)
}
dataset59 <- df_flanker %>%
  mutate(datasetid = 59, 
         subject = Subject, 
         block = Block, 
         trial = TrialNum,
         congruency = CongruencyNum,
         between = NA, 
         within = NA, 
         accuracy = Accuracy, 
         rt = RT) %>%
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

# Löffler stroop 
participants <- seq(1, 151)[-c(4,34,58,103)]
data_colnames <- c("Subject","Block", "TrialNum", "TaskDescription", 
                   "WordNum", "Word", "ColorNum", "Color", "CongruencyNum","Congruency",
                   "Fix", "ISI", "ITI",
                   "Accuracy", "RT","Response", "CorrResp")
data_colclasses <- c("integer", "integer", "integer","character","integer",
                     "character", "integer", "character", "integer", "character",
                     "numeric","numeric","numeric",
                     "integer", "numeric","character", "character")
df_stroop <- data.frame()
for(i in 1:length(participants)){
  filepath <-  paste0("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/loeffler_2022_common/Stroop_task/Inhibition%20-%20Stroop%20TaskStroop_Task_S",
                      participants[i], "_Ses1exp.txt")
  dat <- read.table(filepath, col.names = data_colnames,
                    colClasses = data_colclasses)
  df_stroop  <- rbind(df_stroop, dat)
  rm(dat)
}
dataset60 <- df_stroop %>%
  mutate(datasetid = 60, 
         subject = Subject, 
         block = Block, 
         trial= TrialNum,  
         congruency = CongruencyNum,  
         between = NA, 
         within = NA,
         accuracy = Accuracy, 
         rt = RT) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)
         

# Löffler negative priming
# missing: 4, 27, 34, 79, 90, 103, 142, 143, 
participants <- seq(1, 150)[-c(4,27,34,79,90,103,142,143)]
data_colnames <- c("Subject","Session" ,"Block", "TrialNum", "TaskDescription", 
                   "Condition", "Condition_Descr", "Pos_X", "Pos_O", 
                   "Fix", "ISI", "ITI",
                   "Accuracy", "RT","Response", "CorrResp")
data_colclasses <- c("integer", "integer","integer","integer", "character",
                     "integer", "character", "integer", "integer",
                     "numeric","numeric","numeric",
                     "integer", "numeric","character", "character")
df_priming <- data.frame()
for(i in 1:length(participants)){
  filepath <- paste0("https://raw.githubusercontent.com/jstbcs/acdc-database/main/data/loeffler_2022_common/Neg_priming_task/Negative%20Priming%20TaskSternbergTask_",
                     participants[i], "_Ses1exp.txt")
  dat <- read.table(filepath, col.names = data_colnames,
                    colClasses = data_colclasses)
  df_priming  <- rbind(df_priming, dat)
  rm(dat)
}
dataset61 <- df_priming %>%
  mutate(datasetid = 61, 
         subject = Subject, 
         block = Block, 
         trial = TrialNum, 
         congruency = Condition, # no priming = congruent 
         between = NA, 
         within = NA, 
         accuracy = Accuracy, 
         rt = RT) %>% 
  select(datasetid, subject, block, trial, congruency, between, within, accuracy, rt)

