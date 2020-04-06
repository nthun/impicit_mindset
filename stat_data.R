install.packages("tidyverse")
library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)
library(dplyr)

#defining important blocks
important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")

# read data -------------------------------------------------------------------------

#reading and cleaning implicit data

files <- list.files(path = "data (30)/", pattern = "immtest.*.txt$", full.names = TRUE)
gnat_raw <- 
  vroom(file = files, 
        id = "id", 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target")) %>% 
  extract(col = id, 
          into = c(NA, "id", NA), 
          regex = "^(.*data.)(.*)(.txt)$")

#keeping only important blocks
gnat_important<- gnat_raw %>%  
  filter(target %in% important_targets) %>% 
  filter(trial_type=="go_trial")

#checking for block correct rate
correct_rate <-
  gnat_important %>%  
  group_by(id, target) %>% 
  summarise(correct = 1 - mean(error))

#must delete "82ddd565-aaf9-47c4-8ac6-91a6301acaed", "d2c74513-b0ed-4843-86cd-31766dedbf4b", as block error rate is above 40%
correct_gnat <- 
  correct_rate %>% 
  filter(correct<="0.6")

final_gnat <- gnat_important %>% 
  filter(!str_detect(id, '82ddd565-aaf9-47c4-8ac6-91a6301acaed', )) %>% 
  filter(!str_detect(id, 'd2c74513-b0ed-4843-86cd-31766dedbf4b', ))

#deleting rows with too quick and too slow response windows (too quick is 300 and too slow is 3 SD above averagert ==694+193*3)
final_gnat %>% 
  summarise(pers_a = mean(rt))
final_gnat %>% 
  summarise(pers_sd = sd(rt))
694+193*3

final_gnat<- 
  gnat_important%>% filter(rt %in% (300:1273))

#reading explicit data
explicit_raw <- read.xlsx("data (30)/data.xlsx", sheetIndex = 1) %>% 
  extract(col = participant, 
          into = c(NA, "id", NA), 
          regex = "^(.*s.)(.*)(.txt)$")

##cleaning explicit data
#reversing items
explicit_reversed <- explicit_raw %>% 
  mutate_at(vars(IQ1.1, IQ2.1, FMS3.1, FMS4.1, CrMS3.1, CrMS4.1, ChMS3.1, ChMS4.1), 
            ~recode(., `1` = 6,
                    `2` = 5,
                    `3` = 4,
                    `4` = 3,
                    `5` = 2,
                    `6` = 1,
            ))

explicit_reversed <- explicit_reversed %>% 
  mutate_at(vars(Failurescenario.1, Criticismscenario.1), 
            ~recode(., `1` = 5,
                    `2` = 4,
                    `4` = 2,
                    `5` = 1,
            ))

#creating means of explicit measures
explicit_coded<-explicit_reversed %>%
  mutate(IQMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "IQ"))))
explicit_coded<-explicit_coded %>%
  mutate(CRMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "CrMS"))))
explicit_coded<-explicit_coded %>%
  mutate(CHMS_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "ChMS"))))
explicit_coded<-explicit_coded %>%
  mutate(FMS_M = rowMeans(x = select(.data = .,
                                     starts_with(match = "FMS"))))
explicit_coded<-explicit_coded %>%
  mutate(FSC_M = rowMeans(x = select(.data = .,
                                     starts_with(match = "Failure"))))
explicit_coded<-explicit_coded %>%
  mutate(CrSC_M = rowMeans(x = select(.data = .,
                                      starts_with(match = "Criticism"))))
#final explicit data
explicit <-
  explicit_coded %>% 
  select(id, Challengescenario.1, gender.1, age.1, IQMS_M, CRMS_M, CHMS_M, FMS_M, FSC_M, CrSC_M)

#calculating implicit mindset (D) scores by computing average rt for critical blocks separately, dividing by standard deviations across all critical blocks and subtracting positive blocks from negative blocks
dscores <-
  final_gnat %>% 
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  separate(target, c("target1", "target2")) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  unite(targets, c("target1", "target2"), sep = "_") %>% 
  spread(targets, d) %>% 
  transmute(challange_d = chall_neg - chall_pos,
            crit_d = crit_neg - crit_pos)

#calculating D scores for separate blocks
d_spread<-
  final_gnat %>%  
  filter(target %in% important_targets) %>%
  filter(trial_type=="go_trial") %>% 
  filter(error=="0") %>% 
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  spread(target, d)

#collecting all data in one dataframe

final_data <- dscores %>% 
  left_join(explicit, by = "id")
final_data <- d_spread %>% 
  left_join(final_data, by = "id")
final_data <- personal_average %>% 
  left_join(final_data, by="id")

write.csv(final_data, "stat_final.csv")

#plotting implicit citicism mindset and explicit criticism mindset

install.packages("ggpubr")
library("ggpubr")
ggscatter(final_data, x = "crit_d", y = "CRMS_M", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Implicit Criticism Mindset", ylab = "Explicit Criticism Mindset",
          add.params = list(color = "olivedrab3",
                            fill = "gray27"))
#computing correlations in JASP




