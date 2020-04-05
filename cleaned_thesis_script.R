library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)
library(dplyr)

#defining important blocks
important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")
crit_pos <- c("crit_pos")
crit_neg <- c("crit_neg")
chall_pos <- c("chall_pos")
chall_neg <- c("chall_neg")
important_crit_words <- c("visszajelzés", "értékelés", "vélemény", "destruktív", "konstruktív", "bírálat")

# read data -------------------------------------------------------------------------

#implicit data
gnat_raw <- 
  vroom(file = list.files(path = "/Users/katasik/Downloads/thesisdata_all/final/", pattern = ".*.txt",
                          full.names = TRUE), 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target"), 
        id = "id")

#explicit data
explicit_raw <- read.xlsx("//Users/katasik/Downloads/thesisdata_all/explicit.xlsx", sheetIndex = 1)

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

##cleaning implicit data

gnat_raw <- gnat_raw %>% 
  mutate(id = str_remove(id, ".txt")) %>% 
  mutate(id = str_remove(id, "/Users/katasik/Downloads/thesisdata_all/final//"))

dscores <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  filter(trial_type=="go_trial") %>% 
  filter(error=="0") %>% 
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

correct_rate <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  group_by(id) %>% 
  summarise(correct = 1 - mean(error))

final_data <- correct_rate %>% 
  left_join(explicit, by = "id")
final_data <- dscores %>% 
  left_join(final_data, by = "id")

#selecing cases with correct rate 90% and above

final_data <- 
  final_data %>% 
  filter(correct>="0.9")

#selecting random numbers
set.seed(1)
randomly<-
  final_data [sample(nrow(final_data ), 86), ] 

duplicated(final_data)