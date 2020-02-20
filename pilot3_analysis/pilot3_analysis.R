library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)

important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")

# read data -------------------------------------------------------------------------
gnat_raw3 <- 
  vroom(file = list.files(path = "/Users/katasik/Pilot 3", pattern = ".*.txt",
                          full.names = TRUE), 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target"), 
        id = "file")

explicit_raw <- read.xlsx("/Users/katasik/Pilot 3/explicit_raw.xlsx", sheetIndex = 1)

explicit <-
  explicit_raw %>% 
  clean_names() %>% 
  select(id = kod,
         kudarcscenario_1:ki4_1)

gnat_raw3 %>% 
  filter(target %in% important_targets) %>%
  group_by(file) %>% 
  summarise(correct = 1 - mean(error)) %>% view()

dscores <-
  gnat_raw3 %>%  
  filter(target %in% important_targets) %>%
  group_by(file) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(file, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  ungroup() %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  separate(target, c("target1", "target2")) %>% 
  select(-pers_block_avg, -pers_sd) %>% 
  unite(targets, c("target1", "target2"), sep = "_") %>% 
  spread(targets, d) %>% 
  transmute(id = str_remove_all(file, "data/pilot_2/|.txt"),
            challange_d = chall_neg - chall_pos,
            crit_d = crit_neg - crit_pos)

dscores %>% 
  left_join(explicit, by = "id") %>% 
  select(-id) %>%
  psych::cor.plot(numbers = TRUE, cuts = FALSE, pval = FALSE)
  
dscores %>% 
  left_join(explicit, by = "id") %>% 
  write.xlsx("dscores_explicit.xlsx")

write.xlsx(dscores, "dscores_pilot3.xlsx")


explicit_reversed <- explicit_raw %>% 
  mutate_at(vars(IQ1.1, IQ2.1, FMS3.1, FMS4.1, CrMS3.1, CrMS4.1, ChMS3.1, ChMS4.1), 
            ~recode(., `1` = 6,
                    `4` = 3,
                    `3` = 4,
                    `2` = 5,
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


write.xlsx(explicit_coded, "ecplicit_pilot3.xlsx")
