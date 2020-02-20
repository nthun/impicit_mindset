library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)

important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")

# read data -------------------------------------------------------------------------
gnat_raw <- 
  vroom(file = list.files(path = "/Users/katasik/data/", pattern = ".*.txt",
                          full.names = TRUE), 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target"), 
        id = "file")

explicit_raw <- read_csv2("data/pilot_2/data_d.csv")

explicit <-
  explicit_raw %>% 
  clean_names() %>% 
  select(id = kod,
         kudarcscenario_1:ki4_1)

gnat_raw %>% 
  filter(target %in% important_targets) %>%
  group_by(file) %>% 
  summarise(correct = 1 - mean(error)) %>% view()

### included no_go_trials
dscores_wrong <-
  gnat_raw %>%  
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

### with only go_trials
dscores <-
  gnat_raw %>%  
  filter(target %in% important_targets) %>%
  filter(trial_type=="go_trial") %>% 
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
  write.xlsx("dscoresexplicit.xlsx")

write.xlsx(dscores, "dscores_good_pilot2.xlsx")

