install.packages("tidyverse")
library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)
library(dplyr)
library(readxl)

#defining important blocks
important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")

# read data -------------------------------------------------------------------------

#reading and cleaning implicit data

files <- list.files(path = "implicit_mindset/thesis/", pattern = "immtest.*.txt$", full.names = TRUE)

gnat_raw <- 
  vroom(file = files, 
        id = "id", 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target")) %>% 
  extract(col = id, 
          into = c(NA, "id", NA), 
          regex = "^(.*data.)(.*)(.txt)$")
gnat_raw <- 
  vroom(file = files, 
        id = "id", 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target")) %>% 
  extract(col = id, 
          into = c(NA, "id", NA), 
          regex = "^(.*data.)(.*)(.txt)$")
#keeping only important blocks
gnat_important <- 
  gnat_raw %>%  
  filter(target %in% important_targets) %>% 
  filter(trial_type == "go_trial")

#checking for block error rate above 40%
correct_block_rate <-
  gnat_important %>%  
  group_by(id, target) %>% 
  summarise(correct_block = 1 - mean(error))

#no more removal needed
correct_all_rate <-
  gnat_important %>% 
  filter(target %in% important_targets) %>%
  group_by(id) %>% 
  summarise(correct_all = 1 - mean(error))


#must delete these observations due to high block error rate
final_gnat <-
  gnat_important %>% 
  left_join(correct_block_rate, by = c("id", "target")) %>% 
  left_join(correct_all_rate, by = c("id")) %>% 
  filter(correct_block > 0.6 & correct_all > 0.8 & error == 0) %>% 
  filter(rt > 300 & rt < 1399)

#deleting rows with too quick and too slow response windows (too quick is 300 and too slow is 3 SD above averagert ==654+165*3)
#final_gnat %>% 
 # summarise(pers_a = mean(rt))
#final_gnat %>% 
 # summarise(pers_sd = sd(rt))
#654+165*3

#reading explicit data
explicit_raw <- 
  read_excel("implicit_mindset/thesis_data/data.xlsx", 1) %>%
  extract(col = participant, 
          into = c(NA, "id", NA), 
          regex = "^(.*s.)(.*)(.txt)$") %>% 
  clean_names()

##cleaning explicit data
#reversing items
explicit_reversed <- 
  explicit_raw %>% 
  mutate_at(vars(iq1_1, iq2_1, fms3_1, fms4_1, cr_ms3_1, cr_ms4_1, ch_ms3_1, ch_ms4_1), 
            ~recode(., `1` = 6,
                    `2` = 5,
                    `3` = 4,
                    `4` = 3,
                    `5` = 2,
                    `6` = 1,
            ))

explicit_reversed <- 
  explicit_reversed %>% 
  mutate_at(vars(failurescenario_1, criticismscenario_1), 
            ~recode(., `1` = 5,
                    `2` = 4,
                    `4` = 2,
                    `5` = 1,
            ))

#creating means of explicit measures
explicit_coded <-
  explicit_reversed %>%
  mutate(iqms_avg = rowMeans(x = select(.data = ., starts_with(match = "iq"))))

explicit_coded <- explicit_coded %>%
  mutate(crms_avg = rowMeans(x = select(.data = ., starts_with(match = "cr_ms"))))

explicit_coded <- explicit_coded %>%
  mutate(chms_avg = rowMeans(x = select(.data = ., starts_with(match = "ch_ms"))))

explicit_coded <- explicit_coded %>%
  mutate(fms_avg = rowMeans(x = select(.data = ., starts_with(match = "fms"))))

explicit_coded <- explicit_coded %>%
  mutate(fsc_avg = rowMeans(x = select(.data = ., starts_with(match = "failure"))))

explicit_coded <- explicit_coded %>%
  mutate(crsc_avg = rowMeans(x = select(.data = ., starts_with(match = "criticism"))))

#final explicit data
explicit <-
  explicit_coded %>% 
  select(id, gender_1, age_1, iqms_avg, crms_avg, chms_avg, fms_avg, fsc_avg, crsc_avg, challengescenario_1)

#calculating implicit mindset (D) scores by computing average rt for critical blocks separately, dividing by standard deviations across all critical blocks and subtracting positive blocks from negative blocks
dscores <-
  final_gnat %>% 
  group_by(id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  group_by(id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  select(-pers_block_avg, -pers_sd) %>%
  spread(target, d) %>% 
  mutate(challange_d = chall_neg - chall_pos,
         crit_d = crit_neg - crit_pos)

#collecting all data in one dataframe and subsetting ELTE sample

final_data <- 
  dscores %>% 
  left_join(explicit, by = "id")


set.seed(1)

randomly <-
  final_data %>% 
  sample_n(86)
  
test_set <- 
  final_data %>% 
  filter(!id %in% pull(randomly, id))
  
install.packages("sjPlot")
library(sjPlot)
write_excel_csv(randomly, "randomly.csv")

sjPlot::sjp.corr(select(randomly, crit_pos, crsc_avg), sort.corr = FALSE)

write_excel_csv(test_set, "test_set.csv")

#plotting implicit citicism mindset and explicit criticism mindset

install.packages("ggpubr")
library("ggpubr")
ggscatter(randomly, x = "crit_pos", y = "crsc_avg", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Criticism Positive D Score", ylab = "Criticism Scenario",
          add.params = list(color = "coral3",
                            fill = "gray27"))
#computing correlations in JASP
