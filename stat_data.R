install.packages("tidyverse")
library(tidyverse)
library(vroom)
library(janitor)
library(xlsx)
library(widyr)
library(dplyr)
install.packages("psych")
library(psych)

#defining important blocks
important_targets <- c("chall_pos", "chall_neg", "crit_pos", "crit_neg")

# read data -------------------------------------------------------------------------

#implicit data

gnat_raw <- 
  vroom(file = list.files(path = "//Users/katasik/Downloads/data (30)", pattern = ".*.txt",
                          full.names = TRUE), 
        col_names = c("block", "block_id", "word", "max_rt", "trial_type", "word_category", "rt", "error", "target"), 
        id = "id")

short_id <- str_sub(gnat_raw$id, 90, 101)