library(tidyverse)
library(lme4)




#### Functions ####

prepare_data = function(d) {
  d %>% rename_all(tolower) %>%
    rename(sd_group=sd) %>% 
    mutate(session=factor(time),
           sd = (sd_group == "Sleep Deprivation" & time > 0)*1,
           sd_baseline = (sd_group == "Sleep Deprivation"  & time == 0)*1,
           mistake = if_else(correct == 0, 1,0))
}

#### Read data ####

data = list()
data$math = read_csv("arithmetic_data.csv") %>% prepare_data()
data$rt = read_csv("simple_attention_data.csv") %>% prepare_data() %>% mutate(false_responses = if_else(is.na(false_responses), 0, 1))
data$stm =  read_csv("episodic_memory_data.csv") %>% prepare_data()
data$wm = read_csv("working_memory_data.csv") %>% prepare_data()
data$stroop = read_csv("Stroop_data.csv") %>% prepare_data()
data$rt_var = read_csv("simple_attention_data.csv") %>% prepare_data()
data$kss = read.csv("kss_data.csv") %>% prepare_data()

data$stroop_conflict <- data$stroop %>% 
  arrange(id,time,order_in_test) %>% 
  group_by(id,time) %>% 
  mutate(previous_stimuli_combination = lag(stimuli_combination)) %>% 
  mutate(previous_rt = lag(reaction_time))  %>% 
  mutate(conflict_cost = if_else(stimuli_combination == "Congruent -> Incongruent" & previous_stimuli_combination == "Congruent -> Congruent",reaction_time-previous_rt ,NULL)) %>% 
  filter((!is.na(conflict_cost))) %>% 
  ungroup()

data$stroop_update <- data$stroop %>% 
  arrange(id,time,order_in_test) %>% 
  group_by(id,time) %>% 
  mutate(previous_stimuli_combination = lag(stimuli_combination)) %>% 
  mutate(previous_rt = lag(reaction_time))  %>% 
  mutate(update_gain = if_else(stimuli_combination == "Incongruent -> Incongruent" & previous_stimuli_combination == "Congruent -> Incongruent",reaction_time-previous_rt ,NULL)) %>% 
  filter((!is.na(update_gain))) %>% 
  ungroup()

data$stroop_conflict_rtvar <- 
  data$stroop_conflict %>%
  group_by(id,time,sd,sd_baseline,session) %>% 
  summarise(rt_stdev = sd(conflict_cost))

data$stroop_update_rtvar <- 
  data$stroop_update %>%
  group_by(id,time,sd,sd_baseline,session) %>% 
  summarise(rt_stdev = sd(update_gain))

save(data, file="data/wakeapp_datasets.RDta")
