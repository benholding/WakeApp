library(tidyverse)
library(magrittr)
library(lme4)

load("data/wakeapp_datasets.RDta")

#### this file fits only the continuous (eg gaussian) models ####

#### Dependent variables ####

# rt: reaction_time, rt_var
# math: time_m
# stroop: reaction_time

#### fixed and random effects ####

#Reaction time
re_rt=list(
  re_id = " + (1 | id)",
  re_id_rc = " + (1 + order_in_test | id)",
  re_id_ses = " + (1 | id/session)")

fe_rt=list(
  null = "reaction_time ~ 1 ",
  base = "reaction_time ~ sd_baseline + order_in_test + session ",
  sd   = "reaction_time ~ sd_baseline + order_in_test + session + sd ",
  interaction = "reaction_time ~ sd_baseline + order_in_test + session * sd "
)

#Reaction time - variability
re_rtvar=list(
  re_id = " + (1 | id)")

fe_rtvar=list(
  null = "reaction_time_standard_deviation ~ 1 ",
  base = "reaction_time_standard_deviation ~ sd_baseline + session ",
  sd   = "reaction_time_standard_deviation ~ sd_baseline + session + sd ",
  interaction = "reaction_time_standard_deviation ~ sd_baseline + session * sd "
)

# math
re_math=list(
  re_id = " + (1 | id)",
  re_id_rc = " + (1 + order_in_test | id)",
  re_id_ses = " + (1 | id/session)")

fe_math=list(
  null = "time_m ~ 1 ",
  base = "time_m ~ sd_baseline + order_in_test + session ",
  sd   = "time_m ~ sd_baseline + order_in_test + session + sd ",
  interaction = "time_m ~ sd_baseline + order_in_test + session * sd "
)

#Stroop, conflict cost
re_stroop_conflictcost=list(
  re_id = " + (1 | id)",
  re_id_ses = " + (1 | id/session)")

fe_stroop_conflictcost=list(
  null = "conflict_cost ~ 1 ",
  base = "conflict_cost ~ sd_baseline + session",
  sd   = "conflict_cost ~ sd_baseline + session  + sd ",
  interaction = "conflict_cost ~ sd_baseline + session * sd"
)

#Stroop, update gain
re_stroop_update=list(
  re_id = " + (1 | id)",
  re_id_ses = " + (1 | id/session)")

fe_stroop_update=list(
  null = "update_gain ~ 1 ",
  base = "update_gain ~ sd_baseline + session",
  sd   = "update_gain ~ sd_baseline + session + sd ",
  interaction = "update_gain ~ sd_baseline + session * sd"
)

#Stroop, congruent - RTVAR
re_stroop_confict_rtvar=list(
  re_id = " + (1 | id)")

fe_stroop_confict_rtvar=list(
  null = "rt_stdev ~ 1 ",
  base = "rt_stdev ~ sd_baseline + session",
  sd   = "rt_stdev ~ sd_baseline + session + sd ",
  interaction = "rt_stdev ~ sd_baseline + session * sd"
)

#Stroop, cognitive update - RTVAR
re_stroop_update_rtvar=list(
  re_id = " + (1 | id)")

fe_stroop_update_rtvar=list(
  null = "rt_stdev ~ 1 ",
  base = "rt_stdev ~ sd_baseline + session",
  sd   = "rt_stdev ~ sd_baseline + session + sd ",
  interaction = "rt_stdev ~ sd_baseline + session * sd"
)

#KSS
re_kss=list(
  re_id = " + (1 | id)",
  re_id_rc = " + (1 + order_t | id)",
  re_id_ses = " + (1 | id/session)")

fe_kss=list(
  null = "rating1 ~ 1 ",
  base = "rating1 ~ sd_baseline + session",
  sd   = "rating1 ~ sd_baseline + session + sd ",
  interaction = "rating1 ~ sd_baseline + session * sd "
)



##### Gaussian models ####

gaussian_models=list()
for (ds in c("math", "rt", "rt_var", "stroop_conflict", "stroop_update","stroop_conflict_rtvar", "stroop_update_rtvar", "kss")) {
  if (ds == "rt") {re_formula=re_rt; fe_formula=fe_rt}
  if (ds == "math") {re_formula=re_math; fe_formula=fe_math}
  if (ds == "stroop_conflict") {re_formula=re_stroop_conflictcost; fe_formula=fe_stroop_conflictcost}
  if (ds == "rt_var") {re_formula=re_rtvar; fe_formula=fe_rtvar}
  if (ds == "stroop_update") {re_formula=re_stroop_update; fe_formula=fe_stroop_update}
  if (ds == "stroop_conflict_rtvar") {re_formula=re_stroop_confict_rtvar; fe_formula=fe_stroop_confict_rtvar}
  if (ds == "stroop_update_rtvar") {re_formula=re_stroop_update_rtvar; fe_formula=fe_stroop_update_rtvar}
  if (ds == "kss") {re_formula=re_kss; fe_formula=fe_kss}
  gaussian_models[[ds]]=list()
  for (fe in names(fe_formula)) {
    gaussian_models[[ds]][[fe]] = list()
    for (re in names(re_formula)) {
      gaussian_models[[ds]][[fe]][[re]] = lmer(formula(paste0(fe_formula[[fe]], re_formula[[re]])), 
                                       data=data[[ds]], 
                                       control=lmerControl(optimizer="bobyqa"),
                                       REML=FALSE)
    }
  }
}

save(gaussian_models, file="data/wakeapp_gaussian_models.RDta")
