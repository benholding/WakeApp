library(tidyverse)
library(magrittr)
library(lme4)

load("data/wakeapp_datasets.RDta")

#### this file fits only the logistic (binary) models ####

#### Dependent variables ####

# math: correct
# stm: correct
# wm: correct
# rt: 1000ms lapses
# stroop: accuracy

#### fixed and random effects ####

# short-term memory
re_stm=list(
  re_id = " + (1 | id)",
  re_id_ses = " + (1 | id/session)",
  re_crossed  = " + (1 | id/session) + (1 | word)")

fe_stm=list(
  null = "mistake ~ 1 ",
  base = "mistake ~ sd_baseline + recall_session + session ",
  sd   = "mistake ~ sd_baseline + recall_session + session + sd ",
  interaction = "mistake ~ sd_baseline + recall_session + session * sd "
)

# math
re_math=list(
  re_id = " + (1 | id)",
  re_id_rc = " + (1 + order_in_test | id)",
  re_id_ses = " + (1 | id/session)")

fe_math=list(
  null = "mistake ~ 1 ",
  base = "mistake ~ sd_baseline + order_in_test + session ",
  sd   = "mistake ~ sd_baseline + order_in_test + session + sd ",
  interaction = "mistake ~ sd_baseline + order_in_test + session * sd "
)

# working memory
re_wm=list(
  re_id = " + (1 | id)",
  re_id_rc = " + (1 + order_in_test | id)",
  re_id_ses = " + (1 | id/session)")

fe_wm=list(
  null = "mistake ~ 1 ",
  base = "mistake ~ sd_baseline + order_in_test + session ",
  sd   = "mistake ~ sd_baseline + order_in_test + session + sd ",
  interaction = "mistake ~ sd_baseline + order_in_test + session * sd "
)


#Simple attention lapses (simpler model because less variation in response)
re_rt_lapse=list(
  re_id = " + (1 | id)",
  re_id_ses = " + (1 | id/session)")

fe_rt_lapse=list(
  null = "lapse1000 ~ 1 ",
  base = "lapse1000 ~ sd_baseline + session ",
  sd   = "lapse1000 ~ sd_baseline + session + sd ",
  interaction = "lapse1000 ~ sd_baseline + session * sd "
)

# stroop accuracy (simpler model because less variation in response)
re_stroop=list(
  re_id = " + (1 | id)",
  re_id_ses = " + (1 | id/session)")

fe_stroop=list(
  null = "error ~ 1 ",
  base = "error ~ sd_baseline + session",
  sd   = "error ~ sd_baseline + session + sd ",
  interaction = "error ~ sd_baseline + session * sd"
)

#### Logistic models ####

models=list()
for (ds in c("math", "wm", "stm", "rt", "stroop")) {
  if (ds == "stm") {re_formula=re_stm; fe_formula=fe_stm}
  if (ds == "math") {re_formula=re_math; fe_formula=fe_math}
  if (ds == "wm") {re_formula=re_wm; fe_formula=fe_wm}
  if (ds == "rt") {re_formula=re_rt_lapse; fe_formula=fe_rt_lapse}
  if (ds == "stroop") {re_formula=re_stroop; fe_formula=fe_stroop}
  models[[ds]]=list()
  for (fe in names(fe_formula)) {
    models[[ds]][[fe]] = list()
    for (re in names(re_formula)) {
      models[[ds]][[fe]][[re]] = glmer(formula(paste0(fe_formula[[fe]], re_formula[[re]])), 
                                     data=data[[ds]], family=binomial, 
                                     control=glmerControl(optimizer="bobyqa"))
    }
  }
}

save(models, file="data/wakeapp_binary_models.RDta")

