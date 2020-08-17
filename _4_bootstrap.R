library(lme4)
library(plyr)
library(tidyverse)

#load("data/wakeapp_gaussian_models.RDta")
#load("data/wakeapp_binary_models.RDta")
load("data/wakeapp_datasets.RDta")
load("data/wakeapp_gaussian_final_models.RDta")
load("data/wakeapp_final_models.RDta")

number_subjects = list(
  math = data$math %>% mutate(sd_grp = (sd | sd_baseline)*1) %>%
    distinct(sd_grp, session, id) %>%
    group_by(sd_grp, session) %>%
    summarize(n=n()),
  wm = data$wm %>% mutate(sd_grp = (sd | sd_baseline)*1) %>%
    distinct(sd_grp, session, id) %>%
    group_by(sd_grp, session) %>%
    summarize(n=n()),
  stm = data$stm %>% mutate(sd_grp = (sd | sd_baseline)*1) %>%
    distinct(sd_grp, session, id) %>%
    group_by(sd_grp, session) %>%
    summarize(n=n()),
  stroop = data$stroop %>% mutate(sd_grp = (sd | sd_baseline)*1) %>%
    distinct(sd_grp, session, id) %>%
    group_by(sd_grp, session) %>%
    summarize(n=n()),
  rt = data$rt %>% mutate(sd_grp = (sd | sd_baseline)*1) %>%
    distinct(sd_grp, session, id) %>%
    group_by(sd_grp, session) %>%
    summarize(n=n()),
  kss = data$kss %>% mutate(sd_grp = (sd | sd_baseline)*1) %>%
    distinct(sd_grp, session, id) %>%
    group_by(sd_grp, session) %>%
    summarize(n=n())
  )

newdata = list(
  math = data$math %>% distinct(sd_baseline, session, sd) %>%
    select(sd_baseline, session, sd) %>%
    mutate(order_in_test = mean(data$math$order_in_test), id=0) %>%
    mutate(sd_grp = (sd | sd_baseline)*1) %>%
    left_join(number_subjects$math) %>%
    arrange(sd_grp, session),
  wm = data$wm %>% distinct(sd_baseline, session, sd) %>%
    select(sd_baseline, session, sd) %>%
    mutate(order_in_test = mean(data$wm$order_in_test), id=0) %>%
    mutate(sd_grp = (sd | sd_baseline)*1) %>%
    left_join(number_subjects$wm) %>%
    arrange(sd_grp, session),
  stm = data$stm %>% distinct(sd_baseline, session, sd) %>%
    select(sd_baseline, session, sd) %>%
    mutate(recall_session = mean(data$stm$recall_session), id=0, word=0) %>%
    mutate(sd_grp = (sd | sd_baseline)*1) %>%
    left_join(number_subjects$stm) %>%
    arrange(sd_grp, session),
  stroop = data$stroop %>% distinct(sd_baseline, session, sd, congruent,cognitive_update) %>%
    mutate(order_in_test = mean(data$stroop$order_in_test), id=0) %>% 
    mutate(sd_grp = (sd | sd_baseline)*1) %>%
    left_join(number_subjects$stroop) %>%
    arrange(sd_grp, session),
  rt = data$rt %>% distinct(sd_baseline, session, sd) %>%
    mutate(order_in_test = mean(data$rt$order_in_test), id=0) %>% 
    mutate(sd_grp = (sd | sd_baseline)*1) %>%
    left_join(number_subjects$rt) %>%
    arrange(sd_grp, session),
  kss = data$kss %>% distinct(sd_baseline, session, sd) %>%
    mutate(sd_grp = (sd | sd_baseline)*1) %>%
    left_join(number_subjects$kss) %>%
    arrange(sd_grp, session) %>% 
    na.omit
)

# datasets for merging  bootstrap results
save(newdata, file="data/wakeapp_newdata.RDta")

m=list(math=final_models$math,
       stm=final_models$stm,
       wm=final_models$wm,
       rt_lapse=final_models$rt_lapse,
       stroop_accuracy=final_models$stroop,
       math_rt=gaussian_final_models$math,
       rt=gaussian_final_models$rt,
       rt_var=gaussian_final_models$rt_var,
       stroop_conflict=gaussian_final_models$stroop_conflict,
       stroop_update=gaussian_final_models$stroop_update,
       stroop_conflict_rtvar=gaussian_final_models$stroop_conflict_rtvar,
       stroop_update_rtvar=gaussian_final_models$stroop_update_rtvar,
       kss=gaussian_final_models$kss)
                      
#### bootstrap functions ####

d=newdata

predict_stm=function(x) predict(x, newdata=d$stm, re.form=NA, allow.new.levels=TRUE, type="response")
predict_wm=function(x) predict(x, newdata=d$wm, re.form=NA, allow.new.levels=TRUE, type="response")
predict_math=function(x) predict(x, newdata=d$math, re.form=NA, allow.new.levels=TRUE, type="response")
predict_rt=function(x) predict(x, newdata=d$rt, re.form=NA, allow.new.levels=TRUE, type="response")
predict_stroop=function(x) predict(x, newdata=d$stroop, re.form = NA, allow.new.levels=TRUE, type="response")
predict_kss=function(x) predict(x, newdata=d$kss, re.form = NA, allow.new.levels=TRUE, type="response")

# simulate_stm=function(x) simulate(x, newdata=d$stm, re.form=NA, allow.new.levels=TRUE)[[1]]
# simulate_wm=function(x) simulate(x, newdata=d$wm, re.form=NA, allow.new.levels=TRUE)[[1]]
# simulate_math=function(x) simulate(x, newdata=d$math, re.form=NA, allow.new.levels=TRUE)[[1]]
# 
# simulate2_stm=function(x) simulate(x, newdata=d$stm, re.form=NA, allow.new.levels=TRUE, cond.sim=FALSE)[[1]]
# simulate2_wm=function(x) simulate(x, newdata=d$wm, re.form=NA, allow.new.levels=TRUE, cond.sim=FALSE)[[1]]
# simulate2_math=function(x) simulate(x, newdata=d$math, re.form=NA, allow.new.levels=TRUE, cond.sim=FALSE)[[1]]

#### Bootstrapping ####

nsim=100

# boot_predict_wm <- bootMer(m$wm, predict_wm, nsim=nsim, seed=2019)
# save(boot_predict_wm, file="data/100sim/boot_predict_wm.RDta")
 # boot_predict_stm = bootMer(m$stm, predict_stm, nsim=nsim, seed=2019)
 # save(boot_predict_stm, file="data/100sim/boot_predict_stm.RDta")
 # boot_predict_math <- bootMer(m$math, predict_math, nsim=nsim, seed=2019)
 # save(boot_predict_math, file="data/100sim/boot_predict_math.RDta")
# #boot_predict_math_rt <- bootMer(m$math_rt, predict_math, nsim=nsim, seed=2019)
# #save(boot_predict_math_rt, file="data/100sim/boot_predict_math_rt.RDta")
# #boot_predict_rt <- bootMer(m$rt, predict_rt, nsim=nsim, seed=2019)
# #save(boot_predict_rt, file="data/100sim/boot_predict_rt.RDta")
# boot_predict_stroop_update <- bootMer(m$stroop_update, predict_stroop, nsim=nsim, seed=2019) #totally non-sig model
# save(boot_predict_stroop_update, file="data/100sim/boot_predict_stroop_update.RDta")
# boot_predict_stroop_conflict <- bootMer(m$stroop_conflict, predict_stroop, nsim=nsim, seed=2019) #totally non-sig model
# save(boot_predict_stroop_conflict, file="data/100sim/boot_predict_stroop_conflict.RDta")
# #boot_predict_stroop_accuracy <- bootMer(m$stroop_accuracy, predict_stroop, nsim=nsim, seed=2019)
# #save(boot_predict_stroop_accuracy, file="data/100sim/boot_predict_stroop_accuracy.RDta")
# boot_predict_stroop_conflict_rtvar <- bootMer(m$stroop_conflict_rtvar, predict_stroop, nsim=nsim, seed=2019)
# save(boot_predict_stroop_conflict_rtvar, file="data/100sim/boot_predict_stroop_conflict_rtvar.RDta")
# boot_predict_stroop_update_rtvar <- bootMer(m$stroop_update_rtvar, predict_stroop, nsim=nsim, seed=2019)
# save(boot_predict_stroop_update_rtvar, file="data/100sim/boot_predict_stroop_update_rtvar.RDta")
# boot_predict_rt_var <- bootMer(m$rt_var, predict_rt, nsim=nsim, seed=2019)
# save(boot_predict_rt_var, file="data/100sim/boot_predict_rt_var.RDta")
# #boot_predict_rt_lapse <- bootMer(m$rt_lapse, predict_rt, nsim=nsim, seed=2019)
# #save(boot_predict_rt_lapse, file="data/100sim/boot_predict_rt_lapse.RDta")
# boot_predict_kss <- bootMer(m$kss, predict_kss, nsim=nsim, seed=2019)
# save(boot_predict_kss, file="data/100sim/boot_predict_kss.RDta")

# boot_simulate_wm <- bootMer(m$wm, simulate_wm, nsim=nsim, seed=2019)
# save(boot_simulate_wm, file="data/boot_simulate_wm.RDta")
# boot_simulate_stm = bootMer(m$stm, simulate_stm, nsim=nsim, seed=2019)
# save(boot_simulate_stm, file="data/boot_simulate_stm.RDta")
# boot_simulate_math <- bootMer(m$math, simulate_math, nsim=nsim, seed=2019)
# save(boot_simulate_math, file="data/boot_simulate_math.RDta")
# boot_simulate2_wm <- bootMer(m$wm, simulate2_wm, nsim=nsim, seed=2019)
# save(boot_simulate2_wm, file="data/boot_simulate2_wm.RDta")
# boot_simulate2_stm = bootMer(m$stm, simulate2_stm, nsim=nsim, seed=2019)
# save(boot_simulate2_stm, file="data/boot_simulate2_stm.RDta")
# boot_simulate2_math <- bootMer(m$math, simulate2_math, nsim=nsim, seed=2019)
# save(boot_simulate2_math, file="data/boot_simulate2_math.RDta")

#### Aggregating files ####

load("data/100sim/boot_predict_wm.RDta")
load("data/100sim/boot_predict_stm.RDta")
load("data/100sim/boot_predict_math.RDta")
load("data/100sim/boot_predict_math_rt.RDta")
load("data/100sim/boot_predict_rt.RDta")
load("data/100sim/boot_predict_stroop_accuracy.RDta")
load("data/100sim/boot_predict_rt_var.RDta")
load("data/100sim/boot_predict_rt_lapse.RDta")
load("data/100sim/boot_predict_stroop_conflict.RDta")
load("data/100sim/boot_predict_stroop_update.RDta")
load("data/100sim/boot_predict_stroop_conflict_rtvar.RDta")
load("data/100sim/boot_predict_stroop_update_rtvar.RDta")
load("data/100sim/boot_predict_kss.RDta")

# load("data/100sim/boot_simulate_wm.RDta")
# load("data/100sim/boot_simulate_stm.RDta")
# load("data/100sim/boot_simulate_math.RDta")
# 
# load("data/100sim/boot_simulate2_wm.RDta")
# load("data/100sim/boot_simulate2_stm.RDta")
# load("data/100sim/boot_simulate2_math.RDta")

bootstrap = list(
  newdata=newdata,
  wm=list(pred=boot_predict_wm#,
          #sim=boot_simulate_wm,
          #sim2=boot_simulate2_wm
          ),
  stm=list(pred=boot_predict_stm#,
          #sim=boot_simulate_stm,
          #sim2=boot_simulate2_stm
          ),
  math=list(pred=boot_predict_math#,
          #sim=boot_simulate_math,
          #sim2=boot_simulate2_math
          ),
  math_rt=list(pred=boot_predict_math_rt
          ),
  rt=list(pred=boot_predict_rt
          ),
  rt_var=list(pred=boot_predict_rt_var
              ),
  rt_lapse=list(pred=boot_predict_rt_lapse
                ),
  stroop_accuracy=list(pred=boot_predict_stroop_accuracy
                       ),
  stroop_conflict_rtvar=list(pred=boot_predict_stroop_conflict_rtvar
  ),
  stroop_update_rtvar=list(pred=boot_predict_stroop_update_rtvar
  ),
  kss=list(pred=boot_predict_kss),
  stroop_conflictRT = list(pred=boot_predict_stroop_conflict
                            ),
  stroop_updateRT = list(pred=boot_predict_stroop_update))

save(bootstrap, file="data/wakeapp_bootstrap.RDta")
