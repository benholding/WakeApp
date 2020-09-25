library(lme4)
load("data/wakeapp_binary_models.RDta")
load("data/wakeapp_gaussian_models.RDta")
load("data/wakeapp_datasets.RDta")

table <- list()

################################# BINARY MODELS ###############################

#### AIC + LR test - stm ####
m=models$stm

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_ses, m[[fe]]$re_crossed)

re="re_crossed"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$stm <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - math ####

m=models$math

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_rc, m[[fe]]$re_id_ses)

re="re_id_rc"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$math <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - wm ####

m=models$wm

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_rc, m[[fe]]$re_id_ses)

re="re_id"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
anova(m$base[[re]], m$interaction[[re]])
table$wm <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$wm2 <- anova(m$base[[re]], m$interaction[[re]])

#### AIC + LR test - RT LAPSES ####
m=models$rt

fe="interaction"
anova(m[[fe]]$re_id,  m[[fe]]$re_id_ses)

re="re_id_ses"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$rt_lapses <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - STROOP mistakes ####
m=models$stroop

fe="interaction"
anova(m[[fe]]$re_id,  m[[fe]]$re_id_ses)

re="re_id_ses"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$stroop_mistakes <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

##### combining best models
final_models=
  list(
    stm=models$stm$interaction$re_crossed,
    math=models$math$sd$re_id_rc,
    wm=models$wm$interaction$re_id,
    rt_lapse=models$rt$sd$re_id,
    stroop=models$stroop$base$re_id_ses)

save(final_models, file="data/wakeapp_final_models.RDta")


################################# GAUSSIAN MODELS ###############################
#### AIC + LR test - math reaction time ####

m=gaussian_models$math

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_rc, m[[fe]]$re_id_ses)

re="re_id_rc"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$math_rt <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - reaction time ####

m=gaussian_models$rt

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_rc, m[[fe]]$re_id_ses)

re="re_id_ses"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$rt <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - stroop, conflict ####
m=gaussian_models$stroop_conflict

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_ses)

re="re_id"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$stroop_conflict_rt <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - stroop, update ####
m=gaussian_models$stroop_update

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_rc, m[[fe]]$re_id_ses)

re="re_id"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$stroop_update_rt <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - stroop, conflict _ RTVAR ####
m=gaussian_models$stroop_conflict_rtvar

re="re_id"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$stroop_conflict_rtvar <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - stroop, update _ RTVAR ####
m=gaussian_models$stroop_update_rtvar

re="re_id"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
anova(m$null[[re]], m$sd[[re]])
table$stroop_update_rtvar <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$stroop_update_rtvar2 <- anova(m$null[[re]], m$sd[[re]])

#### AIC + LR test - reaction time VAR ####

m=gaussian_models$rt_var

re="re_id"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$rtvar <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### AIC + LR test - math reaction time ####

m=gaussian_models$kss

fe="interaction"
anova(m[[fe]]$re_id, m[[fe]]$re_id_rc, m[[fe]]$re_id_ses)

re="re_id_ses"
anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])
table$kss <- anova(m$null[[re]], m$base[[re]], m$sd[[re]], m$interaction[[re]])

#### final models ####

gaussian_final_models=
  list(
  math=gaussian_models$math$interaction$re_id_rc,
  rt=gaussian_models$rt$sd$re_id_ses,
  stroop_conflict=gaussian_models$stroop_conflict$null$re_id,
  stroop_update=gaussian_models$stroop_update$null$re_id,
  rt_var=gaussian_models$rt_var$interaction$re_id,
  stroop_conflict_rtvar=gaussian_models$stroop_conflict_rtvar$base$re_id,
  stroop_update_rtvar=gaussian_models$stroop_update_rtvar$base$re_id,
  kss=gaussian_models$kss$interaction$re_id_ses)

save(gaussian_final_models, file="data/wakeapp_gaussian_final_models.RDta")

library(sjPlot)
#final model tables
final_model_tables <- list()
final_model_tables$T1simple_attention_table <- tab_model(gaussian_final_models$rt, final_models$rt_lapse, gaussian_final_models$rt_var, title = "Table 1. Simple attention", show.re.var=F, show.icc=F)
final_model_tables$T2Arithmetic_table <- tab_model(final_models$math, gaussian_final_models$math, title = "Table 2. Arithmetic Ability", show.re.var=F, show.icc=F)
final_model_tables$T3STM_table <- tab_model(final_models$stm, title = "Table 3. Short-term Memory", show.re.var=F, show.icc=F)
final_model_tables$T4WM_table <- tab_model(final_models$wm, title = "Table 4. Working memory", show.re.var=F, show.icc=F)
final_model_tables$T5Stroop_accuracy <- tab_model(final_models$stroop, title = "Table 5. Stroop overall accuracy", show.re.var=F, show.icc=F) #supplement?
final_model_tables$T6Stroop_rt_conflict <-tab_model(gaussian_final_models$stroop_conflict,gaussian_final_models$stroop_conflict_rtvar, title = "Table 6. Stroop cognitive conflict reaction time", show.re.var=F, show.icc=F) #supplement?
final_model_tables$T7Stroop_rt_update <- tab_model(gaussian_final_models$stroop_update, gaussian_final_models$stroop_update_rtvar, title = "Table 7. Stroop updating reaction time", show.re.var=F, show.icc=F)
final_model_tables$T8KSS <- tab_model(gaussian_final_models$kss, title = "Table 8. KSS", show.re.var=F, show.icc=F)


save(final_model_tables, file="data/wakeapp_finalmodeltables.RDta")

#model comparison tables
library(dplyr)
tab_anova <- function(model, digits = 2, eps = .001, title = NULL, footnote=NULL){
  sjPlot::tab_df(broom::tidy(model) %>%
                   mutate(p.value = format.pval(p.value, digits = digits, eps = eps),
                          AIC=round(AIC,2),
                          BIC=round(BIC,2),
                          logLik=round(logLik,2),
                          deviance = round(deviance,2),
                          statistic = round(statistic,2),
                          term = c("Intercept-only", "Base", "Sleep Deprivation", "Sleep Deprivation x Session")) %>% 
                   select(" " = term, "DF" = df, AIC,BIC,"Log-Likelihood" = logLik, "Deviance" = deviance, "Chi-square" = statistic, "Chi DF" = Chi.Df, p.value), 
  digits = digits,title = title,footnote=footnote, show.footnote = T)
}

Footnote <- "Models are compared against the previous level of model complexity. 
A base model repesents a model with condition at baseline (22:00) and other factors, 
such as session (time-of-day) and order of stimulus within a given test. Interaction represents a interaction between the effect of sleep deprivation and time-of-day (session)"
model_comparison_tables <- list()
model_comparison_tables$TableS1_simpleattention_RT <- tab_anova(table$rt, title="Table S1. Simple Attention Reaction-Time Model Comparions", footnote=Footnote)
model_comparison_tables$TableS2_simpleattention_lapses <- tab_anova(table$rt_lapses, title="Table S2. Simple Attention Lapses Model Comparions", footnote=Footnote)
model_comparison_tables$TableS3_simpleattention_RTvar <- tab_anova(table$rtvar, title="Table S3. Simple Attention Reaction-Time Variation Model Comparions", footnote=Footnote)

model_comparison_tables$TableS4_arithmetic <- tab_anova(table$math, title="Table S4. Arithmetic Accuracy Model Comparions", footnote=Footnote)
model_comparison_tables$TableS5_arithmetic_RT <- tab_anova(table$math_rt, title="Table S5. Arithmetic Reaction-Time Model Comparions", footnote=Footnote)

model_comparison_tables$TableS6_STM <- tab_anova(table$stm, title="Table S6. Short-term memory Accuracy Model Comparions", footnote=Footnote)

model_comparison_tables$TableS7a_WM <- tab_anova(table$wm, title="Table S7a. Working Memory Accuracy Model Comparions", footnote=Footnote)
model_comparison_tables$TableS7b_WM <- tab_df(broom::tidy(table$wm2) %>%  mutate(p.value = format.pval(p.value, digits = 1, eps = .001),
                                                                                AIC=round(AIC,2),
                                                                                BIC=round(BIC,2),
                                                                                logLik=round(logLik,2),
                                                                                deviance = round(deviance,2),
                                                                                statistic = round(statistic,2)), title="Table S7b. Working Memory Accuracy Model Comparions")

model_comparison_tables$TableS8_Stroop_mistakes <- tab_anova(table$stroop_mistakes, title="Table S8. Stroop Mistakes Model Comparions", footnote=Footnote)
model_comparison_tables$TableS9_Stroop_conflict_RT <- tab_anova(table$stroop_conflict_rt, title="Table S9. Stroop Conflict Reaction-Time Model Comparions", footnote=Footnote)
model_comparison_tables$TableS10_Stroop_conflict_RTvar <- tab_anova(table$stroop_conflict_rtvar, title="Table S10. Stroop Conflict Reaction-Time Variation Model Comparions", footnote=Footnote)
model_comparison_tables$TableS11_Stroop_update_RT <- tab_anova(table$stroop_update_rt, title="Table S11. Stroop Update Reaction-Time Model Comparions", footnote=Footnote)
model_comparison_tables$TableS12_Stroop_update_RTvar <- tab_anova(table$stroop_update_rtvar, title="Table S12. Stroop Update Reaction-Time Variation Model Comparions", footnote=Footnote)
model_comparison_tables$TableS13_KSS <- tab_anova(table$kss, title="Table S13. KSS score Model Comparions", footnote=Footnote)

save(model_comparison_tables, file="data/wakeapp_supplementarytables.RDta")
