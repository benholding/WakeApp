library(tidyverse)
library(lme4)
library(lmerTest)

load("data/wakeapp_datasets.RDta")

data$rt

rtdata_per_trial <- data$rt %>% 
  group_by(id,session,sd,sd_baseline) %>% 
  summarise(lapse_count = sum(lapse1000),
            falsestart_count = sum(false_responses))

lapse_null_model <- lmer(lapse_count ~ 1 + (1|id), rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE)
lapse_baseline_model <- lmer(lapse_count ~ sd_baseline + session+ (1|id),rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE)
lapse_TSD_model <- lmer(lapse_count ~ sd_baseline  + session + sd + (1|id),rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE) #best
lapse_interaction_model <- lmer(lapse_count ~ sd_baseline  + session * sd+ (1|id),rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE)

lapse_anova_table <- anova(lapse_null_model,lapse_baseline_model,lapse_TSD_model,lapse_interaction_model)
summary(lapse_TSD_model)

#

falsestart_null_model <- lmer(falsestart_count ~ 1 + (1|id), rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE)
falsestart_baseline_model <- lmer(falsestart_count ~ sd_baseline + session+ (1|id),rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE)
falsestart_TSD_model <- lmer(falsestart_count ~ sd_baseline  + session + sd + (1|id),rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE) 
falsestart_interaction_model <- lmer(falsestart_count ~ sd_baseline  + session * sd+ (1|id),rtdata_per_trial,control=lmerControl(optimizer="bobyqa"),REML=FALSE)

falsestart_anova_table <- anova(falsestart_null_model,falsestart_baseline_model,falsestart_TSD_model,falsestart_interaction_model)
summary(falsestart_baseline_model)

##making tables
library(sjPlot)

##model comparison tables
tab_anova <- function(model, digits = 2, eps = .001, title = NULL, footnote=NULL){
  sjPlot::tab_df(broom::tidy(model) %>%
                   mutate(p.value = format.pval(p.value, digits = digits, eps = eps),
                          AIC=round(AIC,2),
                          BIC=round(BIC,2),
                          logLik=round(logLik,2),
                          deviance = round(deviance,2),
                          statistic = round(statistic,2),
                          term = c("Intercept-only", "Base", "Sleep Deprivation", "Sleep Deprivation x Session")) %>% 
                   select(" " = term, "DF" = df, AIC,BIC,"Log-Likelihood" = logLik, "Deviance" = deviance, "Chi-square" = statistic, "Chi DF" = df, p.value), 
                 digits = digits,title = title,footnote=footnote, show.footnote = T)
}

Footnote <- "Models are compared against the previous level of model complexity. 
A base model repesents a model with condition at baseline (22:00) and other factors, 
such as session (time-of-day) and order of stimulus within a given test. Interaction represents a interaction between the effect of sleep deprivation and time-of-day (session)"

TableS15_simpleattention_lapsecount <- tab_anova(lapse_anova_table, title="Table S15. Simple Attention Lapses (Counts) Model Comparions", footnote=Footnote)
TableS17_simpleattention_falsestartcount <- tab_anova(falsestart_anova_table, title="Table S17. Simple Attention False Starts (Counts) Model Comparions", footnote=Footnote)

tab_model(lapse_TSD_model, title = "Table S16. Simple Attention Lapses (Counts)", show.re.var=F, show.icc=F)

tab_model(falsestart_baseline_model, title = "Table S18. Simple Attention Fakse Starts (Counts)", show.re.var=F, show.icc=F)

