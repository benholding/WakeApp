library(plyr)
library(tidyverse)
library(magrittr)
library(lme4)
library(cowplot)


#### Read data ####

#load("data/wakeapp_binary_models.RDta")
#load("data/wakeapp_gaussian_models.RDta")
load("data/wakeapp_gaussian_final_models.RDta")
load("data/wakeapp_final_models.RDta")
load("data/wakeapp_bootstrap.RDta")
load("data/wakeapp_datasets.RDta")

newdata=bootstrap$newdata
models=list(math=final_models$math,
            math_rt=gaussian_final_models$math,
            stm=final_models$stm,
            wm=final_models$wm,
            rt=gaussian_final_models$rt,
            rt_var=gaussian_final_models$rt_var,
            rt_lapse=final_models$rt_lapse,
            stroop_conflict=gaussian_final_models$stroop_conflict,
            stroop_update=gaussian_final_models$stroop_update,
            stroop_accuracy=final_models$stroop,
            stroop_conflict_rtvar=gaussian_final_models$stroop_conflict_rtvar,
            stroop_update_rtvar=gaussian_final_models$stroop_update_rtvar,
            kss=gaussian_final_models$kss)

#### Functions ####
extractci <- function(x, q) {
  aaply(x$t,2,function(x) c(quantile(x,q)))
}

sefrom95ci = function(lwr, upr) {
  (log(upr)-log(lwr))/(qnorm(.975)*2)
}

plotit = function(d, m, b, title, ylab,xlab,f) {
  
  d %<>% bind_cols(tibble(pred=predict(m, newdata=d, allow.new.levels=T, re.form=NA, type="response"))) %>%
    mutate(lwr1=extractci(b$pred, .025), upr1=extractci(b$pred, .975)) %>%
    mutate(not_baseline_session = replace(session, session=="0", NA)) %>%
    mutate(sd_grp=factor(sd_grp, levels=c(0,1), labels=c("Normal sleep", "Sleep deprivation")))
  
  pd <- position_dodge(0.2) 
  
  ggplot(d, aes(x=as.numeric(session), y=pred, colour=sd_grp, shape = sd_grp))  +
    geom_line(linetype="dashed") +
    geom_line(linetype="solid", aes(x=as.numeric(not_baseline_session))) +
    scale_colour_manual(values=c("#999999","#000000")) + 
    geom_pointrange(aes(ymin=lwr1, ymax=upr1), width=.1, position=pd) +
    ggtitle(title) +
    ylab(ylab) +
    xlab(xlab) +
    guides(colour=guide_legend(title=NULL), shape = guide_legend(title=NULL)) +
    theme_minimal() + 
    scale_size_manual(values = c(5,1)) +
    scale_x_continuous(labels=c("22:30", "08:00","12:30","16:30")) # +
    #facet_grid(f)
}

#### plot data ####

p1=plotit(newdata$rt,models$rt, bootstrap$rt, title = NULL, 
          ylab="Response time (ms)",xlab="Session")
p2=plotit(newdata$rt,models$rt_lapse, bootstrap$rt_lapse, title = NULL, 
          ylab="Probability of a lapse",xlab="Session")
p3=plotit(newdata$rt,models$rt_var, bootstrap$rt_var, title = NULL, 
          ylab="Response time variation (SD)",xlab="Session")


p4=plotit(newdata$math, models$math, bootstrap$math, title=NULL, ylab="Probability of an mistake",xlab="Session")

p5=plotit(newdata$math,models$math_rt, bootstrap$math_rt, title = NULL, 
          ylab="Response Time (ms)",xlab="Session")

p6=plotit(newdata$stm, models$stm, bootstrap$stm, title=NULL, ylab="Probability of an mistake",xlab="Session")

p7=plotit(newdata$wm, models$wm, bootstrap$wm, title=NULL, ylab="Probability of an mistake",xlab="Session")

p8= plotit(newdata$kss,models$kss, bootstrap$kss, title = NULL, 
            ylab="Subjective sleepiness",xlab="Session")

 p9=plotit(newdata$stroop,models$stroop_accuracy, bootstrap$stroop_accuracy, title = NULL, 
           ylab="Probability of a mistake",xlab="Test session", f = NA)
#
# p8=plotit(newdata$stroop,models$stroop_conflict, bootstrap$stroop_conflictRT, title = "Stroop, conflict", 
#           ylab="Response Time (ms)",xlab="Test session", f = ~congruent)
# 
# p9=plotit(newdata$stroop,models$stroop_update, bootstrap$stroop_updateRT, title = "Stroop, update", 
#           ylab="Response Time (ms)",xlab="Test session", f = ~cognitive_update)
# 
 p10=plotit(newdata$stroop,models$stroop_conflict_rtvar, bootstrap$stroop_conflict_rtvar, title = NULL, 
         ylab="Response time variation (SD)",xlab="Session", f = ~congruent)
# 
p11=plotit(newdata$stroop,models$stroop_update_rtvar, bootstrap$stroop_update_rtvar, title = NULL, 
           ylab="Response time variation (SD)",xlab="Session", f = ~cognitive_update)

library(ggpubr)
plot_simpleattention <- ggarrange(p1,p2,p3, ncol=3,common.legend=T,labels="AUTO")
ggsave("plots/figure1_simpleattention.pdf", plot=plot_simpleattention, device="pdf", dpi=300, units="cm", width=30, height=10)

plot_arithmetic <- ggarrange(p4,p5, ncol=2,common.legend=T,labels="AUTO")
ggsave("plots/figure2_arithmetic.pdf", plot=plot_arithmetic, device="pdf", dpi=300, units="cm", width=20, height=10)

plot_episodic <- p6 + theme(legend.position="top")
ggsave("plots/figure3_episodic.pdf", plot=plot_episodic, device="pdf", dpi=300, units="cm", width=10, height=10)

plot_workingmemory <- p7 + theme(legend.position="top")
ggsave("plots/figure4_workingmemory.pdf", plot=plot_workingmemory, device="pdf", dpi=300, units="cm", width=10, height=10)

plot_stroop <- ggarrange(p9,p10,p11, ncol=3,common.legend=T,labels="AUTO")
ggsave("plots/figure5_stroop.pdf", plot=plot_stroop, device="pdf", dpi=300, units="cm", width=30, height=10)

plot_kss <- p8 + theme(legend.position="top") + scale_y_continuous(limits = c(1,9), breaks = c(1:9))
ggsave("plots/figure6_sleepiness.pdf", plot=plot_kss, device="pdf", dpi=300, units="cm", width=10, height=10)

