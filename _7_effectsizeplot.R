#calculating effect sizes
library(effsize)
library(questionr)
library(compute.es)
library(forestplot)

load("data/wakeapp_datasets.RDta")

########################Reaction time#####################
###RT
RT.only.TSD <- subset(KSS_Complete_TIDY,SD == "Sleep Deprivation")
RT.only.TSD$Baseline_or_treatment <- ifelse(RT.only.TSD$time == 0, "Baseline", "Treatment")
effsize::cohen.d(RT.only.TSD$reaction_time ~ RT.only.TSD$Baseline_or_treatment)

t.stat1 <- t.test(RT.only.TSD$reaction_time ~ RT.only.TSD$Baseline_or_treatment)$statistic
table(RT.only.TSD$Baseline_or_treatment)
compute.es::tes(-t.stat1, n.2 = 7130, n.1 = 2475, level = 95)


###RT variance
RTvar.only.TSD <- subset(RT_with_variance,SD == "Sleep Deprivation")
RTvar.only.TSD$Baseline_or_treatment <- ifelse(RTvar.only.TSD$time == 0, "Baseline", "Treatment")
effsize::cohen.d(RTvar.only.TSD$RT_var ~ RTvar.only.TSD$Baseline_or_treatment)

t.stat2 <- t.test(RTvar.only.TSD$RT_var ~ RTvar.only.TSD$Baseline_or_treatment)$statistic
table(RTvar.only.TSD$Baseline_or_treatment)
compute.es::tes(-t.stat2, n.2 = 88, n.1 = 262, level = 95)

### Lapses
fisher.test(RT.only.TSD$lapse1000,RT.only.TSD$Baseline_or_treatment)

prop.table(table(RT.only.TSD$lapse1000[which(RT.only.TSD$Baseline_or_treatment == "Baseline")])) #baseline
prop.table(table(RT.only.TSD$lapse1000[which(RT.only.TSD$Baseline_or_treatment == "Treatment")])) #Treatment

propes(0.992323232,0.96661992,2475,7130)

#######################Working memory#####################
WM.only.TSD <- subset(WM_Complete_TIDY,SD == "Sleep Deprivation")
WM.only.TSD$Baseline_or_treatment <- ifelse(WM.only.TSD$time == 0, "Baseline", "Treatment")
fisher.test(WM.only.TSD$correct,WM.only.TSD$Baseline_or_treatment)

prop.table(table(WM.only.TSD$correct[which(WM.only.TSD$Baseline_or_treatment == "Baseline")])) #baseline
prop.table(table(WM.only.TSD$correct[which(WM.only.TSD$Baseline_or_treatment == "Treatment")])) #Treatment

propes(0.1732804,0.2168626,756,2467)
########################Episodic memory###################

STM.only.TSD <- subset(STM_Complete_TIDY,SD == "Sleep Deprivation")
STM.only.TSD$Baseline_or_treatment <- ifelse(STM.only.TSD$time == 0, "Baseline", "Treatment")
fisher.test(STM.only.TSD$correct,STM.only.TSD$Baseline_or_treatment)

prop.table(table(STM.only.TSD$correct[which(STM.only.TSD$Baseline_or_treatment == "Baseline")])) #baseline
prop.table(table(STM.only.TSD$correct[which(STM.only.TSD$Baseline_or_treatment == "Treatment")])) #Treatment

propes(0.07355967,0.1092112,3888,11638)

########################MATHS############################
### Correct/incorrect
Maths.only.TSD <- subset(Maths_Complete_TIDY,SD == "Sleep Deprivation")
Maths.only.TSD$Baseline_or_treatment <- ifelse(Maths.only.TSD$time == 0, "Baseline", "Treatment")
fisher.test(Maths.only.TSD$correct,Maths.only.TSD$Baseline_or_treatment)

prop.table(table(Maths.only.TSD$correct[which(Maths.only.TSD$Baseline_or_treatment == "Baseline")])) #baseline
prop.table(table(Maths.only.TSD$correct[which(Maths.only.TSD$Baseline_or_treatment == "Treatment")])) #Treatment

propes(0.09916095,0.1286564,1311,4034)

###speed
effsize::cohen.d(Maths.only.TSD$time_m ~ Maths.only.TSD$Baseline_or_treatment)

t.stat3 <- t.test(Maths.only.TSD$time_m ~ Maths.only.TSD$Baseline_or_treatment)$statistic
table(Maths.only.TSD$Baseline_or_treatment)
compute.es::tes(-t.stat3, n.1 = 1311, n.2 = 4034, level = 95)

###total score
Maths_score.only.TSD <- subset(Maths_score_per_2mins,SD == "Sleep Deprivation")
Maths_score.only.TSD$Baseline_or_treatment <- ifelse(Maths_score.only.TSD$time == 0, "Baseline", "Treatment")
effsize::cohen.d(Maths_score.only.TSD$Score ~ Maths_score.only.TSD$Baseline_or_treatment)

t.stat4 <- t.test(Maths_score.only.TSD$Score ~ Maths_score.only.TSD$Baseline_or_treatment)$statistic
table(Maths_score.only.TSD$Baseline_or_treatment)
compute.es::tes(t.stat4, n.1 = 78, n.2 = 246, level = 95)

########################STROOP############################
###ERROR RATE
Stroop.only.TSD <- subset(Stroop_Complete_TIDY,SD == "Sleep Deprivation")
Stroop.only.TSD$Baseline_or_treatment <- ifelse(Stroop.only.TSD$time == 0, "Baseline", "Treatment")
fisher.test(Stroop.only.TSD$correct,Stroop.only.TSD$Baseline_or_treatment)

prop.table(table(Stroop.only.TSD$correct[which(Stroop.only.TSD$Baseline_or_treatment == "Baseline")])) #baseline
prop.table(table(Stroop.only.TSD$correct[which(Stroop.only.TSD$Baseline_or_treatment == "Treatment")])) #Treatment

propes(0.96893788,0.9649693,7984,24921)

###reaction time (correct only)
Stroop_correct.only.TSD <- subset(Stroop_complete_correct,SD == "Sleep Deprivation")
Stroop_correct.only.TSD$Baseline_or_treatment <- ifelse(Stroop_correct.only.TSD$time == 0, "Baseline", "Treatment")
effsize::cohen.d(Stroop_correct.only.TSD$reaction_time ~ Stroop_correct.only.TSD$Baseline_or_treatment)

t.stat5 <- t.test(Stroop_correct.only.TSD$reaction_time ~ Stroop_correct.only.TSD$Baseline_or_treatment)$statistic
table(Stroop_correct.only.TSD$Baseline_or_treatment)
compute.es::tes(t.stat5, n.1 = 7736, n.2 = 24048, level = 95)


### reaction time variance
Stroop_RTvariance_data
Stroop_rtvar.only.TSD <- subset(Stroop_RTvariance_data,SD == "Sleep Deprivation")
Stroop_rtvar.only.TSD$Baseline_or_treatment <- ifelse(Stroop_rtvar.only.TSD$time == 0, "Baseline", "Treatment")
effsize::cohen.d(Stroop_rtvar.only.TSD$RT_var ~ Stroop_rtvar.only.TSD$Baseline_or_treatment)

t.stat6 <- t.test(Stroop_rtvar.only.TSD$RT_var ~ Stroop_rtvar.only.TSD$Baseline_or_treatment)$statistic
table(Stroop_rtvar.only.TSD$Baseline_or_treatment)
compute.es::tes(t.stat6, n.1 = 323, n.2 = 996, level = 95)

##############################making plot##########################
forest.data <- read.csv("forestplotdata.csv", stringsAsFactors=FALSE)
colnames(forest.data)[1] <- "Variable" #sometimes this name is changed for some reason

## Labels defining subgroups are a little indented!
subgps <- c(2,3,4,7,8,11,14,17,18,19,20,21,24)
forest.data$Variable[subgps] <- paste("  ",forest.data$Variable[subgps]) 

## The rest of the columns in the table. 
tabletext <- cbind(c("Cognitive test","\n",forest.data$Variable), 
                   c("Cliff's delta","\n",forest.data$Cliffs.delta), 
                   c("Odds ratio","\n",forest.data$Odds.ratio))

tabletext[20,2] <-"0.10"

#making forestplot
png(file.path("Plots/Figure7_Forestplot.png"),width=960, height=640)
forestplot::forestplot(labeltext=tabletext, graph.pos=2, 
                       mean=c(NA,NA,forest.data$Point.Estimate), 
                       lower=c(NA,NA,forest.data$Low), upper=c(NA,NA,forest.data$High),
                       title="Cohen's d (95% CI)",
                       xlab="Effect size of change in cognitive performance following sleep deprivation",
                       hrzl_lines=list("2.5" = grid::gpar(lwd=1, col="#99999922"), 
                                       "10" = grid::gpar(lwd=100, lineend="butt", columns=c(2:4), col="#99999922"),
                                       "16" = grid::gpar(lwd=80, lineend="butt", columns=c(2:4), col="#99999922"),
                                       "26" = grid::gpar(lwd=52, lineend="butt", columns=c(2:4), col="#99999922")),
                       txt_gp=forestplot::fpTxtGp(label=grid::gpar(cex=1.25),
                                                  ticks=grid::gpar(cex=1.1),
                                                  xlab=grid::gpar(cex = 1.2),
                                                  title=grid::gpar(cex = 1.2)),
                       col=forestplot::fpColors(box="black", lines="black", zero = "gray50"),
                       zero=0, cex=0.9, lineheight = "auto", boxsize=0.3, colgap=grid::unit(6,"mm"),
                       lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.3)
dev.off()
