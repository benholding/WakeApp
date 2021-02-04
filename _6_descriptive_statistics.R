#descriptive statistics

load("data/wakeapp_datasets.RDta")

# participants section
x <- sort(unique(as.vector(unlist(lapply(data, function(x) x$id))))) #all participants that at least gave one bit of data
length(x) #182

key <- read.csv("MasterKey.csv")
all.equal(key$ID,x)

mean(key$Age)
sd(key$Age)
table(key$SD)
table(key$Woman)

# chronotype
library(dplyr)
screening_info <- read.csv("screening_responses.csv")[,c(1,44)]
colnames(screening_info) <- c("ID", "chronotype")
screening_info$chronotype_numeric <- c(4,2,5,1,3)[as.numeric(screening_info$chronotype)]

all_participants_with_ages_and_screening <- left_join(key, screening_info)

all_participants_with_ages_and_screening %>%	
  group_by(SD) %>%
  summarise_at(vars(chronotype_numeric), funs(mean(., na.rm=TRUE)))

all_participants_with_ages_and_screening %>%	
  group_by(SD) %>%
  summarise_at(vars(chronotype_numeric), funs(sd(., na.rm=TRUE)))

t.test(chronotype_numeric ~ SD, all_participants_with_ages_and_screening)

#other demographics split by condition
# AGE
key %>%	
  group_by(SD) %>%
  summarise_at(vars(Age), funs(mean(., na.rm=TRUE),
                               sd(., na.rm=TRUE)))

# GENDER
key %>%	
  group_by(SD) %>%
  tally(Woman)

# stroop raw data plots
library(ggplot2)

stroop_plot_data <- 
  data$stroop %>% group_by(id,session,sd_group) %>% 
  summarise(mean_mistakes = mean(error)) #aggregated over participants
  
pd <- position_dodge(0.2) 
  
  ggplot(stroop_plot_data, aes(x=as.numeric(session), y=mean_mistakes, colour=sd_group))  +
    stat_summary(fun=mean, geom="line", linetype="solid",position=pd) +
    stat_summary(fun=mean, geom="point", position=pd)+
    stat_summary(fun.data=mean_se, geom="errorbar", width=0.2,position=pd) + #standard error
    scale_colour_brewer(palette = "Dark2") +
    ylab("Mean Mistakes %") +
    xlab("Session") +
    guides(colour=guide_legend(title=NULL)) +
    theme_minimal() +
    scale_x_continuous(labels=c("22:30", "08:00","12:30","16:30"))+ 
    scale_y_continuous(labels = scales::percent)
  
stroop_ConflictRT_plot_data <- 
  data$stroop_conflict %>% group_by(id,session,sd_group) %>% 
  summarise(mean_conflictcost = mean(conflict_cost)) #aggregated over participants

ggplot(stroop_ConflictRT_plot_data, aes(x=as.numeric(session), y=mean_conflictcost, colour=sd_group))  +
  stat_summary(fun=mean, geom="line", linetype="solid",position=pd) +
  stat_summary(fun=mean, geom="point", position=pd)+
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2,position=pd) + #standard error
  scale_colour_brewer(palette = "Dark2") +
  ylab("Conflict cost (increase in RT in ms)") +
  xlab("Session") +
  guides(colour=guide_legend(title=NULL)) +
  theme_minimal() +
  scale_x_continuous(labels=c("22:30", "08:00","12:30","16:30")) 

stroop_UpdateRT_plot_data <- 
  data$stroop_update %>% group_by(id,session,sd_group) %>% 
  summarise(mean_update_gain = mean(update_gain)) #aggregated over participants

ggplot(stroop_UpdateRT_plot_data, aes(x=as.numeric(session), y=mean_update_gain, colour=sd_group))  +
  stat_summary(fun=mean, geom="line", linetype="solid",position=pd) +
  stat_summary(fun=mean, geom="point", position=pd)+
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2,position=pd) + #standard error
  scale_colour_brewer(palette = "Dark2") +
  ylab("Update gain (change in RT in ms)") +
  xlab("Session") +
  guides(colour=guide_legend(title=NULL)) +
  theme_minimal() +
  scale_x_continuous(labels=c("22:30", "08:00","12:30","16:30")) 

# prior sleep
actigraphs <- read.csv("SleSI_actigraphy_Complete_2.csv")
actigraphs$Morning <- as.numeric(as.character(actigraphs$Morning))
actigraphs2 <- cbind(actigraphs$FP, actigraphs$SD., actigraphs$Morning, actigraphs$Time.in.bed..min., actigraphs$Assumed.sleep.time..min.)
colnames(actigraphs2) <- c("ID", "SD", "Night", "TIB", "TST")
actigraphs2 <- as.data.frame(actigraphs2)
actigraphs2$ID <- as.integer(actigraphs2$ID)

actigraphs_and_key_data <- left_join(key, actigraphs2, by = c("ID", "SD"))

#average time in bed per participant prior
temp1 <- actigraphs_and_key_data[which(actigraphs_and_key_data$Night==4),]
temp2 <- temp1[which(temp1$SD==0),] #some data NA due to missing actigraphy data

# sleep duration for controls day prior
## mean
WakeApp.TST <- mean(temp2$TST, na.rm = T)/60
paste(floor(WakeApp.TST), round((WakeApp.TST-floor(WakeApp.TST))*60), sep=":")

## standard deviation
sd(temp2$TST, na.rm = T)

#comparing nights 1-3 between conditions
pre_study_sleep <- actigraphs_and_key_data[which(actigraphs_and_key_data$Night!=4),]

pre_study_sleep_hours <- pre_study_sleep %>%
  group_by(SD) %>%
  summarise_at(vars(TST), funs(mean(., na.rm=TRUE)/60))

paste(floor(pre_study_sleep_hours$TST), round((pre_study_sleep_hours$TST-floor(pre_study_sleep_hours$TST))*60), sep=":")

pre_study_sleep_hours_stdev <- pre_study_sleep %>%
  group_by(SD) %>%
  summarise_at(vars(TST), funs(sd(., na.rm=TRUE)/60))

paste(floor(pre_study_sleep_hours_stdev$TST), round((pre_study_sleep_hours_stdev$TST-floor(pre_study_sleep_hours_stdev$TST))*60), sep=":")



#assessing average finishing time for first wakeapp test
time1 <- data$kss %>% 
  filter(order_t == 1) %>% 
  mutate(CLOCK = as.numeric(as.difftime(as.character(clock), format="%H:%M:%S", units="hours"))) %>% 
  group_by(sd_group,time) %>% 
  summarise(decimal_time_mean = mean(ifelse(CLOCK < 4, CLOCK + 24, CLOCK),na.rm=T),
            decimal_time_sd = sd(ifelse(CLOCK < 4, CLOCK + 24, CLOCK),na.rm=T)) %>% 
  mutate(twentyfourhour_time = paste(floor(decimal_time_mean), round((decimal_time_mean-floor(decimal_time_mean))*60), sep=":"))%>% 
  mutate(twentyfourhour_time_sd = paste(floor(decimal_time_sd), round((decimal_time_sd-floor(decimal_time_sd))*60), sep=":"))

#creating reaction time response distribution
attention_hist <- ggplot(data$rt, aes(x = reaction_time)) +
  geom_histogram(binwidth = 20) +
  ggtitle("Simple Attention: Histogram of Response Times") +
  xlab("Response Time (ms)") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

Arithmetic_hist <- ggplot(data$math, aes(x = time_m)) +
  geom_histogram(binwidth = 20) +
  ggtitle("Arithmetic: Histogram of Response Times") +
  xlab("Response Time (ms)") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

#no hist since time wasn't measured

wm_hist <- ggplot(data$wm, aes(x = response_time*1000)) +
  geom_histogram(binwidth = 20) +
  ggtitle("Working memory: Histogram of Response Times") +
  xlab("Response Time (ms)") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

stroop_hist <- ggplot(data$stroop, aes(x = reaction_time)) +
  geom_histogram(binwidth = 20) +
  ggtitle("Stroop: Histogram of Response Times") +
  xlab("Response Time (ms)") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

library(ggpubr)
plot_histograms <- ggarrange(attention_hist,Arithmetic_hist,wm_hist,stroop_hist, ncol=2,nrow=2, common.legend=T,labels="AUTO")
ggsave("plots/figureS7_taskhists.pdf", plot=plot_histograms, device="pdf", dpi=300, units="cm", width=25, height=20)

# checking if incongruent stroop is slower than congruent
stroop_rt_congruent <- data$stroop$reaction_time[which(data$stroop$congruent == "Congruent")]
stroop_rt_incongruent <- data$stroop$reaction_time[which(data$stroop$congruent == "Incongruent")]
t.test(x=stroop_rt_congruent, y=stroop_rt_incongruent)
