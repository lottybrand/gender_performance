
# Plotting condition pattern for each question. Keep questions how they were asked (i.e. NON-REVERSED) for this

# load my color palette
nice_cols <- c("#ff9b54","#500c0b","#689689","#2E4057","#b2e6d4","#d4a686")
david_cols <- c("#5494f2", "#C4DFFF","#FFF0C2","#FFC099", "#8F0000")

library(ggplot2)
library(ggstats)
library(scales)

##### reload data NON REVERSED ####

solo <- read.csv("solo_final_151.csv")
peer <- read.csv("peer_final_150.csv")
city <- read.csv("citymen_final_152.csv")
elder <- read.csv("elders_final_149.csv")
#
# # select subset of relevant variables in each file before merging:
#
# colnames(solo)
#
sub_solo <- subset(solo, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20"))
sub_peer <- subset(peer, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20"))
sub_city <- subset(city, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20"))
sub_elder <- subset(elder, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20"))

# # add Condition Column
#
sub_solo$condition <- "control"
sub_peer$condition <- "peer"
sub_city$condition <- "city"
sub_elder$condition <- "elder"
#
# # merge into one
 data_full <- rbind(sub_solo,sub_peer,sub_city,sub_elder)
#
# # wide to long:
datalong <- reshape(data_full, idvar = "ppt",
                    varying = list(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
                    v.names = c("empowerment_support"),
                    direction = "long")

 colnames(datalong)[colnames(datalong) == 'time'] <- 'question'
#
# # reverse all so 5 is always strongly agree
 table(datalong$empowerment_support)
 datalong$empowerment_support <- 6 - datalong$empowerment_support
#
#
 write.csv(datalong, file="datalong_NV.csv", row.names=FALSE)

setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")

library(ggplot2)
library(ggstats)
library(scales)


##### load full data #####

setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")
fulldata <- read.csv("datalong_NV.csv")
setwd("~/Desktop/CES FELLOWSHIP/gender_performance")
fulldata$conditionf <- as.character(fulldata$condition)
fulldata$conditionf = factor(fulldata$condition, levels=c('control','peer','elder','city'))
levels(fulldata$conditionf) <- c("Control Condition","Peer Condition","Elder Condition","City Condition")
fulldata$empowerment_support <- as.character(fulldata$empowerment_support)

##### PER QUESTION PLOTS #####

Q1data <- fulldata[fulldata$question==1,]
Q1facet <- ggplot(Q1data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A man should have the final say about decisions in his home") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q1facet
ggsave(path = "plots", filename = "Q1.png", width = 8, height = 6)

#####

Q2data <- fulldata[fulldata$question==2,]
Q2facet <- ggplot(Q2data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.76)) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A woman should be able to travel to visit her family,\n even if against her husband’s wishes") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q2facet 
ggsave(path = "plots", filename = "Q2.png", width = 8, height = 6)
#####

Q3data <- fulldata[fulldata$question==3,]
Q3facet <- ggplot(Q3data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Women can be as effective leaders as men") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q3facet 
ggsave(path = "plots", filename = "Q3.png", width = 8, height = 6)
#####

Q4data <- fulldata[fulldata$question==4,]
Q4facet <- ggplot(Q4data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Women should be involved in senior community leadership (i.e. Mwenye kiti)") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q4facet 
ggsave(path = "plots", filename = "Q4.png", width = 8, height = 6)
#####

Q5data <- fulldata[fulldata$question==5,]
Q5facet <- ggplot(Q5data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Women should be educated to the same level as men") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q5facet 
ggsave(path = "plots", filename = "Q5.png", width = 8, height = 6)
#####

Q6data <- fulldata[fulldata$question==6,]
Q6facet <- ggplot(Q6data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.76)) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A man is justified in hitting his wife \nif she argues with him") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q6facet 
ggsave(path = "plots", filename = "Q6.png", width = 8, height = 6)
#####

Q7data <- fulldata[fulldata$question==7,]
Q7facet <- ggplot(Q7data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A man is justified in hitting his wife if she refuses to have sex with him") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q7facet 
ggsave(path = "plots", filename = "Q7.png", width = 8, height = 6)
#####

Q8data <- fulldata[fulldata$question==8,]
Q8facet <- ggplot(Q8data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("The more income a woman contributes to a household, the more say she should have in household decisions") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q8facet 
ggsave(path = "plots", filename = "Q8.png", width = 8, height = 6)
#####
Q9data <- fulldata[fulldata$question==9,]
Q9facet <- ggplot(Q9data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Women should hand her earnings over to her husband and ask his permission to spend it") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q9facet 
ggsave(path = "plots", filename = "Q9.png", width = 8, height = 6)
#####
Q10data <- fulldata[fulldata$question==10,]
Q10facet <- ggplot(Q10data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A woman should not earn more money than her husband") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q10facet 
ggsave(path = "plots", filename = "Q10.png", width = 8, height = 6)
#####
Q11data <- fulldata[fulldata$question==11,]
Q11facet <- ggplot(Q11data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.76)) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A husband should respect his wife's choice\n to use a condom during sexual activity") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q11facet
ggsave(path = "plots", filename = "Q11.png", width = 8, height = 6)
#####
Q12data <- fulldata[fulldata$question==12,]
Q12facet <- ggplot(Q12data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("If a husband and wife disagree on their desired number of children, \n the wife should defer to her husband's perspective") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q12facet
ggsave(path = "plots", filename = "Q12.png", width = 8, height = 6)
#####
Q13data <- fulldata[fulldata$question==13,]
Q13facet <- ggplot(Q13data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A man is the one who decides when to have sex with his wife") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q13facet
ggsave(path = "plots", filename = "Q13.png", width = 8, height = 6)
#####
Q14data <- fulldata[fulldata$question==14,]
Q14facet <- ggplot(Q14data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Married women should not own their own land") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q14facet
ggsave(path = "plots", filename = "Q14.png", width = 8, height = 6)
#####
Q15data <- fulldata[fulldata$question==15,]
Q15facet <- ggplot(Q15data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Married women should be able to own and manage their own business") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q15facet
ggsave(path = "plots", filename = "Q15.png", width = 8, height = 6)
#####
Q16data <- fulldata[fulldata$question==16,]
Q16facet <- ggplot(Q16data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("It is ok for a woman to be more educated than her husband") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q16facet
ggsave(path = "plots", filename = "Q16.png", width = 8, height = 6)
#####
Q17data <- fulldata[fulldata$question==17,]
Q17facet <- ggplot(Q17data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A woman should be free to divorce (or leave) her husband even if he does not wish to end the marriage") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q17facet
ggsave(path = "plots", filename = "Q17.png", width = 8, height = 6)
#####
Q18data <- fulldata[fulldata$question==18,]
Q18facet <- ggplot(Q18data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A woman should have an equal say in how the household spends their money") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q18facet
ggsave(path = "plots", filename = "Q18.png", width = 8, height = 6)
#####
Q19data <- fulldata[fulldata$question==19,]
Q19facet <- ggplot(Q19data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A wife should be able to prevent her husband from taking another wife, if it is against her wishes") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q19facet
ggsave(path = "plots", filename = "Q19.png", width = 8, height = 6)
#####
Q20data <- fulldata[fulldata$question==20,]
Q20facet <- ggplot(Q20data, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Only women and girls should wash and feed young children") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
Q20facet
ggsave(path = "plots", filename = "Q20.png", width = 8, height = 6)


##### checking modal responses #####

table(Q11data$empowerment_support, Q11data$condition)
table(Q12data$empowerment_support, Q12data$condition)
table(Q19data$empowerment_support, Q19data$condition)
table(Q14data$empowerment_support, Q14data$condition)

##### Four Examples For Paper ####
Q2data <- fulldata[fulldata$question==2,]
Q2peersolo <- Q2data[(Q2data$condition=="control"|Q2data$condition=="peer"),]
Q2facet <- ggplot(Q2peersolo, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.76)) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A woman should be able to travel to visit her family,\n even if against her husband’s wishes") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"),plot.title = element_text(hjust = 0.5))
Q2facet 
ggsave(path = "plots", filename = "Q2ps.png", width = 7, height = 3)

Q5data <- fulldata[fulldata$question==5,]
Q5peersolo <- Q5data[(Q5data$condition=="control"|Q5data$condition=="peer"),]
Q5facet <- ggplot(Q5peersolo, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("Women should be educated to the same \n level as men") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"), plot.title = element_text(hjust = 0.5))
Q5facet 
ggsave(path = "plots", filename = "Q5ps.png", width = 7, height = 3)



Q6data <- fulldata[fulldata$question==6,]
Q6peersolo <- Q6data[(Q6data$condition=="control"|Q6data$condition=="peer"),]
Q6facet <- ggplot(Q6peersolo, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.76)) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A man is justified in hitting his wife \nif she argues with him") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"),plot.title = element_text(hjust = 0.5))
Q6facet 
ggsave(path = "plots", filename = "Q6ps.png", width = 7, height = 3)


Q11data <- fulldata[fulldata$question==11,]
Q11peersolo <- Q11data[(Q11data$condition=="control"|Q11data$condition=="peer"),]
Q11facet <- ggplot(Q11peersolo, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.76)) +
  facet_wrap(~conditionf) +
  xlab("Agreement") + ylab("Percentage") +
  labs(fill='Agreement') +
  ggtitle("A husband should respect his wife's choice\n to use a condom during sexual activity") +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \nnor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"),plot.title = element_text(hjust = 0.5))
Q11facet
ggsave(path = "plots", filename = "Q11ps.png", width = 7, height = 3)
