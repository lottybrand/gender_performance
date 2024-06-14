
##### Final condition plots and all questions together. 

# load my color palette
nice_cols <- c("#ff9b54","#500c0b","#689689","#2E4057","#b2e6d4","#d4a686")
david_cols <- c("#5494f2", "#C4DFFF","#FFF0C2","#FFC099", "#8F0000")

library(ggplot2)
library(ggstats)
library(scales)

setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")
fulldata <- read.csv("datalong.csv")
setwd("~/Desktop/CES FELLOWSHIP/gender_performance")


fulldata$conditionf = factor(fulldata$condition, levels=c('solo','peer','elder','city'))
fulldata$empowerment_support <- as.character(fulldata$empowerment_support)
levels(fulldata$conditionf) <- c("Control Condition","Peer Condition","Elder Condition","City Condition")


##### CONDITION PLOTS #####

barCondsSide <- ggplot(fulldata, aes(x=conditionf, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  coord_flip()+
  theme_bw() +
  labs(fill='Empowerment Support') +
  scale_fill_manual(values = c("1" = "#2e4057", "2" = "#689689", "3" = "#b2e6d4", "4" ="#ffd685","5" = "#ff9b54"),
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
barCondsSide 

barConds <- ggplot(fulldata, aes(x=conditionf, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  theme_bw() +
  labs(fill='Empowerment Support') +
  scale_fill_manual(values = c("1" = "#2e4057", "2" = "#689689", "3" = "#b2e6d4", "4" ="#ffd685","5" = "#ff9b54"),
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
barConds

barCondsfacet <- ggplot(fulldata, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Empowerment Support") + ylab("Percentage") +
  labs(fill='Empowerment Support') +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
barCondsfacet 
ggsave(path = "plots", filename = "ConditionPlot.png", width = 6, height = 4)

##### Private v Public #####

public <- c(3,4,5,10,14,15,16)
private <- c(1,2,6,7,8,9,11,12,13,17,18,19,20)

fulldata$public <- ifelse(fulldata$question%in%public,"public","private")
table(fulldata$question,fulldata$public)

barPublicfacet <- ggplot(fulldata, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~public) +
  xlab("Empowerment Support") + ylab("Percentage") +
  labs(fill='Empowerment Support') +
  scale_fill_manual(values = c("1" = "#2e4057", "2" = "#689689", "3" = "#b2e6d4", "4" ="#ffd685","5" = "#ff9b54"),
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#b2e6d4"))+
  theme(strip.text = element_text(colour = "#2e4057"))
barPublicfacet


##### Each individual Question #####

# Want to re-do the below for non-reversed Qs. & for Control only.


setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")
fulldata <- read.csv("datalong_NV.csv")
setwd("~/Desktop/CES FELLOWSHIP/gender_performance")
fulldata$conditionf <- as.character(fulldata$condition)
fulldata$conditionf = factor(fulldata$condition, levels=c('control','peer','elder','city'))
levels(fulldata$conditionf) <- c("Control Condition","Peer Condition","Elder Condition","City Condition")
fulldata$empowerment_support <- as.character(fulldata$empowerment_support)


# for all conditions at once (non-reversed)

fulldata$QuestionsFull <- mapvalues(fulldata$question, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                            to = c("A M should have the final say about decisions in his home",
                                                   "A W should be able to travel to visit her family, even if against her H’s wishes",
                                                   "W can be as effective leaders as M",
                                                   "W should be involved in senior community leadership",
                                                   "W should be educated to the same level as M",
                                                   "A H is justified in hitting his W if she argues with him",
                                                   "A H is justified in hitting his W if she refuses to have sex with him",
                                                   "The more income a W contributes to a household, the more say she should have in household decisions",
                                                   "W should hand her earnings over to her H and ask his permission to spend it",
                                                   "A W should not earn more money than her H",
                                                   "A H should respect his W's choice to use a condom during sexual activity",
                                                   "If a H and W disagree on their desired number of children, the W should defer to her H's perspective",
                                                   "A M is the one who decides when to have sex with his W",
                                                   "Married W should not own their own land",
                                                   "Married W should be able to own and manage their own business",
                                                   "It is ok for a W to be more educated than her H",
                                                   "A W should be free to divorce (or leave) her H even if he does not wish to end the marriage",
                                                   "A W should have an equal say in how the household spends their money",
                                                   "A W should be able to prevent her H from taking another W, if it is against her wishes",
                                                   "Only women and girls should wash and feed young children"))

#fulldata$QuestionsShort <- mapvalues(fulldata$question, from = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
#                                    to = c("(R) final say decisions",
#                                           "travel to family",
#                                           "effective leaders as men",
#                                           "involved community leadership",
#                                           "educated to the same level",
#                                           "(R)justified hitting if argues",
#                                           "(R)justified hitting sex refused",
#                                           "more income more say",
#                                           "hand her earnings over to her husband (R)",
#                                           "(R) shouldnt earn more money",
#                                           "wife's choice to use a condom",
#                                           "(R) decides when to have sex",
#                                           "(R) desired number of children",
#                                           "(R) women shouldnt own land",
#                                           "women should own business",
#                                           "woman to be more educated",
#                                           "free to divorce husband",
#                                           "equal say how spends money",
#                                           "prevent taking another wife",
#                                           "(R) wash/feed young children"))




# For all conds (non-reversed)
barQuestionfacet <- ggplot(fulldata, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~QuestionsFull, labeller = label_wrap_gen(35)) +
  xlab("Empowerment Support") + ylab("Percentage") +
  labs(fill='Agreement') +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \n nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
barQuestionfacet
ggsave(path = "plots", filename = "allQs_allConds_nonReversed.png", width = 15, height = 10)

# Now for Control

justControl <- fulldata[fulldata$condition=="control",]

controlQuestionfacet <- ggplot(justControl, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~QuestionsFull, ncol = 4, labeller = label_wrap_gen(40)) +
  xlab("Agreement") + ylab("") +
  labs(fill='Agreement') +
  scale_fill_manual(values = david_cols,
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree \n nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#FFF0C2"))
controlQuestionfacet
ggsave(path = "plots", filename = "allQs_justControl_nonR.png", width = 15, height = 10)
