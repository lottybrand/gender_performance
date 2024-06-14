
##### First Go at Plotting. Trying lots of different ways including Dan's and Altay's at the bottom from previous papers with Likert data. In the end stuck with barCondsfacet (line 70)


# load my color palette
nice_cols <- c("#ff9b54","#500c0b","#689689","#2E4057","#b2e6d4","#d4a686")
# this page very helpful: 
# https://stackoverflow.com/questions/38788357/change-bar-plot-colour-in-geom-bar-with-ggplot2-in-r

library(ggplot2)
library(ggstats)
library(scales)

setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")
fulldata <- read.csv("datalong.csv")
setwd("~/Desktop/CES FELLOWSHIP/gender_performance")

##### try creating percentage agree (binning) ##### 

# think we need to create contiguous unique ppt IDs for this full dataset... 
# probably by combining the four separate ones from the analysis file, but then 
# adding the ppt IDs subsequently (manually?) to each file...

fulldata$Agreed <- ifelse((fulldata$empowerment_support == 5 | fulldata$empowerment_support == 4), 1, 0)

table(fulldata$Agreed, fulldata$condition)
#
#city elder peer solo
#0 1447  1434 1501 1523
#1 1593  1546 1499 1497

# city = 0.54
# elder = 0.52
# peer = 0.50
# solo = 0.50 


fulldata$DISAgreed <- ifelse((fulldata$empowerment_support == 1 | fulldata$empowerment_support == 2), 1, 0)
table(fulldata$DISAgreed, fulldata$condition)



#### trying plotting and percentages ##### 

peer_solo <- read.csv("peer_solo.csv")

hist.default(peer_solo$empowerment_support)

peers <- peer_solo[peer_solo$COND==1,]
solos <- peer_solo[peer_solo$COND==0,]

hist(solos$empowerment_support)
hist(peers$empowerment_support)

#### Separate Plotting ####

# Solo # 

solo <- fulldata[fulldata$condition=="solo",]

hist(solo$empowerment_support)


soloplot <- ggplot(data = solo) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.5)) + 
  ggtitle("Solo Condition") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))
  
soloplot

# Peer #

peer <- fulldata[fulldata$condition=="peer",]

hist(peer$empowerment_support)

peerplot <- ggplot(data = peer) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.5)) + 
  ggtitle("Peer Condition") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

peerplot

# City # 

city <- fulldata[fulldata$condition=="city",]

hist(city$empowerment_support)

cityplot <- ggplot(data = city) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.5)) + 
  ggtitle("City Condition") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

cityplot

# Elders #

elder <- fulldata[fulldata$condition=="elder",]

hist(elder$empowerment_support)

elderplot <- ggplot(data = elder) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.5)) + 
  ggtitle("Elder Condition") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

elderplot


#### IPV ONLY ####

solo6 <- solo[((solo$question==6)|(solo$question ==7)),]

peer6 <- peer[((peer$question==6)|(peer$question ==7)),]

elder6 <- elder[((elder$question==6)|(elder$question ==7)),]

city6 <- city[((city$question==6)|(city$question ==7)),]

hist(solo6$empowerment_support)
hist(peer6$empowerment_support)
hist(elder6$empowerment_support)
hist(city6$empowerment_support)


solo6plot <- ggplot(data = solo6) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.8)) + 
  ggtitle("Solo Condition IPV") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

solo6plot

peer6plot <- ggplot(data = peer6) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.8)) + 
  ggtitle("Peer Condition IPV") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

peer6plot


city6plot <- ggplot(data = city6) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.8)) + 
  ggtitle("City Condition IPV") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

city6plot

elder6plot <- ggplot(data = elder6) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.8)) + 
  ggtitle("Elder Condition IPV") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

elder6plot

  

##### Education Only ####

solo_edu <- solo[((solo$question==5)|(solo$question ==16)),]

peer_edu <- peer[((peer$question==5)|(peer$question ==16)),]

elder_edu <- elder[((elder$question==5)|(elder$question ==16)),]

city_edu <- city[((city$question==5)|(city$question ==16)),]


solo_edu_plot <- ggplot(data = solo_edu) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.8)) + 
  ggtitle("Solo Condition Edu") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

solo_edu_plot

peer_edu_plot <- ggplot(data = peer_edu) + 
  geom_bar(mapping = aes(x = empowerment_support, y = stat(prop), group=1), fill="aquamarine4") + 
  xlab("Empowerment Support") + ylab("Proportion") +
  theme_bw() +
  theme(strip.background =element_rect(fill="aquamarine4")) +
  scale_y_continuous(limits=c(0,0.8)) + 
  ggtitle("Peer Condition EDU") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))

peer_edu_plot

##### boxplot for all Qs ####

fulldata$question <- as.factor(fulldata$question)

boxQs <- ggplot(fulldata, aes(x = question, y = empowerment_support)) +
  geom_boxplot()
boxQs

fulldata$question <- as.character(fulldata$question)
fulldata$empowerment_support <- as.character(fulldata$empowerment_support)

barQs <- ggplot(fulldata, aes(x=question, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")) +
  xlab("Empowerment Question") + ylab("Percentage") +
  theme_bw() +
  scale_fill_manual(values = c("1" = "#500c0b", "2" = "#2e4057", "3" = "#b2e6d4", "4" = "#689689", "5" = "#ff9b54")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
barQs 

##### BAR CONDS #####

barConds <- ggplot(fulldata, aes(x=condition, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  theme_bw() +
  scale_fill_manual(values = c("1" = "#500c0b", "2" = "#2e4057", "3" = "#b2e6d4", "4" = "#689689", "5" = "#ff9b54")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
barConds 

barCondsSide <- ggplot(fulldata, aes(x=condition, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  coord_flip()+
  theme_bw() +
  scale_fill_manual(values = c("1" = "#500c0b", "2" = "#2e4057", "3" = "#b2e6d4", "4" = "#689689", "5" = "#ff9b54")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
barCondsSide 

fulldata$conditionf = factor(fulldata$condition, levels=c('solo','peer','elder','city'))
fulldata$empowerment_support <- as.character(fulldata$empowerment_support)
levels(fulldata$conditionf) <- c("Solo Condition","Peer Condition","Elder Condition","City Condition")


barCondsfacet <- ggplot(fulldata, aes(x= empowerment_support, y = after_stat(prop))) + 
  geom_bar(aes(fill=empowerment_support), stat = "prop", position = "dodge", width = 0.95) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~conditionf) +
  xlab("Empowerment Support") + ylab("Percentage") +
  labs(fill='Empowerment Support') +
  scale_fill_manual(values = c("1" = "#2e4057", "2" = "#689689", "3" = "#b2e6d4", "4" ="#ffd685","5" = "#ff9b54"),
                    labels = c("1" = "Strongly Disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly Agree"),
                    breaks = c("5","4","3","2","1")) +
  theme_bw()+
  theme(strip.background =element_rect(fill="#b2e6d4"))+
  theme(strip.text = element_text(colour = "#2e4057"))
barCondsfacet 



###### IPV ONLY #####
IPVONLY <- fulldata[((fulldata$question==6)|(fulldata$question ==7)),]

IPVall <- ggplot(IPVONLY, aes(x=condition, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  theme_bw() +
  ggtitle("IPV Only") + 
  scale_fill_manual(values = c("1" = "#500c0b", "2" = "#2e4057", "3" = "#b2e6d4", "4" = "#689689", "5" = "#ff9b54")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
IPVall 

# EDU ONLY 
EDUONLY <- fulldata[((fulldata$question==5)|(fulldata$question ==16)),]

EDUall <- ggplot(EDUONLY, aes(x=condition, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  theme_bw() +
  ggtitle("EDU Only") + 
  scale_fill_manual(values = c("1" = "#500c0b", "2" = "#2e4057", "3" = "#b2e6d4", "4" = "#689689", "5" = "#ff9b54")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
EDUall

# TRAVEL ONLY 
TRAVELONLY <- fulldata[(fulldata$question==2),]

TRAVELall <- ggplot(TRAVELONLY, aes(x=condition, fill = empowerment_support)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Condition") + ylab("Percentage") +
  theme_bw() +
  ggtitle("Travel Only (Q2)") + 
  scale_fill_manual(values = c("1" = "#500c0b", "2" = "#2e4057", "3" = "#b2e6d4", "4" = "#689689", "5" = "#ff9b54")) + 
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 
TRAVELall


##### Trying Dan's #####

install.packages("ggdist")
library(ggdist)
plot_dan <- ggplot(data = fulldata,
                   mapping = aes(x = condition)) +
  stat_histinterval(aes(y = empowerment_support, fill = condition),
                    normalize = "none", slab_color = "white",
                    outline_bars = TRUE, show.legend = F,
                    breaks = seq(from = 0, to = 5), 
                    justification = 0.5) +
  xlab("Condition") + ylab("Empowerment Support") +
  theme_bw() 
plot_dan

plot_dan <- ggplot(data = fulldata,
                   mapping = aes(x = condition)) +
  geom_bar(aes(y = empowerment_support, fill = condition),
                    normalize = "none", slab_color = "white",
                    outline_bars = TRUE, show.legend = F,
                    breaks = seq(from = 0, to = 5), 
                    justification = 0.5) +
  xlab("Condition") + ylab("Empowerment Support") +
  theme_bw() 
plot_dan

##### Trying Altay's #####

install.packages("gghalves")
library(gghalves)
library(dplyr)

fulldata$empowerment_support <- as.integer(fulldata$empowerment_support)

trying_violin <- ggplot(data=fulldata, aes(y=empowerment_support)) +
  geom_density(data = fulldata %>% filter(condition=="solo"),aes(x=condition, y = empowerment_support, bw = 0.1), 
                   position = position_nudge(x = .5), 
                   side = "l", fill = "#2e4057")+
  geom_half_boxplot(data = fulldata %>% filter(condition=="solo"), aes(x=condition, y = empowerment_support), 
                    position = position_nudge(x = .6),
                    side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = .2, fill = "#2e4057")+
  geom_half_violin(data = fulldata %>% filter(condition=="peer"),aes(x = condition, y = empowerment_support), 
                   position = position_nudge(x = -.2), side = "l", fill = "#b2e6d4")+
  geom_half_boxplot(data = fulldata %>% filter(condition=="peer"), aes(x=condition, y = empowerment_support), 
                    position = position_nudge(x = -.2), side = "r",outlier.shape = NA, center = TRUE, 
                    errorbar.draw = FALSE, width = -.2, fill = "#b2e6d4")+
  
  xlab("Condition") + ylab("Women's Empowerment Support")+
  theme_classic()+
  ggtitle('Conditions')
trying_violin

tryingdens <- ggplot(data = fulldata, aes(y=empowerment_support)) +
  geom_density(data = fulldata, aes(x=condition, y = empowerment_support) +
  xlab("Condition") + ylab("Women's Empowerment Support")+
  theme_classic()+
  ggtitle('Conditions'))
tryingdens
  
  