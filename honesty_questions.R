
##### HONESTY QUESTIONS #####

# set nice cols
david_cols <- c("#5494f2","#C4DFFF","#FFF0C2","#FFC099", "#8F0000")

# set working directory to where you have the data saved: 
setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")

library(ggplot2)
library(plyr)

# load each condition's data: 

solo <- read.csv("solo_final_151.csv")
peer <- read.csv("peer_final_150.csv")
city <- read.csv("citymen_final_152.csv")
elder <- read.csv("elders_final_149.csv")

setwd("~/Desktop/CES FELLOWSHIP/social_performance")

# subset honest 1 & 2 for solo & rest comparison: 

colnames(solo)

sub_solo <- subset(solo, select = c("honest1","honest2"))
sub_peer <- subset(peer, select = c("honest1","honest2"))
sub_city <- subset(city, select = c("honest1","honest2"))
sub_elder <- subset(elder, select = c("honest1","honest2"))
# add Condition Column 

sub_solo$condition <- "solo"
sub_peer$condition <- "peer"
sub_city$condition <- "city"
sub_elder$condition <- "elder"

# merge into one
honestySolo <- rbind(sub_solo,sub_peer,sub_city,sub_elder)

honestySolo$honest_1 <- mapvalues(honestySolo$honest1, from = c(1,2,3),
                                            to = c("Yes","No","Somewhat"))
honestySolo$honest_2 <- mapvalues(honestySolo$honest2, from = c(1,2,3),
                                  to = c("Yes","No","Somewhat"))
table(honestySolo$honest1, honestySolo$condition)
table(honestySolo$honest2, honestySolo$condition)

honesty1_plot <- ggplot(honestySolo, aes(x=condition, fill = honest_1)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Did you feel you could be honest?") + ylab("Percentage") +
  theme_bw() +
  scale_fill_manual(values = c("No" = "#8F0000", "Yes"="#5494f2", "Somewhat" = "#FFF0C2")) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12),legend.title=element_blank())  
honesty1_plot 
ggsave(path = "plots", filename = "honesty1.png", width = 6, height =5)


honesty2_plot <- ggplot(honestySolo, aes(x=condition, fill = honest_2)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Do you think you would answer differently?") + ylab("Percentage") +
  theme_bw() +
  scale_fill_manual(values = c("No" = "#8F0000", "Yes"="#5494f2", "Somewhat" = "#FFF0C2")) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12),legend.title=element_blank())  
honesty2_plot 
ggsave(path = "plots", filename = "honesty2.png", width = 6, height =5)



#####
##### Honesty 3 & 4 #####
#####

sub_peer <- subset(peer, select = c("honest3","honest4"))
sub_city <- subset(city, select = c("honest3","honest4"))
sub_elder <- subset(elder, select = c("honest3","honest4"))

# add Condition Column 

sub_peer$condition <- "peer"
sub_city$condition <- "city"
sub_elder$condition <- "elder"

# merge into one
honestyGroup <- rbind(sub_peer,sub_city,sub_elder)

honestyGroup$honest_3 <- mapvalues(honestyGroup$honest3, from = c(1,2,3),
                                  to = c("Yes","No","Somewhat"))
honestyGroup$honest_4 <- mapvalues(honestyGroup$honest4, from = c(1,2,3),
                                  to = c("More","Less","Same"))

honesty3_plot <- ggplot(honestyGroup, aes(x=condition, fill = honest_3)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Do you think the other men would answer differently to you?") + ylab("Percentage") +
  theme_bw() +
  scale_fill_manual(values = c("No" = "#8F0000", "Yes"="#5494f2", "Somewhat" = "#FFF0C2")) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12),legend.title=element_blank())  
honesty3_plot 
ggsave(path = "plots", filename = "honesty3.png", width = 6, height =5)


honesty4_plot <- ggplot(honestyGroup, aes(x=condition, fill = honest_4)) + 
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Do you think the other men are more or less supportive?") + ylab("Percentage") +
  theme_bw() +
  scale_fill_manual(values = c("Less" = "#8F0000", "More"="#5494f2", "Same" = "#FFF0C2")) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12),legend.title=element_blank()) 
honesty4_plot 
ggsave(path = "plots", filename = "honesty4.png", width = 6, height =5)
