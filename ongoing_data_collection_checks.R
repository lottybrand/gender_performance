
##### Ongoing Data Collection Checks #####

# Script to regularly check if each condition is balanced in terms of: 
# age, education and marriage status of the ppts
# how the ppt was found: alone or in a group 
# and whether the peers in the Peer Group are balanced in terms of:
# peer education, age, and if known to the ppt

# First the general checks across all conditions: 


##### Prepare the data ####
# set working directory to where you have the data saved: 
setwd("~/Desktop/CES FELLOWSHIP/social_performance/data_finals")

library(ggplot2)
library(plyr)
library(dplyr)
nice_cols <- c("#ff9b54","#500c0b","#689689","#2E4057","#b2e6d4","#d4a686")
nice_cols_extra <- c("#ff9b54","#500c0b","#689689","#2E4057","#FFD685","#d4a686","#b2e6d4","#D96D7D")
# load each condition's data: 

solo <- read.csv("solo_final_151.csv")
peer <- read.csv("peer_final_150.csv")
city <- read.csv("citymen_final_152.csv")
elder <- read.csv("elders_final_149.csv")

# select subset of relevant variables in each file before merging: 

colnames(solo)

sub_solo <- subset(solo, select = c("m_age","m_edu","numwife","m_prevwife","m_numkids","m_work","m_workother","who_present","context"))
sub_peer <- subset(peer, select = c("m_age","m_edu","numwife","m_prevwife","m_numkids","m_work","m_workother", "who_present","context"))
sub_city <- subset(city, select = c("m_age","m_edu","numwife","m_prevwife","m_numkids","m_work","m_workother", "who_present","context"))
sub_elder <- subset(elder, select = c("m_age","m_edu","numwife","m_prevwife","m_numkids","m_work", "m_workother","who_present","context"))

# add Condition Column 

sub_solo$condition <- "control"
sub_peer$condition <- "peer"
sub_city$condition <- "city"
sub_elder$condition <- "elder"

# merge into one
data_check <- rbind(sub_solo,sub_peer,sub_city,sub_elder)



#### AGE ####
hist(data_check$m_age)
# manually remove age error (200 presumably should be 20)
data_check$m_age <- ifelse((data_check$m_age ==200),20,data_check$m_age)

theMeansAge = tapply(data_check$m_age, list(data_check$condition),mean)
theMeansAge

boxAges <- ggplot(data_check, aes(x = condition, y = m_age)) +
  geom_boxplot() + 
  xlab("Condition") + ylab("Age") +
  theme_bw() +
  scale_y_continuous(breaks = seq(17,30,by=1)) + 
  ggtitle("Age Condition Splits") + 
  theme(strip.text = element_text(colour = 'yellow3', size=12))
boxAges

# binning age
#data_check$bin_age <- ifelse(data_check$m_age > 23, "Above 23", "Below 23")
#data_check$bin_age <- as.factor(data_check$bin_age)

#age_plot <- ggplot(data = data_check) + 
#  geom_bar(mapping = aes(x = bin_age, fill = condition), position = position_dodge()) +
#  theme_bw() +
#  theme(text = element_text(size=16), axis.text = element_text(size=14)) + 
#  scale_x_discrete(limits = c("Below 23", "Above 23")) + 
#  xlab("Age") + ylab("Count")

#age_plot

###### trying to bin age differently for plots #####
#data_check$agebins <- ifelse(((data_check$m_age > 18)&(data_check$m_age<=21)), "18-21", 
#                             ifelse((data_check$m_age >21)&(data_check$m_age<=25)),"22-25",
#                                    ifelse((data_check$m_age >25)&(data_check$m_age<=28)),"26-28",
#                                           ifelse((data_check$m_age > 28)&(data_check$m_age<=30)),"28-30","999")))))

#data_check$agebins <- as.factor(data_check$agebins))
#table(data_check$bin_age, data_check$condition)


####
#### Occupation ####
####

# MUST BE DONE IN THE BELOW ORDER 

# check other column details and their spaces too! 
table(data_check$m_workother)

occupation_data <- subset(data_check, select=c("m_work","m_workother"))
#farmer, fishing, pastoralist, none:
Subsistence <- c(1,9,11,0)
#cook, unskilled:
Unskilled_manual <- c(3,8)
#small business, trader, driver, fundi:
Skilled_manual <- c(2,4,6,7)
# professional: 
Professional <- c(5)
# studying: 
Studying <- c(10)

other_unskilled <- c("Ulinzi","Sanaa","Msanii","Usanii", "Kinyozi","Dancer","Duka la dawa  na Mpesa","Farming")
other_skilled <- c("Bodaboda","Fundi ujenzi ", "Mechanic", "Counselling","Bus agent","Footballer")

table(data_check$m_work)

data_check$m_work <- as.factor(data_check$m_work)
table(data_check$m_work)

# RE DO. Separate columns for occupations. overlapping.
# first change 98s to x so subsistence and unskilled aren't included with others
data_check$m_work <- gsub("98", "x", data_check$m_work)

data_check$studying <- ifelse(grepl("10",data_check$m_work), 1, 0)
data_check$unskilled_labour <- ifelse(grepl("3|8",data_check$m_work), 1, 0)
data_check$skilled_labour <- ifelse(grepl("2|4|6|7",data_check$m_work), 1, 0)
data_check$professional <- ifelse(grepl("5",data_check$m_work), 1, 0)
data_check$subsistence <- ifelse((grepl("1| 1|1 |11|9| 0|0 |0",data_check$m_work)), 1, 0)

data_check$unskilled_labour <- ifelse((data_check$m_workother %in% other_unskilled), 1, data_check$unskilled_labour)
data_check$skilled_labour <- ifelse((data_check$m_workother %in% other_skilled), 1, data_check$skilled_labour)

#check by looking at subset:
occupation_data <- subset(data_check, select=c("m_work","m_workother","studying","unskilled_labour","skilled_labour","professional","subsistence"))
# The above works just need to remove the 12 total blanks without affecting all other blanks...need to think about this... leave for now

studyingtable <- data_check %>%
     group_by(condition, studying) %>%
     summarise(n = n()) %>%
     mutate(freq = n / sum(n))
studyingtable<- as.data.frame(studyingtable)

unskilledtable <- data_check %>%
  group_by(condition, unskilled_labour) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
unskilledtable<- as.data.frame(unskilledtable)

skilledtable <- data_check %>%
  group_by(condition, skilled_labour) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
skilledtable<- as.data.frame(skilledtable)

profftable <- data_check %>%
  group_by(condition, professional) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
profftable<- as.data.frame(profftable)

substable <- data_check %>%
  group_by(condition, subsistence) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
substable<- as.data.frame(substable)


# The Below had not worked as assumed. it also removes the spaces of the separate work entries. 
# recode the missings as subsistence (0) as confirmed with data collection (no occupation)
#data_check$m_work <- ifelse(data_check$m_work=="",0,data_check$m_work)

# 
# data_check$occupation <- ifelse(grepl("10",data_check$m_work), "Studying", "99999")
# table(data_check$occupation, data_check$m_work)
# occupation_data <- subset(data_check, select=c("m_work","m_workother","occupation"))
# data_check$occupation <- ifelse(grepl("3|8",data_check$m_work), "Unskilled", data_check$occupation)
# table(data_check$occupation, data_check$m_work)
# data_check$occupation <- ifelse(grepl("2|4|6|7",data_check$m_work), "Skilled", data_check$occupation)
# data_check$occupation <- ifelse(grepl("5",data_check$m_work), "Professional", data_check$occupation)
# data_check$occupation <- ifelse((grepl("1 |9|11| 0",data_check$m_work)), "Subsistence", data_check$occupation)
# # The above leaves 0s and 1s on their own (to avoid subsuming the 10s from the studying group)
# # to remedy this the 99999s then become subsistence (they are all 1s and 0s)
# data_check$occupation[data_check$occupation=="99999"] <- "Subsistence"
# 
# data_check$occupation <- ifelse((data_check$m_workother %in% other_unskilled), "Unskilled", data_check$occupation)
# data_check$occupation <- ifelse((data_check$m_workother %in% other_skilled), "Skilled", data_check$occupation)
# 
# table(data_check$occupation, data_check$condition)
# 
# proptable <- data_check %>%
#   group_by(condition, occupation) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n))
# proptable<- as.data.frame(proptable)

# still subsistence and unskilled manual includes 98s which it shouldn't. need to check those 98s 
#occupation_data$ninetyeight <- ifelse(grepl("98",occupation_data$m_work), 1, 0)
#occupation_data$trying <- ifelse((grepl("1 |9|11| 0" & !"98",occupation_data$m_work)), 1, 0)
#occupation_data$trying <- ifelse((grepl("1 |9|11| 0",occupation_data$m_work)) & grepl(!grepl("98", occupation_data$m_work)),1,0)
#occupation_data$trying <- ifelse((grepl("1 |9|11| 0",occupation_data$m_work)) & grepl(!grepl("98", occupation_data$m_work)),1,0)

#occupation_data$m_work <- gsub("98", "x", occupation_data$m_work)



occuplot <- ggplot(data_check, aes(x=condition, fill = occupation)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Condition") + ylab("Percentage") + 
  theme_bw() +
  scale_fill_manual(values = nice_cols) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12)) 
occuplot

#ocu_plot <- ggplot(data = data_check) + 
#  geom_bar(mapping = aes(x = occcupation, fill = condition), position = position_dodge()) +
#  xlab("Occupation") + ylab("Count") +
#  theme_bw() +
#  theme(text = element_text(size=16), axis.text = element_text(size=14))

#ocu_plot



#####
##### EDU ##### 
#####

#bin edu to be above Form 2 (10) based on pilot split 
#data_check$bin_edu <- ifelse(data_check$m_edu > 10, "Form 3 or above", "Form 2 or below")

#bin_edu_plot <- ggplot(data = data_check) + 
#  geom_bar(mapping = aes(x = bin_edu, fill = condition), position = position_dodge()) +
#  xlab("Edu") + ylab("Count") +
#  theme_bw() +
#  theme(text = element_text(size=16), axis.text = element_text(size=14)) +
#  scale_x_discrete(limits = c("Form 2 or below", "Form 3 or above"))

#bin_edu_plot

#splitting edu further

#data_check$edu_split <- ifelse((data_check$m_edu==0), "none",
#                                 ifelse((data_check$m_edu < 8), "primary",
#                                        ifelse(((data_check$m_edu > 7) & (!data_check$m_edu ==14) & (!data_check$m_edu ==15)), "secondary",
#                                               ifelse((data_check$m_edu ==14), "vocational",
#                                                      ifelse((data_check$m_edu==15), "higher edu", 99)))))


#eduplot <- ggplot(data_check, aes(x=condition, fill = edu_split)) +
#  geom_bar(position = "fill") +
#  scale_y_continuous(labels = scales::percent) + 
#  xlab("Condition") + ylab("Percentage") + 
#  scale_fill_manual(values = nice_cols) + 
#  theme_bw() +
#  theme(text = element_text(size=12), axis.text = element_text(size=12)) 
#eduplot

#table(data_check$edu_split, data_check$condition)

#split_edu_plot <- ggplot(data = data_check) + 
#  geom_bar(mapping = aes(x = edu_split, y = stat(count), fill = condition), position = position_dodge()) +
#  xlab("Edu") + ylab("count") +
#  scale_x_discrete(limits = c("none","primary","secondary","vocational","higher edu")) +
#  theme_bw() +
#  theme(text = element_text(size=16), axis.text = element_text(size=14))

#split_edu_plot

##### full edu breakdow ####
table(data_check$m_edu)


data_check$edu_full <- ifelse((data_check$m_edu==0), "none",
                              ifelse((data_check$m_edu < 7), "some primary",
                                     ifelse((data_check$m_edu ==7), "completed primary",
                                        ifelse(((data_check$m_edu > 7) & (data_check$m_edu <11)), "lower secondary",
                                              ifelse(((data_check$m_edu > 10) & (data_check$m_edu < 13)), "some higher secondary",
                                                      ifelse((data_check$m_edu ==13), "completed higher secondary",
                                                          ifelse((data_check$m_edu ==14), "vocational",
                                                              ifelse((data_check$m_edu==15), "higher edu", 99))))))))

data_check <- data_check[!is.na(data_check$edu_full), ]
table(data_check$edu_full,data_check$m_edu)
data_check$edu_full <- factor(data_check$edu_full, levels=c("none","some primary","completed primary","lower secondary","some higher secondary","completed higher secondary","vocational","higher edu"))
data_check$condition <- factor(data_check$condition, levels=c("control","peer","elder","city"))


fulleduplot <- ggplot(data_check, aes(x=condition, fill = edu_full)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Edu") + ylab("Percentage") + 
  labs(title = "Education achieved per condition") +
  theme_bw() +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = nice_cols_extra) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12)) 
fulleduplot
ggsave(path = "plots", filename = "full_edu_breakdown.png", width = 8, height = 6)

proptable <- data_check %>%
  group_by(condition, edu_full) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
proptable<- as.data.frame(proptable)

table(data_check$edu_full,data_check$condition)
prop.table(data_check$edu_full,data_check$condition)

#                             control peer elder city
#none                             3    1     1    4
#some primary                    10    8     8    7
#completed primary               42   39    37   36
#lower secondary                 21   20    25   15
#some higher secondary           55   62    56   64
#completed higher secondary       3    6     4    6
#vocational                       0    0     4    3
#higher edu                      17   12    14   17


####
#### Marriage / kids ####
####

# sort NAs here, make all zeros for now?
table(data_check$numwife)
table(data_check$m_prevwife)
data_check$married <- ifelse((data_check$numwife > 0), 1, 0)
table(data_check$married,data_check$condition)
data_check$kids <- ifelse((data_check$m_numkids > 0), 1, 0)
table(data_check$kids,data_check$condition)

                                   
data_check$marriage_kids <- ifelse((data_check$numwife > 0), 1, 
                                   ifelse((data_check$m_prevwife > 0), 1,
                                          ifelse((data_check$m_numkids > 0), 1, 0)))

data_check$marriage_kids <- ifelse(data_check$marriage_kids ==1, "Married or Kids" , "Neither")

# finally got percentages sorted from this page: 
# https://stackoverflow.com/questions/46984296/proportion-with-ggplot-geom-bar

mkids <- ggplot(data_check, aes(x=condition, fill = marriage_kids)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Condition") + ylab("Percentage") + 
  theme_bw() +
  scale_fill_manual(values = nice_cols) + 
  theme(text = element_text(size=12), axis.text = element_text(size=12)) 
mkids

table(data_check$marriage_kids, data_check$condition)

#marriageKids_plot <- ggplot(data = data_check) + 
#  geom_bar(mapping = aes(x = condition, fill = marriage_kids), position = position_dodge()) +
#  xlab("Condition") + ylab("prop") +
#  theme_bw() +
#  scale_fill_manual(values = nice_cols) + 
#  theme(text = element_text(size=16), axis.text = element_text(size=14)) 

#marriageKids_plot

####
#### How found ####
####
# convert to words
data_check$who_present_f <- ifelse(data_check$who_present ==1, "Alone", "With Others") 

found_plot <- ggplot(data = data_check) + 
  geom_bar(mapping = aes(x = who_present_f, fill = condition), position = position_dodge()) +
  xlab("How Found") + ylab("Count") +
  scale_y_continuous(breaks = seq(0,90,by=10)) + 
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 

found_plot

table(data_check$who_present_f, data_check$condition)

##### Context #####

# convert to words
data_check$context_n <- mapvalues(data_check$context, from = c(1,2,3,4),
                                            to = c("Working","At Home", "Socialising","Travelling"))

context_plot <- ggplot(data = data_check) + 
  geom_bar(mapping = aes(x = context_n, fill = condition), position = position_dodge()) +
  xlab("Doing when found") + ylab("Count") +
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) 

context_plot

table(data_check$context_n, data_check$condition)


##### PEER CHECKS ##### 

# Prepare the Data

colnames(peer)
peer_check <- subset(peer, select = c("age_peer1","edu_peer1","known_peer1","age_peer2","edu_peer2","known_peer2","age_peer3","edu_peer3","known_peer3"))

#wide to long for each peer: 
peer_check_l <- reshape(peer_check, 
                         varying = list(c("age_peer1","age_peer2","age_peer3"),
                                        c("edu_peer1","edu_peer2","edu_peer3"),
                                        c("known_peer1","known_peer2","known_peer3")),
                         v.names = c("peers_age", "peers_edu","peers_known"), 
                         direction = "long")

peer_check_l$time <- NULL
colnames(peer_check_l)[colnames(peer_check_l) == 'id'] <- 'Group'


##### Peer Age  #####

table(peer_check_l$peers_age)
peer_check_l <- peer_check_l[!peer_check_l$peers_age == 2,]
peer_check_l <- peer_check_l[!peer_check_l$peers_age == 15,]

peer_age <- ggplot(data = peer_check_l) + 
  geom_bar(mapping = aes(x = peers_age)) +
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) + 
  xlab("Age") + ylab("Count")

peer_age

table(peer_check_l$peers_age)
#18 19 20 21 22 23 24 25 26 27 28 29 30 31 
#46 49 39 39 36 68 55 22 26 13 21  8 25  1 

mean(peer_check_l$peers_age, na.rm = TRUE)
#22.79

##### Per Edu #####

#Edu split 
peer_check_l$edu_split <- ifelse((peer_check_l$peers_edu==0), "none",
                            ifelse((peer_check_l$peers_edu < 8), "primary",
                                  ifelse(((peer_check_l$peers_edu > 7) & (!peer_check_l$peers_edu ==14) & (!peer_check_l$peers_edu ==15)), "secondary",
                                        ifelse((peer_check_l$peers_edu ==14), "vocational",
                                               ifelse((peer_check_l$peers_edu==15), "higher edu", 99)))))


peer_edu <- ggplot(data = peer_check_l) + 
  geom_bar(mapping = aes(x = edu_split)) +
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) + 
  xlab("Edu") + ylab("Count")

peer_edu

table(peer_check_l$edu_split)
#higher edu  none    primary  secondary 
#16          7        215        210 

#### Peer Known ####

peer_check_l$peers_known <- ifelse(peer_check_l$peers_known == 1, "yes","no")

peer_known <- ggplot(data = peer_check_l) + 
  geom_bar(mapping = aes(x = peers_known)) +
  theme_bw() +
  theme(text = element_text(size=16), axis.text = element_text(size=14)) + 
  xlab("Known") + ylab("Count")

peer_known
#no yes 
#56 391
# 87% known to them

table(peer_check_l$peers_known)
