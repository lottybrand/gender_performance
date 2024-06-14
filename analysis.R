
##### first analysis look #####

# First the general checks across all conditions: 


##### Prepare the data ####
# set working directory to where you have the data saved: 
setwd("~/Desktop/CES FELLOWSHIP/gender_performance/data_finals")

library(ggplot2)
library(plyr)
library (rethinking)

##### process data: ####

# solo <- read.csv("solo_final_151.csv")
# peer <- read.csv("peer_final_150.csv")
# city <- read.csv("citymen_final_152.csv")
# elder <- read.csv("elders_final_149.csv")
# 
# # select subset of relevant variables in each file before merging: 
# 
# colnames(solo)
# 
# sub_solo <- subset(solo, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20","honest1","honest2"))
# sub_peer <- subset(peer, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20","honest1","honest2","honest3","honest4"))
# sub_city <- subset(city, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20","honest1","honest2","honest3","honest4"))
# sub_elder <- subset(elder, select = c("emp1","emp2","emp3","emp4","emp5","emp6","emp7","emp8","emp9","emp10","emp11","emp12","emp13","emp14","emp15","emp16","emp17","emp18","emp19","emp20","honest1","honest2","honest3","honest4"))
# 
# # add Condition Column 
# 
# sub_solo$condition <- "solo"
# sub_peer$condition <- "peer"
# sub_city$condition <- "city"
# sub_elder$condition <- "elder"
# 
# # merge into one
# data_full <- rbind(sub_solo,sub_peer,sub_city,sub_elder)
# 
# hist(data_full$emp1)
# 
# # reverse code the long way 
# 
# table(data_full$emp2)
# 
# # columns to reverse code are: 
# # 2,3,4,5,8,11,15,16,17,18,19
# 
# data_full[c(2,3,4,5,8,11,15,16,17,18,19)] <- 6 - data_full[c(2,3,4,5,8,11,15,16,17,18,19)] 
# 
# datalong <- reshape(data_full, idvar = "ppt", 
#                      varying = list(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
#                      v.names = c("empowerment_support"), 
#                      direction = "long")
# 
# colnames(datalong)[colnames(datalong) == 'time'] <- 'question'

#write.csv(datalong, file="datalong.csv", row.names=FALSE)

##### load data #####
datalong <- read.csv("datalong.csv")

theMeansEmpowerment = tapply(datalong$empowerment_support, list(datalong$condition),mean)
theMeansEmpowerment

#city    elder     peer     solo 
#3.170395 3.187248 3.090667 3.250993

theSDsEmpowerment = tapply(datalong$empowerment_support, list(datalong$condition),sd)
theSDsEmpowerment

#city    elder     peer     solo 
#1.374776 1.419369 1.448843 1.448892

theSDsperQ = tapply(datalong$empowerment_support, list(datalong$question),sd)
theSDsperQ

theSDsperQ <- as.data.frame(theSDsperQ)

#1         2         3         4         5         6         7         8 
#1.1640296 0.9409768 1.4158387 1.2309690 1.0475244 1.3028359 0.9104916 0.9789875 
#9        10        11        12        13        14        15        16 
#1.2747557 1.2669163 1.2882347 1.0509765 1.2018437 1.3629386 1.1733100 1.3190325 
#17        18        19        20 
#1.2761372 1.3688363 1.2924855 1.3796157

theMeansPerQ = tapply(datalong$empowerment_support, list(datalong$question),mean)
theMeansPerQ

theMeansPerQ <- as.data.frame(theMeansPerQ)

##### write first model #####


## Just straight ordinal for peer vs solo, with no varying intercepts for now ##

peercond <- datalong[datalong$condition=="peer",]
solocond <- datalong[datalong$condition=="solo",]
peer_solo <- rbind(peercond,solocond)
peer_solo$COND <- ifelse(peer_solo$condition=="peer",1,0)
table(peer_solo$COND,peer_solo$condition)


model_peerSolo2 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND,
    b_cond ~ dnorm(0,1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_solo, 
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerSolo2)

#
#mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond -0.21 0.05 -0.28 -0.13  1436     1

### adding varying intercept for ppt ID : 

model_peerSolo3 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[ppt]*sigma_a,
    b_cond ~ dnorm(0,1),
    b_ppt[ppt] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_solo, 
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerSolo3)

#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.21 0.06 -0.31 -0.10  1251     1
#sigma_a  0.39 0.03  0.34  0.44   931     1


# Just varying intercept for question 
model_peerSolo4 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_q[question]*sigma_b,
    b_cond ~ dnorm(0,1),
    b_q[question] ~ dnorm(0,1),
    sigma_b ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_solo, 
  constraints = list(sigma_b = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerSolo4)

#### add varying intercept for question too #### 

model_peerSolo1 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[ppt]*sigma_a + b_q[question]*sigma_b,
    b_cond ~ dnorm(0,1),
    b_ppt[ppt] ~ dnorm(0,1),
    b_q[question] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_b ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_solo, 
  constraints = list(sigma_a = "lower=0", sigma_b = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerSolo1)
#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.30 0.08 -0.42 -0.18  1171     1
#sigma_a  0.53 0.03  0.48  0.58   882     1
#sigma_b  0.66 0.04  0.59  0.73   892     1

precis(model_peerSolo1, prob=0.5)
#         mean   sd   25%   75% n_eff Rhat4
#b_cond  -0.30 0.08 -0.35 -0.25   943  1.00
#sigma_a  0.53 0.03  0.51  0.56  1045  1.00
#sigma_b  0.66 0.05  0.63  0.69   752  1.01

plot(precis(model_peerSolo1, depth=2, pars = c("b_q[1]","b_q[2]","b_q[3]","b_q[4]","b_q[5]","b_q[6]","b_q[7]","b_q[8]","b_q[9]","b_q[10]","b_q[11]","b_q[12]","b_q[13]","b_q[14]","b_q[15]","b_q[16]","b_q[17]","b_q[18]","b_q[19]","b_q[20]")))
(precis(model_peerSolo1, depth=2, pars = c("b_q[1]","b_q[2]","b_q[3]","b_q[4]","b_q[5]","b_q[6]","b_q[7]","b_q[8]","b_q[9]","b_q[10]","b_q[11]","b_q[12]","b_q[13]","b_q[14]","b_q[15]","b_q[16]","b_q[17]","b_q[18]","b_q[19]","b_q[20]")))
# 
#           mean   sd  5.5% 94.5% n_eff Rhat4
# b_q[1]  -3.04 0.34 -3.57 -2.51   428  1.00
# b_q[2]  -3.33 0.35 -3.87 -2.80   461  1.01
# b_q[3]   0.09 0.28 -0.37  0.53   345  1.00
# b_q[4]   1.31 0.29  0.85  1.77   322  1.00
# b_q[5]   3.17 0.36  2.63  3.76   454  1.00
# b_q[6]   0.73 0.28  0.29  1.21   312  1.00
# b_q[7]   2.78 0.33  2.25  3.31   411  1.00
# b_q[8]  -3.06 0.34 -3.58 -2.52   458  1.00
# b_q[9]  -0.30 0.27 -0.76  0.12   347  1.01
# b_q[10]  0.89 0.29  0.43  1.34   359  1.00
# b_q[11] -0.99 0.28 -1.43 -0.55   336  1.01
# b_q[12] -1.23 0.28 -1.67 -0.77   346  1.00
# b_q[13]  0.73 0.28  0.27  1.17   354  1.00
# b_q[14]  0.37 0.29 -0.11  0.84   342  1.00
# b_q[15]  1.72 0.30  1.24  2.20   368  1.00
# b_q[16]  0.97 0.29  0.51  1.44   344  1.00
# b_q[17] -1.33 0.28 -1.77 -0.87   364  1.01
# b_q[18]  0.87 0.29  0.41  1.33   387  1.00
# b_q[19] -1.08 0.29 -1.54 -0.62   353  1.01
# b_q[20]  0.55 0.28  0.10  1.02   345  1.00

compare(model_peerSolo1,model_peerSolo2,model_peerSolo3,model_peerSolo4)
#                 WAIC    SE  dWAIC   dSE pWAIC weight
#model_peerSolo1 16730.9 90.50    0.0    NA 211.5      1
#model_peerSolo3 18875.2 44.41 2144.4 79.68 155.5      0
#model_peerSolo4 17107.4 86.92  376.6 36.49  23.6      0
#model_peerSolo2 19044.7 36.38 2313.8 81.44   5.0      0


# for question and topic #

financial <- c(8,9,10,14,15,18)
reproductive <- c(11,12,13)
decisionmaking <- c(1,2,17,19)
educationLeadership <- c(3,4,5,16)
ipv_household <- c(6,7,20)

peer_solo$topic <- ifelse((peer_solo$question %in% financial), 1,
                          ifelse((peer_solo$question %in% reproductive), 2,
                                 ifelse((peer_solo$question %in% decisionmaking),3,
                                        ifelse((peer_solo$question %in% educationLeadership),4,
                                          ifelse((peer_solo$question %in% ipv_household), 5,99)))))

table(peer_solo$topic, peer_solo$question)

write.csv(peer_solo, "peer_solo.csv", row.names = FALSE)


model_peerSolo11 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[ppt]*sigma_a + b_q[question]*sigma_b + b_t[topic]*sigma_c,
    b_cond ~ dnorm(0,1),
    b_ppt[ppt] ~ dnorm(0,1),
    b_q[question] ~ dnorm(0,1),
    b_t[topic] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_b ~ normal(0,0.1),
    sigma_c ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_solo, 
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerSolo11)

# oh dear hates the rhat! maybe stick to just question and participant for now! 
#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.30 0.08 -0.42 -0.18   969  1.00
#sigma_a  0.53 0.03  0.48  0.59  1298  1.00
#sigma_b -0.19 0.54 -0.65  0.62     2 11.30
#sigma_c  0.00 0.31 -0.39  0.39     3  1.81

#### solo city full model ##### 

citycond <- datalong[datalong$condition=="city",]
solo_city <- rbind(solocond,citycond)
solo_city$COND <- ifelse(solo_city$condition=="city",1,0)
table(solo_city$COND,solo_city$condition)

Nppts = length(unique(solo_city$ppt))
oldID <- solo_city$ppt
contiguousID <- array(0,length(solo_city$ppt))
for (index in 1:Nppts){
  contiguousID[oldID == unique(oldID)[index]] = index
}
solo_city$PPT <- contiguousID
table(solo_city$PPT)

model_soloCity1 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[PPT]*sigma_a,
    b_cond ~ dnorm(0,1),
    b_ppt[PPT] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=solo_city, 
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 1, cores = 3, iter=1200)

precis(model_soloCity1)
#
#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.12 0.07 -0.23 -0.01   286  1.00
#sigma_a  0.44 0.03  0.39  0.49   306  1.01


# with question effect too 

model_soloCity2 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[PPT]*sigma_a + b_q[question]*sigma_b,
    b_cond ~ dnorm(0,1),
    b_ppt[PPT] ~ dnorm(0,1),
    b_q[question] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_b ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=solo_city, 
  constraints = list(sigma_a = "lower=0", sigma_b = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_soloCity2)
#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.18 0.08 -0.31 -0.05  1010     1
#sigma_a  0.57 0.03  0.52  0.62  1223     1
#sigma_b  0.65 0.05  0.57  0.72   810     1

precis(model_soloCity2, prob=0.5)
#         mean   sd   25%   75% n_eff Rhat4
#b_cond  -0.18 0.08 -0.24 -0.13   967     1
#sigma_a  0.56 0.03  0.54  0.59  1038     1
#sigma_b  0.64 0.05  0.61  0.68   584     1

##### Now Solo & Elder Full Model ####


eldercond <- datalong[datalong$condition=="elder",]
solo_elder <- rbind(solocond,eldercond)
solo_elder$COND <- ifelse(solo_elder$condition=="elder",1,0)
table(solo_elder$COND,solo_elder$condition)

Nppts = length(unique(solo_elder$ppt))
oldID <- solo_elder$ppt
contiguousID <- array(0,length(solo_elder$ppt))
for (index in 1:Nppts){
  contiguousID[oldID == unique(oldID)[index]] = index
}
solo_elder$PPT <- contiguousID
table(solo_elder$PPT)

model_soloElder2 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[PPT]*sigma_a + b_q[question]*sigma_b,
    b_cond ~ dnorm(0,1),
    b_ppt[PPT] ~ dnorm(0,1),
    b_q[question] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_b ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=solo_elder, 
  constraints = list(sigma_a = "lower=0", sigma_b = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_soloElder2)
#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.13 0.08 -0.26  0.00   700     1
#sigma_a  0.59 0.03  0.54  0.65   912     1
#sigma_b  0.64 0.05  0.56  0.71   724     1

precis(model_soloElder2 ,prob=0.95)
#         mean   sd  2.5% 97.5% n_eff Rhat4
#b_cond  -0.13 0.08 -0.29  0.03   880     1
#sigma_a  0.59 0.03  0.53  0.66  1204     1
#sigma_b  0.64 0.04  0.56  0.72   721     1


## next check peer & elder as next biggest difference 

peercond <- datalong[datalong$condition=="peer",]
eldercond <- datalong[datalong$condition=="elder",]
peer_elder <- rbind(peercond,eldercond)
peer_elder$COND <- ifelse(peer_elder$condition=="peer",1,0)
table(peer_elder$COND,peer_elder$condition)

table(peer_elder$empowerment_support)

Nppts = length(unique(peer_elder$ppt))
oldID <- peer_elder$ppt
contiguousID <- array(0,length(peer_elder$ppt))
for (index in 1:Nppts){
  contiguousID[oldID == unique(oldID)[index]] = index
}
peer_elder$PPT <- contiguousID


model_peerElder1 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[PPT]*sigma_a,
    b_cond ~ dnorm(0,1),
    b_ppt[PPT] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_elder, 
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerElder1)

#         mean   sd  5.5% 94.5% n_eff Rhat4
#sigma_a  0.46 0.03  0.41  0.51  1235     1
#b_cond  -0.13 0.07 -0.24 -0.01  1140     1


#### peer city ####


peercond <- datalong[datalong$condition=="peer",]
citycond <- datalong[datalong$condition=="city",]
peer_city <- rbind(peercond,citycond)
peer_city$COND <- ifelse(peer_city$condition=="peer",1,0)
table(peer_city$COND,peer_city$condition)

table(peer_city$empowerment_support)

Nppts = length(unique(peer_city$ppt))
oldID <- peer_city$ppt
contiguousID <- array(0,length(peer_city$ppt))
for (index in 1:Nppts){
  contiguousID[oldID == unique(oldID)[index]] = index
}
peer_city$PPT <- contiguousID


model_peerCity1 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[PPT]*sigma_a,
    b_cond ~ dnorm(0,1),
    b_ppt[PPT] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peer_city, 
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerCity1)

#         mean   sd  5.5% 94.5% n_eff Rhat4
#sigma_a  0.43 0.03  0.38  0.48   850     1
#b_cond  -0.09 0.07 -0.20  0.01  1086     1



#### to complete the puzzle let's do city/elder ##### 

elder_city <- rbind(eldercond,citycond)
elder_city$COND <- ifelse(elder_city$condition=="elder",1,0)
table(elder_city$COND,elder_city$condition)

table(elder_city$empowerment_support)

Nppts = length(unique(elder_city$ppt))
oldID <- elder_city$ppt
contiguousID <- array(0,length(elder_city$ppt))
for (index in 1:Nppts){
  contiguousID[oldID == unique(oldID)[index]] = index
}
elder_city$PPT <- contiguousID


model_ElderCity2 <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[PPT]*sigma_a + b_q[question]*sigma_b,
    b_cond ~ dnorm(0,1),
    b_ppt[PPT] ~ dnorm(0,1),
    b_q[question] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_b ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=elder_city, 
  constraints = list(sigma_a = "lower=0", sigma_b = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_ElderCity2)

#         mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  0.06 0.09 -0.08  0.20   668     1
#sigma_a 0.63 0.03  0.58  0.68   916     1
#sigma_b 0.64 0.05  0.57  0.72   514     1



#### looking at private / public ####


hidden <- c(1,6,7,8,9,11,12,13,18,20)

peer_solo$hidden <- ifelse(peer_solo$question%in%hidden,1,0)
table(peer_solo$question,peer_solo$hidden)

peersolo_hidden <- peer_solo[peer_solo$hidden==1,]

#probably need to make question contiguous again now: 


Nqs = length(unique(peersolo_hidden$question))
oldQ <- peersolo_hidden$question
contiguousQ <- array(0,length(peersolo_hidden$question))
for (index in 1:Nqs){
  contiguousQ[oldQ == unique(oldQ)[index]] = index
}
peersolo_hidden$QID <- contiguousQ

model_peerSoloHidden <- map2stan(
  alist(
    empowerment_support ~ dordlogit(phi, cutpoints),
    phi <-  b_cond*COND + b_ppt[ppt]*sigma_a + b_q[QID]*sigma_b,
    b_cond ~ dnorm(0,1),
    b_ppt[ppt] ~ dnorm(0,1),
    b_q[QID] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_b ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=peersolo_hidden, 
  constraints = list(sigma_a = "lower=0", sigma_b = "lower=0"),
  start = list(cutpoints=c(-1,-0.5,0.5,1)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_peerSoloHidden)
#mean   sd  5.5% 94.5% n_eff Rhat4
#b_cond  -0.25 0.09 -0.39 -0.11  1777     1
#sigma_a  0.50 0.04  0.43  0.57   942     1
#sigma_b  0.58 0.05  0.50  0.66  1100     1
