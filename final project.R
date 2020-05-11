##### Cuzzolino, Jillian - Final Project #####
############################# Which political party supports government healthcare? ############################# 

setwd("R")
healthcare <- read.csv("kff2019.csv")

hc.new <- healthcare[c(1:2,4,6,9,13:14,27:28,44,68,70,72,83:84,101,109)]

names(hc.new)

##### NO SPECIAL VARIABLES #####

table(hc.new$party5)
levels(hc.new$party5)

levels(hc.new$aca)
levels(hc.new$m4all)

# Democrats overall support for governmental healthcare#

hc.dem <- hc.new[hc.new$party5=="DEMOCRAT",]
demsup <- mean(hc.dem$aca=="Very favorable"|hc.dem$aca=="Somewhat favorable"|
                 hc.dem$m4all=="Strongly favor"|hc.dem$m4all=="Somewhat favor")
demsup #94.7%

# Republicans overall support for Government Healthcare #

hc.rep <- hc.new[hc.new$party5=="REPUBLICAN",]
repsup <- mean(hc.rep$aca=="Very favorable"|hc.rep$aca=="Somewhat favorable"|
                 hc.rep$m4all=="Strongly favor"|hc.rep$m4all=="Somewhat favor")
repsup #29.6%

############################# SPECIAL VARIABLES: Age ############################# 

table(hc.new$recage2)
levels(hc.new$recage2)

# Young Democrat support for Government Healthcare #

hc.youngdem <- hc.new[(hc.new$recage2=="18-29"|hc.new$recage2=="30-49") & 
                        hc.new$party5=="DEMOCRAT",]
youngdemsup <- mean(hc.youngdem$aca=="Very favorable"|hc.youngdem$aca=="Somewhat favorable"|
                      hc.youngdem$m4all=="Strongly favor"|hc.youngdem$m4all=="Somewhat favor")
youngdemsup #94.9%

# Old Democrat support for Government Healthcare #

hc.olddem <- hc.new[(hc.new$recage2=="50-64"|hc.new$recage2=="65+") & 
                      hc.new$party5=="DEMOCRAT",]
olddemsup <- mean(hc.olddem$aca=="Very favorable"|hc.olddem$aca=="Somewhat favorable"|
                    hc.olddem$m4all=="Strongly favor"|hc.olddem$m4all=="Somewhat favor")
olddemsup #94.5%

# Young Republican support for Government Healthcare #

hc.youngrep <- hc.new[(hc.new$recage2=="18-29"|hc.new$recage2=="30-49") & 
                        hc.new$party5=="REPUBLICAN",]
youngrepsup <- mean(hc.youngrep$aca=="Very favorable"|hc.youngrep$aca=="Somewhat favorable"|
                      hc.youngrep$m4all=="Strongly favor"|hc.youngrep$m4all=="Somewhat favor")
youngrepsup #35.6%

# Old Republican support for Government Healthcare #

hc.oldrep <- hc.new[(hc.new$recage2=="50-64"|hc.new$recage2=="65+") & 
                      hc.new$party5=="REPUBLICAN",]
oldrepsup <- mean(hc.oldrep$aca=="Very favorable"|hc.oldrep$aca=="Somewhat favorable"|
                      hc.oldrep$m4all=="Strongly favor"|hc.oldrep$m4all=="Somewhat favor")
oldrepsup #27.5%

########################### SPECIAL VARIABLES: Health ########################### 

table(hc.new$rechealth2)
levels(hc.new$rechealth2)

# Healthy Democrat support for Government Healthcare #

hc.healthdem <- hc.new[hc.new$rechealth2=="E/VG/G" & 
                         hc.new$party5=="DEMOCRAT",]
healthdemsup <- mean(hc.healthdem$aca=="Very favorable"|hc.healthdem$aca=="Somewhat favorable"|
                       hc.healthdem$m4all=="Strongly favor"|hc.healthdem$m4all=="Somewhat favor")
healthdemsup #95.4%

# Unhealthy Democrat support for Government Healthcare #

hc.unhealthdem <- hc.new[hc.new$rechealth2=="OF/P" & 
                           hc.new$party5=="DEMOCRAT",]
unhealthdemsup <- mean(hc.unhealthdem$aca=="Very favorable"|hc.unhealthdem$aca=="Somewhat favorable"|
                         hc.unhealthdem$m4all=="Strongly favor"|hc.unhealthdem$m4all=="Somewhat favor")
unhealthdemsup #91.2%

# Healthy Republican support for Government Healthcare #

hc.healthrep <- hc.new[hc.new$rechealth2=="E/VG/G" & 
                         hc.new$party5=="REPUBLICAN",]
healthrepsup <- mean(hc.healthrep$aca=="Very favorable"|hc.healthrep$aca=="Somewhat favorable"|
                       hc.healthrep$m4all=="Strongly favor"|hc.healthrep$m4all=="Somewhat favor")
healthrepsup #29%

# Unhealthy Republican support for Government Healthcare #

hc.unhealthrep <- hc.new[hc.new$rechealth2=="OF/P" & 
                           hc.new$party5=="REPUBLICAN",]
unhealthrepsup <- mean(hc.unhealthrep$aca=="Very favorable"|hc.unhealthrep$aca=="Somewhat favorable"|
                         hc.unhealthrep$m4all=="Strongly favor"|hc.unhealthrep$m4all=="Somewhat favor")
unhealthrepsup #32.1%


########################### SPECIAL VARIABLES: Income ########################### 

table(hc.new$recincome)
levels(hc.new$recincome)

# Rich Democrat support for Government Healthcare #

hc.richdem <- hc.new[hc.new$recincome=="$90K+" & 
                         hc.new$party5=="DEMOCRAT",]
richdemsup <- mean(hc.richdem$aca=="Very favorable"|hc.richdem$aca=="Somewhat favorable"|
                       hc.richdem$m4all=="Strongly favor"|hc.richdem$m4all=="Somewhat favor")
richdemsup #96.4%

# Poor Democrat support for Government Healthcare #

hc.poordem <- hc.new[hc.new$recincome=="LESS THAN $40K" & 
                           hc.new$party5=="DEMOCRAT",]
poordemsup <- mean(hc.poordem$aca=="Very favorable"|hc.poordem$aca=="Somewhat favorable"|
                         hc.poordem$m4all=="Strongly favor"|hc.poordem$m4all=="Somewhat favor")
poordemsup #90.9%

# Rich Republican support for Government Healthcare #

hc.richrep <- hc.new[hc.new$recincome=="$90K+" & 
                         hc.new$party5=="REPUBLICAN",]
richrepsup <- mean(hc.richrep$aca=="Very favorable"|hc.richrep$aca=="Somewhat favorable"|
                       hc.richrep$m4all=="Strongly favor"|hc.richrep$m4all=="Somewhat favor")
richrepsup #21.9%

# Poor Republican support for Government Healthcare #

hc.poorrep <- hc.new[hc.new$recincome=="LESS THAN $40K" & 
                           hc.new$party5=="REPUBLICAN",]
poorrepsup <- mean(hc.poorrep$aca=="Very favorable"|hc.poorrep$aca=="Somewhat favorable"|
                         hc.poorrep$m4all=="Strongly favor"|hc.poorrep$m4all=="Somewhat favor")
poorrepsup #44.1%

########################### SPECIAL VARIABLES: Combinations ########################### 

# "Young, Rich, Healthy" Democrats vs. Republicans #

hc.yrhd <- hc.new[hc.new$party5=="DEMOCRAT" &
                    hc.new$rechealth2=="E/VG/G" &
                    (hc.new$recage2=="18-29"|hc.new$recage2=="30-49") &
                    hc.new$recincome=="$90K+",]
yrhd <- mean(hc.yrhd$aca=="Very favorable"|hc.yrhd$aca=="Somewhat favorable"|
                 hc.yrhd$m4all=="Strongly favor"|hc.yrhd$m4all=="Somewhat favor")
yrhd #94.4%%

hc.yrhr <- hc.new[hc.new$party5=="REPUBLICAN" &
                    hc.new$rechealth2=="E/VG/G" &
                    (hc.new$recage2=="18-29"|hc.new$recage2=="30-49") & 
                    hc.new$recincome=="$90K+",]
yrhr <- mean(hc.yrhr$aca=="Very favorable"|hc.yrhr$aca=="Somewhat favorable"|
                  hc.yrhr$m4all=="Strongly favor"|hc.yrhr$m4all=="Somewhat favor")
yrhr #23.3%

# "Old, Rich, Healthy" Democrats vs. Republicans #

hc.orhd <- hc.new[hc.new$party5=="DEMOCRAT" &
                    hc.new$rechealth2=="E/VG/G" &
                    (hc.new$recage2=="50-64"|hc.new$recage2=="65+") &
                    hc.new$recincome=="$90K+",]
orhd <- mean(hc.orhd$aca=="Very favorable"|hc.orhd$aca=="Somewhat favorable"|
               hc.orhd$m4all=="Strongly favor"|hc.orhd$m4all=="Somewhat favor")
orhd #97.6%%

hc.orhr <- hc.new[hc.new$party5=="REPUBLICAN" &
                    hc.new$rechealth2=="E/VG/G" &
                    (hc.new$recage2=="50-64"|hc.new$recage2=="65+") & 
                    hc.new$recincome=="$90K+",]
orhr <- mean(hc.orhr$aca=="Very favorable"|hc.orhr$aca=="Somewhat favorable"|
               hc.orhr$m4all=="Strongly favor"|hc.orhr$m4all=="Somewhat favor")
orhr #26.1%

# "Young, Poor, Healthy" Democrats vs. Republicans #

hc.yphd <- hc.new[hc.new$party5=="DEMOCRAT" &
                    hc.new$rechealth2=="E/VG/G" &
                    (hc.new$recage2=="18-29"|hc.new$recage2=="30-49") &
                    hc.new$recincome=="LESS THAN $40K",]
yphd <- mean(hc.yphd$aca=="Very favorable"|hc.yphd$aca=="Somewhat favorable"|
               hc.yphd$m4all=="Strongly favor"|hc.yphd$m4all=="Somewhat favor")
yphd #93.5%%

hc.yphr <- hc.new[hc.new$party5=="REPUBLICAN" &
                    hc.new$rechealth2=="E/VG/G" &
                    (hc.new$recage2=="18-29"|hc.new$recage2=="30-49") & 
                    hc.new$recincome=="LESS THAN $40K",]
yphr <- mean(hc.yphr$aca=="Very favorable"|hc.yphr$aca=="Somewhat favorable"|
               hc.yphr$m4all=="Strongly favor"|hc.yphr$m4all=="Somewhat favor")
yphr #60%

########################### Difference in Means ########################### 

# Y(treatment) - Y(control)

# Y(control) = demsup | Y(control) = repsup

# Y(age) = youngdemsup, olddemsup, youngrepsup, oldrepsup
# Y(health) = healthdemsup, unhealthdemsup, healthrepsup, unhealthrepsup
# Y(income) = richdemsup, poordemsup, richrepsup, poorrepsup
# Y(yrhd) | yrhr
# Y(orhd) | orhr
# Y(yphd) | yphr

##### Democrat Loop

somenumbers1 <- c(youngdemsup, olddemsup, healthdemsup, 
                  unhealthdemsup, richdemsup, poordemsup,
                  yrhd, orhd, yphd)
result1 <- rep(NA, length(somenumbers1))
for (i in 1:length(somenumbers1)) {
  result1[i] <- somenumbers1[i] - demsup
}
result1

averesult1 <- mean(result1)
averesult1 #average .03% DECREASE w/ variables

##### Republican Loop

somenumbers2 <- c(youngrepsup, oldrepsup, healthrepsup,
                  unhealthrepsup, richrepsup, poorrepsup,
                  yrhr, orhr, yphr)
result2 <- rep(NA, length(somenumbers2))
for (i in 1:length(somenumbers2)) {
  result2[i] <- somenumbers2[i] - repsup
}
result2

averesult2 <- mean(result2)
averesult2 #average 3% INCREASE w/ variables

########################### Grouped Bar Plot ########################### 

install.packages("ggplot2")
library(ggplot2)

democrat <- c(demsup, somenumbers1)
republican <- c(repsup, somenumbers2)

df <- data.frame(Group=rep(c("Democrat","Republican"), each=10),
                 Type=c("Party-sup", "Young-sup", "Old-sup", 
                        "Healthy-sup", "Unhealthy-sup", "Rich-sup", 
                        "Poor-sup", "YRH-sup", "ORH-sup", "YPH-sup",
                        "Party-sup", "Young-sup", "Old-sup", 
                        "Healthy-sup", "Unhealthy-sup", "Rich-sup", 
                        "Poor-sup", "YRH-sup", "ORH-sup", "YPH-sup"),
                Sup=rep(c(democrat, republican)))
df

df$Type <- factor(df$Type, levels = c("Party-sup", "Young-sup", "Old-sup",
                                      "Healthy-sup", "Unhealthy-sup", "Rich-sup",
                                      "Poor-sup", "YRH-sup", "ORH-sup", "YPH-sup"))

p <- ggplot(df, aes(x=Type, y=Sup, fill=Group))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual("legend", values = c("Democrat" = "dodgerblue2", "Republican" = "firebrick2"))
p         

p + ggtitle("Party Support for Government Healthcare") +
  xlab("Type of Support") + ylab("Support (in %)")

########################### Difference in Means Bar Plot ########################### 

meansdata1 <- c(result1,averesult1)
meansdata1

meansdata2 <- c(result2, averesult2)
meansdata2

df2 <- data.frame(Group2=rep(c("Democrat","Republican"), each=10),
                  Type2=c("Young-sup", "Old-sup", 
                          "Healthy-sup", "Unhealthy-sup", "Rich-sup",
                          "Poor-sup", "YRH-sup", "ORH-sup", "YPH-sup",
                          "Ave.Change",
                          "Young-sup", "Old-sup", 
                          "Healthy-sup", "Unhealthy-sup", "Rich-sup",
                          "Poor-sup", "YRH-sup", "ORH-sup", "YPH-sup",
                          "Ave.Change"),
                  Sup2=c(meansdata1,meansdata2))
df2

df2$Type2 <- factor(df2$Type2, levels = c("Young-sup", "Old-sup", 
                                       "Healthy-sup", "Unhealthy-sup", "Rich-sup",
                                       "Poor-sup", "YRH-sup", "ORH-sup", "YPH-sup",
                                       "Ave.Change"))

p2 <- ggplot(df2, aes(x=Type2, y=Sup2, fill=Group2))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_manual("legend", values = c("Democrat" = "dodgerblue2", "Republican" = "firebrick2"))
p2         

p2 + ggtitle("Difference in Mean Support of Treatment Groups") +
  xlab("Type of Support") + ylab("Difference in Means (in %)")


