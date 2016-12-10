# By Keith Lohse, Rehabilitation Informatics Lab, 2016-11-17

# For this analysis, you will need to install and then open the following packages:
# install.packages("metafor"); install.packages("dplyr"); install.packages("ggplot2")
library("metafor"); library("dplyr"); library("ggplot2"); library("lme4")

##----------------------- Results 1.0 ------------------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
setwd("C:/Users/krl0022/Documents/GitHub/SCOAR/group_desc/") 
list.files()

## Description of Groups Data --------------------------------------------------
GROUPS<-read.csv("./dat_GROUPS.csv", header = TRUE)
head(GROUPS)

## Table 2 ---------------------------------------------------------------------
summary(GROUPS$PT_inc[GROUPS$group=="exp"])
summary(GROUPS$PT_inc[GROUPS$group=="ctrl"])
summary(GROUPS$OT_inc[GROUPS$group=="exp"])
summary(GROUPS$OT_inc[GROUPS$group=="ctrl"])
summary(GROUPS$ST_inc[GROUPS$group=="exp"])
summary(GROUPS$ST_inc[GROUPS$group=="ctrl"])
# Frequency Data
# Hours per day 
mean(GROUPS$hpd[GROUPS$group=="exp"], na.rm=TRUE)
sd(GROUPS$hpd[GROUPS$group=="exp"], na.rm=TRUE)
min(GROUPS$hpd[GROUPS$group=="exp"], na.rm=TRUE)
max(GROUPS$hpd[GROUPS$group=="exp"], na.rm=TRUE)
sum(is.na(GROUPS$hpd[GROUPS$group=="exp"]))

mean(GROUPS$hpd[GROUPS$group=="ctrl"], na.rm=TRUE)
sd(GROUPS$hpd[GROUPS$group=="ctrl"], na.rm=TRUE)
min(GROUPS$hpd[GROUPS$group=="ctrl"], na.rm=TRUE)
max(GROUPS$hpd[GROUPS$group=="ctrl"], na.rm=TRUE)
sum(is.na(GROUPS$hpd[GROUPS$group=="ctrl"]))

# Days per Week
mean(GROUPS$dpw[GROUPS$group=="exp"], na.rm=TRUE)
sd(GROUPS$dpw[GROUPS$group=="exp"], na.rm=TRUE)
min(GROUPS$dpw[GROUPS$group=="exp"], na.rm=TRUE)
max(GROUPS$dpw[GROUPS$group=="exp"], na.rm=TRUE)
sum(is.na(GROUPS$dpw[GROUPS$group=="exp"]))

mean(GROUPS$dpw[GROUPS$group=="ctrl"], na.rm=TRUE)
sd(GROUPS$dpw[GROUPS$group=="ctrl"], na.rm=TRUE)
min(GROUPS$dpw[GROUPS$group=="ctrl"], na.rm=TRUE)
max(GROUPS$dpw[GROUPS$group=="ctrl"], na.rm=TRUE)
sum(is.na(GROUPS$dpw[GROUPS$group=="ctrl"]))

# Duration of Therapy (in weeks)
# Days per Week
mean(GROUPS$duration[GROUPS$group=="exp"], na.rm=TRUE)
sd(GROUPS$duration[GROUPS$group=="exp"], na.rm=TRUE)
min(GROUPS$duration[GROUPS$group=="exp"], na.rm=TRUE)
max(GROUPS$duration[GROUPS$group=="exp"], na.rm=TRUE)
sum(is.na(GROUPS$duration[GROUPS$group=="exp"]))

mean(GROUPS$duration[GROUPS$group=="ctrl"], na.rm=TRUE)
sd(GROUPS$duration[GROUPS$group=="ctrl"], na.rm=TRUE)
min(GROUPS$duration[GROUPS$group=="ctrl"], na.rm=TRUE)
max(GROUPS$duration[GROUPS$group=="ctrl"], na.rm=TRUE)
sum(is.na(GROUPS$duration[GROUPS$group=="ctrl"]))

# Total Time Scheduled for Therapy
mean(GROUPS$total_hours[GROUPS$group=="exp"], na.rm=TRUE)
sd(GROUPS$total_hours[GROUPS$group=="exp"], na.rm=TRUE)
min(GROUPS$total_hours[GROUPS$group=="exp"], na.rm=TRUE)
max(GROUPS$total_hours[GROUPS$group=="exp"], na.rm=TRUE)
sum(is.na(GROUPS$total_hours[GROUPS$group=="exp"]))

mean(GROUPS$total_hours[GROUPS$group=="ctrl"], na.rm=TRUE)
sd(GROUPS$total_hours[GROUPS$group=="ctrl"], na.rm=TRUE)
min(GROUPS$total_hours[GROUPS$group=="ctrl"], na.rm=TRUE)
max(GROUPS$total_hours[GROUPS$group=="ctrl"], na.rm=TRUE)
sum(is.na(GROUPS$total_hours[GROUPS$group=="ctrl"]))

# Method of Reporting Dose of Therapy
summary(as.factor(GROUPS$how_quantified[GROUPS$group=="exp"]))
summary(as.factor(GROUPS$how_quantified[GROUPS$group=="ctrl"]))


## Word and Reference Counts ---------------------------------------------------
# Filtering
WORD<-subset(GROUPS, is.na(word_count) != 1)
summary(WORD$word_count)
length(WORD$index)
m0<-lmer(word_count~1+(1|id), data=WORD, REML=FALSE)
m1<-lmer(word_count~1+group+(1|id), data=WORD, REML=FALSE)
summary(m0)
summary(m1)

g1<-ggplot(WORD, aes(x = group, y = word_count, bg=group)) +
    geom_jitter(alpha = .7) + geom_violin(alpha = .75)
g2<-g1+scale_x_discrete(name = "Group Type") +
    scale_y_continuous(name = "Word Count in Methods")
g3 <- g2 + theme(axis.text=element_text(size=14), 
                 axis.title=element_text(size=16,face="bold"))
plot(g3) 

# Reference Counts
# First we will exclude Kim 2009.
# The Experimental group in this study was an extreme outlier with 46 references
# The index number for this group is 81
REF<-subset(GROUPS, index != 81)
summary(REF$ref_count)
m0<-lmer(ref_count~1+(1|id), data=REF, REML=FALSE)
m1<-lmer(ref_count~1+group+(1|id), data=REF, REML=FALSE)
summary(m0)
summary(m1)

g1<-ggplot(REF, aes(x = group, y = ref_count, bg=group)) +
    geom_jitter(alpha = .7) + geom_violin(alpha = .75)
g2<-g1+scale_x_discrete(name = "Group Type") +
    scale_y_continuous(name = "Reference Count in Methods")
g3 <- g2 + theme(axis.text=element_text(size=14), 
                 axis.title=element_text(size=16,face="bold"))
plot(g3) 

## Tidier Criteria by Group ----------------------------------------------------
# Filtering
TIDIER<-subset(GROUPS, is.na(tidy_sum) != 1)
summary(TIDIER$tidy_sum)
m0<-lmer(tidy_sum~1+(1|id), data=TIDIER, REML=FALSE)
m1<-lmer(tidy_sum~1+group+(1|id), data=TIDIER, REML=FALSE)
summary(m0)
summary(m1)

g1<-ggplot(TIDIER, aes(x = group, y = tidy_sum, bg=group)) +
    geom_jitter(alpha = .7) + geom_violin(alpha = .75)
g2<-g1+scale_x_discrete(name = "Group Type") +
    scale_y_continuous(name = "TIDIER Score", limits=c(-1,12))
g3 <- g2 + theme(axis.text=element_text(size=14), 
                 axis.title=element_text(size=16,face="bold"))
plot(g3)    

