# By Keith Lohse, Rehabilitation Informatics Lab, 2016-11-17

# For this analysis, you will need to install and then open the following packages:
# install.packages("metafor"); install.packages("dplyr"); install.packages("ggplot2")
library("metafor"); library("dplyr"); library("ggplot2"); library("lme4")
library("lmerTest")

##----------------------- Results 1.0 ------------------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
setwd("C:/Users/krl0022/Documents/GitHub/SCOAR/group_desc/") 
list.files()

## Description of Groups Data --------------------------------------------------
GROUPS<-read.csv("./data_GROUPS_protocols_added.csv", header = TRUE)
head(GROUPS)
GROUPS$exp.c<-(as.numeric(GROUPS$group)-1.5)
CTRL<-subset(GROUPS, group=="ctrl")
EXP <-subset(GROUPS, group=="exp")

## Table 2 ---------------------------------------------------------------------
summary(EXP$PT_inc)
summary(CTRL$PT_inc)
summary(EXP$OT_inc)
summary(CTRL$OT_inc)
summary(EXP$ST_inc)
summary(CTRL$ST_inc)

# Frequency Data
## Hours per day 
mean(EXP$hpd, na.rm=TRUE)
sd(EXP$hpd, na.rm=TRUE)
min(EXP$hpd, na.rm=TRUE)
max(EXP$hpd, na.rm=TRUE)
sum(EXP$h_IS_NUM)
# Missing cases
length(EXP$hpd_STR[EXP$hpd_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(EXP$hpd))-length(EXP$hpd_STR[EXP$hpd_STR==""])
 

mean(CTRL$hpd, na.rm=TRUE)
sd(CTRL$hpd, na.rm=TRUE)
min(CTRL$hpd, na.rm=TRUE)
max(CTRL$hpd, na.rm=TRUE)
sum(CTRL$h_IS_NUM)
# Missing cases
length(CTRL$hpd_STR[CTRL$hpd_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CTRL$hpd))-length(CTRL$hpd_STR[CTRL$hpd_STR==""])

## Days per Week
mean(EXP$dpw, na.rm=TRUE)
sd(EXP$dpw, na.rm=TRUE)
min(EXP$dpw, na.rm=TRUE)
max(EXP$dpw, na.rm=TRUE)
sum(EXP$d_IS_NUM)
# Missing cases
length(EXP$dpw_STR[EXP$dpw_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(EXP$dpw))-length(EXP$dpw_STR[EXP$dpw_STR==""])

mean(CTRL$dpw, na.rm=TRUE)
sd(CTRL$dpw, na.rm=TRUE)
min(CTRL$dpw, na.rm=TRUE)
max(CTRL$dpw, na.rm=TRUE)
sum(CTRL$d_IS_NUM)
# Missing cases
length(CTRL$dpw_STR[CTRL$dpw_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CTRL$dpw))-length(CTRL$dpw_STR[CTRL$dpw_STR==""])

## Duration of Therapy (in weeks)
mean(EXP$ther_duration, na.rm=TRUE)
sd(EXP$ther_duration, na.rm=TRUE)
min(EXP$ther_duration, na.rm=TRUE)
max(EXP$ther_duration, na.rm=TRUE)
# Missing cases
length(EXP$ther_duration[EXP$ther_duration==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(EXP$ther_duration))-length(EXP$ther_duration[EXP$ther_duration==""])


mean(CTRL$ther_duration, na.rm=TRUE)
sd(CTRL$ther_duration, na.rm=TRUE)
min(CTRL$ther_duration, na.rm=TRUE)
max(CTRL$ther_duration, na.rm=TRUE)
# Missing cases
length(EXP$ther_duration[EXP$ther_duration==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(EXP$ther_duration))-length(EXP$ther_duration[EXP$ther_duration==""])


# Method of Reporting Dose of Therapy
summary(as.factor(EXP$detailed_time))
summary(as.factor(CTRL$detailed_time))
summary(as.factor(GROUPS$detailed_time))


## Word and Reference Counts ---------------------------------------------------
# Filtering out blanks
WORD<-subset(GROUPS, is.na(word_count) != 1)
summary(WORD$word_count)
m0<-lmer(word_count~1+(1|id), data=WORD, REML=FALSE)
m1<-lmer(word_count~1+exp.c+(1|id), data=WORD, REML=FALSE)
summary(m0)
summary(m1)
# Experimental Group Word Counts
mean(WORD$word_count[WORD$group=="exp"], na.rm=TRUE)
sd(WORD$word_count[WORD$group=="exp"], na.rm=TRUE)

# Control Group Word Counts
mean(WORD$word_count[WORD$group=="ctrl"], na.rm=TRUE)
sd(WORD$word_count[WORD$group=="ctrl"], na.rm=TRUE)

# Figure 1A --------------------------------------------------------------------
g1<-ggplot(WORD, aes(x = group, y = word_count, bg=group)) +
    geom_jitter(alpha = .5, width= 0.25) + geom_boxplot(alpha = .75, notch=TRUE)
g2<-g1+scale_x_discrete(name = "Group Type") +
    scale_y_continuous(name = "Word Count in Methods")
g3 <- g2 + theme(axis.text=element_text(size=16, colour="black"), 
                 axis.title=element_text(size=16,face="bold")) + 
    theme(legend.text=element_text(size=16), legend.title=element_text(size=16))
plot(g3) 
## -----------------------------------------------------------------------------

# Reference Counts
REF<-subset(GROUPS, is.na(ref_count) != 1)
summary(REF$ref_count)
m0<-lmer(ref_count~1+(1|id), data=REF, REML=FALSE)
m1<-lmer(ref_count~1+exp.c+(1|id), data=REF, REML=FALSE)
summary(m0)
summary(m1)

# Experimental Group Reference Counts
mean(REF$ref_count[REF$group=="exp"], na.rm=TRUE)
sd(REF$ref_count[REF$group=="exp"], na.rm=TRUE)

# Control Group Reference Counts
mean(REF$ref_count[REF$group=="ctrl"], na.rm=TRUE)
sd(REF$ref_count[REF$group=="ctrl"], na.rm=TRUE)

# Figure 1B --------------------------------------------------------------------
g1<-ggplot(REF, aes(x = group, y = ref_count, bg=group)) +
    geom_jitter(alpha = 0.5, width=0.25) + geom_boxplot(alpha = .75, notch=TRUE)
g2<-g1+scale_x_discrete(name = "Group Type") +
    scale_y_continuous(name = "Reference Count in Methods", limits=c(-1,20))
g3 <- g2 + theme(axis.text=element_text(size=16, colour="black"), 
                 axis.title=element_text(size=16,face="bold")) +
    theme(legend.text=element_text(size=16), legend.title=element_text(size=16))

plot(g3) 
## -----------------------------------------------------------------------------


## Tidier Criteria by Group ----------------------------------------------------
# Filtering
TIDIER<-subset(GROUPS, is.na(tidy_sum) != 1)
summary(TIDIER$tidy_sum)
m0<-lmer(tidy_sum~1+(1|id), data=TIDIER, REML=FALSE)
m1<-lmer(tidy_sum~1+exp.c+(1|id), data=TIDIER, REML=FALSE)
summary(m0)
summary(m1)

# Experimental Group Total TIDIER Scores
mean(TIDIER$tidy_sum[TIDIER$group=="exp"], na.rm=TRUE)
sd(TIDIER$tidy_sum[TIDIER$group=="exp"], na.rm=TRUE)

# Control Group Total TIDIER Scores
mean(TIDIER$tidy_sum[TIDIER$group=="ctrl"], na.rm=TRUE)
sd(TIDIER$tidy_sum[TIDIER$group=="ctrl"], na.rm=TRUE)

# Figure 1C --------------------------------------------------------------------
g1<-ggplot(TIDIER, aes(x = group, y = tidy_sum, bg=group)) +
    geom_jitter(alpha = 0.5, width=0.25) + geom_boxplot(alpha = .7, notch=TRUE)
g2<-g1+scale_x_discrete(name = "Group Type") +
    scale_y_continuous(name = "Total TIDIER Score", limits=c(-1,12))
g3 <- g2 + theme(axis.text=element_text(size=16, colour="black"), 
                 axis.title=element_text(size=16,face="bold")) +
    theme(legend.text=element_text(size=16), legend.title=element_text(size=16))

plot(g3)    
## -----------------------------------------------------------------------------

# No evidence for a change in TIDIER criteria over time.
m2<-lmer(tidy_sum~1+exp.c+year_ini+(1|id), data=TIDIER, REML=FALSE)
summary(m2)

## Figure 2A
g1<-ggplot(TIDIER, aes(x = year_ini, y = tidy_sum)) +
    geom_jitter(aes(fill=group), size=3, pch=21, alpha=0.7, width=0.25)
g2<-g1+scale_x_continuous(name = "Year of Publication") +
    scale_y_continuous(name = "Total TIDIER Score", limits=c(-1,12))
g3 <- g2 + theme(axis.text=element_text(size=16, colour="black"), 
                 axis.title=element_text(size=16,face="bold")) +
theme(legend.text=element_text(size=16), legend.title=element_text(size=16),
      legend.position = "top")

plot(g3)
## -----------------------------------------------------------------------------

# Experimental Group reporting is positively correlated with control 
# group reporting.
A<-data.frame(EXP$id, EXP$tidy_sum)
colnames(A)[1] <- "id"
B<-data.frame(CTRL$id, CTRL$tidy_sum)
colnames(B)[1] <- "id"
head(A)
head(B)

TIDY_MERGE<-merge(A, B, by="id")
TIDY_MERGE$EXP_TIDY_c<-TIDY_MERGE$EXP.tidy_sum-mean(TIDY_MERGE$EXP.tidy_sum)
TIDY_MERGE$CTRL_TIDY_c<-TIDY_MERGE$CTRL.tidy_sum-mean(TIDY_MERGE$CTRL.tidy_sum)
# 
m3<-lmer(EXP.tidy_sum~1+CTRL.tidy_sum+(1|id), data=TIDY_MERGE, REML=FALSE)
summary(m3)
# mean centered TIDIER scores
m4<-lmer(EXP_TIDY_c~1+CTRL_TIDY_c+(1|id), data=TIDY_MERGE, REML=FALSE)
summary(m4)

# Figure 2B -------------------------------------------------------------------
g1<-ggplot(TIDY_MERGE, aes(x = CTRL.tidy_sum, y = EXP.tidy_sum)) +
    geom_jitter(size=3, alpha=0.7, width=0.25)+geom_abline(intercept = 6.582, slope = 0.121)
g2<-g1+scale_x_continuous(name = "CTRL TIDIER Score", limits=c(-1,12)) +
    scale_y_continuous(name = "EXP TIDIER Score", limits=c(-1,12))
g3 <- g2 + theme(axis.text=element_text(size=16, colour="black"), 
                 axis.title=element_text(size=16,face="bold")) +
    theme(legend.text=element_text(size=16), legend.title=element_text(size=16))

plot(g3)
## -----------------------------------------------------------------------------

## Table 3. --------------------------------------------------------------------
## Analysis of Individual TIDIER Criteria 
# Brief Name
head(TIDIER)
m0<-glmer(name~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(name~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(name~group, data=TIDIER)

# Why
m0<-glmer(why~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(why~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(why~group, data=TIDIER)

# What Materials
m0<-glmer(materials~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(materials~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(materials~group, data=TIDIER)

# What procedures
m0<-glmer(procedure~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(procedure~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(procedure~group, data=TIDIER)

# Provided by whom
m0<-glmer(who~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(who~1+group+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(who~group, data=TIDIER)

# How
m0<-glmer(how~1+(1|id), family=binomial, data=TIDIER)
## For some reason the model fails to converge with the contrast coded variable
## I have dummy coded the variable here.
m1<-glmer(how~1+(as.numeric(group)-1)+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(how~group, data=TIDIER)

# Where
m0<-glmer(where~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(where~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(where~group, data=TIDIER)

# When and how much
m0<-glmer(how_much~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(how_much~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(how_much~group, data=TIDIER)

# Tailoring
m0<-glmer(tailor~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(tailor~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(tailor~group, data=TIDIER)

# Modifications
m0<-glmer(modify~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(modify~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(modify~group, data=TIDIER)

# How well planned
m0<-glmer(planned~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(planned~1+(as.numeric(group)-1)+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(planned~group, data=TIDIER)

# How well actual
m0<-glmer(actual~1+(1|id), family=binomial, data=TIDIER)
m1<-glmer(actual~1+exp.c+(1|id), family=binomial, data=TIDIER)
summary(m1)
xtabs(actual~group, data=TIDIER)






## Restricting Analyses to Conventional Therapy Groups--------------------------
head(GROUPS)
CPT<-subset(GROUPS, cpt_only==1)
write.csv(CPT, "conventional_only.csv")
# Five of the conventional therapy groups were experimental groups in their 
# original studies. These groups were experimental due to being delivered at 
# a higher dose than normal for dose-matching purposes. 
CPT[CPT$group=="exp",]

CPT_CTRL<-subset(CPT, group=="ctrl")
CPT_EXP<-subset(CPT, group=="exp")


## Table 4 ---------------------------------------------------------------------
summary(CPT$PT_inc[CPT$group=="exp"])
summary(CPT$PT_inc[CPT$group=="ctrl"])
summary(CPT$OT_inc[CPT$group=="exp"])
summary(CPT$OT_inc[CPT$group=="ctrl"])
summary(CPT$ST_inc[CPT$group=="exp"])
summary(CPT$ST_inc[CPT$group=="ctrl"])

# Frequency Data
## Hours per day 
mean(CPT_EXP$hpd, na.rm=TRUE)
sd(CPT_EXP$hpd, na.rm=TRUE)
min(CPT_EXP$hpd, na.rm=TRUE)
max(CPT_EXP$hpd, na.rm=TRUE)
sum(CPT_EXP$h_IS_NUM)
# Missing cases
length(CPT_EXP$hpd_STR[CPT_EXP$hpd_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CPT_EXP$hpd))-length(CPT_EXP$hpd_STR[CPT_EXP$hpd_STR==""])


mean(CPT_CTRL$hpd, na.rm=TRUE)
sd(CPT_CTRL$hpd, na.rm=TRUE)
min(CPT_CTRL$hpd, na.rm=TRUE)
max(CPT_CTRL$hpd, na.rm=TRUE)
sum(CPT_CTRL$h_IS_NUM)
# Missing cases
length(CPT_CTRL$hpd_STR[CTRL$hpd_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CPT_CTRL$hpd))-length(CPT_CTRL$hpd_STR[CPT_CTRL$hpd_STR==""])

## Days per Week
mean(CPT_EXP$dpw, na.rm=TRUE)
sd(CPT_EXP$dpw, na.rm=TRUE)
min(CPT_EXP$dpw, na.rm=TRUE)
max(CPT_EXP$dpw, na.rm=TRUE)
sum(CPT_EXP$d_IS_NUM)
# Missing cases
length(CPT_EXP$dpw_STR[CPT_EXP$dpw_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CPT_EXP$dpw))-length(CPT_EXP$dpw_STR[CPT_EXP$dpw_STR==""])

mean(CPT_CTRL$dpw, na.rm=TRUE)
sd(CPT_CTRL$dpw, na.rm=TRUE)
min(CPT_CTRL$dpw, na.rm=TRUE)
max(CPT_CTRL$dpw, na.rm=TRUE)
sum(CPT_CTRL$d_IS_NUM)
# Missing cases
length(CPT_CTRL$dpw_STR[CPT_CTRL$dpw_STR==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CPT_CTRL$dpw))-length(CPT_CTRL$dpw_STR[CPT_CTRL$dpw_STR==""])

## Duration of Therapy (in weeks)
mean(CPT_EXP$ther_duration, na.rm=TRUE)
sd(CPT_EXP$ther_duration, na.rm=TRUE)
min(CPT_EXP$ther_duration, na.rm=TRUE)
max(CPT_EXP$ther_duration, na.rm=TRUE)
# Missing cases
length(CPT_EXP$ther_duration[CPT_EXP$ther_duration==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CPT_EXP$ther_duration))-length(CPT_EXP$ther_duration[CPT_EXP$ther_duration==""])


mean(CPT_CTRL$ther_duration, na.rm=TRUE)
sd(CPT_CTRL$ther_duration, na.rm=TRUE)
min(CPT_CTRL$ther_duration, na.rm=TRUE)
max(CPT_CTRL$ther_duration, na.rm=TRUE)
# Missing cases
length(CPT_EXP$ther_duration[CPT_EXP$ther_duration==""])
# Numeric NAs - missing cases yields ranges
sum(is.na(CPT_EXP$ther_duration))-length(CPT_EXP$ther_duration[CPT_EXP$ther_duration==""])


# Method of Reporting Dose of Therapy
summary(as.factor(CPT_EXP$detailed_time))
summary(as.factor(CPT_CTRL$detailed_time))
summary(as.factor(CPT$detailed_time))

## Individual TIDIER Criteria 
# Brief Name
xtabs(name~group, data=CPT)
# Why
xtabs(why~group, data=CPT)
# What Materials
xtabs(materials~group, data=CPT)
# What procedures
xtabs(procedure~group, data=CPT)
# Provided by whom
xtabs(who~group, data=CPT)
# How
xtabs(how~group, data=CPT)
# Where
xtabs(where~group, data=CPT)
# When and how much
xtabs(how_much~group, data=CPT)
# Tailoring
xtabs(tailor~group, data=CPT)
# Modifications
xtabs(modify~group, data=CPT)
# How well planned
xtabs(planned~group, data=CPT)
# How well actual
xtabs(actual~group, data=CPT)

