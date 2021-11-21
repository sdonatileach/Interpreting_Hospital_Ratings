#####  Interpreting Hospital Ratings Based on Quality Measures #####

# Load libraries
library(ggplot2)
# install.packages("cobalt")
library(cobalt)
# install.packages("MatchIt")
library(MatchIt)
# install.packages("randomForest")
library(randomForest)
library(dplyr)
library(xtable)
library(lme4)
library(viridis)
library(sjPlot)
library(qqplotr)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(knitr)
library(MASS)
library(nnet)

hospital = read.csv("C:/Users/sdona/Documents/Duke/702IDS/FinalProject/Hospital_General_Information.csv", header=TRUE)

hospital$rate <- hospital$Hospital.overall.rating
hospital$ht <- hospital$Hospital.Type 
hospital$ho <- hospital$Hospital.Ownership
hospital$emerg <- hospital$Emergency.Services
hospital$ehr <- hospital$Meets.criteria.for.meaningful.use.of.EHRs
hospital$mort <- hospital$Mortality.national.comparison
hospital$safe <- hospital$Safety.of.care.national.comparison
hospital$readmis <- hospital$Readmission.national.comparison
hospital$exp <- hospital$Patient.experience.national.comparison
hospital$effect <- hospital$Effectiveness.of.care.national.comparison
hospital$time <- hospital$Timeliness.of.care.national.comparison
hospital$image <- hospital$Efficient.use.of.medical.imaging.national.comparison

# factor all the variables of interest
hospital$ht <- factor(hospital$Hospital.Type)
hospital$ho <- factor(hospital$Hospital.Ownership)
hospital$es <- factor(hospital$Emergency.Services)
hospital$ehr <- factor(hospital$Meets.criteria.for.meaningful.use.of.EHRs)
hospital$mort <- factor(hospital$Mortality.national.comparison)
hospital$safe <- factor(hospital$Safety.of.care.national.comparison)
hospital$readmis <- factor(hospital$Readmission.national.comparison)
hospital$exp <- factor(hospital$Patient.experience.national.comparison)
hospital$effect <- factor(hospital$Effectiveness.of.care.national.comparison)
hospital$time <- factor(hospital$Timeliness.of.care.national.comparison)
hospital$image <- factor(hospital$Efficient.use.of.medical.imaging.national.comparison)

#remove the variables we don't need
# removed_var <- c("Hospital.Type","Hospital.Ownership","Emergency.Services","Meets.criteria.for.meaningful.use.of.EHRs","Mortality.national.comparison","Mortality.national.comparison.footnote","Safety.of.care.national.comparison","Safety.of.care.national.comparison.footnote","Readmission.national.comparison","Readmission.national.comparison.footnote","Patient.experience.national.comparison","Patient.experience.national.comparison.footnote","Effectiveness.of.care.national.comparison","Effectiveness.of.care.national.comparison.footnote","Timeliness.of.care.national.comparison","Timeliness.of.care.national.comparison.footnote","Efficient.use.of.medical.imaging.national.comparison","Efficient.use.of.medical.imaging.national.comparison.footnote")
# hospital <- hospital[,!(removed_var(hospital) %in% drop)]

# combine U.S. territories that have few obs
territories <- c("AS","GU","MP","PR","VI")
hospital$State[is.element(hospital$State,territories)] <- "Others"

# factor the states now
hospital$State <- factor(hospital$State)

# remove hospitals that have a rating of "Not Available"
removed_na <- which(hospital$rate != "Not Available")
hospital <- hospital[removed_na,]

# factor the rating with only 5 levels now
hospital$rate <- factor(hospital$Hospital.overall.rating)

# order the ratings
hospital$rate <- ordered(hospital$rate,levels=c("1","2","3","4","5"))

#############################################
################# EDA #######################
#############################################

########### response variable ###############
table(hospital$rate)

########### varying intercept ###############

table(hospital$State, hospital$rate)

######### categorical predictors ############
# ht, ho, emerg, ehr, mort, safe, readmis, exp, effect, time, image

# hospital type
table(hospital$ht, hospital$rate)
prop.table(table(hospital$rate, hospital$ht), 2)
chisq.test(table(hospital$rate, hospital$ht))

# hospital ownership
table(hospital$ho, hospital$rate)
prop.table(table(hospital$rate, hospital$ho), 2)
chisq.test(table(hospital$rate, hospital$ho))

# mortality
table(hospital$mort, hospital$rate)
prop.table(table(hospital$rate, hospital$mort), 2)
chisq.test(table(hospital$rate, hospital$mort))

# safety
table(hospital$safe, hospital$rate)
prop.table(table(hospital$rate, hospital$safe), 2)
chisq.test(table(hospital$rate, hospital$safe))

# readmission
table(hospital$readmis, hospital$rate)
prop.table(table(hospital$rate, hospital$readmis), 2)
chisq.test(table(hospital$rate, hospital$readmis))

# patient experience
table(hospital$exp, hospital$rate)
prop.table(table(hospital$rate, hospital$exp), 2)
chisq.test(table(hospital$rate, hospital$exp))

# effectiveness of care
table(hospital$effect, hospital$rate)
prop.table(table(hospital$rate, hospital$effect), 2)
chisq.test(table(hospital$rate, hospital$effect))

# timeliness of care
table(hospital$time, hospital$rate)
prop.table(table(hospital$rate, hospital$time), 2)
chisq.test(table(hospital$rate, hospital$time))

# Efficient use of medical imaging
table(hospital$image, hospital$rate)
prop.table(table(hospital$rate, hospital$image), 2)
chisq.test(table(hospital$rate, hospital$image))

############### interactions #####################

table(hospital$rate, hospital$mort, hospital$exp)

##################################################
################# Modeling #######################
##################################################

model1 <- polr(rate ~ mort + safe + readmis + exp + effect + time + image, data=hospital)
summary(model1)

# couldn't include state because this had too few data
# can collapse the ratings to 1,2,3
# do this after we determine the best model without State
