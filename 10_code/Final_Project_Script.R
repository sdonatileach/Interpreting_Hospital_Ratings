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
prop.table(table(hospital$rate, hospital$mort), 4)
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

# model 1: everything but ho
model1 <- polr(rate ~ mort + safe + readmis + exp + effect + time + image, data=hospital)
summary(model1)
# look at coefficients
coef(model1)
# look at confidence intervals. If it crosses over zero it is significant
confint(model1)
# insignificant variables:
  # image Same as the national average  
  # time Not Available
  # effect Not Available
  # effect Same as the national average
# exponentiate to get off the log odds scale
exp(coef(model1))
exp(confint(model1))
# check for multicolinearity
vif(model1)

# model 2: add ho
model2 <- polr(rate ~ ho + mort + safe + readmis + exp + effect + time + image, data=hospital)
summary(model2)
# look at coefficients
coef(model2)
# look at confidence intervals. If it crosses over zero it is significant
confint(model2)
# insignificant variables:
  # hoGovernment - Hospital District or Authority -0.8445664  1.52175474
  # hoGovernment - Local                          -1.2614900  1.12447006
  # hoGovernment - State                          -1.9665151  0.71870395
  # hoProprietary                                 -1.0065078  1.33526755
  # hoTribal                                      -5.3563049  1.75964884
  # hoVoluntary non-profit - Church               -0.5022099  1.86617439
  # hoVoluntary non-profit - Other                -0.6044322  1.75233354
  # hoVoluntary non-profit - Private              -0.6774844  1.64438126
  # hoPhysician the only one that IS significant
# exponentiate to get off the log odds scale
exp(coef(model2))
exp(confint(model2))
# determine the best model using anova
anova(model1, model2, test = "Chisq")
# p-value is EXTREMELY small -- ho is a useful predictor


# model 3: add mort:exp
model3 <- polr(rate ~ ho + mort + safe + readmis + exp + effect + time + image + mort:exp, data=hospital)
summary(model3)
# look at coefficients
coef(model3)
# look at confidence intervals. If it crosses over zero it is significant
confint(model3)
# insignificant variables:
  # image Same as the national average 
  # time Not Available
  # effect Not Available
  # effect Same as the national average
# exponentiate to get off the log odds scale
exp(coef(model3))
exp(confint(model3))
# determine the best model using anova
anova(model2, model3, test = "Chisq")
# p-value is EXTREMELY small -- mort:exp is a useful predictor


# model 4: add safe:exp
model4 <- polr(rate ~ ho + mort + safe + readmis + exp + effect + time + image + mort:exp + safe:exp, data=hospital)
summary(model4)
# look at coefficients
coef(model4)
# look at confidence intervals. If it crosses over zero it is significant
confint(model4)
# insignificant variables:
  # image Same as the national average 
  # time Not Available
  # effect Not Available
  # effect Same as the national average
# exponentiate to get off the log odds scale
exp(coef(model4))
exp(confint(model4))
# determine the best model using anova
anova(model3, model4, test = "Chisq")
# p-value is NOT small -- safe:exp is a useful predictor

# move forward with model 3


# couldn't include state before because the levels have too few data
# can collapse the ratings to 1,2,3
# 
# hospital$rate_new[hospital$rate == 2]<- "1"
# hospital$rate_new[hospital$rate == 3]<- "2"
# hospital$rate_new[hospital$rate == 4]<- "3"
# hospital$rate_new[hospital$rate == 5]<- "3"
# 
# # factor the rating with only 5 levels now
# hospital$rate_new <- factor(hospital$rate_new)
# 
# # order the ratings
# hospital$rate_new <- ordered(hospital$rate_new,levels=c("1","2","3"))