
############  Interpreting Hospital Ratings Based on Quality Measures ##########


################################################################################
################################ Data Cleaning #################################
################################################################################

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
library(rms)
library(ordinal)


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

################################################################################
########################### Prop Odds 5 Levels #################################
################################################################################

# order the ratings
hospital$rate <- ordered(hospital$rate,levels=c("1","2","3","4","5"))

# order the predictors? NO, DON'T DO THIS!!!
# hospital$ehr <- ordered(hospital$ehr,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$mort <- ordered(hospital$mort,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$safe <- ordered(hospital$safe,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$readmis <- ordered(hospital$readmis,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$exp <- ordered(hospital$exp,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$effect <- ordered(hospital$effect,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$time <- ordered(hospital$time,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))
# hospital$image <- ordered(hospital$image,levels=c("Not Available","Below the national average","Same as the national average","Above the national average"))

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

# hospital ownership
table(hospital$ho, hospital$rate)
prop.table(table(hospital$rate, hospital$ho), 2)

# mortality IMPORTANT
table(hospital$mort, hospital$rate)
prop.table(table(hospital$rate, hospital$mort), 1)

# safety semi important
table(hospital$safe, hospital$rate)
prop.table(table(hospital$rate, hospital$safe), 2)

# readmission
table(hospital$readmis, hospital$rate)
prop.table(table(hospital$rate, hospital$readmis), 2)

# patient experience IMPORTANT
table(hospital$exp, hospital$rate)
prop.table(table(hospital$rate, hospital$exp), 2)

# effectiveness of care
table(hospital$effect, hospital$rate)
prop.table(table(hospital$rate, hospital$effect), 2)

# timeliness of care
table(hospital$time, hospital$rate)
prop.table(table(hospital$rate, hospital$time), 2)

# Efficient use of medical imaging
table(hospital$image, hospital$rate)
prop.table(table(hospital$rate, hospital$image), 2)

############### interactions #####################

### with patient experience ###

# mortality and patient experience
table(hospital$rate, hospital$mort, hospital$exp)
prop.table(table(hospital$rate, hospital$mort, hospital$exp), 2)

# safety and patient experience
table(hospital$rate, hospital$safe, hospital$exp)
prop.table(table(hospital$rate, hospital$safe, hospital$exp), 2)

# readmission and patient experience
table(hospital$rate, hospital$readmis, hospital$exp)
prop.table(table(hospital$rate, hospital$readmis, hospital$exp), 2)

### with hospital ownership ### 
#non are important looking; too split up

# hospital ownership and mortality
table(hospital$rate, hospital$mort, hospital$ho)
prop.table(table(hospital$rate, hospital$mort, hospital$ho), 2)

# hospital ownership and safety
table(hospital$rate, hospital$safe, hospital$ho)
prop.table(table(hospital$rate, hospital$safe, hospital$ho), 2)

# hospital ownership and readmission
table(hospital$rate, hospital$mort, hospital$ho)
prop.table(table(hospital$rate, hospital$mort, hospital$ho), 2)

##################################################
################# Modeling #######################
##################################################

# model 1: everything but ho
model1 <- polr(rate ~ mort + safe + readmis + exp + effect + time + image, data=hospital)


# model 2: add ho
model2 <- polr(rate ~ ho + mort + safe + readmis + exp + effect + time + image, data=hospital)
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
# significant variables:
    # hoPhysician                                                       0.11339461  3.295897666
    # mortBelow the national average                                   -5.96288721 -4.553830325
    # mortNot Available                                                -1.74196744 -0.270120482
    # mortSame as the national average                                 -2.29219382 -1.292968592
    # safeBelow the national average                                   -4.65627865 -4.040976205
    # safeNot Available                                                -1.94331682 -1.306906516
    # safeSame as the national average                                 -2.25949063 -1.787393076
    # readmisBelow the national average                                -5.00942186 -4.397656659
    # readmisNot Available                                             -2.22622978 -0.542515703
    # readmisSame as the national average                              -2.54144391 -2.056884406
    # expBelow the national average                                    -4.52480865 -3.285682650
    # expSame as the national average                                  -2.01960969 -0.793371234
    # effectBelow the national average                                 -1.64908259 -0.595153900
    # timeBelow the national average                                   -1.34815895 -0.848615846
    # timeSame as the national average                                 -0.49936769 -0.104875310
    # imageBelow the national average                                  -1.32150828 -0.623486225
    # imageNot Available                                               -0.81850393 -0.128411299
    # mortNot Available:expBelow the national average                  -3.56589058 -1.307708871
    # mortNot Available:expSame as the national average                -2.49980570 -0.342278337
    # mortSame as the national average:expSame as the national average -1.71650665 -0.413117877
# exponentiate to get off the log odds scale
exp(coef(model3))
exp(confint(model3))
# determine the best model using anova
anova(model2, model3, test = "Chisq")
# p-value is small -- mort:exp is a useful predictor


# model 4: add safe:exp
model4 <- polr(rate ~ ho + mort + safe + readmis + exp + effect + time + image + mort:exp + safe:exp, data=hospital)
# determine the best model using anova
anova(model3, model4, test = "Chisq")
# p-value is small -- safe:exp is a useful predictor

# model 5: add readmis:exp
model5 <- polr(rate ~ ho + mort + safe + readmis + exp + effect + time + image + mort:exp + safe:exp + readmis:exp, data=hospital)
# determine the best model using anova
anova(model4, model5, test = "Chisq")
# p-value is NOT small -- readmis:exp is NOT a useful predictor


# move forward with interpretation of model 4
summary(model4)
# look at coefficients
coef(model4)
# look at confidence intervals. If it crosses over zero it is significant
confint(model4)
# significant variables:
# image Same as the national average 
# time Not Available
# effect Not Available
# effect Same as the national average
# exponentiate to get off the log odds scale
exp(coef(model4))
exp(confint(model4))

# don't need to check for multicolinearity because they are all factor variables

################################################################################
########################## Prop Odds 3 Levels ##################################
################################################################################

# couldn't include state before because the levels have too few data
# can collapse the ratings to 1,2,3

hospital$rate_new <- "1"
hospital$rate_new[hospital$rate == 2]<- "1"
hospital$rate_new[hospital$rate == 3]<- "2"
hospital$rate_new[hospital$rate == 4]<- "3"
hospital$rate_new[hospital$rate == 5]<- "3"

# factor the rating with only 3 levels now
hospital$rate_new <- factor(hospital$rate_new)

# order the new ratings
hospital$rate_new <- ordered(hospital$rate_new,levels=c("1","2","3"))

# look at distribution of obs now
table(hospital$rate_new)

# look at distribution by state
table(hospital$State, hospital$rate_new)

# attempt to model with State as a categorical variable
hier_model1 <- polr(rate_new ~ State + mort + safe + readmis + exp + effect + time + image, 
              data=hospital)

summary(hier_model1)


hier_model2 <- clm(rate_new ~ State + mort + safe + readmis + exp + effect + time + image, 
               data=hospital)

summary(hier_model2)


hier_model3 <- clmm(rate_new ~ mort + safe + readmis + exp + effect + time + image + (1|State), 
              data=hospital)

summary(hier_model3)
effects <- ranef(hier_model3,condVar = T)$State

effects[order(`(Intercept)`), ]

effects[order(effects$`(Intercept)`),]

sort(effects)
