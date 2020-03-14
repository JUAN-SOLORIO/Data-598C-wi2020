# setting working directory
setwd('C:/Users/Juan Solorio/Desktop/DataScience/Grad School/Classes/Data 598C/Data-598C-wi2020/')

# Including necessary libraries 
library(tidyverse)
library(rvest)
library(survival)
library(survminer)
library(caTools)
library(ROCR)
library(ggfortify)
library(ggExtra)

# data loaded from file 2 - Feature Engineering process
load( "data_train_forModels.RData")

data_train.notna$YardsGain <- lead(data_train.notna$YardsInDriveOff, n=1)

data2019 <- data_train.notna[data_train.notna$SeasonYear == 2019,]
data_train.notna <- data_train.notna[data_train.notna$SeasonYear != 2019,]

split <- sample.split(data_train.notna, SplitRatio = .8)

data.training <- subset(data_train.notna, split == "TRUE")
data.testing <- subset(data_train.notna, split == "FALSE")

# attributes for possible team specific drives
totest <- c('Quarter','Minute','Week','Down','ToGo','PlayNumInDrive','OffDriveOver','IsOffHomeTeam', 'IsPass')

# attributes to take into account for the general none specific team (league avg)
general.drive.test <- data.testing[,totest]
general.drive.train <- data.training[,totest]

# KM is most common survival model so we are just checking how this holds for the general premise.
KMsurv.base <- survfit(Surv(PlayNumInDrive, OffDriveOver)~1, data = general.drive.train)

ggsurvplot(KMsurv.base, data = general.drive.test)

# Checking if Home field has any effect on Offensive drives, given the 12th man in seattle might be something to consider for coach. So for the general team
KMsurv.homefield <- survfit(Surv(PlayNumInDrive, OffDriveOver)~IsOffHomeTeam, data = general.drive.train)

KMsurv.homefield <- survfit(Surv(PlayNumInDrive, OffDriveOver)~IsOffHomeTeam, data = general.drive.train)

ggsurvplot(KMsurv.homefield, data = general.drive.test, conf.int = TRUE, legend.labs=c("Away Team", "Home Team"),
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival,
           ggtheme = theme_minimal())
# Both look pretty close and deviate for longer plays, with better survival for away team.


# for a more complex survival model we'll use Cox
coxsurv.all <- coxph(Surv(PlayNumInDrive, OffDriveOver) ~ . , method="breslow", data = general.drive.train)

CoxSurvivalProb <- predict(coxsurv.all,general.drive.test,type = 'survival')

ggsurvplot(surv_fit(coxsurv.all, data = general.drive.test), conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival,
           ggtheme = theme_minimal())
# The more complex model gives us better chances of survival for drives


CoxFit <- c() 
for(i in c(1:100)){
  CoxFit[i] <- mean((1-as.numeric(predict(coxsurv.all,general.drive.test, type = 'survival') > i/100)) == general.drive.test$OffDriveOver,na.rm = T)
}

# if we were looking for 80% accuracy, we predicted about 89% of the data correctly in our test data
mean((1-as.numeric(predict(coxsurv.all,general.drive.test, type = 'survival') > .8)) == general.drive.test$OffDriveOver,na.rm = T)

dummydata <- with(general.drive.test,
                  data.frame(Quarter=as.factor(c(4)),Minute=rep(1,1),Week=rep(15,1),Down =rep(as.factor(2),1),ToGo=rep(1,1),IsOffHomeTeam=rep('Home',1), IsPass=rep(as.factor(0),1), PlayNumInDrive=9))
# transforming coxfit into an object to graph with ggsurvplot
cox_fit <- survfit(coxsurv.all)

data.test.df <- data.testing[,]
data.test.df$CoxSurvProb <- CoxSurvivalProb

# creating the logistic regression model for t
logRegmodel <- glm(OffDriveOver ~ ., data = general.drive.train, family = 'binomial')

summary(logRegmodel)

# From the summary we see that the fifth quarter or overtime has small P value, and that home field has the least of the p values for the variables that show significance

# lets predict the probability of a drive to continue, we need to subtract from 1 as this is showing low values for a drive being equal to 1 signalling drive ending
logreg.predict.t1 <- 1-predict(logRegmodel, general.drive.test, type = 'response')

logProb <- c()
for(i in c(1:100)){
  logProb[i] <- mean((as.numeric(1 - predict(logRegmodel,general.drive.test, type = 'response') > i/100)) == general.drive.test$OffDriveOver,na.rm = T)
}

mean((as.numeric(predict(logRegmodel,general.drive.test, type = 'response') > .8)) == general.drive.test$OffDriveOver,na.rm = T)

data.test.df$LogRegProb <- logreg.predict.t1

# data for testing one specific play (psudo-superbowl play)
dummydata <- with(general.drive.test,
                  data.frame(Quarter=as.factor(c(4)),Minute=rep(1,1),Week=rep(15,1),Down =rep(as.factor(2),1),ToGo=rep(1,1),IsOffHomeTeam=rep('Home',1), IsPass=rep(as.factor(0),1), PlayNumInDrive=9))


