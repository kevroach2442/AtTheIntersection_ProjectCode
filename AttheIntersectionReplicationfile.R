### This is the replication file for At the Intersection
### Date 9/26/2020
### Author: Kevin Roach

#This file will have 12 parts.  
#Part 1 runs the male analysis
#Part 2 Female main analysis
#Part 3 constructs male graphs
#Part 4 constructs female graphs
#Part 5 Contraband analysis men
#Part 6 contraband analysis women
#Part 7 contraband graphs men
#Part 8 contraband graphs women
#The following parts are for different robustness checks
#Part 9 Intersectional analysis (combining men and women)
#Part 10 Intersectional contraband analysis
#Part 11 Predicting search without agency fe for men (this allows us to keep a bunch of data in NC)
#Part 12 Predicting search without agency fe for women

#To use this file find and replace "C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication"
# with the file path for the replication folder with the data inside. 

#Warning: IL and NC are VERY large datasets.  The regressions take over 10 hours to run, and even loading the data can be very time consuming.

#These parts all have a similar structure.  I will load a states data subset on the correct subgroups for that part
#Then I will run the analysis for that part, and save the results, and repeat for the next state.  
# The order of states in each part is as follow.  CT, MD, IL, NC, and NC with no hour variable.

#The graphing parts will instead load the results and create the figures represented in the paper.  

#Throughout this file you may see references to stopb, stoph, stoppurpose2, Hourfixed and type.
#Stopb is a dummy variable for Black drivers
#Stoph is a dummy variable for Hispanic drivers
#Stoppurpose2 is a dummy variable for investigatory stops.
#Hourfixed is a NC specific variable where we replaced 00:00 with . as it is clear this was used a placeholder in the original data time variable
#The type variable is NC specific, we drop one stop type, checkpoint stops.  

#You can search this document for Table X or Figure X to find the corresponding part of the R file that produced these tables/figures.  

#Part 1
#This script will run a FE model for each state, dropping Hispanics


##
## CT
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(xlsx)
library(ggeffects)

#a useful function to get the mode for later
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)
library(sandwich)

#loading the data
ct_data = read.dta13("CT_TrafficStops_6June2020.dta")
#subsetting the data on the variables/obs of interest
ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))
ct_data = subset(ct_data, ((ct_data$gender==1)))
ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

#encoding the agency
ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

#running the logit
glm= glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek)+factor(agencyid),
         data=ct_data,family="binomial")
#Getting robust standard errors
cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

#Calculating the confidence intervals
r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)
summary(glm)$coefficients[, 2]
r.est



#creating predicted values across race and stop type
#first set up the dataframe
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
newdata$stoph=0
newdata$stoppurpose2= seq(0,1,by=1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)
newdata$stopo<-0
newdata$age=as.numeric(summary(ct_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(ct_data$hour)
newdata$dayofweek=getmode(ct_data$dayofweek)
newdata$agencyid=getmode(ct_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

#saving the important information
ct_output<-data.frame(0)
ct_output$model2predictwhite = predictedvalues[1]
ct_output$model2sewhite = predictedse[1]
ct_output$model2predictwhiteinter = predictedvalues[2]
ct_output$model2sewhiteinter = predictedse[2]
ct_output$model2predictblack = predictedvalues[3]
ct_output$model2seblack = predictedse[3]
ct_output$model2predictblackinter = predictedvalues[4]
ct_output$model2seblackinter = predictedse[4]
ct_output<-ct_output[,-1]

#saving the pred probs for later
write.csv(ct_output,"ct_Output_black.csv")

nobs(glm)

#Line 1 Table 5
write.csv(r.est, file="regressionct.csv") 





##
## Maryland
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data = subset(md_data, (md_data$gender==1))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))

# the model
glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday)+factor(agencyid),
          data=md_data,family="binomial")
#robust std. errors
cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(md_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(md_data$hour)
newdata$weekday=getmode(md_data$weekday)
newdata$agencyid=getmode(md_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

md_output<-data.frame(0)
md_output$model2predictwhite = predictedvalues[1]
md_output$model2sewhite = predictedse[1]
md_output$model2predictwhiteinter = predictedvalues[2]
md_output$model2sewhiteinter = predictedse[2]
md_output$model2predictblack = predictedvalues[3]
md_output$model2seblack = predictedse[3]
md_output$model2predictblackinter = predictedvalues[4]
md_output$model2seblackinter = predictedse[4]
md_output<-md_output[,-1]

#saving the results
write.csv(md_output,"md_Output_black.csv")

nobs(glm)
#Line 2 Table 5
write.csv(as.data.frame(r.est), file="regressionmd.csv") 




#IL

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


il_data = read.dta13("IL_TrafficStops.dta")



il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
il_data<-subset(il_data, (il_data$gender==1))
il_data$stopt<-1
il_data$stops<-1
#the size of IL means we need to introduce a stop threshold for agencies
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))
#implementing an agency stop threshold.
il_data<-subset(il_data,il_data$stops>10000)

il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))

#the model
glm<-glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek)+factor(agencyid),
         data=il_data,family="binomial")

#robust std. error
cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
summary(glm)$coefficients

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(il_data$age)[4])
newdata$vehicleage=as.numeric(summary(il_data$vehicleage)[4])
newdata$hour=(getmode(il_data$hour))
newdata$dayofweek=(getmode(il_data$dayofweek))
newdata$agencyid<-(getmode(il_data$agencyid))

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

il_output<-data.frame(0)
#Now to save these predicted probs
il_output$model2predictwhite = predictedvalues[1]
il_output$model2sewhite = predictedse[1]
il_output$model2predictwhiteinter = predictedvalues[2]
il_output$model2sewhiteinter = predictedse[2]
il_output$model2predictblack = predictedvalues[3]
il_output$model2seblack = predictedse[3]
il_output$model2predictblackinter = predictedvalues[4]
il_output$model2seblackinter = predictedse[4]
il_output<-il_output[,-1]

#saving results
write.csv(il_output,"il_Output_black.csv")

nobs(glm)
summary(glm)
#Line 3 Table 5
write.csv(as.data.frame(r.est), file="regressionil.csv") 


##
## NC
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

#the model
glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")
#robust std. errors
cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hourfixed=getmode(nc_data$hourfixed)
newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

#saving results
write.csv(nc_output,"nc_Output_black.csv")
nobs(glm)

#Line 4 Table 5
write.csv(as.data.frame(r.est), file="regressionnc.csv") 


##
##NC again without hour fixed
###

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")


library(readstata13)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

#the model
glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")
#robust std errors
cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0

newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"nc_Output_black_nohour.csv")
nobs(glm)
summary(glm)

#Table B1
write.csv(as.data.frame(r.est), file="regressionncnohour.csv") 





#Part 2
#This script will run a FE model for each state, dropping hispanics, for women


##
## CT
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(xlsx)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)

# the data
ct_data = read.dta13("CT_TrafficStops_14Jan2018.dta")
#857923
ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))
ct_data = subset(ct_data, ((ct_data$gender==0)))
ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

glm= glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek)+factor(agencyid),
         data=ct_data,family="binomial")


newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
newdata$stoph=0
newdata$stoppurpose2= seq(0,1,by=1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)
newdata$stopo<-0
newdata$age=as.numeric(summary(ct_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(ct_data$hour)
newdata$dayofweek=getmode(ct_data$dayofweek)
newdata$agencyid=getmode(ct_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

ct_output<-data.frame(0)
ct_output$model2predictwhite = predictedvalues[1]
ct_output$model2sewhite = predictedse[1]
ct_output$model2predictwhiteinter = predictedvalues[2]
ct_output$model2sewhiteinter = predictedse[2]
ct_output$model2predictblack = predictedvalues[3]
ct_output$model2seblack = predictedse[3]
ct_output$model2predictblackinter = predictedvalues[4]
ct_output$model2seblackinter = predictedse[4]
ct_output<-ct_output[,-1]

write.csv(ct_output,"ct_Output_black_female.csv")

nobs(glm)
summary(glm)
#Line 6 Table 5
write.csv(as.data.frame(summary(glm)$coef), file="regressionctf.csv") 


##
## Maryland
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(xlsx)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data = subset(md_data, (md_data$gender==0))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))

glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday)+factor(agencyid),
          data=md_data,family="binomial")
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(md_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(md_data$hour)
newdata$weekday=getmode(md_data$weekday)
newdata$agencyid=getmode(md_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

md_output<-data.frame(0)
md_output$model2predictwhite = predictedvalues[1]
md_output$model2sewhite = predictedse[1]
md_output$model2predictwhiteinter = predictedvalues[2]
md_output$model2sewhiteinter = predictedse[2]
md_output$model2predictblack = predictedvalues[3]
md_output$model2seblack = predictedse[3]
md_output$model2predictblackinter = predictedvalues[4]
md_output$model2seblackinter = predictedse[4]
md_output<-md_output[,-1]

write.csv(md_output,"md_Output_black_female.csv")

nobs(glm)
summary(glm)
#Line 7 Table 5
write.csv(as.data.frame(summary(glm)$coef), file="regressionmdf.csv") 




#IL

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(readstata13)
library(xlsx)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#If you want the full data use this line instead
il_data = read.dta13("IL_TrafficStops.dta")



il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
il_data<-subset(il_data, (il_data$gender==0))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))
il_data<-subset(il_data,il_data$stops>10000)

il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))

glm<-glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek)+factor(agencyid),
         data=il_data,family="binomial")
summary(glm)$coefficients
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(il_data$age)[4])
newdata$vehicleage=as.numeric(summary(il_data$vehicleage)[4])
newdata$hour=(getmode(il_data$hour))
newdata$dayofweek=(getmode(il_data$dayofweek))
newdata$agencyid<-(getmode(il_data$agencyid))


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

il_output<-data.frame(0)
#Now to save these predicted probs
il_output$model2predictwhite = predictedvalues[1]
il_output$model2sewhite = predictedse[1]
il_output$model2predictwhiteinter = predictedvalues[2]
il_output$model2sewhiteinter = predictedse[2]
il_output$model2predictblack = predictedvalues[3]
il_output$model2seblack = predictedse[3]
il_output$model2predictblackinter = predictedvalues[4]
il_output$model2seblackinter = predictedse[4]
il_output<-il_output[,-1]

write.csv(il_output,"il_Output_black_female.csv")

nobs(glm)
summary(glm)
#Line 8 Table 5
write.csv(as.data.frame(summary(glm)$coef), file="regressionilf.csv") 


##
## NC
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data = subset(nc_data, (nc_data$gender==0))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hourfixed=getmode(nc_data$hourfixed)
newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicted values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"nc_Output_black_female.csv")
nobs(glm)

#Table B2
write.csv(as.data.frame(summary(glm)$coef), file="regressionncf.csv") 


#Part 3
#This file will use the male regression results and construct graphs from it
# clearing the workspace
rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(data.table)

#now to load the relevant datasets
ctblack<-read.csv("ct_Output_black.csv")

mdblack<-read.csv("md_Output_black.csv")



ilblack<-read.csv("il_Output_black.csv")

ncblack<-read.csv("nc_Output_black.csv")



#Creating some empty DF to use later
ctpreds<-data.frame(0)
ctpreds$Mean<-0
ctpreds$Upper<-0
ctpreds$Lower<-0
ctpreds$Group<-"Ct"
ctpreds$Race<-"Ct"
ctpreds$Invest<-"Ct"
ctpreds$State<-"Ct"
ctpreds$SE<-0
ctpreds<-ctpreds[,-1]


mdpreds<-data.frame(0)
mdpreds$Mean<-0
mdpreds$Upper<-0
mdpreds$Lower<-0
mdpreds$Group<-"Md"
mdpreds$Race<-"Md"
mdpreds$Invest<-"Md"
mdpreds$State<-"Md"
mdpreds$SE<-0
mdpreds<-mdpreds[,-1]


ncpreds<-data.frame(0)
ncpreds$Mean<-0
ncpreds$Upper<-0
ncpreds$Lower<-0
ncpreds$Group<-"Nc"
ncpreds$Race<-"Nc"
ncpreds$Invest<-"Nc"
ncpreds$State<-"Nc"
ncpreds$SE<-0
ncpreds<-ncpreds[,-1]


ilpreds<-data.frame(0)
ilpreds$Mean<-0
ilpreds$Upper<-0
ilpreds$Lower<-0
ilpreds$Group<-"Il"
ilpreds$Race<-"Il"
ilpreds$Invest<-"Il"
ilpreds$State<-"Il"
ilpreds$SE<-0
ilpreds<-ilpreds[,-1]

#Filling in these dataframes with predicted values and upper and lower CIs
#from the models we ran earlier
#CT
ctpreds[1,1]<-ctblack[2]
ctpreds[1,2]<-ctblack[2]+1.94*ctblack[3]
ctpreds[1,3]<-ctblack[2]-1.94*ctblack[3]
ctpreds[2,1]<-ctblack[4]
ctpreds[2,2]<-ctblack[4]+1.94*ctblack[5]
ctpreds[2,3]<-ctblack[4]-1.94*ctblack[5]
ctpreds[3,1]<-ctblack[6]
ctpreds[3,2]<-ctblack[6]+1.94*ctblack[7]
ctpreds[3,3]<-ctblack[6]-1.94*ctblack[7]
ctpreds[4,1]<-ctblack[8]
ctpreds[4,2]<-ctblack[8]+1.94*ctblack[9]
ctpreds[4,3]<-ctblack[8]-1.94*ctblack[9]
ctpreds[1,5]<-"White"
ctpreds[2,5]<-"White"
ctpreds[3,5]<-"Black"
ctpreds[4,5]<-"Black"
ctpreds[1,6]<-"Safety"
ctpreds[2,6]<-"Invest"
ctpreds[3,6]<-"Safety"
ctpreds[4,6]<-"Invest"
ctpreds[,7]<-"CT"
ctpreds[1,8]<-ctblack[3]
ctpreds[2,8]<-ctblack[5]
ctpreds[3,8]<-ctblack[7]
ctpreds[4,8]<-ctblack[9]
ctpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#Maryland
mdpreds[1,1]<-mdblack[2]
mdpreds[1,2]<-mdblack[2]+1.94*mdblack[3]
mdpreds[1,3]<-mdblack[2]-1.94*mdblack[3]
mdpreds[2,1]<-mdblack[4]
mdpreds[2,2]<-mdblack[4]+1.94*mdblack[5]
mdpreds[2,3]<-mdblack[4]-1.94*mdblack[5]
mdpreds[3,1]<-mdblack[6]
mdpreds[3,2]<-mdblack[6]+1.94*mdblack[7]
mdpreds[3,3]<-mdblack[6]-1.94*mdblack[7]
mdpreds[4,1]<-mdblack[8]
mdpreds[4,2]<-mdblack[8]+1.94*mdblack[9]
mdpreds[4,3]<-mdblack[8]-1.94*mdblack[9]
mdpreds[1,5]<-"White"
mdpreds[2,5]<-"White"
mdpreds[3,5]<-"Black"
mdpreds[4,5]<-"Black"
mdpreds[1,6]<-"Safety"
mdpreds[2,6]<-"Invest"
mdpreds[3,6]<-"Safety"
mdpreds[4,6]<-"Invest"
mdpreds[,7]<-"MD"
mdpreds[1,8]<-mdblack[3]
mdpreds[2,8]<-mdblack[5]
mdpreds[3,8]<-mdblack[7]
mdpreds[4,8]<-mdblack[9]
mdpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#Illinois
ilpreds[1,1]<-ilblack[2]
ilpreds[1,2]<-ilblack[2]+1.94*ilblack[3]
ilpreds[1,3]<-ilblack[2]-1.94*ilblack[3]
ilpreds[2,1]<-ilblack[4]
ilpreds[2,2]<-ilblack[4]+1.94*ilblack[5]
ilpreds[2,3]<-ilblack[4]-1.94*ilblack[5]
ilpreds[3,1]<-ilblack[6]
ilpreds[3,2]<-ilblack[6]+1.94*ilblack[7]
ilpreds[3,3]<-ilblack[6]-1.94*ilblack[7]
ilpreds[4,1]<-ilblack[8]
ilpreds[4,2]<-ilblack[8]+1.94*ilblack[9]
ilpreds[4,3]<-ilblack[8]-1.94*ilblack[9]
ilpreds[1,5]<-"White"
ilpreds[2,5]<-"White"
ilpreds[3,5]<-"Black"
ilpreds[4,5]<-"Black"
ilpreds[1,6]<-"Safety"
ilpreds[2,6]<-"Invest"
ilpreds[3,6]<-"Safety"
ilpreds[4,6]<-"Invest"
ilpreds[,7]<-"IL"
ilpreds[1,8]<-ilblack[3]
ilpreds[2,8]<-ilblack[5]
ilpreds[3,8]<-ilblack[7]
ilpreds[4,8]<-ilblack[9]
ilpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#NC
ncpreds[1,1]<-ncblack[2]
ncpreds[1,2]<-ncblack[2]+1.94*ncblack[3]
ncpreds[1,3]<-ncblack[2]-1.94*ncblack[3]
ncpreds[2,1]<-ncblack[4]
ncpreds[2,2]<-ncblack[4]+1.94*ncblack[5]
ncpreds[2,3]<-ncblack[4]-1.94*ncblack[5]
ncpreds[3,1]<-ncblack[6]
ncpreds[3,2]<-ncblack[6]+1.94*ncblack[7]
ncpreds[3,3]<-ncblack[6]-1.94*ncblack[7]
ncpreds[4,1]<-ncblack[8]
ncpreds[4,2]<-ncblack[8]+1.94*ncblack[9]
ncpreds[4,3]<-ncblack[8]-1.94*ncblack[9]
ncpreds[1,5]<-"White"
ncpreds[2,5]<-"White"
ncpreds[3,5]<-"Black"
ncpreds[4,5]<-"Black"
ncpreds[1,6]<-"Safety"
ncpreds[2,6]<-"Invest"
ncpreds[3,6]<-"Safety"
ncpreds[4,6]<-"Invest"
ncpreds[,7]<-"NC"
ncpreds[1,8]<-ncblack[3]
ncpreds[2,8]<-ncblack[5]
ncpreds[3,8]<-ncblack[7]
ncpreds[4,8]<-ncblack[9]
ncpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#The difference in difference simulation to illustrate the interaction effect
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

#creating 10000 sets of predicted values from the predicted probs for different subsets of the interaction
temp<-rnorm(n=10000,mean=ctblack$model2predictwhite,sd=ctblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))


temp<-rnorm(n=10000,mean=ctblack$model2predictwhiteinter,sd=ctblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ctblack$model2predictblack,sd=ctblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ctblack$model2predictblackinter,sd=ctblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


#putting it all together
difindifpred<-vector()
difindifpred<-(blackinter-black)-(whiteinter-white)
#finding the quantiles from these 10000
ctdifindif<-data.frame(0)
ctdifindif[1,1]<-mean(as.numeric(difindifpred))
ctdifindif[1,2]<-quantile(difindifpred,probs=.975)
ctdifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=mdblack$model2predictwhite,sd=mdblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=mdblack$model2predictwhiteinter,sd=mdblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=mdblack$model2predictblack,sd=mdblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=mdblack$model2predictblackinter,sd=mdblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
mddifindif<-data.frame(0)
mddifindif[1,1]<-mean(as.numeric(difindifpred))
mddifindif[1,2]<-quantile(difindifpred,probs=.975)
mddifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ilblack$model2predictwhite,sd=ilblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ilblack$model2predictwhiteinter,sd=ilblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ilblack$model2predictblack,sd=ilblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ilblack$model2predictblackinter,sd=ilblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-as.numeric((blackinter-black)-(whiteinter-white))
ildifindif<-data.frame(0)
ildifindif[1,1]<-mean(as.numeric(difindifpred))
ildifindif[1,2]<-quantile(difindifpred,probs=.975)
ildifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ncblack$model2predictwhite,sd=ncblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ncblack$model2predictwhiteinter,sd=ncblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ncblack$model2predictblack,sd=ncblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ncblack$model2predictblackinter,sd=ncblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ncdifindif<-difindifpred
ncdifindif<-data.frame(0)
ncdifindif[1,1]<-mean(as.numeric(difindifpred))
ncdifindif[1,2]<-quantile(difindifpred,probs=.975)
ncdifindif[1,3]<-quantile(difindifpred,probs=.025)

#combining each state into one DF
difindifs<-data.frame(0)
difindifs[1,1]<-ctdifindif[1,1]
difindifs[1,2]<-ctdifindif[1,2]
difindifs[1,3]<-ctdifindif[1,3]
difindifs[2,1]<-mddifindif[1,1]
difindifs[2,2]<-mddifindif[1,2]
difindifs[2,3]<-mddifindif[1,3]
difindifs[3,1]<-ildifindif[1,1]
difindifs[3,2]<-ildifindif[1,2]
difindifs[3,3]<-ildifindif[1,3]
difindifs[4,1]<-ncdifindif[1,1]
difindifs[4,2]<-ncdifindif[1,2]
difindifs[4,3]<-ncdifindif[1,3]
colnames(difindifs)<-c("Mean","Upper","Lower")
difindifs$State<-c("CT","MD","IL","NC")

#The graphs
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(ggplot2)

allstates<-rbind(ctpreds,mdpreds,ilpreds,ncpreds)
write.csv(allstates, file="figuredata.csv")

#The following code puts all states into the same graph and changes the interaction graph to a bar graph
#Making one big graph of all 4 states, separated

allstates<-rbind(ctpreds,mdpreds,ilpreds,ncpreds)

#some sample code
png("allstates.png")
dodge <- position_dodge(width = 0.9)
ggplot(allstates, aes(x = interaction(Race, State), y = Mean, fill = factor(Invest))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower), position = dodge, width = 0.2)+ylab("Predicted Search Rate")+
  scale_fill_grey(start=.5,name="Stop Type",labels=c("Investigatory Stop", "Safety Stops"))+
  xlab("")+scale_x_discrete(labels=c("CT-Black","CT-White","IL-Black","IL-White","MD-Black","MD-White","NC-Black","NC-White"))
dev.off()

png("interactionmbar.png")
dodge <- position_dodge(width = 0.9)
ggplot(difindifs, aes(x = State, y = Mean)) +
  geom_bar(stat = "identity", position = position_dodge(),fill = "gray") +
  geom_errorbar(aes(ymax = Upper, ymin = Lower), position = dodge, width = 0.2)+ylab("Difference in Predicted Probabilities")+
  xlab("")+scale_x_discrete(labels=c("CT","IL","MD","NC"))+theme_bw(base_size = 15)
dev.off()

library(readr)
library(ggplot2)
figuredata = allstates

figuredata$State = ifelse(figuredata$State=="NC","North Carolina",
                          ifelse(figuredata$State=="CT","Connecticut",
                                 ifelse(figuredata$State=="IL","Illinois",
                                        "Maryland")))
#Figure 1a
png("Fig_PredictedSR_StateRaceStop.png",928,591)
ggplot(figuredata, aes(x = Race, y = Mean, fill = Invest)) +
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_errorbar(aes(ymax = Upper, ymin = Lower),
                position = position_dodge(1), width = 0.2)+
  ylab("Predicted Probabilty of a Search")+
  scale_fill_grey(start=.5,name="Stop Type",labels=c("Investigatory", "Safety"))+
  xlab("")+theme_bw(base_size = 15)+
  theme(legend.position = "bottom")+facet_grid(. ~ State)
dev.off()

#Figure 2a

png("Fig_PredictedInteraction_StateRaceStop.png")
dodge <- position_dodge(width = 0.9)
ggplot(difindifs, aes(x = State, y = Mean)) +
  geom_bar(stat = "identity", position = position_dodge(),fill = "gray") +
  geom_errorbar(aes(ymax = Upper, ymin = Lower), position = dodge, width = 0.2)+ylab("Difference in Predicted Probabilities")+
  xlab("")+scale_x_discrete(labels=c("CT","IL","MD","NC"))+theme_bw(base_size = 15)
dev.off()



#Part 4
#Same as part 3 but for women

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(data.table)


#now to load the relevant datasets
ctblack<-read.csv("ct_Output_black_female.csv")

mdblack<-read.csv("md_Output_black_female.csv")


ilblack<-read.csv("il_Output_black_female.csv")


ncblack<-read.csv("nc_Output_black_female.csv")




ctpreds<-data.frame(0)
ctpreds$Mean<-0
ctpreds$Upper<-0
ctpreds$Lower<-0
ctpreds$Group<-"Ct"
ctpreds$Race<-"Ct"
ctpreds$Invest<-"Ct"
ctpreds$State<-"Ct"
ctpreds$SE<-0
ctpreds<-ctpreds[,-1]


mdpreds<-data.frame(0)
mdpreds$Mean<-0
mdpreds$Upper<-0
mdpreds$Lower<-0
mdpreds$Group<-"Md"
mdpreds$Race<-"Md"
mdpreds$Invest<-"Md"
mdpreds$State<-"Md"
mdpreds$SE<-0
mdpreds<-mdpreds[,-1]


ncpreds<-data.frame(0)
ncpreds$Mean<-0
ncpreds$Upper<-0
ncpreds$Lower<-0
ncpreds$Group<-"Nc"
ncpreds$Race<-"Nc"
ncpreds$Invest<-"Nc"
ncpreds$State<-"Nc"
ncpreds$SE<-0
ncpreds<-ncpreds[,-1]


ilpreds<-data.frame(0)
ilpreds$Mean<-0
ilpreds$Upper<-0
ilpreds$Lower<-0
ilpreds$Group<-"Il"
ilpreds$Race<-"Il"
ilpreds$Invest<-"Il"
ilpreds$State<-"Il"
ilpreds$SE<-0
ilpreds<-ilpreds[,-1]

ctpreds[1,1]<-ctblack[2]
ctpreds[1,2]<-ctblack[2]+1.94*ctblack[3]
ctpreds[1,3]<-ctblack[2]-1.94*ctblack[3]
ctpreds[2,1]<-ctblack[4]
ctpreds[2,2]<-ctblack[4]+1.94*ctblack[5]
ctpreds[2,3]<-ctblack[4]-1.94*ctblack[5]
ctpreds[3,1]<-ctblack[6]
ctpreds[3,2]<-ctblack[6]+1.94*ctblack[7]
ctpreds[3,3]<-ctblack[6]-1.94*ctblack[7]
ctpreds[4,1]<-ctblack[8]
ctpreds[4,2]<-ctblack[8]+1.94*ctblack[9]
ctpreds[4,3]<-ctblack[8]-1.94*ctblack[9]
ctpreds[1,5]<-"White"
ctpreds[2,5]<-"White"
ctpreds[3,5]<-"Black"
ctpreds[4,5]<-"Black"
ctpreds[1,6]<-"Safety"
ctpreds[2,6]<-"Invest"
ctpreds[3,6]<-"Safety"
ctpreds[4,6]<-"Invest"
ctpreds[,7]<-"CT"
ctpreds[1,8]<-ctblack[3]
ctpreds[2,8]<-ctblack[5]
ctpreds[3,8]<-ctblack[7]
ctpreds[4,8]<-ctblack[9]
ctpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


mdpreds[1,1]<-mdblack[2]
mdpreds[1,2]<-mdblack[2]+1.94*mdblack[3]
mdpreds[1,3]<-mdblack[2]-1.94*mdblack[3]
mdpreds[2,1]<-mdblack[4]
mdpreds[2,2]<-mdblack[4]+1.94*mdblack[5]
mdpreds[2,3]<-mdblack[4]-1.94*mdblack[5]
mdpreds[3,1]<-mdblack[6]
mdpreds[3,2]<-mdblack[6]+1.94*mdblack[7]
mdpreds[3,3]<-mdblack[6]-1.94*mdblack[7]
mdpreds[4,1]<-mdblack[8]
mdpreds[4,2]<-mdblack[8]+1.94*mdblack[9]
mdpreds[4,3]<-mdblack[8]-1.94*mdblack[9]
mdpreds[1,5]<-"White"
mdpreds[2,5]<-"White"
mdpreds[3,5]<-"Black"
mdpreds[4,5]<-"Black"
mdpreds[1,6]<-"Safety"
mdpreds[2,6]<-"Invest"
mdpreds[3,6]<-"Safety"
mdpreds[4,6]<-"Invest"
mdpreds[,7]<-"MD"
mdpreds[1,8]<-mdblack[3]
mdpreds[2,8]<-mdblack[5]
mdpreds[3,8]<-mdblack[7]
mdpreds[4,8]<-mdblack[9]
mdpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


ilpreds[1,1]<-ilblack[2]
ilpreds[1,2]<-ilblack[2]+1.94*ilblack[3]
ilpreds[1,3]<-ilblack[2]-1.94*ilblack[3]
ilpreds[2,1]<-ilblack[4]
ilpreds[2,2]<-ilblack[4]+1.94*ilblack[5]
ilpreds[2,3]<-ilblack[4]-1.94*ilblack[5]
ilpreds[3,1]<-ilblack[6]
ilpreds[3,2]<-ilblack[6]+1.94*ilblack[7]
ilpreds[3,3]<-ilblack[6]-1.94*ilblack[7]
ilpreds[4,1]<-ilblack[8]
ilpreds[4,2]<-ilblack[8]+1.94*ilblack[9]
ilpreds[4,3]<-ilblack[8]-1.94*ilblack[9]
ilpreds[1,5]<-"White"
ilpreds[2,5]<-"White"
ilpreds[3,5]<-"Black"
ilpreds[4,5]<-"Black"
ilpreds[1,6]<-"Safety"
ilpreds[2,6]<-"Invest"
ilpreds[3,6]<-"Safety"
ilpreds[4,6]<-"Invest"
ilpreds[,7]<-"IL"
ilpreds[1,8]<-ilblack[3]
ilpreds[2,8]<-ilblack[5]
ilpreds[3,8]<-ilblack[7]
ilpreds[4,8]<-ilblack[9]
ilpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


ncpreds[1,1]<-ncblack[2]
ncpreds[1,2]<-ncblack[2]+1.94*ncblack[3]
ncpreds[1,3]<-ncblack[2]-1.94*ncblack[3]
ncpreds[2,1]<-ncblack[4]
ncpreds[2,2]<-ncblack[4]+1.94*ncblack[5]
ncpreds[2,3]<-ncblack[4]-1.94*ncblack[5]
ncpreds[3,1]<-ncblack[6]
ncpreds[3,2]<-ncblack[6]+1.94*ncblack[7]
ncpreds[3,3]<-ncblack[6]-1.94*ncblack[7]
ncpreds[4,1]<-ncblack[8]
ncpreds[4,2]<-ncblack[8]+1.94*ncblack[9]
ncpreds[4,3]<-ncblack[8]-1.94*ncblack[9]
ncpreds[1,5]<-"White"
ncpreds[2,5]<-"White"
ncpreds[3,5]<-"Black"
ncpreds[4,5]<-"Black"
ncpreds[1,6]<-"Safety"
ncpreds[2,6]<-"Invest"
ncpreds[3,6]<-"Safety"
ncpreds[4,6]<-"Invest"
ncpreds[,7]<-"NC"
ncpreds[1,8]<-ncblack[3]
ncpreds[2,8]<-ncblack[5]
ncpreds[3,8]<-ncblack[7]
ncpreds[4,8]<-ncblack[9]
ncpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ctblack$model2predictwhite,sd=ctblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ctblack$model2predictwhiteinter,sd=ctblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ctblack$model2predictblack,sd=ctblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ctblack$model2predictblackinter,sd=ctblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ctdifindif<-data.frame(0)
ctdifindif[1,1]<-mean(as.numeric(difindifpred))
ctdifindif[1,2]<-quantile(difindifpred,probs=.975)
ctdifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=mdblack$model2predictwhite,sd=mdblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=mdblack$model2predictwhiteinter,sd=mdblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=mdblack$model2predictblack,sd=mdblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=mdblack$model2predictblackinter,sd=mdblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
mddifindif<-data.frame(0)
mddifindif[1,1]<-mean(as.numeric(difindifpred))
mddifindif[1,2]<-quantile(difindifpred,probs=.975)
mddifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ilblack$model2predictwhite,sd=ilblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ilblack$model2predictwhiteinter,sd=ilblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ilblack$model2predictblack,sd=ilblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ilblack$model2predictblackinter,sd=ilblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-as.numeric((blackinter-black)-(whiteinter-white))
ildifindif<-data.frame(0)
ildifindif[1,1]<-mean(as.numeric(difindifpred))
ildifindif[1,2]<-quantile(difindifpred,probs=.975)
ildifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ncblack$model2predictwhite,sd=ncblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ncblack$model2predictwhiteinter,sd=ncblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ncblack$model2predictblack,sd=ncblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ncblack$model2predictblackinter,sd=ncblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ncdifindif<-difindifpred
ncdifindif<-data.frame(0)
ncdifindif[1,1]<-mean(as.numeric(difindifpred))
ncdifindif[1,2]<-quantile(difindifpred,probs=.975)
ncdifindif[1,3]<-quantile(difindifpred,probs=.025)
difindifs<-data.frame(0)
difindifs[1,1]<-ctdifindif[1,1]
difindifs[1,2]<-ctdifindif[1,2]
difindifs[1,3]<-ctdifindif[1,3]
difindifs[2,1]<-mddifindif[1,1]
difindifs[2,2]<-mddifindif[1,2]
difindifs[2,3]<-mddifindif[1,3]
difindifs[3,1]<-ildifindif[1,1]
difindifs[3,2]<-ildifindif[1,2]
difindifs[3,3]<-ildifindif[1,3]
difindifs[4,1]<-ncdifindif[1,1]
difindifs[4,2]<-ncdifindif[1,2]
difindifs[4,3]<-ncdifindif[1,3]
colnames(difindifs)<-c("Mean","Upper","Lower")
difindifs$State<-c("CT","MD","IL","NC")
#The graphs
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(ggplot2)


library(readr)
library(ggplot2)
allstates<-rbind(ctpreds,mdpreds,ilpreds,ncpreds)
figuredata = allstates

figuredata$State = ifelse(figuredata$State=="NC","North Carolina",
                          ifelse(figuredata$State=="CT","Connecticut",
                                 ifelse(figuredata$State=="IL","Illinois",
                                        "Maryland")))
#Figure 1b
png("Fig_PredictedSR_StateRaceStop_Female.png",928,591)
ggplot(figuredata, aes(x = Race, y = Mean, fill = Invest)) +
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_errorbar(aes(ymax = Upper, ymin = Lower),
                position = position_dodge(1), width = 0.2)+
  ylab("Predicted Probabilty of a Search")+
  scale_fill_grey(start=.5,name="Stop Type",labels=c("Investigatory", "Safety"))+
  xlab("")+theme_bw(base_size = 15)+
  theme(legend.position = "bottom")+facet_grid(. ~ State)
dev.off()

#Figure 2b

png("Fig_PredictedInteraction_StateRaceStop_Female.png")
dodge <- position_dodge(width = 0.9)
ggplot(difindifs, aes(x = State, y = Mean)) +
  geom_bar(stat = "identity", position = position_dodge(),fill = "gray") +
  geom_errorbar(aes(ymax = Upper, ymin = Lower), position = dodge, width = 0.2)+ylab("Difference in Predicted Probabilities")+
  xlab("")+scale_x_discrete(labels=c("CT","IL","MD","NC"))+theme_bw(base_size = 15)
dev.off()


#Part 5
#this part will cover the contraband analysis for men, note the datasets are subsetted on only those stops where a search was conducted.

##
## CT
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(readstata13)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)

# the data
ct_data =read.dta13("CT_TrafficStops_14Jan2018.dta")

#857923
ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))
ct_data = subset(ct_data, ((ct_data$gender==1)))
ct_data = subset(ct_data, ((ct_data$searchoccur==1)))
ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

glm= glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek)+factor(agencyid),
         data=ct_data,family="binomial")


newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
newdata$stoph=0
newdata$stoppurpose2= seq(0,1,by=1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)
newdata$stopo<-0
newdata$age=as.numeric(summary(ct_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(ct_data$hour)
newdata$dayofweek=getmode(ct_data$dayofweek)
newdata$agencyid=getmode(ct_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

ct_output<-data.frame(0)
ct_output$model2predictwhite = predictedvalues[1]
ct_output$model2sewhite = predictedse[1]
ct_output$model2predictwhiteinter = predictedvalues[2]
ct_output$model2sewhiteinter = predictedse[2]
ct_output$model2predictblack = predictedvalues[3]
ct_output$model2seblack = predictedse[3]
ct_output$model2predictblackinter = predictedvalues[4]
ct_output$model2seblackinter = predictedse[4]
ct_output<-ct_output[,-1]

write.csv(ct_output,"contraband_ct_Output_black.csv")

nobs(glm)
summary(glm)
#Line 1 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandregressionct.csv") 


##
## Maryland- Black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(xlsx)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data = subset(md_data, (md_data$searchoccur==1))
md_data = subset(md_data, (md_data$gender==1))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))

glm = glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday)+factor(agencyid),
          data=md_data,family="binomial")
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(md_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(md_data$hour)
newdata$weekday=getmode(md_data$weekday)
newdata$agencyid=getmode(md_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

md_output<-data.frame(0)
md_output$model2predictwhite = predictedvalues[1]
md_output$model2sewhite = predictedse[1]
md_output$model2predictwhiteinter = predictedvalues[2]
md_output$model2sewhiteinter = predictedse[2]
md_output$model2predictblack = predictedvalues[3]
md_output$model2seblack = predictedse[3]
md_output$model2predictblackinter = predictedvalues[4]
md_output$model2seblackinter = predictedse[4]
md_output<-md_output[,-1]

write.csv(md_output,"contraband_md_Output_black.csv")

nobs(glm)
summary(glm)
#Line 2 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandregressionmd.csv") 




#IL

# clearing the workspace
rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
library(xlsx)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




il_data<-read.dta13("IL_TrafficStops.dta")


il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
il_data<-subset(il_data, (il_data$gender==1))
il_data<-subset(il_data, (il_data$searchoccur==1))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))
il_data<-subset(il_data,il_data$stops>10000)

il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))

glm<-glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek)+factor(agencyid),
         data=il_data,family="binomial")
summary(glm)$coefficients
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(il_data$age)[4])
newdata$vehicleage=as.numeric(summary(il_data$vehicleage)[4])
newdata$hour=(getmode(il_data$hour))
newdata$dayofweek=(getmode(il_data$dayofweek))
newdata$agencyid<-(getmode(il_data$agencyid))


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

il_output<-data.frame(0)
#Now to save these predicted probs
il_output$model2predictwhite = predictedvalues[1]
il_output$model2sewhite = predictedse[1]
il_output$model2predictwhiteinter = predictedvalues[2]
il_output$model2sewhiteinter = predictedse[2]
il_output$model2predictblack = predictedvalues[3]
il_output$model2seblack = predictedse[3]
il_output$model2predictblackinter = predictedvalues[4]
il_output$model2seblackinter = predictedse[4]
il_output<-il_output[,-1]

write.csv(il_output,"contraband_il_Output_black.csv")

nobs(glm)
summary(glm)
#Line 3 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandregressionil.csv") 


##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")


nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data = subset(nc_data, (nc_data$searchoccur==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hourfixed=getmode(nc_data$hourfixed)
newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"contraband_nc_Output_black.csv")
nobs(glm)
summary(glm)

#Line 4 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandregressionnc.csv") 


##
##NC again without hour fixed
##

# clearing the workspace
rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")


nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data = subset(nc_data, (nc_data$searchoccur==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(contraoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0

newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"contraband_nc_Output_black_nohour.csv")
nobs(glm)
summary(glm)

#Table B3
write.csv(as.data.frame(summary(glm)$coef), file="contrabandregressionncnohour.csv") 



#Part 6
#This script will repeat the regressions ran for searches on contaband being found, for women


##
## CT
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(readstata13)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)

# the data
ct_data =read.dta13("CT_TrafficStops_6June2020.dta")

#857923
ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))
ct_data = subset(ct_data, ((ct_data$gender==0)))
ct_data = subset(ct_data, ((ct_data$searchoccur==1)))
ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

glm= glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek)+factor(agencyid),
         data=ct_data,family="binomial")


newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
newdata$stoph=0
newdata$stoppurpose2= seq(0,1,by=1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)
newdata$stopo<-0
newdata$age=as.numeric(summary(ct_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(ct_data$hour)
newdata$dayofweek=getmode(ct_data$dayofweek)
newdata$agencyid=getmode(ct_data$agencyid)
newdata$agencyid=48

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit
predictedvalues

ct_output<-data.frame(0)
ct_output$model2predictwhite = predictedvalues[1]
ct_output$model2sewhite = predictedse[1]
ct_output$model2predictwhiteinter = predictedvalues[2]
ct_output$model2sewhiteinter = predictedse[2]
ct_output$model2predictblack = predictedvalues[3]
ct_output$model2seblack = predictedse[3]
ct_output$model2predictblackinter = predictedvalues[4]
ct_output$model2seblackinter = predictedse[4]
ct_output<-ct_output[,-1]

write.csv(ct_output,"contrabandf_ct_Output_black.csv")

nobs(glm)
summary(glm)
#Line 5 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandfregressionct.csv") 


##
## Maryland- Black
##

# clearing the workspace
rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data = subset(md_data, (md_data$searchoccur==1))
md_data = subset(md_data, (md_data$gender==0))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))

glm = glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday)+factor(agencyid),
          data=md_data,family="binomial")
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(md_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(md_data$hour)
newdata$weekday=getmode(md_data$weekday)
newdata$agencyid=getmode(md_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

md_output<-data.frame(0)
md_output$model2predictwhite = predictedvalues[1]
md_output$model2sewhite = predictedse[1]
md_output$model2predictwhiteinter = predictedvalues[2]
md_output$model2sewhiteinter = predictedse[2]
md_output$model2predictblack = predictedvalues[3]
md_output$model2seblack = predictedse[3]
md_output$model2predictblackinter = predictedvalues[4]
md_output$model2seblackinter = predictedse[4]
md_output<-md_output[,-1]

write.csv(md_output,"contrabandf_md_Output_black.csv")

nobs(glm)
summary(glm)
#Line 6 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandfregressionmd.csv") 




#IL

# clearing the workspace
rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(readstata13)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




il_data<-read.dta13("IL_TrafficStops.dta")


il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
il_data<-subset(il_data, (il_data$gender==0))
il_data<-subset(il_data, (il_data$searchoccur==1))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))
il_data<-subset(il_data,il_data$stops>1000)

il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))

glm<-glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek)+factor(agencyid),
         data=il_data,family="binomial")
summary(glm)$coefficients
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(il_data$age)[4])
newdata$vehicleage=as.numeric(summary(il_data$vehicleage)[4])
newdata$hour=(getmode(il_data$hour))
newdata$dayofweek=(getmode(il_data$dayofweek))
newdata$agencyid<-(getmode(il_data$agencyid))


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

il_output<-data.frame(0)
#Now to save these predicted probs
il_output$model2predictwhite = predictedvalues[1]
il_output$model2sewhite = predictedse[1]
il_output$model2predictwhiteinter = predictedvalues[2]
il_output$model2sewhiteinter = predictedse[2]
il_output$model2predictblack = predictedvalues[3]
il_output$model2seblack = predictedse[3]
il_output$model2predictblackinter = predictedvalues[4]
il_output$model2seblackinter = predictedse[4]
il_output<-il_output[,-1]

write.csv(il_output,"contrabandf_il_Output_black.csv")

nobs(glm)
summary(glm)
#Line 7 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandfregressionil.csv") 


##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")


nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data = subset(nc_data, (nc_data$gender==0))
nc_data = subset(nc_data, (nc_data$searchoccur==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>1000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hourfixed=getmode(nc_data$hourfixed)
newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"contrabandf_nc_Output_black.csv")
nobs(glm)
summary(glm)

#Line 8 Table 6
write.csv(as.data.frame(summary(glm)$coef), file="contrabandfregressionnc.csv") 


##
##NC again without hour fixed

##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")


nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$gender==0))
nc_data = subset(nc_data, (nc_data$searchoccur==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>1000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(contraoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0

newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"contrabandf_nc_Output_black_nohour.csv")
nobs(glm)
summary(glm)

#table B4
write.csv(as.data.frame(summary(glm)$coef), file="contrabandfregressionncnohour.csv") 



#Part 7
#this part cover graph constuction for the male contraband analysis


# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(data.table)


#now to load the relevant datasets
ctblack<-read.csv("contraband_ct_Output_black.csv")

mdblack<-read.csv("contraband_md_Output_black.csv")


ilblack<-read.csv("contraband_il_Output_black.csv")


ncblack<-read.csv("contraband_nc_Output_black.csv")



ctpreds<-data.frame(0)
ctpreds$Mean<-0
ctpreds$Upper<-0
ctpreds$Lower<-0
ctpreds$Group<-"Ct"
ctpreds$Race<-"Ct"
ctpreds$Invest<-"Ct"
ctpreds$State<-"Ct"
ctpreds$SE<-0
ctpreds<-ctpreds[,-1]


mdpreds<-data.frame(0)
mdpreds$Mean<-0
mdpreds$Upper<-0
mdpreds$Lower<-0
mdpreds$Group<-"Md"

mdpreds$Race<-"Ct"
mdpreds$Invest<-"Ct"
mdpreds$State<-"Md"
mdpreds$SE<-0
mdpreds<-mdpreds[,-1]


ncpreds<-data.frame(0)
ncpreds$Mean<-0
ncpreds$Upper<-0
ncpreds$Lower<-0
ncpreds$Group<-"Nc"
ncpreds$Race<-"Nc"
ncpreds$Invest<-"Nc"
ncpreds$State<-"Nc"
ncpreds$SE<-0
ncpreds<-ncpreds[,-1]


ilpreds<-data.frame(0)
ilpreds$Mean<-0
ilpreds$Upper<-0
ilpreds$Lower<-0
ilpreds$Group<-"Il"
ilpreds$Race<-"Il"
ilpreds$Invest<-"Il"
ilpreds$State<-"Il"
ilpreds$SE<-0
ilpreds<-ilpreds[,-1]

ctpreds[1,1]<-ctblack[2]
ctpreds[1,2]<-ctblack[2]+1.94*ctblack[3]
ctpreds[1,3]<-ctblack[2]-1.94*ctblack[3]
ctpreds[2,1]<-ctblack[4]
ctpreds[2,2]<-ctblack[4]+1.94*ctblack[5]
ctpreds[2,3]<-ctblack[4]-1.94*ctblack[5]
ctpreds[3,1]<-ctblack[6]
ctpreds[3,2]<-ctblack[6]+1.94*ctblack[7]
ctpreds[3,3]<-ctblack[6]-1.94*ctblack[7]
ctpreds[4,1]<-ctblack[8]
ctpreds[4,2]<-ctblack[8]+1.94*ctblack[9]
ctpreds[4,3]<-ctblack[8]-1.94*ctblack[9]
ctpreds[1,5]<-"White"
ctpreds[2,5]<-"White"
ctpreds[3,5]<-"Black"
ctpreds[4,5]<-"Black"
ctpreds[1,6]<-"Safety"
ctpreds[2,6]<-"Invest"
ctpreds[3,6]<-"Safety"
ctpreds[4,6]<-"Invest"
ctpreds[,7]<-"CT"
ctpreds[1,8]<-ctblack[3]
ctpreds[2,8]<-ctblack[5]
ctpreds[3,8]<-ctblack[7]
ctpreds[4,8]<-ctblack[9]
ctpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


mdpreds[1,1]<-mdblack[2]
mdpreds[1,2]<-mdblack[2]+1.94*mdblack[3]
mdpreds[1,3]<-mdblack[2]-1.94*mdblack[3]
mdpreds[2,1]<-mdblack[4]
mdpreds[2,2]<-mdblack[4]+1.94*mdblack[5]
mdpreds[2,3]<-mdblack[4]-1.94*mdblack[5]
mdpreds[3,1]<-mdblack[6]
mdpreds[3,2]<-mdblack[6]+1.94*mdblack[7]
mdpreds[3,3]<-mdblack[6]-1.94*mdblack[7]
mdpreds[4,1]<-mdblack[8]
mdpreds[4,2]<-mdblack[8]+1.94*mdblack[9]
mdpreds[4,3]<-mdblack[8]-1.94*mdblack[9]
mdpreds[1,5]<-"White"
mdpreds[2,5]<-"White"
mdpreds[3,5]<-"Black"
mdpreds[4,5]<-"Black"
mdpreds[1,6]<-"Safety"
mdpreds[2,6]<-"Invest"
mdpreds[3,6]<-"Safety"
mdpreds[4,6]<-"Invest"
mdpreds[,7]<-"MD"
mdpreds[1,8]<-mdblack[3]
mdpreds[2,8]<-mdblack[5]
mdpreds[3,8]<-mdblack[7]
mdpreds[4,8]<-mdblack[9]
mdpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


ilpreds[1,1]<-ilblack[2]
ilpreds[1,2]<-ilblack[2]+1.94*ilblack[3]
ilpreds[1,3]<-ilblack[2]-1.94*ilblack[3]
ilpreds[2,1]<-ilblack[4]
ilpreds[2,2]<-ilblack[4]+1.94*ilblack[5]
ilpreds[2,3]<-ilblack[4]-1.94*ilblack[5]
ilpreds[3,1]<-ilblack[6]
ilpreds[3,2]<-ilblack[6]+1.94*ilblack[7]
ilpreds[3,3]<-ilblack[6]-1.94*ilblack[7]
ilpreds[4,1]<-ilblack[8]
ilpreds[4,2]<-ilblack[8]+1.94*ilblack[9]
ilpreds[4,3]<-ilblack[8]-1.94*ilblack[9]
ilpreds[1,5]<-"White"
ilpreds[2,5]<-"White"
ilpreds[3,5]<-"Black"
ilpreds[4,5]<-"Black"
ilpreds[1,6]<-"Safety"
ilpreds[2,6]<-"Invest"
ilpreds[3,6]<-"Safety"
ilpreds[4,6]<-"Invest"
ilpreds[,7]<-"IL"
ilpreds[1,8]<-ilblack[3]
ilpreds[2,8]<-ilblack[5]
ilpreds[3,8]<-ilblack[7]
ilpreds[4,8]<-ilblack[9]
ilpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


ncpreds[1,1]<-ncblack[2]
ncpreds[1,2]<-ncblack[2]+1.94*ncblack[3]
ncpreds[1,3]<-ncblack[2]-1.94*ncblack[3]
ncpreds[2,1]<-ncblack[4]
ncpreds[2,2]<-ncblack[4]+1.94*ncblack[5]
ncpreds[2,3]<-ncblack[4]-1.94*ncblack[5]
ncpreds[3,1]<-ncblack[6]
ncpreds[3,2]<-ncblack[6]+1.94*ncblack[7]
ncpreds[3,3]<-ncblack[6]-1.94*ncblack[7]
ncpreds[4,1]<-ncblack[8]
ncpreds[4,2]<-ncblack[8]+1.94*ncblack[9]
ncpreds[4,3]<-ncblack[8]-1.94*ncblack[9]
ncpreds[1,5]<-"White"
ncpreds[2,5]<-"White"
ncpreds[3,5]<-"Black"
ncpreds[4,5]<-"Black"
ncpreds[1,6]<-"Safety"
ncpreds[2,6]<-"Invest"
ncpreds[3,6]<-"Safety"
ncpreds[4,6]<-"Invest"
ncpreds[,7]<-"NC"
ncpreds[1,8]<-ncblack[3]
ncpreds[2,8]<-ncblack[5]
ncpreds[3,8]<-ncblack[7]
ncpreds[4,8]<-ncblack[9]
ncpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ctblack$model2predictwhite,sd=ctblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ctblack$model2predictwhiteinter,sd=ctblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ctblack$model2predictblack,sd=ctblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ctblack$model2predictblackinter,sd=ctblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ctdifindif<-data.frame(0)
ctdifindif[1,1]<-mean(as.numeric(difindifpred))
ctdifindif[1,2]<-quantile(difindifpred,probs=.975)
ctdifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=mdblack$model2predictwhite,sd=mdblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=mdblack$model2predictwhiteinter,sd=mdblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=mdblack$model2predictblack,sd=mdblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=mdblack$model2predictblackinter,sd=mdblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
mddifindif<-data.frame(0)
mddifindif[1,1]<-mean(as.numeric(difindifpred))
mddifindif[1,2]<-quantile(difindifpred,probs=.975)
mddifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ilblack$model2predictwhite,sd=ilblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ilblack$model2predictwhiteinter,sd=ilblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ilblack$model2predictblack,sd=ilblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ilblack$model2predictblackinter,sd=ilblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ildifindif<-data.frame(0)
ildifindif[1,1]<-mean(as.numeric(difindifpred))
ildifindif[1,2]<-quantile(difindifpred,probs=.975)
ildifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ncblack$model2predictwhite,sd=ncblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ncblack$model2predictwhiteinter,sd=ncblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ncblack$model2predictblack,sd=ncblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ncblack$model2predictblackinter,sd=ncblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ncdifindif<-difindifpred
ncdifindif<-data.frame(0)
ncdifindif[1,1]<-mean(as.numeric(difindifpred))
ncdifindif[1,2]<-quantile(difindifpred,probs=.975)
ncdifindif[1,3]<-quantile(difindifpred,probs=.025)
difindifs<-data.frame(0)
difindifs[1,1]<-ctdifindif[1,1]
difindifs[1,2]<-ctdifindif[1,2]
difindifs[1,3]<-ctdifindif[1,3]
difindifs[2,1]<-mddifindif[1,1]
difindifs[2,2]<-mddifindif[1,2]
difindifs[2,3]<-mddifindif[1,3]
difindifs[3,1]<-ildifindif[1,1]
difindifs[3,2]<-ildifindif[1,2]
difindifs[3,3]<-ildifindif[1,3]
difindifs[4,1]<-ncdifindif[1,1]
difindifs[4,2]<-ncdifindif[1,2]
difindifs[4,3]<-ncdifindif[1,3]
colnames(difindifs)<-c("Mean","Upper","Lower")
difindifs$State<-c("CT","MD","IL","NC")
#The graphs
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(ggplot2)

#The following code puts all states into the same graph and changes the interaction graph to a bar graph

allstates<-rbind(ctpreds,mdpreds,ilpreds,ncpreds)
library(readr)
library(ggplot2)
figuredata = allstates

figuredata$State = ifelse(figuredata$State=="NC","North Carolina",
                          ifelse(figuredata$State=="CT","Connecticut",
                                 ifelse(figuredata$State=="IL","Illinois",
                                        "Maryland")))

png("Fig_Contraband_StateRaceStop.png",928,591)
ggplot(figuredata, aes(x = Race, y = Mean, fill = Invest)) +
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_errorbar(aes(ymax = Upper, ymin = Lower),
                position = position_dodge(1), width = 0.2)+
  ylab("Predicted Probabilty of finding Contraband given a Search")+
  scale_fill_grey(start=.5,name="Stop Type",labels=c("Investigatory", "Safety"))+
  xlab("")+theme_bw(base_size = 15)+
  theme(legend.position = "bottom")+facet_grid(. ~ State)
dev.off()

#Part 8
#this part covers female contraband analysis

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(data.table)


#now to load the relevant datasets
ctblack<-read.csv("contrabandf_ct_Output_black.csv")

mdblack<-read.csv("contrabandf_md_Output_black.csv")


ilblack<-read.csv("contrabandf_il_Output_black.csv")


ncblack<-read.csv("contrabandf_nc_Output_black.csv")



ctpreds<-data.frame(0)
ctpreds$Mean<-0
ctpreds$Upper<-0
ctpreds$Lower<-0
ctpreds$Group<-"Ct"
ctpreds$Race<-"Ct"
ctpreds$Invest<-"Ct"
ctpreds$State<-"Ct"
ctpreds$SE<-0
ctpreds<-ctpreds[,-1]


mdpreds<-data.frame(0)
mdpreds$Mean<-0
mdpreds$Upper<-0
mdpreds$Lower<-0
mdpreds$Group<-"Md"

mdpreds$Race<-"Ct"
mdpreds$Invest<-"Ct"
mdpreds$State<-"Md"
mdpreds$SE<-0
mdpreds<-mdpreds[,-1]


ncpreds<-data.frame(0)
ncpreds$Mean<-0
ncpreds$Upper<-0
ncpreds$Lower<-0
ncpreds$Group<-"Nc"
ncpreds$Race<-"Nc"
ncpreds$Invest<-"Nc"
ncpreds$State<-"Nc"
ncpreds$SE<-0
ncpreds<-ncpreds[,-1]


ilpreds<-data.frame(0)
ilpreds$Mean<-0
ilpreds$Upper<-0
ilpreds$Lower<-0
ilpreds$Group<-"Il"
ilpreds$Race<-"Il"
ilpreds$Invest<-"Il"
ilpreds$State<-"Il"
ilpreds$SE<-0
ilpreds<-ilpreds[,-1]

ctpreds[1,1]<-ctblack[2]
ctpreds[1,2]<-ctblack[2]+1.94*ctblack[3]
ctpreds[1,3]<-ctblack[2]-1.94*ctblack[3]
ctpreds[2,1]<-ctblack[4]
ctpreds[2,2]<-ctblack[4]+1.94*ctblack[5]
ctpreds[2,3]<-ctblack[4]-1.94*ctblack[5]
ctpreds[3,1]<-ctblack[6]
ctpreds[3,2]<-ctblack[6]+1.94*ctblack[7]
ctpreds[3,3]<-ctblack[6]-1.94*ctblack[7]
ctpreds[4,1]<-ctblack[8]
ctpreds[4,2]<-ctblack[8]+1.94*ctblack[9]
ctpreds[4,3]<-ctblack[8]-1.94*ctblack[9]
ctpreds[1,5]<-"White"
ctpreds[2,5]<-"White"
ctpreds[3,5]<-"Black"
ctpreds[4,5]<-"Black"
ctpreds[1,6]<-"Safety"
ctpreds[2,6]<-"Invest"
ctpreds[3,6]<-"Safety"
ctpreds[4,6]<-"Invest"
ctpreds[,7]<-"CT"
ctpreds[1,8]<-ctblack[3]
ctpreds[2,8]<-ctblack[5]
ctpreds[3,8]<-ctblack[7]
ctpreds[4,8]<-ctblack[9]
ctpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


mdpreds[1,1]<-mdblack[2]
mdpreds[1,2]<-mdblack[2]+1.94*mdblack[3]
mdpreds[1,3]<-mdblack[2]-1.94*mdblack[3]
mdpreds[2,1]<-mdblack[4]
mdpreds[2,2]<-mdblack[4]+1.94*mdblack[5]
mdpreds[2,3]<-mdblack[4]-1.94*mdblack[5]
mdpreds[3,1]<-mdblack[6]
mdpreds[3,2]<-mdblack[6]+1.94*mdblack[7]
mdpreds[3,3]<-mdblack[6]-1.94*mdblack[7]
mdpreds[4,1]<-mdblack[8]
mdpreds[4,2]<-mdblack[8]+1.94*mdblack[9]
mdpreds[4,3]<-mdblack[8]-1.94*mdblack[9]
mdpreds[1,5]<-"White"
mdpreds[2,5]<-"White"
mdpreds[3,5]<-"Black"
mdpreds[4,5]<-"Black"
mdpreds[1,6]<-"Safety"
mdpreds[2,6]<-"Invest"
mdpreds[3,6]<-"Safety"
mdpreds[4,6]<-"Invest"
mdpreds[,7]<-"MD"
mdpreds[1,8]<-mdblack[3]
mdpreds[2,8]<-mdblack[5]
mdpreds[3,8]<-mdblack[7]
mdpreds[4,8]<-mdblack[9]
mdpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


ilpreds[1,1]<-ilblack[2]
ilpreds[1,2]<-ilblack[2]+1.94*ilblack[3]
ilpreds[1,3]<-ilblack[2]-1.94*ilblack[3]
ilpreds[2,1]<-ilblack[4]
ilpreds[2,2]<-ilblack[4]+1.94*ilblack[5]
ilpreds[2,3]<-ilblack[4]-1.94*ilblack[5]
ilpreds[3,1]<-ilblack[6]
ilpreds[3,2]<-ilblack[6]+1.94*ilblack[7]
ilpreds[3,3]<-ilblack[6]-1.94*ilblack[7]
ilpreds[4,1]<-ilblack[8]
ilpreds[4,2]<-ilblack[8]+1.94*ilblack[9]
ilpreds[4,3]<-ilblack[8]-1.94*ilblack[9]
ilpreds[1,5]<-"White"
ilpreds[2,5]<-"White"
ilpreds[3,5]<-"Black"
ilpreds[4,5]<-"Black"
ilpreds[1,6]<-"Safety"
ilpreds[2,6]<-"Invest"
ilpreds[3,6]<-"Safety"
ilpreds[4,6]<-"Invest"
ilpreds[,7]<-"IL"
ilpreds[1,8]<-ilblack[3]
ilpreds[2,8]<-ilblack[5]
ilpreds[3,8]<-ilblack[7]
ilpreds[4,8]<-ilblack[9]
ilpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")


ncpreds[1,1]<-ncblack[2]
ncpreds[1,2]<-ncblack[2]+1.94*ncblack[3]
ncpreds[1,3]<-ncblack[2]-1.94*ncblack[3]
ncpreds[2,1]<-ncblack[4]
ncpreds[2,2]<-ncblack[4]+1.94*ncblack[5]
ncpreds[2,3]<-ncblack[4]-1.94*ncblack[5]
ncpreds[3,1]<-ncblack[6]
ncpreds[3,2]<-ncblack[6]+1.94*ncblack[7]
ncpreds[3,3]<-ncblack[6]-1.94*ncblack[7]
ncpreds[4,1]<-ncblack[8]
ncpreds[4,2]<-ncblack[8]+1.94*ncblack[9]
ncpreds[4,3]<-ncblack[8]-1.94*ncblack[9]
ncpreds[1,5]<-"White"
ncpreds[2,5]<-"White"
ncpreds[3,5]<-"Black"
ncpreds[4,5]<-"Black"
ncpreds[1,6]<-"Safety"
ncpreds[2,6]<-"Invest"
ncpreds[3,6]<-"Safety"
ncpreds[4,6]<-"Invest"
ncpreds[,7]<-"NC"
ncpreds[1,8]<-ncblack[3]
ncpreds[2,8]<-ncblack[5]
ncpreds[3,8]<-ncblack[7]
ncpreds[4,8]<-ncblack[9]
ncpreds$Group<-c("White Safety","White Invest.","Black Safety","Black Invest.")

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ctblack$model2predictwhite,sd=ctblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ctblack$model2predictwhiteinter,sd=ctblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ctblack$model2predictblack,sd=ctblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ctblack$model2predictblackinter,sd=ctblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ctdifindif<-data.frame(0)
ctdifindif[1,1]<-mean(as.numeric(difindifpred))
ctdifindif[1,2]<-quantile(difindifpred,probs=.975)
ctdifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=mdblack$model2predictwhite,sd=mdblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=mdblack$model2predictwhiteinter,sd=mdblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=mdblack$model2predictblack,sd=mdblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=mdblack$model2predictblackinter,sd=mdblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
mddifindif<-data.frame(0)
mddifindif[1,1]<-mean(as.numeric(difindifpred))
mddifindif[1,2]<-quantile(difindifpred,probs=.975)
mddifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ilblack$model2predictwhite,sd=ilblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ilblack$model2predictwhiteinter,sd=ilblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ilblack$model2predictblack,sd=ilblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ilblack$model2predictblackinter,sd=ilblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ildifindif<-data.frame(0)
ildifindif[1,1]<-mean(as.numeric(difindifpred))
ildifindif[1,2]<-quantile(difindifpred,probs=.975)
ildifindif[1,3]<-quantile(difindifpred,probs=.025)

#The difference in difference simulation
white<-data.frame()
whiteinter<-data.frame()
black<-data.frame()
blackinter<-data.frame()

temp<-rnorm(n=10000,mean=ncblack$model2predictwhite,sd=ncblack$model2sewhite)
white<-as.data.frame(rbind(white,temp))






temp<-rnorm(n=10000,mean=ncblack$model2predictwhiteinter,sd=ncblack$model2sewhiteinter)
whiteinter<-as.data.frame(rbind(whiteinter,temp))



temp<-rnorm(n=10000,mean=ncblack$model2predictblack,sd=ncblack$model2seblack)
black<-as.data.frame(rbind(black,temp))

temp<-rnorm(n=10000,mean=ncblack$model2predictblackinter,sd=ncblack$model2seblackinter)
blackinter<-as.data.frame(rbind(blackinter,temp))


difindifpred<-vector()

difindifpred<-(blackinter-black)-(whiteinter-white)
ncdifindif<-difindifpred
ncdifindif<-data.frame(0)
ncdifindif[1,1]<-mean(as.numeric(difindifpred))
ncdifindif[1,2]<-quantile(difindifpred,probs=.975)
ncdifindif[1,3]<-quantile(difindifpred,probs=.025)
difindifs<-data.frame(0)
difindifs[1,1]<-ctdifindif[1,1]
difindifs[1,2]<-ctdifindif[1,2]
difindifs[1,3]<-ctdifindif[1,3]
difindifs[2,1]<-mddifindif[1,1]
difindifs[2,2]<-mddifindif[1,2]
difindifs[2,3]<-mddifindif[1,3]
difindifs[3,1]<-ildifindif[1,1]
difindifs[3,2]<-ildifindif[1,2]
difindifs[3,3]<-ildifindif[1,3]
difindifs[4,1]<-ncdifindif[1,1]
difindifs[4,2]<-ncdifindif[1,2]
difindifs[4,3]<-ncdifindif[1,3]
colnames(difindifs)<-c("Mean","Upper","Lower")
difindifs$State<-c("CT","MD","IL","NC")
#The graphs
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(ggplot2)


#The following code puts all states into the same graph and changes the interaction graph to a bar graph

allstates<-rbind(ctpreds,mdpreds,ilpreds,ncpreds)
library(readr)
library(ggplot2)
figuredata = allstates

figuredata$State = ifelse(figuredata$State=="NC","North Carolina",
                          ifelse(figuredata$State=="CT","Connecticut",
                                 ifelse(figuredata$State=="IL","Illinois",
                                        "Maryland")))

png("Fig_Contraband_StateRaceStop.png",928,591)
ggplot(figuredata, aes(x = Race, y = Mean, fill = Invest)) +
  geom_bar(stat = "identity", position = position_dodge(1))+
  geom_errorbar(aes(ymax = Upper, ymin = Lower),
                position = position_dodge(1), width = 0.2)+
  ylab("Predicted Probabilty of finding Contraband given a Search")+
  scale_fill_grey(start=.5,name="Stop Type",labels=c("Investigatory", "Safety"))+
  xlab("")+theme_bw(base_size = 15)+
  theme(legend.position = "bottom")+facet_grid(. ~ State)
dev.off()



# Part 9
#This part covers the intersectional search model

##
## CT
##

# clearing the workspace

rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)
library(sandwich)

# the data
#The first thing I will do after load the data is subsetting on the correct groups
#This is dependent on the part, but generally I subset on race, gender, and on certain dataset specific variables
ct_data = read.dta13("CT_TrafficStops_6June2020.dta")

ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))

ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)

ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

ct_data$WM<-ifelse(ct_data$race==1&ct_data$gender==1,1,0)
ct_data$WF<-ifelse(ct_data$race==1&ct_data$gender==0,1,0)
ct_data$BM<-ifelse(ct_data$race==2&ct_data$gender==1,1,0)
ct_data$BF<-ifelse(ct_data$race==2&ct_data$gender==0,1,0)

glm= glm(searchoccur~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek)+factor(agencyid),
         data=ct_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
newdata$stoph=0
newdata$stoppurpose2= seq(0,1,by=1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)
newdata$stopo<-0
newdata$age=as.numeric(summary(ct_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(ct_data$hour)
newdata$dayofweek=getmode(ct_data$dayofweek)
newdata$agencyid=getmode(ct_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

ct_output<-data.frame(0)
ct_output$model2predictwhite = predictedvalues[1]
ct_output$model2sewhite = predictedse[1]
ct_output$model2predictwhiteinter = predictedvalues[2]
ct_output$model2sewhiteinter = predictedse[2]
ct_output$model2predictblack = predictedvalues[3]
ct_output$model2seblack = predictedse[3]
ct_output$model2predictblackinter = predictedvalues[4]
ct_output$model2seblackinter = predictedse[4]
ct_output<-ct_output[,-1]

write.csv(ct_output,"ct_Output_black_intersectional.csv")

nobs(glm)
summary(glm)
#Line 1 Table C1
write.csv(r.est, file="regressionctintersectional.csv") 


##
## Maryland- Black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)


md_data$agencyid<-match(md_data$agency, unique(md_data$agency))

md_data$WM<-ifelse(md_data$race==1&md_data$gender==1,1,0)
md_data$WF<-ifelse(md_data$race==1&md_data$gender==0,1,0)
md_data$BM<-ifelse(md_data$race==2&md_data$gender==1,1,0)
md_data$BF<-ifelse(md_data$race==2&md_data$gender==0,1,0)

glm = glm(searchoccur~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday)+factor(agencyid),
          data=md_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(md_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(md_data$hour)
newdata$weekday=getmode(md_data$weekday)
newdata$agencyid=getmode(md_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

md_output<-data.frame(0)
md_output$model2predictwhite = predictedvalues[1]
md_output$model2sewhite = predictedse[1]
md_output$model2predictwhiteinter = predictedvalues[2]
md_output$model2sewhiteinter = predictedse[2]
md_output$model2predictblack = predictedvalues[3]
md_output$model2seblack = predictedse[3]
md_output$model2predictblackinter = predictedvalues[4]
md_output$model2seblackinter = predictedse[4]
md_output<-md_output[,-1]

write.csv(md_output,"md_Output_black_intersectional.csv")

nobs(glm)
summary(glm)
r.est
#Line 2 Table C1
write.csv(as.data.frame(r.est), file="regressionmdintersectional.csv") 




#IL

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#If you want the full data use this line instead
il_data = read.dta13("IL_TrafficStops.dta")




il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
#il_data<-subset(il_data, (il_data$gender==0))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))
il_data<-subset(il_data,il_data$stops>10000)

il_data$WM<-ifelse(il_data$race==1&il_data$gender==1,1,0)
il_data$WF<-ifelse(il_data$race==1&il_data$gender==0,1,0)
il_data$BM<-ifelse(il_data$race==2&il_data$gender==1,1,0)
il_data$BF<-ifelse(il_data$race==2&il_data$gender==0,1,0)
#il_data = subset(il_data, (il_data$searchoccur==1))
il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))

glm<-glm(searchoccur~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek)+factor(agencyid),
         data=il_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

summary(glm)$coefficients
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(il_data$age)[4])
newdata$vehicleage=as.numeric(summary(il_data$vehicleage)[4])
newdata$hour=(getmode(il_data$hour))
newdata$dayofweek=(getmode(il_data$dayofweek))
newdata$agencyid<-(getmode(il_data$agencyid))


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

il_output<-data.frame(0)
#Now to save these predicted probs
il_output$model2predictwhite = predictedvalues[1]
il_output$model2sewhite = predictedse[1]
il_output$model2predictwhiteinter = predictedvalues[2]
il_output$model2sewhiteinter = predictedse[2]
il_output$model2predictblack = predictedvalues[3]
il_output$model2seblack = predictedse[3]
il_output$model2predictblackinter = predictedvalues[4]
il_output$model2seblackinter = predictedse[4]
il_output<-il_output[,-1]

write.csv(il_output,"il_Output_black_intersectional.csv")

nobs(glm)
summary(glm)
#Line 3 Table C1
write.csv(as.data.frame(r.est), file="regressionilintersectional.csv") 


##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data<-read.dta13("NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>1000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

nc_data$WM<-ifelse(nc_data$race==1&nc_data$gender==1,1,0)
nc_data$WF<-ifelse(nc_data$race==1&nc_data$gender==0,1,0)
nc_data$BM<-ifelse(nc_data$race==2&nc_data$gender==1,1,0)
nc_data$BF<-ifelse(nc_data$race==2&nc_data$gender==0,1,0)

glm = glm(searchoccur~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hourfixed=getmode(nc_data$hourfixed)
newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"nc_Output_black_intersectional.csv")
nobs(glm)
summary(glm)
#Line 4 Table C1
write.csv(as.data.frame(r.est), file="regressionncintersectional.csv") 


##
##NC again without hour fixed

##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0

newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"nc_Output_black_intersectional_nohour.csv")
nobs(glm)
summary(glm)


write.csv(as.data.frame(r.est), file="regressionncnohourintersectional.csv") 







# Part 10
#This part covers the intersecitonal contraband analysis

##
## CT
##

# clearing the workspace

rm(list=ls(all=TRUE)) 
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)
library(sandwich)

# the data
ct_data = read.dta13("CT_TrafficStops_6June2020.dta")

ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))

ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data = subset(ct_data, (ct_data$searchoccur==1))
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

ct_data$WM<-ifelse(ct_data$race==1&ct_data$gender==1,1,0)
ct_data$WF<-ifelse(ct_data$race==1&ct_data$gender==0,1,0)
ct_data$BM<-ifelse(ct_data$race==2&ct_data$gender==1,1,0)
ct_data$BF<-ifelse(ct_data$race==2&ct_data$gender==0,1,0)

glm= glm(contraband~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek)+factor(agencyid),
         data=ct_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
newdata$stoph=0
newdata$stoppurpose2= seq(0,1,by=1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)
newdata$stopo<-0
newdata$age=as.numeric(summary(ct_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(ct_data$hour)
newdata$dayofweek=getmode(ct_data$dayofweek)
newdata$agencyid=getmode(ct_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

ct_output<-data.frame(0)
ct_output$model2predictwhite = predictedvalues[1]
ct_output$model2sewhite = predictedse[1]
ct_output$model2predictwhiteinter = predictedvalues[2]
ct_output$model2sewhiteinter = predictedse[2]
ct_output$model2predictblack = predictedvalues[3]
ct_output$model2seblack = predictedse[3]
ct_output$model2predictblackinter = predictedvalues[4]
ct_output$model2seblackinter = predictedse[4]
ct_output<-ct_output[,-1]

write.csv(ct_output,"ct_Output_black_intersectional_contraband.csv")

nobs(glm)
summary(glm)
#Line 1 Table C2
write.csv(r.est, file="regressionctintersectional_contraband.csv") 


##
## Maryland- Black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)
md_data = subset(md_data, (md_data$searchoccur==1))

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))

md_data$WM<-ifelse(md_data$race==1&md_data$gender==1,1,0)
md_data$WF<-ifelse(md_data$race==1&md_data$gender==0,1,0)
md_data$BM<-ifelse(md_data$race==2&md_data$gender==1,1,0)
md_data$BF<-ifelse(md_data$race==2&md_data$gender==0,1,0)

glm = glm(contraband~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday)+factor(agencyid),
          data=md_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(md_data$age)[4])
newdata$outofstate=0
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hour=getmode(md_data$hour)
newdata$weekday=getmode(md_data$weekday)
newdata$agencyid=getmode(md_data$agencyid)

#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

md_output<-data.frame(0)
md_output$model2predictwhite = predictedvalues[1]
md_output$model2sewhite = predictedse[1]
md_output$model2predictwhiteinter = predictedvalues[2]
md_output$model2sewhiteinter = predictedse[2]
md_output$model2predictblack = predictedvalues[3]
md_output$model2seblack = predictedse[3]
md_output$model2predictblackinter = predictedvalues[4]
md_output$model2seblackinter = predictedse[4]
md_output<-md_output[,-1]

write.csv(md_output,"md_Output_black_intersectional_contraband.csv")

nobs(glm)
summary(glm)
r.est
#Line 2 Table C2
write.csv(as.data.frame(r.est), file="regressionmdintersectional_contraband.csv") 




#IL

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#If you want the full data use this line instead
il_data = read.dta13("IL_TrafficStops.dta")




il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
#il_data<-subset(il_data, (il_data$gender==0))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))
il_data<-subset(il_data,il_data$stops>10000)

il_data$WM<-ifelse(il_data$race==1&il_data$gender==1,1,0)
il_data$WF<-ifelse(il_data$race==1&il_data$gender==0,1,0)
il_data$BM<-ifelse(il_data$race==2&il_data$gender==1,1,0)
il_data$BF<-ifelse(il_data$race==2&il_data$gender==0,1,0)
il_data = subset(il_data, (il_data$searchoccur==1))
il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))

glm<-glm(contraband~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek)+factor(agencyid),
         data=il_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

summary(glm)$coefficients
#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$age=as.numeric(summary(il_data$age)[4])
newdata$vehicleage=as.numeric(summary(il_data$vehicleage)[4])
newdata$hour=(getmode(il_data$hour))
newdata$dayofweek=(getmode(il_data$dayofweek))
newdata$agencyid<-(getmode(il_data$agencyid))


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

il_output<-data.frame(0)
#Now to save these predicted probs
il_output$model2predictwhite = predictedvalues[1]
il_output$model2sewhite = predictedse[1]
il_output$model2predictwhiteinter = predictedvalues[2]
il_output$model2sewhiteinter = predictedse[2]
il_output$model2predictblack = predictedvalues[3]
il_output$model2seblack = predictedse[3]
il_output$model2predictblackinter = predictedvalues[4]
il_output$model2seblackinter = predictedse[4]
il_output<-il_output[,-1]

write.csv(il_output,"il_Output_black_intersectional_contraband.csv")

nobs(glm)
summary(glm)
#Line 3 Table C2
write.csv(as.data.frame(r.est), file="regressionilintersectional_contraband.csv") 


##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data<-read.dta13("NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops
nc_data = subset(nc_data, (nc_data$searchoccur==1))
matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>1000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

nc_data$WM<-ifelse(nc_data$race==1&nc_data$gender==1,1,0)
nc_data$WF<-ifelse(nc_data$race==1&nc_data$gender==0,1,0)
nc_data$BM<-ifelse(nc_data$race==2&nc_data$gender==1,1,0)
nc_data$BF<-ifelse(nc_data$race==2&nc_data$gender==0,1,0)

glm = glm(contraoccur~WF+BM+BF+stoppurpose2+WF*stoppurpose2+BM*stoppurpose2+BF*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est
nobs(glm)
summary(glm)

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0
newdata$hourfixed=getmode(nc_data$hourfixed)
newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)



#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"nc_Output_black_intersectional_contraband.csv")
nobs(glm)
summary(glm)
#Line 4 Table C2
write.csv(as.data.frame(r.est), file="regressionncintersectional_contraband.csv") 


##
##NC again without hour fixed

##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data = subset(nc_data, (nc_data$searchoccur==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))
nc_data<-subset(nc_data,nc_data$stops>10000)



nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

glm = glm(contraband~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+factor(dow)+factor(agencyid),
          data=nc_data,family="binomial")

cov.m1 <- vcovHC(glm, type = "HC0",cluster=ct_data$agencyid)

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(glm)
  , "Robust SE" = std.err
  , z = (coef(glm)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(glm)/std.err), lower.tail = FALSE)
  , LL = coef(glm) - q.val  * std.err
  , UL = coef(glm) + q.val  * std.err
)

r.est

#This code will use the predict command to create 4 predicted probabilities.
newdata<-NULL
stopb<-c(0,0,1,1)
newdata = as.data.frame(stopb)
stoph<-c(0,0,0,0)
newdata$stoppurpose2= c(0,1,0,1)
newdata$stopbstoppurpose2 = (newdata$stopb*newdata$stoppurpose2)

newdata$stopo<-0
newdata$age=as.numeric(summary(nc_data$age)[4])
newdata$blackdisparity=0
newdata$hispdisparity=0

newdata$dow=getmode(nc_data$dow)
newdata$agencyid=getmode(nc_data$agencyid)


#The following function will include the robust std error in the se.fit of the predict command
predict.rob <- function(x,clcov,newdata){
  if(missing(newdata)){ newdata <- x$model }
  tt <- terms(x)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=newdata)
  m.coef <- x$coef
  fit <- as.vector(m.mat %*% x$coef)
  se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))}

#now getting predicated values
predicted_set<-predict(glm,cov.m1, newdata)
predictedvalues<-predicted_set$fit
predictedse<-predicted_set$se.fit

nc_output<-data.frame(0)
nc_output$model2predictwhite = predictedvalues[1]
nc_output$model2sewhite = predictedse[1]
nc_output$model2predictwhiteinter = predictedvalues[2]
nc_output$model2sewhiteinter = predictedse[2]
nc_output$model2predictblack = predictedvalues[3]
nc_output$model2seblack = predictedse[3]
nc_output$model2predictblackinter = predictedvalues[4]
nc_output$model2seblackinter = predictedse[4]
nc_output<-nc_output[,-1]

write.csv(nc_output,"nc_Output_black_intersectional_nohour_contraband.csv")
nobs(glm)
summary(glm)


write.csv(as.data.frame(r.est), file="regressionncnohourintersectional_contraband.csv") 





#Part 11
#This script will run a FE model for each state, dropping hispanics, without agency fe


##
## CT
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(xlsx)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)
library(sandwich)

# the data
ct_data = read.dta13("CT_TrafficStops_14Jan2018.dta")
#857923
ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))
ct_data = subset(ct_data, ((ct_data$gender==1)))
ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))

#Line 1 Table D1
glm= glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek),
         data=ct_data,family="binomial")







##
## Maryland- Black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data = subset(md_data, (md_data$gender==1))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))


#Line 2 Table D1


glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday),
          data=md_data,family="binomial")




#IL

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#If you want the full data use this line instead
il_data = read.dta13("IL_TrafficStops.dta")



il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
il_data<-subset(il_data, (il_data$gender==1))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL


il_data$agencyid<-match(il_data$agencyname, unique(il_data$agencyname))
#Line 3 Table D1
glm<-glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek),
         data=il_data,family="binomial")


nobs(glm)
summary(glm)



##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
# reading the necessary packages
library(readstata13)
library(sandwich)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data = subset(nc_data, (nc_data$gender==1))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))




nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))

#Line 4 Table D1
glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow),
          data=nc_data,family="binomial")



#Part 12
#This script will run a FE model for each state, dropping hispanics, for women, without agnecy fe


##
## CT
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(xlsx)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# reading the necessary packages
library(readstata13)

# the data
ct_data = read.dta13("CT_TrafficStops_14Jan2018.dta")
#857923
ct_data = subset(ct_data, ((ct_data$race!=4)))
ct_data = subset(ct_data, ((ct_data$race!=3)))
ct_data = subset(ct_data, ((ct_data$gender==0)))
ct_data$stopb= ifelse(ct_data$race==2,1,0)
ct_data$stoph= ifelse(ct_data$race==3,1,0)
ct_data$stopt = 1
ct_data$agency<-ct_data$departmentname

ct_data$agencyid<-match(ct_data$agency, unique(ct_data$agency))
#Line 6 Table D1


glm= glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
           outofstate+blackdisparity+factor(hour)+factor(dayofweek),
         data=ct_data,family="binomial")



##
## Maryland- Black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

# reading the necessary packages
library(readstata13)
library(xlsx)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# the data
md_data = read.dta13("MD_TrafficStops_10Jan2018.dta")
md_data = subset(md_data, (md_data$year!=2012))
md_data = subset(md_data, (md_data$race!=4))
md_data = subset(md_data, (md_data$race!=3))
md_data = subset(md_data, (md_data$gender==0))
md_data$stopb = ifelse(md_data$race==2,1,0)
md_data$stoph = ifelse(md_data$race==3,1,0)

md_data$agencyid<-match(md_data$agency, unique(md_data$agency))
#Line 7 Table D1

glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+
            outofstate+
            blackdisparity+
            factor(hour)+factor(weekday),
          data=md_data,family="binomial")


#IL

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")
library(readstata13)
library(xlsx)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

il_data = read.dta13("IL_TrafficStops.dta")



il_data<-subset(il_data, (il_data$race!=4))
il_data<-subset(il_data, (il_data$race!=3))
il_data<-subset(il_data, (il_data$gender==0))
il_data$stopt<-1
il_data$stops<-1
matches <- aggregate(stops ~ agencyname, il_data, FUN="length")
il_data$stops<-NULL
il_data <- merge(il_data, matches, by=c("agencyname"))


#Line 7 Table D1

glm<-glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+
           vehicleage+age+
           factor(hour)+factor(dayofweek),
         data=il_data,family="binomial")


##
## NC-black
##

# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\APSA2017revisions\\Attheintersectionsreplication")

library(readstata13)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# the data
nc_data = read.dta13( "NC_UnifiedCoding_apsa.dta")

nc_data = subset(nc_data, (nc_data$stoppurpose1!=10))
nc_data = subset(nc_data, (nc_data$type!=0))
nc_data = subset(nc_data, (nc_data$race!=4))
nc_data = subset(nc_data, (nc_data$race!=3))
nc_data = subset(nc_data, (nc_data$hourfixed>-10))
nc_data = subset(nc_data, (nc_data$gender==0))
nc_data$stopb = ifelse(nc_data$race==2,1,0)
nc_data$stoph = ifelse(nc_data$race==3,1,0)
nc_data$stops<-1
#Now to drop agencies with less than 10000 stops

matches <- aggregate(stops ~ agencydescription, nc_data, FUN="length")
nc_data$stops<-NULL
nc_data <- merge(nc_data, matches, by=c("agencydescription"))


nc_data$agencyid<-match(nc_data$agencydescription, unique(nc_data$agencydescription))
#Line 8 Table D1


glm = glm(searchoccur~stopb+stoppurpose2+stopb*stoppurpose2+age+blackdisparity+
            factor(hourfixed)+factor(dow),
          data=nc_data,family="binomial")





