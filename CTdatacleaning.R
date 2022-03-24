### This file is an R translation of the data cleaning stata file for the state of CT traffic stops
### Date 3/17/2022
### Author: Kevin Roach

##
##  This file will load the raw CT traffic stops data and process the data into our unified coding scheme
##

#First to clear the workspace, setting the wd, and loading relevant packages
# clearing the workspace
rm(list=ls(all=TRUE))
setwd("C:\\Users\\kroac\\Dropbox\\AggregatedTrafficStopsData\\RawMicroFiles\\Connecticut\\")
library(lubridate)


#load the data
ctdata<-read.csv("connecticut-r1.csv")

#generating a race variable.  1 is White non Hispanics.  2 is Black non Hispanic.  3 is for Hispanics of any race
#All others will be treated as NA
#first look at the raw data
table(ctdata$SubjectRaceCode)
table(ctdata$SubjectEthnicityCode)
ctdata$race<-NA
ctdata$race<-ifelse(ctdata$SubjectRaceCode=="W"&ctdata$SubjectEthnicityCode=="N",1,ctdata$race)
ctdata$race<-ifelse(ctdata$SubjectRaceCode=="B"&ctdata$SubjectEthnicityCode=="N",2,ctdata$race)
ctdata$race<-ifelse(ctdata$SubjectEthnicityCode=="H",3,ctdata$race)

#check the cleaned variable
table(ctdata$race)
#double check to ensure that everything got coded here
summary(is.na(ctdata$race))

#generating a gender variable.  1 for men, 2 for women.  
table(ctdata$SubjectSexCode)
ctdata$gender<-NA
ctdata$gender<-ifelse(ctdata$SubjectSexCode=="M",1,ctdata$gender)
ctdata$gender<-ifelse(ctdata$SubjectSexCode=="F",0,ctdata$gender)
table(ctdata$gender)
summary(is.na(ctdata$gender))
#generating an age variable
table(ctdata$SubjectAge)
summary(ctdata$SubjectAge)
ctdata$age<-ctdata$SubjectAge
ctdata$age<-ifelse(ctdata$age<15|ctdata$age>100,NA,ctdata$age)
table(ctdata$age)
summary(ctdata$age)
summary(is.na(ctdata$age))
# An alternate method to processing data beyond a series of ifelse statements
#ctdata$age<-ctdata$SubjectAge
#ctdata$age[ctdata$SubjectAge<15|ctdata$SubjectAge>100]<-NA

#generating the out of state variable.  This uses the resident indicator to determine in state status
table(ctdata$ResidentIndicator)
ctdata$outofstate<-NA
ctdata$outofstate<-ifelse(ctdata$ResidentIndicator=="False",1,ctdata$outofstate)
ctdata$outofstate<-ifelse(ctdata$ResidentIndicator=="True",0,ctdata$outofstate)
table(ctdata$outofstate)
summary(is.na(ctdata$outofstate))

#Now to create a set of time variable.  I need the year, date, day of week, and the time
ctdata$time<-hm(ctdata$InterventionTime)
ctdata$hour<-hour(ctdata$time)
ctdata$minute<-minute(ctdata$time)

ctdata$date<-ymd(ctdata$InterventionDate)
ctdata$year<-year(ctdata$date)
ctdata$dayofweek<-wday(ctdata$date,label = F)

table(ctdata$year)
table(ctdata$hour)
summary(ctdata$minute)
head(ctdata$minute)
head(ctdata$dayofweek)

#generating the stop purpose variable
#this has 2 forms, the first just generates numeric codes for those used in the raw data
#the second bins these codes into safety or investigatory stops

table(ctdata$StatutoryReasonForStop)
ctdata$stoppurpose1<-NA
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Cell Phone",1,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Defective Lights",2,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Display of Plates",3,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Equipment Violation",4,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Moving Violation",5,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Registration",6,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Seatbelt",7,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Speed Related",8,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Stop Sign",9,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Stop Sign ",9,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Suspended License",10,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Traffic Control Signal",11,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Window Tint",12,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Other",13,ctdata$stoppurpose1)
ctdata$stoppurpose1<-ifelse(ctdata$StatutoryReasonForStop=="Other/Error",14,ctdata$stoppurpose1)
table(ctdata$stoppurpose1)

ctdata$stoppurpose2<-NA
ctdata$stoppurpose2<-ifelse(ctdata$stoppurpose1==2|ctdata$stoppurpose1==3|ctdata$stoppurpose1==4|ctdata$stoppurpose1==6|ctdata$stoppurpose1==7
                            |ctdata$stoppurpose1==10|ctdata$stoppurpose1==12|ctdata$stoppurpose1==13|ctdata$stoppurpose1==14,
                            1,ctdata$stoppurpose2)
ctdata$stoppurpose2<-ifelse(ctdata$stoppurpose1==1|ctdata$stoppurpose1==5|ctdata$stoppurpose1==8|ctdata$stoppurpose1==9|
                            ctdata$stoppurpose1==11,
                            0,ctdata$stoppurpose2)

table(ctdata$stoppurpose2)
summary(is.na(ctdata$stoppurpose2))

#generating a search variable.  CT collects information on the authorization for the search and in that records
#no searches with an N.  
table(ctdata$SearchAuthorizationCode)
#In this case its slightly easier to code this beginning with one rather than na
ctdata$searchoccur<-1
ctdata$searchoccur<-ifelse(ctdata$SearchAuthorizationCode=="N",0,ctdata$searchoccur)
ctdata$searchoccur<-ifelse(ctdata$SearchAuthorizationCode=="",NA,ctdata$searchoccur)
table(ctdata$searchoccur)
summary(is.na(ctdata$searchoccur))

#generating a search type variable  
#This just breaks out the search occur variable into potentially useful chunks.
ctdata$searchtype<-NA
ctdata$searchtype<-ifelse(ctdata$SearchAuthorizationCode=="C",1,ctdata$searchtype)
ctdata$searchtype<-ifelse(ctdata$SearchAuthorizationCode=="I",2,ctdata$searchtype)
ctdata$searchtype<-ifelse(ctdata$SearchAuthorizationCode=="O",3,ctdata$searchtype)
table(ctdata$searchtype)
summary(is.na(ctdata$searctype))

#generating a contraband variable, this will be necessary for the second part of the analysis.  
table(ctdata$ContrabandIndicator)
ctdata$contraband=NA
ctdata$contraband<-ifelse(ctdata$ContrabandIndicator=="True",1,ctdata$contraband)
ctdata$contraband<-ifelse(ctdata$ContrabandIndicator=="False",0,ctdata$contraband)
table(ctdata$contrband)
summary(is.na(ctdata$contraband))

#generating an outcome variable
#similarly to the stoppurpose this will have 2 codes.  The first is making the raw data numeric, the second bins these
#However this variable draws from 2 of the raw variable.  We need to use the dispition code, but to also code if there was a custodial arrest
table(ctdata$InterventionDispositionCode)
ctdata$outcome1<-NA
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="N",0,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="V",1,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="W",2,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="I",3,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="I ",3,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="M",4,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$InterventionDispositionCode=="U",5,ctdata$outcome1)
ctdata$outcome1<-ifelse(ctdata$CustodialArrestIndicator=="True",6,ctdata$outcome1)
table(ctdata$outcome1)
summary(is.na(ctdata$outcome1))


ctdata$outcome2<-NA
ctdata$outcome2<-ifelse(ctdata$outcome1<3,1,ctdata$outcome2)
ctdata$outcome2<-ifelse(ctdata$outcome1==3|ctdata$outcome1==4,2,ctdata$outcome2)
ctdata$outcome2<-ifelse(ctdata$outcome1>4,3,ctdata$outcome2)
table(ctdata$outcome2)
summary(is.na(ctdata$outcome2))

#Now to generate a variable coding a officers as high disparity or not, measuring "bad apples"
#officers will be coded as high disparity if they meet the follow conditions
#1. They search at a higher rate than average
#2. They have more than 50 stops for Black and White drivers
#3.They search Blacks at least twice as much as Whites
#Officers will also be coded for being high disparity for Hispanic drivers, replacing the Black conditions above with Hispanic drivers

#To begin, generate a set of dummy variables for stops and searches that can be summed over officers.
ctdata$stopt<-1
ctdata$stopw<-ifelse(ctdata$race==1,1,0)
ctdata$stopb<-ifelse(ctdata$race==2,1,0)
ctdata$stoph<-ifelse(ctdata$race==3,1,0)
ctdata$searchw<-ifelse(ctdata$race==1&ctdata$searchoccur==1,1,0)
ctdata$searchb<-ifelse(ctdata$race==2&ctdata$searchoccur==1,1,0)
ctdata$searchh<-ifelse(ctdata$race==3&ctdata$searchoccur==1,1,0)

#before aggregating it will be useful to check if officer ids are repeated across agencies
ctidcheck<-aggregate(stopt~ReportingOfficerIdentificationID+Department.Name,ctdata,sum)
duplicated(ctidcheck$ReportingOfficerIdentificationID)
#there are repeats, so we need to agg over both officer id and agency
ctofficer<-aggregate(cbind(stopt, stopw, stopb, stoph, searchoccur, searchw, searchb, searchh)~ReportingOfficerIdentificationID+Department.Name, ctdata, sum)
#generating some sample stats to use for making the disparity codes
ctofficer$searchrate<-ctofficer$searchoccur/ctofficer$stopt
ctofficer$searchratew<-ctofficer$searchw/ctofficer$stopw
ctofficer$searchrateb<-ctofficer$searchb/ctofficer$stopb
ctofficer$searchrateh<-ctofficer$searchh/ctofficer$stoph
ctofficer$SRRb<-ctofficer$searchrateb/ctofficer$searchratew
ctofficer$SRRh<-ctofficer$searchrateh/ctofficer$searchratew

#I also need the mean search rate across the state for Blacks and White and Hispanics and Whites
meansearchrate<-mean(ctdata$searchoccur,na.rm=T)

#and now to create the Black and Hisp disparity codes.  Importantly, here we assume officers are not bad apples
ctofficer$blackdisparity<-0
ctofficer$blackdisparity<-ifelse(ctofficer$searchrate>meansearchrate&ctofficer$stopw>=50&ctofficer$stopb>=50&
                                   ctofficer$SRRb>2,1,ctofficer$blackdisparity)
#we decided to add a second condition, if there are 0 White searches we drop condition 3 and replace it with
#3. If the officer has searched at least 10 blacks 
ctofficer$blackdisparity<-ifelse(ctofficer$searchrate>meansearchrate&ctofficer$stopw>=50&ctofficer$stopb>=50&
                                   ctofficer$searchb>10&ctofficer$searchw==0,1,ctofficer$blackdisparity)
table(ctofficer$blackdisparity)



ctofficer$hispdisparity<-0
ctofficer$hispdisparity<-ifelse(ctofficer$searchrate>meansearchrate&ctofficer$stopw>=50&ctofficer$stoph>=50&
                                   ctofficer$SRRh>2,1,ctofficer$hispdisparity)
ctofficer$hispdisparity<-ifelse(ctofficer$searchrate>meansearchrate&ctofficer$stopw>=50&ctofficer$stoph>=50&
                                   ctofficer$searchh>10&ctofficer$searchw==0,1,ctofficer$hispdisparity)
table(ctofficer$hispdisparity)

#We only really want to keep the Id variables and the disparity codes for the merge
ctofficer<-ctofficer[,c(1,2,17,18)]


#Now to merge in these disparity terms back into the original by stop data
ctdatamerge<-merge(ctdata,ctofficer,all.x = T,by = c("ReportingOfficerIdentificationID", "Department.Name"))


#Lastly we want to convert the department name to and encoded variable called agency.
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

ctdatamerge$agency <- encode_ordinal(ctdatamerge$Department.Name)

#Now to save the data
setwd("C:\\Users\\kroac\\Desktop\\othermatierals\\Samplecode")
write.csv(ctdatamerge,"CTdatacleaned.csv")


