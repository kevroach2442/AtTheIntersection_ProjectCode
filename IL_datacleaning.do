* Date 3/25/2020
* Author: Kevin Roach


*  This file will load the raw IL traffic stops data and process the data into our unified coding scheme

*change directory to one appropriate for your machine
cd ""
*Il has a different data structure to most.  Each year different data was collected, however the required variables are there throughout (though with different names
*to begin each year needs to be converted into a common style.  This section will load the year, drop irrelevant variables, and name variables to a common useable format
*The time variable will also be converted to the day of week and hour format needed.
*Keep in mind, the data once combined is very large, as such, all variables not being used should be dropped to keep data manageable.  

*The raw data largely comes in two major formats.  One used between 2004-2009 and then from 2010-2014


//load 2004
import delimited "2004 Raw Data Statewide.txt", delimiter("~") clear
drop vehiclemake typeofroadway beatlocationofstop passenger1searchtype passenger2searchtype passenger3searchtype passenger4searchtype passenger5searchtype passenger6searchtype drugsfound weaponfound stolenpropertyfound othercontrabandfound

*changing the time
generate date=date( dateandtimeofstop, "MDYhms")
replace date= date( dateandtimeofstop,"MDY" ) if date==.
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hms" )
format time %tc
generate hour=hh(time)
generate year=2004
*dropping time variables we wont use
drop time date dateandtimeofstop

*The search variable
*The raw searcdh variable actually includes passenger searches.  We only want driver searches so we need to make a new variable using driver and vehicle searches (exluding passenger searches).  This will be the same throughout each year.
rename searchconducted searchconductedinclpassenger
generate searchconducted=1 if driversearchtype !=""
replace searchconducted=1 if vehiclesearchtype !=""
replace searchconducted=0 if driversearchtype==""
replace searchconducted=0 if vehiclesearchtype==""

// now save the data
save "IL2004.dta", replace

//load 2005
import delimited "RawMicroFiles\Illinois\2005 Raw Data Statewide.txt", delimiter("~") clear
drop vehiclemake typeofroadway beatlocationofstop passenger1searchtype passenger2searchtype passenger3searchtype passenger4searchtype passenger5searchtype passenger6searchtype drugsfound weaponfound stolenpropertyfound othercontrabandfound

*changing time
generate date=date( dateandtimeofstop, "MDYhms")
replace date= date( dateandtimeofstop,"MDY" ) if date==.
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hms" )
format time %tc
generate hour=hh(time)
generate year=2005

drop time date dateandtimeofstop

*the search variable
rename searchconducted searchconductedinclpassenger
generate searchconducted=1 if driversearchtype !="N/A"
replace searchconducted=1 if driversearchtype !="NA"
replace searchconducted=1 if vehiclesearchtype !="N/A"
replace searchconducted=1 if driversearchtype !="NA"
replace searchconducted=0 if driversearchtype=="N/A" 
replace searchconducted=0 if driversearchtype =="NA"
replace searchconducted=0 if vehiclesearchtype=="N/A"
replace searchconducted=0 if driversearchtype =="NA"

*saving
save "IL2005.dta", replace

//2006
import delimited "RawMicroFiles\Illinois\2006 Raw Data Statewide.txt", delimiter("~") clear
drop vehiclemake typeofroadway beatlocationofstop passenger1searchtype passenger2searchtype passenger3searchtype passenger4searchtype passenger5searchtype passenger6searchtype drugsfound weaponfound stolenpropertyfound othercontrabandfound

*time
generate date=date( dateandtimeofstop, "MDYhms")
replace date= date( dateandtimeofstop,"MDY" ) if date==.
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hms" )
format time %tc
generate hour=hh(time)
generate year=2006

drop time date dateandtimeofstop

*search
rename searchconducted searchconductedinclpassenger
generate searchconducted=1 if driversearchtype !=""
replace searchconducted=1 if vehiclesearchtype !=""
replace searchconducted=0 if driversearchtype==""
replace searchconducted=0 if vehiclesearchtype==""

*save
save "IL2006.dta", replace

//2007 is special.
* For 2007 we have seperate files for the Chicago PD.  These need to be prepped and added to 2007 before moving on
import delimited "RawMicroFiles\Illinois\Chicago Raw Data2007 File1.txt", delimiter("~") clear
drop vehiclemake pass6searchtype pass5searchtype pass4searchtype pass3searchtype pass2searchtype pass1searchtype beatlocationstop

*time
generate date=date( dateandtimeofstop, "MDYhm")
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hm" )
format time %tc
generate hour=hh(time)
generate year=2007

drop time date
*save
save "CleanedMicroFiles\Illinois\IL2007chicago1.dta", replace

//chicago part 2
import delimited "RawMicroFiles\Illinois\Chicago Raw Data2007 File2.txt", delimiter("~") clear
drop vehiclemake beatlocationstop passengersearchtype drugsfound paraphernaliafound alcoholfound weaponfound stolenpropertyfound othercontrabandfound drugquantity consentsearchrequested wasconsentgranted wasconsentsearchperformed wasconsentcontrabandfound consentdrugsfound consentparaphernaliafound consentalcoholfound consentweaponfound consentstolenpropertyfound consentothercontrabandfound consentdrugquantity

*time
generate date=date( dateandtimeofstop, "MDYhm")
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hm" )
format time %tc
generate hour=hh(time)
generate year=2007

drop time date
*save
save "CleanedMicroFiles\Illinois\IL2007chicago2.dta", replace

//chicago part 3
import delimited "RawMicroFiles\Illinois\Chicago Raw Data2007 File3.txt", delimiter("~") clear
drop consentdrugquantity consentothercontrabandfound consentstolenpropertyfound consentweaponfound consentalcoholfound consentparaphernaliafound consentdrugsfound wasconsentcontrabandfound wasconsentsearchperformed wasconsentgranted consentsearchrequested drugquantity othercontrabandfound stolenpropertyfound weaponfound alcoholfound paraphernaliafound drugsfound passengersearchtype beat vehmake
*first some variables need name changes
rename datestoptimestop dateandtimeofstop
rename yrbirth driveryearofbirth
rename vehyr vehicleyear

*now time
generate date=date( dateandtimeofstop, "MDYhm")
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hm" )
format time %tc
generate hour=hh(time)
generate year=2007

drop time date

*save
save "CleanedMicroFiles\Illinois\IL2007chicago3.dta", replace


//now to prep the main 2007 file
import delimited "RawMicroFiles\Illinois\2007 Raw Data Statewide without Chicago.txt", delimiter("~") clear
drop consentdrugquantity consentothercontrabandfound consentstolenpropertyfound consentweaponfound consentalcoholfound consentparaphernaliafound consentdrugsfound wasconsentcontrabandfound wasconsentsearchperformed wasconsentgranted consentsearchrequested drugquantity othercontrabandfound stolenpropertyfound weaponfound alcoholfound paraphernaliafound drugsfound passengerssearchtype beatlocationofstop vehiclemake

*time
generate date=date( dateandtimeofstop, "MDYhms")
replace date= date( dateandtimeofstop,"MDY" ) if date==.
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hms" )
format time %tc
generate hour=hh(time)
generate year=2007

drop time date 
*and adding in Chicago 
append using "CleanedMicroFiles\Illinois\IL2007chicago1.dta" "CleanedMicroFiles\Illinois\IL2007chicago2.dta" "CleanedMicroFiles\Illinois\IL2007chicago3.dta"

*getting rid of extra variables
drop dateandtimeofstop
drop durationofstop

*search
rename searchconducted searchconductedinclpassenger

generate searchconducted=1 if driversearchtype !=""
replace searchconducted=1 if vehiclesearchtype !=""
replace searchconducted=0 if driversearchtype==""
replace searchconducted=0 if vehiclesearchtype==""

*save
save "IL2007.dta", replace

// 2008
import delimited "RawMicroFiles\Illinois\2008 Raw Data Statewide.txt", delimiter("~") clear
drop durationofstop vehiclemake beatlocationofstop passengerssearchtype paraphernaliafound weaponfound stolenpropertyfound othercontrabandfound drugquantity consentsearchrequested wasconsentgranted wasconsentsearchperformed wasconsentcontrabandfound consentdrugsfound consentalcoholfound consentparaphernaliafound consentstolenpropertyfound consentweaponfound consentothercontrabandfound consentdrugquantity drugsfound alcoholfound

*time
generate date=date( dateandtimeofstop, "MDYhm")
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hm" )
format time %tc
generate hour=hh(time)
generate year=2008
drop time date dateandtimeofstop


*search
rename searchconducted searchconductedinclpassenger

generate searchconducted=1 if driversearchtype !=""
replace searchconducted=1 if vehiclesearchtype !=""
replace searchconducted=0 if driversearchtype==""
replace searchconducted=0 if vehiclesearchtype==""

*save
save "IL2008.dta", replace

//2009
import delimited "RawMicroFiles\Illinois\2009 Raw Data Statewide.txt", delimiter("~") clear
drop durationofstop vehiclemake beatlocationofstop passengerssearchtype drugsfound alcoholfound paraphernaliafound weaponfound stolenpropertyfound othercontrabandfound drugquantity consentsearchrequested wasconsentgranted wasconsentsearchperformed wasconsentcontrabandfound consentdrugsfound consentalcoholfound consentparaphernaliafound consentweaponfound consentstolenpropertyfound consentothercontrabandfound consentdrugquantity

*time
generate date=date( dateandtimeofstop, "MDYhms")
replace date= date( dateandtimeofstop,"MDY" ) if date==.
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hms" )
format time %tc
generate hour=hh(time)
generate year=2009

drop time date dateandtimeofstop
*search
rename searchconducted searchconductedinclpassenger

generate searchconducted=1 if driversearchtype !=""
replace searchconducted=1 if vehiclesearchtype !=""
replace searchconducted=0 if driversearchtype==""
replace searchconducted=0 if vehiclesearchtype==""

*save
save "IL2009.dta", replace

//2010 on has a different structure than 2004-2009
*The changes being made here are largely the same but variable names and codes will be different in the raw data
import delimited "RawMicroFiles\Illinois\2010 ITSS Data.txt", delimiter("~") clear
drop durationofstop zipcode vehiclemake beatlocationofstop passengerssearchtype drugsfound alcoholfound paraphernaliafound weaponfound stolenpropertyfound othercontrabandfound drugquantity consentsearchrequested wasconsentgranted wasconsentsearchperformed wasconsentcontrabandfound consentdrugsfound consentalcoholfound consentparaphernaliafound consentweaponfound consentstolenpropertyfound consentothercontrabandfound consentdrugquantity

*time
generate date=date( dateandtimeofstop, "MDYhm")
replace date= date( dateandtimeofstop,"MDY" ) if date==.
generate weekday=dow( date )

generate time=clock( dateandtimeofstop, "MDY hm" )
format time %tc
generate hour=hh(time)
generate year=2010
replace search=0 if search==.

drop time date dateandtimeofstop
*search
rename searchconducted searchconductedinclpassenger

generate searchconducted=1 if driversearchtype !=0
replace searchconducted=1 if vehiclesearchtype !=0
replace searchconducted=0 if driversearchtype==0
replace searchconducted=0 if vehiclesearchtype==0

*save
save "IL2010.dta", replace

//2011
import delimited "RawMicroFiles\Illinois\2011 ITSS Data.txt", delimiter("~") clear
drop durationofstop zipcode vehiclemake beatlocationofstop passengerssearchtype drugsfound alcoholfound paraphernaliafound weaponfound stolenpropertyfound othercontrabandfound drugquantity consentsearchrequested wasconsentgranted wasconsentsearchperformed wasconsentcontrabandfound consentdrugsfound consentalcoholfound consentparaphernaliafound consentweaponfound consentstolenpropertyfound consentothercontrabandfound consentdrugquantity

*time
generate date=date( dateofstop, "MDY")
generate weekday=dow( date )

generate time=clock( timeofstop, "hm" )
format time %tc
generate hour=hh(time)
generate year=2011

*search
replace search=0 if search==.

*the vehicle age variable needs to be reformulated
generate vehicleyear2=year(date( vehicleyear, "Y"))
*reordering the variables
order agencyname agencycode vehicleyear2 driveryearofbirth driversex race reasonforstop movingviolationtype searchconducted contrabandfound weekday hour year vehicleyear
*getting rid of the original vehicle age
drop vehicleyear
rename vehicleyear2 vehicleyear

*now to drop the time variables we no longer use
drop dateofstop timeofstop date time


*search
rename searchconducted searchconductedinclpassenger

generate searchconducted=1 if driversearchtype !=0
replace searchconducted=1 if vehiclesearchtype !=0
replace searchconducted=0 if driversearchtype==0
replace searchconducted=0 if vehiclesearchtype==0

*save
save "IL2011.dta", replace

//2012
import delimited "RawMicroFiles\Illinois\2012 ITSS Data.txt", delimiter("~") clear
drop durationofstop zip vehiclemake beatlocationofstop vehiclesearchconductedby vehicledrugsfound vehicledrugparaphernaliafound vehiclealcoholfound vehicleweaponfound vehiclestolenpropertyfound vehicleothercontrabandfound vehicledrugamount driversearchconductedby passengerconsentsearchrequested passengerconsentgiven passengersearchconductedby driverpassengerdrugsfound driverpassengerdrugparaphernalia driverpassengeralcoholfound driverpassengerweaponfound driverpassengerstolenpropertyfou driverpassengerothercontrabandfo driverpassengerdrugamount policedogperformsniffofvehicle policedogalertifsniffed policedogvehiclesearched policedogcontrabandfound policedogdrugsfound policedogdrugparaphernaliafound policedogalcoholfound policedogweaponfound policedogstolenpropertyfound policedogothercontrabandfound policedogdrugamount

*beginning in 2012 the coding for searches and contraband changed, this will use the information given to contruct a search variable consistent with the earlier datasets
generate search=1 if vehiclesearchconducted==1| driversearchconducted==1
replace search=0 if vehiclesearchconducted==2& driversearchconducted==2
generate contraband=1 if vehiclecontrabandfound==1| driverpassengercontrabandfound==1
replace contraband=0 if vehiclecontrabandfound==0& driverpassengercontrabandfound==0
drop vehiclesearchconducted vehiclecontrabandfound driversearchconducted passengersearchconducted driverpassengercontrabandfound
rename contraband contrabandfound

*some other variables that need to be renamed to match previous years
rename driversyearofbirth driveryearofbirth
rename driverrace race
rename typeofmovingviolation movingviolationtype

*time
generate date=date( dateofstop, "MDY")
generate weekday=dow( date )

generate time=clock( timeofstop, "hm" )
format time %tc
generate hour=hh(time)
generate year=2012

drop dateofstop timeofstop date time

save "IL2012.dta", replace


//2013

import delimited "RawMicroFiles\Illinois\2013 ITSS Data.txt", delimiter("~") clear
drop durationofstop zip vehiclemake beatlocationofstop vehiclesearchconductedby vehicledrugsfound vehicledrugparaphernaliafound vehiclealcoholfound vehicleweaponfound vehiclestolenpropertyfound vehicleothercontrabandfound vehicledrugamount driversearchconductedby passengerconsentsearchrequested passengerconsentgiven passengersearchconductedby driverpassengerdrugsfound driverpassengerdrugparaphernalia driverpassengeralcoholfound driverpassengerweaponfound driverpassengerstolenpropertyfou driverpassengerothercontrabandfo driverpassengerdrugamount policedogperformsniffofvehicle policedogalertifsniffed policedogvehiclesearched policedogcontrabandfound policedogdrugsfound policedogdrugparaphernaliafound policedogalcoholfound policedogweaponfound policedogstolenpropertyfound policedogothercontrabandfound policedogdrugamount

*search and contraband
generate search=1 if vehiclesearchconducted==1| driversearchconducted==1
replace search=0 if vehiclesearchconducted==2& driversearchconducted==2
generate contraband=1 if vehiclecontrabandfound==1| driverpassengercontrabandfound==1
replace contraband=0 if vehiclecontrabandfound==0& driverpassengercontrabandfound==0
drop vehiclesearchconducted vehiclecontrabandfound driversearchconducted passengersearchconducted driverpassengercontrabandfound
rename search searchconducted
rename contraband contrabandfound

*renaming
rename driversyearofbirth driveryearofbirth
rename driverrace race
rename typeofmovingviolation movingviolationtype

*time
generate date=date( dateofstop, "YMD hms")
replace date=date( dateofstop, "MDY") if date==.
generate weekday=dow( date )

generate time=clock( timeofstop, "hm" )
format time %tc
generate hour=hh(time)
generate year=2013

drop dateofstop timeofstop date time


*reformating driver age
generate driveryearofbirth2=year(date( driveryearofbirth, "MDY" ))
replace driveryearofbirth2=year(date( driveryearofbirth, "Y" )) if driveryearofbirth2==.
order agencyname agencycode vehicleyear driveryearofbirth2 driversex race reasonforstop movingviolationtype searchconducted contrabandfound weekday hour year driveryearofbirth
drop driveryearofbirth
rename driveryearofbirth2 driveryearofbirth

save "IL2013.dta", replace


//2014
import delimited "RawMicroFiles\Illinois\2014 ITSS Data.txt", delimiter("~") clear
drop durationofstop zip vehiclemake beatlocationofstop vehiclesearchconductedby vehicledrugsfound vehicledrugparaphernaliafound vehiclealcoholfound vehicleweaponfound vehiclestolenpropertyfound vehicleothercontrabandfound vehicledrugamount driversearchconductedby passengerconsentsearchrequested passengerconsentgiven passengersearchconductedby driverpassengerdrugsfound driverpassengerdrugparaphernalia driverpassengeralcoholfound driverpassengerweaponfound driverpassengerstolenpropertyfou driverpassengerothercontrabandfo driverpassengerdrugamount policedogperformsniffofvehicle policedogalertifsniffed policedogvehiclesearched policedogcontrabandfound policedogdrugsfound policedogdrugparaphernaliafound policedogalcoholfound policedogweaponfound policedogstolenpropertyfound policedogothercontrabandfound policedogdrugamount

*search
generate search=1 if vehiclesearchconducted==1| driversearchconducted==1
replace search=0 if vehiclesearchconducted==2& driversearchconducted==2
generate contraband=1 if vehiclecontrabandfound==1| driverpassengercontrabandfound==1
replace contraband=0 if vehiclecontrabandfound==0& driverpassengercontrabandfound==0
drop vehiclesearchconducted vehiclecontrabandfound driversearchconducted passengersearchconducted driverpassengercontrabandfound
rename search searchconducted
rename contraband contrabandfound

*renaming
rename driversyearofbirth driveryearofbirth
rename driverrace race
rename typeofmovingviolation movingviolationtype

*time
generate date=date( dateofstop, "MDY")
generate weekday=dow( date )

generate time=clock( timeofstop, "hm" )
format time %tc
generate hour=hh(time)
generate year=2014

drop dateofstop timeofstop date time

save "IL2014.dta", replace

//Now to combine the various years
//The data uses 2 sets of codes, 2005-2009 and 2010-2014
//The codings need to be brought into a single format
use "IL2009.dta", clear
append using "IL2004.dta" "IL2005.dta" "IL2006.dta" "IL2007.dta" "IL2008.dta"

*recoding gender
generate gender=1 if driversex=="Male"
replace gender=0 if driversex=="Female"

*recoding race
generate race2=1 if race=="Caucasian"
replace race2=2 if race=="African American"
replace race2=3 if race=="Hispanic"
replace race2=4 if race=="Native American/Alaskan"|race=="Asian/Pacific Islander"
rename race raceraw
rename race2 race

*Stop purpose
generate stoppurpose=2 if reasonforstop=="Equipment"
replace stoppurpose=1 if reasonforstop=="License Plate/Registration"
replace stoppurpose=3 if reasonforstop=="Moving Violation"|reasonforstop=="Moving ViolationMoving Violation"
replace stoppurpose=0 if stoppurpose==.

*Moving violation information
generate movingviolation=0 if movingviolationtype=="Other"
replace movingviolation=1 if movingviolationtype=="Follow too Close"
replace movingviolation=2 if movingviolationtype=="Lane Violation"
replace movingviolation=3 if movingviolationtype=="Seatbelt"
replace movingviolation=4 if movingviolationtype=="Speed"
replace movingviolation=5 if movingviolationtype=="Traffic Sign or Signal"

*contraband
generate contraband=1 if contrabandfound=="Yes"
replace contraband=0 if contrabandfound=="No"

*outcome of the stop
generate outcome1=0
replace outcome1=1 if resultofstop=="Written Warning"
replace outcome1=2 if resultofstop=="Verbal Warning"
replace outcome1=3 if resultofstop=="Citation"

*creating our outcome2 variable (warning vs not)
generate outcome2=0
replace outcome2=1 if outcome1==3

*renaming
rename reasonforstop stopurposeraw
rename driversex genderraw
rename contrabandfound contrabandraw

save "ILsub1.dta", replace

//Now to load and change the second half
use "IL2014.dta", clear
append using "IL2013.dta" "IL2012.dta" "IL2011.dta" "IL2010.dta"

*recoding gender
generate gender=1 if driversex==1
replace gender=0 if driversex==2

*recoding race
generate race2=1 if race==1
replace race2=2 if race==2
replace race2=3 if race==4
replace race2=4 if race==3|race==5|race==6
rename race raceraw2
rename race2 race

*recoding stoppurpose
generate stoppurpose=1 if reasonforstop==3
replace stoppurpose=2 if reasonforstop==2
replace stoppurpose=3 if reasonforstop==1
replace stoppurpose=0 if stoppurpose==.

*moving violation information
generate movingviolation=0 if movingviolationtype==6
replace movingviolation=1 if movingviolationtype==5
replace movingviolation=2 if movingviolationtype==2
replace movingviolation=3 if movingviolationtype==3
replace movingviolation=4 if movingviolationtype==1
replace movingviolation=5 if movingviolationtype==4

*contraband
generate contraband=1 if contrabandfound==1
replace contraband=0 if contrabandfound==0

*outcome 1
generate outcome1=0
replace outcome1=1 if resultofstop==2
replace outcome1=2 if resultofstop==3
replace outcome1=3 if resultofstop==1

*outcomes 2
generate outcome2=0
replace outcome2=1 if outcome1==3

*renaming
rename reasonforstop stopurposeraw2
rename driversex genderraw2
rename contrabandfound contrabandraw2
rename resultofstop resultofstop2


drop movingviolationtype vehicleconsentsearchrequested vehicleconsentgiven driverconsentsearchrequested driverconsentgiven


save "ILsub2.dta", replace

*now to combine the 2 parts
use "ILsub1.dta", clear
append using "ILsub2.dta", force

*and to save the results
save "IL_TrafficStops.dta", replace


*Now to fully process the data now that it is together
*First the driver age, derived from their year of birth and the year of the stop
generate age=year- driveryearofbirth
replace age=. if age>=101|age<=15

*Same process for the age of the vehicle
generate vehicleage=(year- vehicleyear)+1
replace vehicleage=. if vehicleage>=100|vehicleage<=0

*Changing the name of weekday to be slightly less confusing.  We arent coding if it was a weekday but what day of the week it was a numeric code
rename weekday dayofweek

*Creating dummy variable for stops of different races
*Stopt will be used in our aggregate codes later to count number of obs.
generate stopt=1
generate stopw=1 if race==1
replace stopw=0 if race==2|race==3
generate stopb=1 if race==2
replace stopb=0 if race==1|race==3
generate stoph=1 if race==3
replace stoph=0 if race==1|race==2

*To make a search variable for fruitless searches.
generate search2=1 if search==1&contraband==0
replace search2=0 if search==0|contraband==1

*Creating dummy variables for the different stop purposes
generate equipment=1 if stoppurpose==2
replace equipment=0 if stoppurpose!=2
generate moving_violation=1 if stoppurpose==3
replace moving_violation=0 if stoppurpose!=3
generate registration=1 if stoppurpose==1
replace registration=0 if stoppurpose!=1



*creating out stop purpose 2 which records if the stop was investigatory or not
rename searchconducted searchoccur
rename stoppurpose stoppurpose1
replace stoppurpose1=. if stoppurpose1==0
gen stoppurpose2=1 if stoppurpose1==2|stoppurpose1==1
replace stoppurpose2=0 if stoppurpose1==3 
*coding seatbelt violations as investigatory
replace stoppurpose2=1 if movingviolation==3


save "IL_TrafficStops.dta", replace



*now to clean the agency variable 
*There are over 1000 agencies in the IL data, and some of them are very tiny.  Given the size of the data you can quickly exceed you computational power.  
*As such you may want to use a stop threshold and drop the very small agencies
*This code allows for the creation of such a threshold 

collapse (sum) stopt search, by(agencyname)

***
*** Changing the threshold to your prefered value here.  Using a very low threshold so not data is dropped if this line isnt recoded
***
generate agencythresh=1 if stopt>=1
*saving this data both to be merged, but also as a handy reference for the different agencies in IL and their size.
save "ILagency.dta", replace
use "IL_TrafficStops.dta", clear
merge m:m agencyname using "ILagency.dta", keepusing(agencythresh)

*Encoding the agencyname variable to a numeric format
encode agencyname, gen(agency)

*Going from a raw age variable to one that bins age in 10 year increments.
generate age2=.
replace age2=0 if age<=25
replace age2=1 if age>25&age<=35
replace age2=2 if age>35&age<=45
replace age2=3 if age>45&age<=55
replace age2=4 if age>55&age<=65
*creating some variables to show the bins for Blacks and Hispanics
generate stopbage=stopb*age2
generate stophage=stoph*age2

*Creating similar bins for vehicles age (but the bin size will be 5
generate vage=.
replace vage=0 if vehicleage<=5
replace vage=1 if vehicleage>5&vehicleage<=10
replace vage=2 if vehicleage>10&vehicleage<=15
replace vage=3 if vehicleage>15
*creating some variables to show the bins for Blacks and Hispanics
generate stopbvage=stopb*vage
generate stophvage=stoph*vage



save "IL_TrafficStops.dta", replace

*Given the size of the Il files it can be very useful to trim out ANY unnecessary data before loading in for modelling.  
*The following code creates a slimmed file, largely dropping any variables that wont be used for models

***
***  Be sure to change the following code according to your model to ensure the right data is being dropped
***

*Generating a slimmed file for regressions
use "IL_TrafficStops.dta", clear




*We want to drop searches incident to arrest

drop if driversearchtype=="Incident to Arrest"

*Keeping only the variable we use

keep agencyname dayofweek hour searchoccur gender race stoppurpose1 stopt stopw stopb stoph age vehicleage stoppurpose2 blackagencywindow hispagencywindow


save "ILslim.dta", replace

