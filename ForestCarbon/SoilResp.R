##############################################
#Respiration data analysis - Ecology Lab
#Drew Kerkhoff
#November 2015

#Read data - adjust for location of data source

SoilResp <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/SoilResp.csv")

require(mosaic)
require(tidyr)
require(dplyr)
require(chron)

#Convert date variables to POSIX dates

SoilResp$CollectDate = as.POSIXct(SoilResp$CollectDate, format="%m/%d/%Y %H:%M:%S")
SoilResp$DeployDate = as.POSIXct(SoilResp$DeployDate, format="%m/%d/%Y %H:%M:%S")
#Add a Day Tag to match samples to blanks later

SoilResp$DayTag = dates(as.chron(SoilResp$CollectDate))
SoilResp = unite(SoilResp, SiteDayTag, Site, DayTag, remove=FALSE)

#calculate duration of assay (defaults to hours)
SoilResp$duration = SoilResp$CollectDate - SoilResp$DeployDate

#calculate mass gained by the soda lime
SoilResp$WeightChange = SoilResp$PostWeight-SoilResp$PreWeight

#subset to separate Blanks and Samples
SoilBlanks=subset(SoilResp, Blank.or.Sample=="Blank", drop=TRUE)
SoilSamples=subset(SoilResp, Blank.or.Sample=="Sample", drop=TRUE)

#isolate just the BlankChange, then join into the samples as a new column
SoilBlanks$BlankChange=SoilBlanks$WeightChange
SoilBlanks=SoilBlanks[,c(4,15)]
#Correct two negative blank values by taking average of the other two sites
SoilBlanks[12,2]=mean(SoilBlanks[7:8,2])
SoilBlanks[13,2]=mean(SoilBlanks[14:15,2])

SoilSamples = left_join(SoilSamples,SoilBlanks, by="SiteDayTag")

#Calculate c gain adjusted for blank, and converting to umol CO2 per m^2 per second
SoilSamples$CO2.mg=1000*1.69*(SoilSamples$WeightChange-SoilSamples$BlankChange)
#area of chamber is pi*(0.21/2)^2 in sq meters
#44 mg per mmol means 0.044 mg/umol
#seconds is hours*3600
SoilSamples$CO2.umol.m2.s = SoilSamples$CO2.mg/0.044/(pi*(0.21/2)^2)/(as.double(SoilSamples$duration)*3600)

#Compare to temperatures
#Isolate only Positive respiration values - we had some strange data
SoilSamplesPositive=subset(SoilSamples, CO2.umol.m2.s>0, drop=TRUE)
xyplot(CO2.umol.m2.s~TempAverage, group=Site, data=SoilSamplesPositive, auto.key=TRUE, type=c("p","r"))
#not much of a relationship evident
summary(lm(CO2.umol.m2.s~TempAverage, data=SoilSamplesPositive))
#calculate mean respiration for each site then convert to annual values based on Bahn et al. equation
SoilRespSiteMeans=mean(CO2.umol.m2.s~Site, data=SoilSamplesPositive)
SoilRespAnnual = 436.2*(SoilRespSiteMeans)^0.926