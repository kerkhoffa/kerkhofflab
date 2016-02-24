#Analyze Forest Carbon at the BFEC
#Drew Kerkhoff
#9-16-2015

#Bring in Data on Tree dbh
Trees <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/ForestTrees.csv")


#Load packages
require(mosaic)
require(lattice)

#Summarize DBH
favstats(DBH, data=Trees)
favstats(DBH~Quadrant, data=Trees)

#Make a pretty histogram of DBH
histogram(~DBH, data=Trees, type="count", nint=9, xlab="Diameter at breast height (cm)", col="maroon")

#Compare Sites using Boxplot
bwplot(DBH~Site, data=Trees, xlab="Site", ylab="DBH (cm)")

#Bring in lookup tables
TreeBiomassEqs <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/TreeBiomassEqs.csv")

TreeSpeciesGroup <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/TreeSpeciesGroup.csv")

#Join the SpeciesGroup into the Tree data
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

#join by lookup
Trees=left_join(Trees, TreeSpeciesGroup, by="TreeSpecies")

#replace NA SpeciesGroup with "mh"
Trees$SpeciesGroup[which(is.na(Trees$SpeciesGroup))]="mh"
Trees=left_join(Trees,TreeBiomassEqs, by="SpeciesGroup")

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
Trees=mutate(Trees, TreeBiomass=exp(beta0+beta1*log(DBH)))

#Compare Sites using Boxplot
bwplot(log10(TreeBiomass)~Site, data=Trees, xlab="Site", ylab="log Biomass (kg)")

#####################
#Growth analysis

TreeGrowth <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/ForestGrowth.csv")

TreeGrowth$DBHprev = TreeGrowth$DBH-2*TreeGrowth$RingWidth/10
xyplot(DBHprev~DBH, groups=Site, data=TreeGrowth, type=c("p","r"))
DBHprev.lm = lm(DBHprev~DBH*Site, data=TreeGrowth)
summary(DBHprev.lm)
DBHprev.lm = lm(DBHprev~DBH+Site, data=TreeGrowth)
summary(DBHprev.lm)
DBHprev.lm = lm(DBHprev~DBH, data=TreeGrowth)
summary(DBHprev.lm)

TreesLiving=subset(Trees, TreeStatus=="Alive", drop=TRUE)
TreesLiving$DBHprev=predict(DBHprev.lm, newdata=TreesLiving)
TreesLiving=mutate(TreesLiving, TreeBiomassInc=TreeBiomass-exp(beta0+beta1*log(DBHprev)))

TreesDead=subset(Trees, TreeStatus!="Alive", drop=TRUE)
#Sums
TreeBiomassIncSums=sum(TreeBiomassInc~Site+Quadrant, data=TreesLiving)
TreeBiomassSums=sum(TreeBiomass~Site+Quadrant, data=TreesLiving)
DeadBiomassSums=sum(TreeBiomass~Site+Quadrant, data=TreesDead)

#Sums per area - each quadrant has area 0.25 * pi * 10^2 = 25*pi
TreeCArea = 1000*TreeBiomassSums/(25*pi)/2
TreeCIncArea = 1000*TreeBiomassIncSums/(25*pi)/2
DeadTreeCArea=1000*DeadBiomassSums/(25*pi)/2

#Site Means
MeanTreeCIncArea=sum(TreeBiomassInc~Site, data=TreesLiving)*1000/(100*pi)/2
MeanTreeCArea=sum(TreeBiomass~Site, data=TreesLiving)*1000/(100*pi)/2
MeanDeadTreeCArea=sum(TreeBiomass~Site, data=TreesDead)*1000/(100*pi)/2

##############################################
#Respiration data analysis
SoilResp <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/SoilResp.csv")

#Convert date variables to POSIX dates

require(chron)

SoilResp$CollectDate = as.POSIXct(SoilResp$CollectDate, format="%m/%d/%Y %H:%M:%S")
SoilResp$DeployDate = as.POSIXct(SoilResp$DeployDate, format="%m/%d/%Y %H:%M:%S")
#Add a Day Tag to match samples to blanks later
require(tidyr)
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
#Isolate only Positive respiration values
SoilSamplesPositive=subset(SoilSamples, CO2.umol.m2.s>0, drop=TRUE)
xyplot(CO2.umol.m2.s~TempAverage, group=Site, data=SoilSamplesPositive, auto.key=TRUE, type=c("p","r"))
#not much of a relationship evident
summary(lm(CO2.umol.m2.s~TempAverage, data=SoilSamplesPositive))
#calculate mean respiration for each site then convert to annual values based on Bahn et al. equation
SoilRespSiteMeans=mean(CO2.umol.m2.s~Site, data=SoilSamplesPositive)
SoilRespAnnual = 436.2*(SoilRespSiteMeans)^0.926

#Forest Floor
ForestFloor <- read.csv("~/Google Drive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/ForestFloor.csv")
ForestFloorCN <- read.csv("~/Google Drive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/ForestFloorCN2014.csv")

#Shape CN data to match ForestFloor better
CN=mean(averageC~Site+Material, data=ForestFloorCN)
CNdf=data.frame(measure=names(CN), value=CN)
CNdf=separate(CNdf, col=measure, into=c("Site","component"))
CNdf=spread(CNdf, key=component, value=value)
names(CNdf)=c("Site","HerbC","LitterC","MinSoilC","OrgSoilC")

#Join C values into ForestFloor
ForestFloor=left_join(ForestFloor, CNdf, by="Site")
ForestFloor=mutate(ForestFloor, HerbCArea=4*HerbBiomass*HerbC/100, LitterCArea=4*LitterMass*LitterC/100, OrgSoilCArea=4*OrganicSoil*OrgSoilC/100, MinSoilCArea=MineralSoil*MinSoilC/100/(0.01^2*pi))

#Litterfall data - sum by site for annual litterfall
#Load Data

ForestLitter <- read.csv("~/GoogleDrive/Courses/Biol229/2015/Carbon Budget/ForestCarbon/ForestLitter.csv")
#sum by site
LitterFallSums=sum(LitterWeight~Site, data=ForestLitter)
LitterFallCArea=4*LitterSums/2

#Assemble Site Summaries of all pools and fluxes
ForestSummaryCArea=summarise_each(group_by(ForestFloorCArea, Site), funs(mean(., na.rm=TRUE)))

ForestSummaryCArea=cbind(ForestSummaryCArea,MeanDeadTreeCArea,MeanTreeCArea,MeanTreeCIncArea,LitterFallCArea,SoilRespAnnual)

#Plot Carbon Pools
barchart(log(t(ForestSummaryCArea[,c(5,4,3,6,7)])), auto.key=list(corner=c(1,0.3)), xlab=expression(paste(log[10]," Carbon stock (",gC," ",m^-2,")")), scale=list(y=list(labels=c("Mineral Soil","Organic Soil","Litter","Dead Wood","Living Trees"))))

barchart(t(ForestSummaryCArea[,c(5,4,3,6,7)]), auto.key=list(corner=c(1,0.3)), xlab=expression(paste("Carbon stock (",gC," ",m^-2,")")), scale=list(y=list(labels=c("Mineral Soil","Organic Soil","Litter","Dead Wood","Living Trees"))))

require(latticeExtra)
segplot(Site~Resp+NPP, data=TonHa, horizontal=FALSE, xlab="Site", ylab=expression(paste("Carbon stock (",MgC," ",ha^-1," ",y^-1,")")))
ladd(panel.abline(0, 0))

