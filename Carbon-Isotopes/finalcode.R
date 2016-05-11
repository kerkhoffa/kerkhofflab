#Cecina Babich Morrow
#May 11, 2016
#Code for creating a phylogenetic tree and fitting alternative evolutionary models using a dataset of carbon isotope fractionation.

#Load required packages:
require(taxize)
require(brranching)
require(tidyr)
require(geiger)
require(Hmisc)
require(stringr)
require(phytools)

#Define functions to adjust branch lengths using scalePhylo:
source('C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Carbon Isotopes/CarbonIsotopes/scalephylo.R')

#Read data:
diefdataedit <- read.csv("C:/Users/Cecina/OneDrive/Documents/Kenyon College/Kerkhoff Lab/Carbon Isotopes/CarbonIsotopes/diefdataedit.csv")
View(diefdataedit)
attach(diefdataedit)
#Reformat data to separate the Species column into a Genus and sp column:
diefdataedit=separate(diefdataedit, col=Species, into=c("Genus","sp"),sep="_",remove=FALSE)
View(diefdataedit)

#Create a phylogenetic tree of the species in the data set.
#Make a vector of species for use by phylomatic:
taxa=as.character(diefdataedit$Species)
#Create a phylogenetic tree of the species using phylomatic:
wholetree<-phylomatic(taxa=taxa,storedtree="R20120829",get="POST")

#Reformat the tree and data so that the formatting matches.
#Capitalize the tip labels:
wholetree$tip.label <- capitalize(wholetree$tip.label)
#Remove the underscores from the tip labels:
wholetree$tip.label<-str_replace(wholetree$tip.label,"_"," ")
plot(wholetree, type="fan",no.margin=TRUE)
plot(wholetree,no.margin=TRUE)
#Remove the underscores from the data:
diefdataedit$Species <- str_replace(diefdataedit$Species,"_"," ")

#Phylocom was used to add branch lengths to the phylogenetic tree using the age file ages.

#Read in the phylogenetic tree with branch lengths added from phylocom.
wholetree.bladj=read.tree("phylo.bladj")
#Reformat this tree to match the data:
wholetree.bladj$tip.label<-str_replace(wholetree.bladj$tip.label,"_"," ")
plot(wholetree.bladj,type="fan",show.tip.label=FALSE,no.margin=TRUE)

#Make the tree dichotomous (fitContinuous only works on dichotomous trees).
wholetree.bladj.di<-multi2di(wholetree.bladj,random=F)
plot(wholetree.bladj.di,type="fan",show.tip.label=FALSE)

#Additonal formatting before applying the models.
deltaleaf<-diefdataedit$deltaleaf
names(deltaleaf) <- diefdataedit$Species
deltaleaf.tip.order<-deltaleaf[wholetree$tip.label]

#Create OU, lambda, and white noise models.
deltaleaf.fit.ou<-fitContinuous(wholetree.bladj.di,deltaleaf.tip.order,model="OU")
deltaleaf.fit.ou
deltaleaf.fit.bm<-fitContinuous(wholetree.bladj.di,deltaleaf.tip.order,model="BM")
deltaleaf.fit.bm
deltaleaf.fit.lambda<-fitContinuous(wholetree.bladj.di,deltaleaf.tip.order,model="lambda")
deltaleaf.fit.lambda
deltaleaf.fit.white<-fitContinuous(wholetree.bladj.di,deltaleaf.tip.order,model="white")
deltaleaf.fit.white
#Extract AIC and log likelihood values for the models and compare:
deltaleaf.fits.aic=c(deltaleaf.fit.ou$opt$aicc,deltaleaf.fit.bm$opt$aicc,deltaleaf.fit.lambda$opt$aicc,deltaleaf.fit.white$opt$aicc)
deltaleaf.fits.lnL=c(deltaleaf.fit.ou$opt$lnL,deltaleaf.fit.bm$opt$lnL,deltaleaf.fit.lambda$opt$lnL,deltaleaf.fit.white$opt$lnL)
names(deltaleaf.fits.aic)<-names(deltaleaf.fits.lnL)<-c("OU","BM","lambda","white")
deltaleaf.delta.aic=deltaleaf.fits.aic-min(deltaleaf.fits.aic)
deltaleaf.fits.aic
#Model with lowest aic "fits best". 
#Delta aic is the difference from the best fit model, delta aic <4 indicates similar fits.
#In this case, the lambda model fits best.

#Transform the tree based on the models in order to make the ancestral estimates.
deltaleaf.ou.tree<-rescale(wholetree.bladj,model="OU",deltaleaf.fit.ou$opt$alpha) #alpha is the OU parameter
deltaleaf.lam.tree<-rescale(wholetree.bladj,model="lambda", deltaleaf.fit.lambda$opt$lambda) #lambda is the random walk parameter

#Perform reconstruction using fastAnc to estimate ancestral states using maximum likelihood.
deltaleaf.ou.fastAnc<-fastAnc(deltaleaf.ou.tree, deltaleaf.tip.order)
deltaleaf.fastAnc<-fastAnc(wholetree.bladj, deltaleaf.tip.order) #BM model uses untransformed tree.
deltaleaf.lam.fastAnc<-fastAnc(deltaleaf.lam.tree, deltaleaf.tip.order)

#Each of the .fastAnc files has a trait estimate associated with each node in the phylogeny.
#Thus these files can be used to examine how the trait value has changed over time.

#Create a tree with colored tip labels corresponding to the deltaleaf values.
require(plotrix) #plotrix will mask the rescale function from geiger, so don't load this package first
plot(wholetree.bladj,no.margin=TRUE,show.tip.label=FALSE,type="fan")
add.scale.bar(-400,-300)
tiplabels(pch=19,col=color.scale(deltaleaf.tip.order,extremes=c("blue","red"),xrange=c(14,30)))
color.legend(-400,-280,-300,-270,legend=c(14,30),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#Color the node labels in the same way based on the ancestral reconstruction estimates.
#Create a tree with colored node labels corresponding to the lambda model reconstruction:
plot(wholetree.bladj,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(deltaleaf.lam.fastAnc,extremes=c("blue","red"),xrange=c(14,30)))
tiplabels(pch=19,col=color.scale(deltaleaf.tip.order,extremes=c("blue","red"),xrange=c(14,30)))
add.scale.bar(-400,-300)
color.legend(-400,-280,-300,-270,legend=c(14,30),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")
#Create a tree with colored node labels corresponding to the OU model reconstruction:
plot(wholetree.bladj,no.margin=TRUE,show.tip.label=FALSE,type="fan")
nodelabels(pch=19,col=color.scale(deltaleaf.ou.fastAnc,extremes=c("blue","red"),xrange=c(14,30)))
tiplabels(pch=19,col=color.scale(deltaleaf.tip.order,extremes=c("blue","red"),xrange=c(14,30)))
add.scale.bar(-400,-300)
color.legend(-400,-280,-300,-270,legend=c(14,30),rect.col=color.gradient(c(0,1),0,c(1,0)),gradient="x")

#Plot the ancestral reconstruction values from the lambda model vs. the OU model.
plot(deltaleaf.lam.fastAnc~deltaleaf.ou.fastAnc,pch=20,col="magenta4",xlab="Ornstein-Uhlenbeck Delta Leaf Reconstruction", ylab="Lambda Model Delta Leaf Reconstruction")
abline(a=0,b=1,lty=2,lwd=1.5)

#Plot the predicted ancestral deltaleaf values from the lambda model as a function of time.
deltaleaf.ages<-branching.times(wholetree.bladj)
plot(deltaleaf.lam.fastAnc~deltaleaf.ages, type="n", xlim=c(350,0), xlab="Lineage age (My)", ylab="Estimated Delta Leaf Value")
points(deltaleaf.lam.fastAnc~deltaleaf.ages, pch=16, bg="white", col=color.scale(deltaleaf.lam.fastAnc, extremes=c("blue","red"), xrange=c(14,30)))

