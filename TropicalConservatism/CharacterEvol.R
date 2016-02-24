#Drew Kerkhoff
#February 3, 2015
#Code for fitting alternative evolution models to trait data given a phylogeny, then reconstructing the character states. This example uses data from Kerkhoff et al. 2014

#Load the data
salvias.tree<-read.tree("taxaPhylo.bladj.nwk")
salvias.data<-read.csv("TCHDataR3.csv")
trop.taxa<-salvias.data$tropicality
names(trop.taxa)<-salvias.data$taxon
trop.taxa.tip.order<-trop.taxa[salvias.tree$tip.label] #put taxon tropicality values in order of tips on the phylogeny
require(geiger)
#First make OU and BM and lambda and white noise models and compare models
#need a dichotomous tree for fitContinuous
salvias.di.tree<-multi2di(salvias.tree, random=F)
trop.fit.ou<-fitContinuous(salvias.di.tree, trop.taxa.tip.order, model="OU")
trop.fit.lam<-fitContinuous(salvias.di.tree, trop.taxa.tip.order, model="lambda")
trop.fit.bm<-fitContinuous(salvias.di.tree, trop.taxa.tip.order, model="BM")
trop.fit.white<-fitContinuous(salvias.di.tree,trop.taxa.tip.order,model="white")
#Then you extract the log likelihoods and AIC values and find that the lambda model fits the best
trop.fits.aic = c(trop.fit.ou$opt$aicc, trop.fit.lam$opt$aicc, trop.fit.bm$opt$aicc, trop.fit.white$opt$aicc)
trop.fits.lnL = c(trop.fit.ou$opt$lnL, trop.fit.lam$opt$lnL, trop.fit.bm$opt$lnL, trop.fit.white$opt$lnL)
names(trop.fits.aic)<-names(trop.fits.lnL)<-c("OU","lambda","BM","white")
trop.delta.aic = trop.fits.aic-min(trop.fits.aic) # model with lowest aic "fits best" delta aic is the difference from the best fit model, delta aic <4 indicates similar fits. In this case the lambda model clearly fits best

#So then to make the ancestral estimates you can transform the tree based on the OU and lambda models, as well as with the original tree for the BM model. First make a dichotomous (binary) tree. Random=F ensures that you can put the nodes back together in a meaningful way to match it to the multichotomous tree
salvias.ou.tree<-ouTree(salvias.di.tree, trop.fit.ou$alpha) # alpha is the OU parameter
salvias.lam.tree<-lambdaTree(salvias.di.tree, trop.fit.lam$lambda) # lambda is the random walk parameter
#Then you can use fastAnc to reconstruct the ancestral states using ML. Here I reconstruct states based on OU, BM, and Lambda models.
#but first make the tree multichotomous again.

salvias.ou.multi.tree<-di2multi(salvias.ou.tree)
salvias.di.multi.tree<-di2multi(salvias.di.tree) #as a check
salvias.lam.multi.tree<-di2multi(salvias.lam.tree)

#Now do reconstruction
salvias.trop.ou.fastAnc<-fastAnc(salvias.ou.multi.tree, trop.taxa.tip.order)
salvias.trop.fastAnc<-fastAnc(salvias.tree, trop.taxa.tip.order) #BM model uses untransformed tree
salvias.trop.lam.fastAnc<-fastAnc(salvias.lam.multi.tree, trop.taxa.tip.order)

#Each of the .fastAnc files has a trait estimate associated with each node in the phylogeny which you can use to examine how the trait estimates change over evolutionary time, for example

salvias.ages<-branching.times(salvias.tree)
plot(salvias.trop.lam.fastAnc~salvias.ages, xlim=c(150,0), ylim=c(-1,1.2), xlab="Lineage age (My)", ylab="Estimated tropicality index")
 