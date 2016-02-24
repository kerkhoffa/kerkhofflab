#This script enumerates analyses of Kerkhoff, et al. (2014, PNAS)
#Goal is to analyze data for >12K species of woody angiosperms in the New World
#to test predictions of the tropical conservatism hypothesis.

#Tree construction was accomplished using phylomatic (xterm commands):
#$ phylomatic -f R20120829.nwk -t Taxa2.txt > taxaPhylo.nwk
#The resulting file was edited to remove the euphylophyte root label and the leading parenthesis which was redundant
#Pseudo branch lengths cleaned out using phylocom cleanphy:
#$ phylocom cleanphy -f taxaPhylo.nwk -e > taxaPhyloClean.nwk
# using ages=node.ages.bell.txt and phylo=taxaPhyloClean.nwk, I then use bladj to add branch lengths
#$ phylocom bladj > taxaPhylo.bladj.nwk

#The tree file was then read into R

salvias.tree<-read.tree("taxaPhylo.bladj.nwk")

#Read in the distributional data for all taxa
salvias.data<-read.csv("TCHDataR3.csv")

#Now look at phylogenetic diversity with latitude using all taxa not families
taxa.latbands.mat<-matrix(0,nrow=24, ncol=length(salvias.data$taxon), dimnames=list(seq(-47.5,67.5,by=5),salvias.data$taxon))
midp<-seq(-47.5,67.5,by=5)
for (x in 1:24) taxa.latbands.mat[x,]<-(salvias.data$limit.n>midp[x]-2.5) & (salvias.data$limit.s<midp[x]+2.5)

#Now look at simple pd as a function of midpoint
taxa.latbands.pd<-pd(taxa.latbands.mat, salvias.tree)
taxa.latbands.ses.pd<-ses.pd(taxa.latbands.mat, salvias.tree, null.model="taxa.labels")

#try test for phylogenetic signal of mean tropicality for all taxa
trop.taxa<-salvias.data$tropicality
names(trop.taxa)<-salvias.data$taxon
#put taxon tropicality values in order of tips on the phylogeny
trop.taxa.tip.order<-trop.taxa[salvias.tree$tip.label]
trop.taxa.sig<-phylosig(salvias.tree, trop.taxa.tip.order, method="lambda", nsim=1000, test=T)

#to estimate ancestral characters on such a large phylogeny, R routines were quite slow
#so we initially used phylocom aot, which was quite speedy

#phylocom aotf -f taxaPhylo.bladj.nwk -t taxa_dist.txt > TaxaDistAOTResults.txt

#the traits file contained limit.s, limit.n, lat.mid, and tropicality. I focus on the latter
#but included the former because aot only calculates contrasts on multiple characters. It also works
#to call phylocom from within R, but you have to give it the path

system("/Users/kerkhoffa/bin/phylocom aotf -f taxaPhylo.bladj.nwk -t taxa_dist.txt > TaxaDistAOTResults.txt")

#Either way, I then edited the results file to separate it into five - one csv file containing the 
#trait estimeates for that trait for each node, and one contrasts file which has all of the contrasts for each node.
#NOTE: Conveniently, the node order appears to be the same between aot output and the way R treats the trees

#So now load up the estimated tropicality states for all nodes

tropicality.aot<-read.csv("TaxaTropicalityAOT.csv")

#assess significance of each estimate based on two tailed test of aot randomizations (2-tailed, alpha=0.05)
tropicality.aot$Node.mn.sig<-pmin(tropicality.aot$Nmn.rankLow, tropicality.aot$Nmn.rankHi)/1000<=0.025
tropicality.aot$Node.mn.trop.sig<-tropicality.aot$Nmn.rankHi/1000<=0.025
tropicality.aot$Node.mn.temp.sig<-tropicality.aot$Nmn.rankLow/1000<=0.025

#Found a way to make it work with fastAnc (but not very Fast)
#First make OU and BM and lambda and white noise models and compare models
#need a dichotomous tree for fitContinuous
salvias.di.tree<-multi2di(salvias.tree, random=F)
trop.fit.ou<-fitContinuous(salvias.di.tree, trop.taxa.tip.order, model="OU")
trop.fit.lam<-fitContinuous(salvias.di.tree, trop.taxa.tip.order, model="lambda")
trop.fit.bm<-fitContinuous(salvias.di.tree, trop.taxa.tip.order, model="BM")
trop.fit.white<-fitContinuous(salvias.di.tree,trop.taxa.tip.order,model="white")
#Then you extract the AIC values and find that the lambda model fits the best
#In this case the lambda model fits best

#So then to make the ancestral estimates you can transform the tree based on the OU and lambda models, as well as with the original tree for the BM model. First make a dichotomous (binary) tree. Random=F ensures that you can put the nodes back together in a meaningful way to match it to the multichotomous tree
salvias.ou.tree<-ouTree(salvias.di.tree, 0.03632981)
salvias.lam.tree<-lambdaTree(salvias.di.tree, 0.7566741)
#Then you can use fastAnc to reconstruct the ancestral states using ML, but first make the tree multichotomous again
salvias.ou.multi.tree<-di2multi(salvias.ou.tree)
salvias.di.multi.tree<-di2multi(salvias.di.tree) #as a check
salvias.lam.multi.tree<-di2multi(salvias.lam.tree)
salvias.trop.ou.fastAnc<-fastAnc(salvias.ou.multi.tree, trop.taxa.tip.order)
salvias.trop.fastAnc<-fastAnc(salvias.tree, trop.taxa.tip.order)
salvias.trop.lam.fastAnc<-fastAnc(salvias.lam.multi.tree, trop.taxa.tip.order)

#Compare tropicality of each node to the tropicality of its parent node (looking at "Out of the Tropics")
#First, realize that all ancestor-descendant relationships are in salvias.tree$edge
#and get all of the ancestral node tropicality values
salvias.ancestor.tropicality<-salvias.trop.lam.fastAnc[salvias.tree$edge[,1]-12521]
#put taxon tropicality values in order of tips on the phylogeny
trop.taxa.tip.order<-trop.taxa[salvias.tree$tip.label]
salvias.descendent.tropicality<-1:length(salvias.tree$edge[,2])
salvias.descendent.tropicality[salvias.tree$edge[, 2]<12522]<-trop.taxa.tip.order
salvias.descendent.tropicality[salvias.tree$edge[, 2]>12521]<-salvias.trop.lam.fastAnc[salvias.tree$edge[salvias.tree$edge[,2]>12521,2]-12521]
tropicality.edge.transitions<-salvias.descendent.tropicality-salvias.ancestor.tropicality

#plot histogram of transition values for tropical (TI>0) and temperate (TI<0) nodes
hist(tropicality.edge.transitions[salvias.ancestor.tropicality>0],border="red",freq=F, xlim=c(-2,2), main=" ", xlab="Change in Tropicality (descendent - ancestor)")
hist(tropicality.edge.transitions[salvias.ancestor.tropicality<0],border="blue", freq=F,add=T)
#make a table classifying transitions by four levels of tropicality
salvias.tropicality.transition.table<-hist2d(salvias.ancestor.tropicality,salvias.descendent.tropicality,nbins=4,same.scale=T, show=F)

#compare observed transitions to null hypothesis using chi squared test
transition.chisq<-chisq.test(salvias.tropicality.transition.table$counts)

#look at table as proportion of ancestors
salvias.tropicality.transition.propAnc<-salvias.tropicality.transition.table$counts/rowSums(salvias.tropicality.transition.table$counts)
#and as proportion of descendents
salvias.tropicality.transition.propDec<-t(salvias.tropicality.transition.table$counts)/colSums(salvias.tropicality.transition.table$counts)

#Histograms of tropicality, with a split y axis
layout(matrix(c(2,1),2,1,byrow=T))
par(mar=c(5,4,0,2)+0.1)
hist(salvias.data$tropicality, ylim=c(0,500),main="",xlab="Tropicality index", col="gray")
par(mar=c(0.3,4,2,2)+0.1)
hist(salvias.data$tropicality, ylim=c(1000,10000),main="",xlab="",xaxt="n", ylab="",yaxp=c(1000,11000,5), col="gray")
par(mfrow=c(1,1))

#Histogram of tropicality for all of the nodes
hist(salvias.trop.lam.fastAnc, breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1), col="grey", main="",xlab="Tropicality index (Node estimate)")
hist(salvias.trop.lam.fastAnc[tropicality.aot$Node.mn.temp.sig>0], breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),col="blue", add=T)
hist(salvias.trop.lam.fastAnc[tropicality.aot$Node.mn.trop.sig>0], breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),col="red", add=T)



#plot 2 panel figure with richness on top and ses pd on bottom
layout(matrix(c(2,1),2,1,byrow=T))
par(mar=c(5,4.2,0,2)+0.1)
par(cex.lab=0.9)
plot(taxa.latbands.richness~midp, xlab="Latitudinal band",  ylab="Species richness", col="black", bg="black")
lines(c(0,0), c(-200,7100), lty="dashed")
lines(c(23.43,23.43), c(-200,7100), lty="dotted")
lines(c(-23.43,-23.43), c(-200,7100), lty="dotted")
par(mar=c(0.3,4.2,2,2)+0.1)
plot(taxa.latbands.ses.pd$pd.obs.z~midp,pch=23, col="red", bg=(taxa.latbands.ses.pd$pd.obs.p<=0.025 | taxa.latbands.ses.pd$pd.obs.p>=0.975)*2, xlab="", xaxt="n", ylab=expression("Phylogenetic diversity"~(P*D[z])))
lines(c(0,0),c(-18,8),lty="dashed")
lines(c(23.43,23.43), c(-18,8), lty="dotted")
lines(c(-23.43,-23.43), c(-18,8), lty="dotted")
abline(0,0,lty="dashed")
par(mfrow=c(1,1))




#Make epoch symbols for plots
epoch.syms=expression("K"["low"], "K"["up"],"P"[epsilon],"E"["O"],"O"["G"],"M"["I"],"P"["O"])
epoch.bounds=c(145.5,99.6,65.5,55.8,33.9,23.03,5.33,2.59)
epoch.spans=c(145.5-99.6,99.6-65.5,65.5-55.8,55.8-33.9,33.9-23.03,23.03-5.33,5.33-2.59)
epoch.mids<-(cumsum(c(145.5,99.6,65.5,55.8,33.9,23.03,5.33,2.59))[2:8]-c(0,cumsum(c(145.5, 99.6,65.5,55.8,33.9,23.03,5.33,2.59))[1:6]))/2
epoch.ats<--(epoch.mids-epoch.mids[1])/40.000

salvias.ages<-branching.times(salvias.tree)
#plot tropicality as a function of node age, with color as salvias.trop.lam.fastAnc node values
plot(salvias.trop.lam.fastAnc~salvias.ages, type="n", xlim=c(150,0), ylim=c(-1,1.2), xlab="Lineage age (My)", ylab="Estimated tropicality index")
rect(c(150, 140.5,137.5,125, 97.5, 91, 71.6, 67.5, 66.5, 42, 39, 36.5), rep(-1.08,12), c(144, 139.5, 136.5, 112, 96.5, 89, 69.6, 66.5, 65.5, 41, 38, 36), rep(1.28, 12),col="light gray",border=NA)
rect(34,-1.08,0.001,1.28,col="gray",border=NA)
points(salvias.trop.lam.fastAnc~salvias.ages, pch=21, bg="white", col=color.scale(salvias.trop.lam.fastAnc, extremes=c("blue","red"), xrange=c(-1,1)))
abline(0,0,lty="dashed")
arrows(x0=epoch.bounds, y0=1.1,y1=1.3,length=0)
abline(1.1,0)
axis(side=3, at=epoch.mids,labels=epoch.syms)
color.legend(xl=-1,yb=-1,xr=-5, yt=1, rect.col=color.scale(seq(-1,1,0.1),extremes=c("blue","red")), gradient="y")




#Make big wheel tree with branches colored by the tropicality of the ancestor

trop.edge.colors<-color.scale(salvias.ancestor.tropicality, extremes=c("blue","red"), xrange=c(-1,1))
plot(salvias.tree, type="fan", show.tip.label=F, edge.color=trop.edge.colors, no.margin=T)
add.scale.bar()



###############################################
#For comparison, make randomized tree, then repeat analyses
trop.taxa.randomized<-trop.taxa.tip.order
names(trop.taxa.randomized)<-sample(names(trop.taxa.tip.order),length(trop.taxa.tip.order))
salvias.trop.lam.rand.fastAnc<-fastAnc(salvias.lam.multi.tree, trop.taxa.randomized)

#plot tropicality as a function of node age, with color as salvias.trop.lam.fastAnc node values
plot(salvias.trop.lam.rand.fastAnc~salvias.ages, type="n", xlim=c(150,0), ylim=c(-1,1.2), xlab="Lineage age (My)", ylab="Estimated tropicality index")
rect(c(150, 140.5,137.5,125, 97.5, 91, 71.6, 67.5, 66.5, 42, 39, 36.5), rep(-1.08,12), c(144, 139.5, 136.5, 112, 96.5, 89, 69.6, 66.5, 65.5, 41, 38, 36), rep(1.28, 12),col="light gray",border=NA)
rect(34,-1.08,0.001,1.28,col="gray",border=NA)
points(salvias.trop.lam.rand.fastAnc~salvias.ages, pch=21, bg="white", col=color.scale(salvias.trop.lam.rand.fastAnc, extremes=c("blue","red"), xrange=c(-1,1)))
abline(0,0,lty="dashed")
arrows(x0=epoch.bounds, y0=1.1,y1=1.3,length=0)
abline(1.1,0)
axis(side=3, at=epoch.mids,labels=epoch.syms)

#Compare tropicality of each node to the tropicality of its parent node (looking at "Out of the Tropics")
#First, realize that all ancestor-descendant relationships are in salvias.tree$edge
#and get all of the ancestral node tropicality values
rand.ancestor.tropicality<-salvias.trop.lam.rand.fastAnc[salvias.tree$edge[,1]-12521]
#put taxon tropicality values in order of tips on the phylogeny
rand.descendent.tropicality<-1:length(salvias.tree$edge[,2])
rand.descendent.tropicality[salvias.tree$edge[, 2]<12522]<-trop.taxa.randomized
rand.descendent.tropicality[salvias.tree$edge[, 2]>12521]<-salvias.trop.lam.rand.fastAnc[salvias.tree$edge[salvias.tree$edge[,2]>12521,2]-12521]
rand.edge.transitions<-rand.descendent.tropicality-rand.ancestor.tropicality

#make a table classifying transitions by four levels of tropicality
rand.tropicality.transition.table<-hist2d(rand.ancestor.tropicality,rand.descendent.tropicality,nbins=4,same.scale=T, show=F)

#compare observed transitions to null hypothesis using chi squared test
rand.transition.chisq<-chisq.test(rand.tropicality.transition.table$counts)

#look at table as proportion of ancestors
rand.tropicality.transition.propAnc<-rand.tropicality.transition.table$counts/rowSums(rand.tropicality.transition.table$counts)
#and as proportion of descendents
rand.tropicality.transition.propDec<-t(rand.tropicality.transition.table$counts)/colSums(rand.tropicality.transition.table$counts)

#Make big wheel tree with branches colored by the tropicality of the ancestor

trop.rand.edge.colors<-color.scale(rand.ancestor.tropicality, extremes=c("blue","red"), xrange=c(-1,1))
plot(salvias.tree, type="fan", show.tip.label=F, edge.color=trop.rand.edge.colors)
add.scale.bar()


###
