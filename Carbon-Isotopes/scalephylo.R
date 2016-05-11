#All code is from http://grokbase.com/t/r/r-sig-phylo/1536dgc4gp/node-ages-without-phylocom
#Accessed 3/30/2016


scalePhylo<- function(tr, tip.ages, node.mins=NULL, min.diff=0.1)
  ## tr is a 'phylo' object
  ## tip.ages is a vector of the ages of terminal taxa (best if this vector has names that match the taxa labels)
## tip.ages MUST BE A NAMED VECTOR, that is, e.g., names(tipages) <-tree$tip.label [where tipages is a numeric vector of ## the tip ages]
                ## node.mins is a vector of optional constraints on nodes
                ## min.diff is the minimum branch length that will be imposed
{
  aa<- assign.ages(tr, tip.ages, node.mins, min.diff)
  trs<- assign.brlen(tr, aa)
  return(trs)
}


assign.ages<- function(tr, tip.ages, node.mins, min.diff)
  # Function to assign ages to internal nodes, given tree and
  # ages of terminal modes
  # --tr is phylo (from package ape)
  # --tip.ages is strat ages of terminal taxa
  # --node.mins are (optional) minimum ages for nodes (or a subset of nodes)
{
  ## do some error checking
  # makes sure tips have labels
  # check node labels?
  if(is.null(node.mins)) {node.mins<- rep(NA, times=tr$Nnode)}
  node.mins[is.na(node.mins)]<- -Inf # so nodes with no information have no effect
  #print(node.mins)
  
  
  # change order of ages to match tip labels of tr
  oo<- charmatch(tr$tip.label, names(tip.ages))
  tip.ages<- tip.ages[oo]
  tn<- tr$edge
  max.term<- length(tr$tip.label)
  min.anc<- max.term+1
  max.anc<- min.anc + tr$Nnode -1
  aa<- array(dim=max(tn))
  names(aa)<- as.character(1:max(tn))
  aa[1:max.term]<- tip.ages
  ii<- min.anc:max.anc
  names(node.mins)<- ii # note: this assumes tree nodes not originally labelled!!
    
    
    # go through internal nodes, assign ages to them
    while (sum(is.na(aa))>=1) # loop through as long as ages for some inodes not yet known
{
  #print(aa)
  for (i in ii)
  {
    ci<- as.character(i)
    yy<- tn[,1]==i
    dec<- tn[,2][yy] # direct descendants of i
    aad<- aa[as.character(dec)] # ages of these direct descendants
    #cat(ci, aad, '\n')
    if (sum(is.na(aad))==0) # if all ages are known.
    {
      aa[ci]<- max(aad) + min.diff # assign age as max() of nodes within
      if(aa[ci] < node.mins[ci]) aa[ci]<- node.mins[ci]
    }
  }
}
return (aa)
}


assign.brlen<- function (t1, all.ages)
  # Function to assign branch lengths to all edges of tree
  # --t1 is phylo
  # --all.ages is vector of ages of terminal and internal nodes (as given by assign.ages() )
{
  ne<- nrow(t1$edge)
  bl<- array(dim=ne)
  
  
  for (i in 1:ne)
  {
    anc<- t1$edge[i,1]
    dec<- t1$edge[i,2]
    bl[i]<- all.ages[anc] - all.ages[dec]
  }
  
  
  t2<- t1
  t2$edge.length<- bl
  return(t2)
}


# ####
# AdjBrLens <- function(trees, ages) {
#   require(ape)
#   
#   
#   # read, then write files to where phylocom executable is
#   tree <- read.tree(trees)
#   age <- read.table(ages)
#   write.tree(tree, "phylo")
#   write.table(age, file = "ages", sep = "\t", col.names = F, row.names = F,
#               quote = F)
#   
#   
#   # bladj method
#   #system("./phylocom bladj > phyloout.txt") # for windows users replace
#   #with: shell("phylocom bladj > phyloout.txt")
#   #bladjmethod <- read.tree("phyloout.txt")
#   
#   
#   # Gene Hunt method
#   age_ <- age[,2]
#   names(age_) <- age[,1]
#   
#   
#   # set node ages
#   diff_ <- rep(NA, length(setdiff(tree$node.label, names(age_))))
#   names(diff_) <- setdiff(tree$node.label, names(age_))
#   nodeages <- c(age_, diff_)
#   
#   
#   # tipages are set to zero as all species are extant
#   tipages <- rep(0, length(tree$tip.label))
#   names(tipages) <- tree$tip.label
#   
#   
#   # you can set min.diff to get different results
#   huntmethod <- scalePhylo(tree, tipages, nodeages, min.diff=0)
#   
#   
#   # plot trees and write to file
#   pdf(paste("",trees,"TheOG",".pdf","",sep=""))
#   plot(tree)
#   dev.off()
#   
#   
#   # phynodes <- data.frame(bladjmethod$node.label, #1:length(bladjmethod$
#   node.label))
# #names(phynodes) <- c("V1", "order")
# #newnodes <- merge(phynodes, age, by="V1", all=T)
# #newnodes_sort <- newnodes[order(newnodes[,2]), ]
# #bladjmethod$node.label <- newnodes_sort[, 3]
# 
# 
# pdf(paste("",trees,"bladjmethod",".pdf","",sep=""))
# plot(bladjmethod)
# nodelabels(bladjmethod$node.label, cex = 0.6)
# dev.off()
# 
# 
# pdf(paste("",trees,"genehuntmethod",".pdf","",sep=""))
# plot(huntmethod)
# dev.off()
# 
# 
# # write bladj tree to file
# # bladjmethod$node.label <- NULL
# # write.tree(bladjmethod, paste("",trees,"bladjtree",".txt","",sep=""))
# 
# 
# # combine trees in list
# trees_out <- list(tree, bladjmethod, huntmethod)
# trees_out
# 
# 
# }
