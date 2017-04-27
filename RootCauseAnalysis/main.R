source("init.R")
source("clustering.R")
source("BasketAnalisysUtil.R")
source("RuleEvaluation.R")
source("loadData.R")

raw <- readDataFrame("Accidents_2015.csv")
transactions <- dataFrameToTransaction(raw)

if(!exists("diss", mode="object")){diss <- readDissFromFile()}


clusteringVectors <- getClusteringVectors(diss)
clu <- hCluster(diss)
silhouette <- silhouette(clusteringVectors$clustering,diss)

allLabels <- predict(transactions[clusteringVectors$medoids], transactions,method = "Jaccard")
c <- split(transactions, allLabels)



freqItemsByCluster = list()
for( i in 1:6)
{
  freqItemsByCluster[[i]] <- frqItms(c[[i]],0.5,"frequent itemsets","info")
  quality(freqItemsByCluster[[i]])$lift <- measureItems(freqItemsByCluster[[i]],"lift",transactions)
  quality(freqItemsByCluster[[i]])$support <- measureItems(freqItemsByCluster[[i]],"support",transactions)
}


maxfreqItemsByCluster = list()
for( i in 1:6)
{
  maxfreqItemsByCluster[[i]] <- frqItms(c[[i]],0.5,"maximally frequent itemsets","info")
  quality(maxfreqItemsByCluster[[i]])$support <- measureItems(maxfreqItemsByCluster[[i]],"support",transactions)
  quality(maxfreqItemsByCluster[[i]])$lift <- measureItems(maxfreqItemsByCluster[[i]],"lift",transactions)
}

rulesByCluster = list()
for(i in 1:6)
{
  rulesByCluster[[i]] <- getRules(c[[i]],0.05,0.5,2,8)
  quality(rulesByCluster[[i]])$lift <- measureQuality(rulesByCluster[[i]], c[[i]],"lift")
  quality(rulesByCluster[[i]])$improvement <- measureQuality(rulesByCluster[[i]], c[[i]],"improvement")  
}


save.image("session.RData")

### tips
## combine rules rComb <- c(r1, r2)
## union of rules rUnion <- union(r1,r2)
###