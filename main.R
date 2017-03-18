source("clustering.R")
source("BasketAnalisysUtil.R")
source("RuleEvaluation.R")
if(!exists("init", mode="function")){source("init.R")}
if(!exists("raw", mode="object")){raw <- source("loadData.R")}
if(!exists("transactions", mode="object")){transactions <- dataFrameToTransaction(raw)}
if(!exists("diss", mode="object")){diss <- readDissFromFile()}

clusteringVectors <- getClusteringVectors(diss)

clu <- hCluster(diss)
##plot hClust




allLabels <- predict(transactions[clusteringVectors$medoids], transactions,method = "Jaccard")
##clusplot(cv, main = "Cluster plot, k = 6", color = TRUE)
c <- split(transactions, allLabels)


silhouette <- silhouette(clusteringVectors$clustering,diss)


summary(c)

##itemFrequencyPlot(c[[2]], population = transactions, support = 0.2)
freqItemsByCluster = list()
for( i in 1:6)
{
  freqItemsByCluster[[i]] <- frqItms(c[[i]],0.5,"frequent itemsets","info")
  quality(freqItemsByCluster[[i]])$lift <- measureItems(freqItemsByCluster[[i]],"support",transactions)
  inspect(head(freqItemsByCluster[[i]],10,by="support"))
}


maxfreqItemsByCluster = list()
for( i in 1:6)
{
  maxfreqItemsByCluster[[i]] <- frqItms(c[[i]],0.5,"maximally frequent itemsets","info")
  quality(maxfreqItemsByCluster[[i]])$lift <- measureItems(maxfreqItemsByCluster[[i]],"lift",transactions)
}

##get top freq items

rulesByCluster = list()
for(i in 1:6)
{
  rulesByCluster[[i]] <- getRules(c[[i]],0.05,0.5)
  quality(rulesByCluster[[i]])$lift <- measureQuality(rulesByCluster[[i]], c[[i]],"lift")  
}




### ti+ps+
## combine rules rComb <- c(r1, r2)
## union of rules rUnion <- union(r1,r2)


data("Groceries")
ct <- crossTable(Groceries, sort=TRUE)
ct[1:5, 1:5]
sp <- crossTable(Groceries, measure="support", sort=TRUE)
sp[1:5,1:5]
lift <- crossTable(Groceries, measure="lift", sort=TRUE)
lift[1:5,1:5]
chi2 <- crossTable(Groceries, measure="chiSquared", sort=TRUE)
chi2[1:5,1:5]

###