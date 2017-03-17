source("clustering.R")
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
summary(c)

##itemFrequencyPlot(c[[2]], population = transactions, support = 0.2)
freqItemsByCluster = list()
for( i in 1:6)
{
  freqItemsByCluster[[i]] <- apriori(c[[i]], parameter= list(support=0.5, target= "frequent itemsets"))
  ##freqItemsByCluster[[i]].sorted <- sort(freqItemsByCluster[[i]], by="support")
  quality(freqItemsByCluster[[i]])$lift <- interestMeasure(freqItemsByCluster[[i]], measure="lift", transactions = c[[i]] ,reuse=FALSE)
  quality(freqItemsByCluster[[i]])$crossSupportRatio <- interestMeasure(freqItemsByCluster[[i]], measure="crossSupportRatio", transactions = c[[i]] ,reuse=FALSE)
  quality(freqItemsByCluster[[i]])$crossSupportRatio <- interestMeasure(freqItemsByCluster[[i]], measure="crossSupportRatio", transactions = c[[i]] ,reuse=FALSE)
}




maxfreqItemsByCluster = list()
for( i in 1:6)
{
  maxfreqItemsByCluster[[i]] <- apriori(c[[i]], parameter= list(support=0.5, target= "maximally frequent itemsets"))
  quality(maxfreqItemsByCluster[[i]])$lift <- interestMeasure(maxfreqItemsByCluster[[i]], measure="lift", transactions = transactions)
}

##get top freq items


rulesByCluster = list()
for(i in 1:6)
{
  rulesByCluster[[i]] <-apriori(c[[i]],parameter = list(supp = 0.5, conf = 0.7, target = "rules"))
  ##rulesByCluster[[i]] <- get rules
  rul <- getRules(c[[i]],0.5,0.7)
  rul <- measureQuality(rul, c[[i]])  ## scope = transactions ??
  rulesByCluster[[i]] <- rul
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