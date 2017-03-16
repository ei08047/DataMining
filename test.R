if(!exists("init", mode="function")) source("load.R")
if(!exists("dataFrameToTransaction", mode="function")) source("loadData.R")
if(!exists("calcDissimilarity", mode="function")) source("clustering.R")
init()
raw <- readDataFrame()
transactions <- dataFrameToTransaction(raw)
diss <- calcDissimilarity(transactions)
cv <- getClusteringVectors(diss)
## compare original and processed df
clu <- hCluster(diss)
##plot hClust

allLabels <- predict(transactions[cv$medoids], transactions,method = "Jaccard")
##clusplot(cv, main = "Cluster plot, k = 6", color = TRUE)
c <- split(transactions, allLabels)

##itemFrequencyPlot(c[[2]], population = transactions, support = 0.2)
freqItemsByCluster = list()
for( i in 1:6)
{
  freqItemsByCluster[[i]] <- apriori(c[[i]], parameter= list(support=0.5, target= "frequent itemsets"))
  ##freqItemsByCluster[[i]].sorted <- sort(freqItemsByCluster[[i]], by="support")
  quality(freqItemsByCluster[[i]])$lift <- interestMeasure(freqItemsByCluster[[i]], measure="lift", transactions = transactions)
}

rulesByCluster = list()
for(i in 1:6)
{
  rulesByCluster[[i]] <-apriori(c[[i]],parameter = list(supp = 0.5, conf = 0.7, target = "rules"))
  quality(rulesByCluster[[i]])$chiSquared <- interestMeasure(rulesByCluster[[i]], measure="chiSquared", transactions = transactions)
}
##rulesC6.sub_serious  <- subset(rulesC6, (rhs %in% c("Accident_Severity=Serious"))  )


##quality(rulesC6)$chiSquared <- interestMeasure(rulesC6, measure="chiSquared", transactions = transactions)
##summary(rulesC6)
##inspect(head(sort(rulesC6, by = "chiSquared"), n=10))


##plot(head(sort(rulesC6, by="chiSquared"), 5),method="graph", control=list(cex=.7))

##rulesC6.sub_serious.sorted <- sort(rulesC6.sub_serious, by="support")
##quality(rulesC6.sub_serious)$lift <- interestMeasure(rulesC6.sub_serious, measure="lift", transactions = transactions)