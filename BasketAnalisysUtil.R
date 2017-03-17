##BasketAnalisysUtil
    ##curr
    ##given a cluster of transactions ->  ret frequent and maxFreq itemsets
    ##calc frequent
    freqItemsByCluster[[i]] <- apriori(c[[i]], parameter= list(support=0.5, target= "frequent itemsets",arem="info"))
    ##freqItemsByCluster[[i]].sorted <- sort(freqItemsByCluster[[i]], by="support")
    ##calc max frequent
