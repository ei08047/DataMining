##BasketAnalisysUtil
library(arules)
    ##curr
    ##given a cluster of transactions ->  ret frequent and maxFreq itemsets
    frqItms <- function(clu,supp,target,other){
       itemSet <- apriori(clu, parameter= list(support=supp, target=target,arem=other))
       itemSet
    }
    ##freqItemsByCluster[[i]].sorted <- sort(freqItemsByCluster[[i]], by="support")
    
    
    # given itemsets measure quality
    ##quality
    measureItems <- function(itemset,measureType,scope){
      ret <- interestMeasure(itemset, measure=measureType,transactions=scope)##,transactions=transactions,reuse=FALSE
      ret
    }
