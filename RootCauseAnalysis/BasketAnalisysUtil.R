##BasketAnalysis
library(arules)

    ##given a cluster of transactions ->  ret frequent|maxFreq itemsets
    frqItms <- function(clu,supp,target,other){
       itemSet <- apriori(clu, parameter= list(support=supp, target=target,arem=other))
       itemSet
    }
    
    # given : itemsets, Quality measure,scope 
    ##return quality
    measureItems <- function(itemset,measureType,scope){
      ret <- interestMeasure(itemset, measure=measureType,transactions=scope)
      ret
    }
