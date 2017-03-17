##calculate rules given a cluster
#curr

# cluster , support , 
getRules <- function(c,su,co){
  rul <-apriori(c, parameter = list(supp = su, conf = co, target = "rules"))
  rul
}

# rules , scope
measureQuality <- function(rules , scope )
{
  ## calc quality metric based on:
  ##support
  quality(rules)$lift <- 
    interestMeasure(rules, measure="lift", transactions = scope ,reuse=FALSE)
  ##confidence
  quality(freqItemsByCluster[[i]])$confidence <- 
    interestMeasure(rules, measure="confidence", transactions = scope ,reuse=FALSE)
  ##cros-suportt-ratio,
  quality(freqItemsByCluster[[i]])$crossSupportRatio <- 
    interestMeasure(freqItemsByCluster[[i]], measure="crossSupportRatio", transactions = scope ,reuse=FALSE)
  
  if (FALSE) {
  
  
  ##cosine,
  quality(freqItemsByCluster[[i]])$cosine <- 
    interestMeasure(freqItemsByCluster[[i]], measure="cosine", transactions = c[[i]] ,reuse=FALSE)
  ##improvement
  quality(freqItemsByCluster[[i]])$improvement <- 
    interestMeasure(freqItemsByCluster[[i]], 
                    measure="improvement",
                    transactions = c[[i]] ,
                    reuse=FALSE)
  ##jaccard
  quality(freqItemsByCluster[[i]])$jaccard <- 
    interestMeasure(freqItemsByCluster[[i]],
                    measure="jaccard", 
                    transactions = c[[i]],
                    reuse=FALSE)
  ##chiSquared
  quality(freqItemsByCluster[[i]])$chiSquared <- 
    interestMeasure(freqItemsByCluster[[i]],
                    measure="chiSquared", 
                    transactions = c[[i]],
                    reuse=FALSE)
  }
  
}



