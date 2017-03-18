##calculate rules given a cluster
#curr

# cluster , support , 
getRules <- function(c,su,co){
  rul <-apriori(c, parameter = list(supp = su, conf = co, target = "rules"))
  summary(rul)
  inspect(head(sort(rul, by="conf"),5))
  rul
}

# rules , scope
measureQuality <- function(rules , scope,metric )
{
  ## calc quality metric based on:
  ##support
  q <- interestMeasure(rules, measure=metric, transactions = scope )
  q

  
  if (FALSE) {
    ##confidence
    quality(rules)$confidence <- 
      interestMeasure(rules, measure="confidence", transactions = scope)
    rules
    ##cros-suportt-ratio,
    quality(freqItemsByCluster[[i]])$crossSupportRatio <- 
      interestMeasure(freqItemsByCluster[[i]], measure="crossSupportRatio", transactions = scope ,reuse=FALSE)
    
  
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



