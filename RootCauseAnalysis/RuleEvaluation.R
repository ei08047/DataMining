##calculate rules given a cluster
#curr

# given : cluster , support , confidence
# return : rules
getRules <- function(c,su,co,min,max){
  rul <-apriori(c, parameter = list(supp=su, conf=co, target="rules", minlen=min, maxlen=max))
  summary(rul)
  inspect(head(sort(rul, by="conf"),5))
  rul
}

# given : rules , scope , quality metric
# metric params : support, confidence,cros-suportt-ratio, cosine,improvement,jaccard,chiSquared
# return quality
measureQuality <- function(rules , scope,metric )
{
  q <- interestMeasure(rules, measure=metric, transactions = scope )
  q
}



