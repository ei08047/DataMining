getSample <- function(transactions,numSamples){
  set.seed(1234)
  s <- sample(transactions, numSamples)
  s
}

calcDissimilarity <- function(transactions)
{
## get sample and calc dissimilarity
s <- getSample(transactions,5000)
d <- dissimilarity(s, method = "cosine")
d
}

getClusteringVectors <- function(diss){
  cv <- pam(diss, k = 6)
  cv
}

hCluster <- function(dissimilarity,transactions)
{
  hcl <- hclust(dissimilarity)
  hcl
}


