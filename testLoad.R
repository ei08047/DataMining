
diss <- readDissFromFile()
cv <- getClusteringVectors(diss)
## compare original and processed df
clu <- hCluster(diss)
summary(clu)


