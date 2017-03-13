if(!exists("init", mode="function")) source("load.R")
init()

if(!exists("dataFrameToTransaction", mode="function")) source("loadData.R")
transactions <- dataFrameToTransaction()

if(!exists("calcDissimilarity", mode="function")) source("clustering.R")
diss <- calcDissimilarity(transactions)
cv <- getClusteringVectors(diss)
##s <- silhouette(transactions,PamObj)
clu <- hCluster(diss)
plot(clu)

clusplot(cv, main = "Cluster plot, k = 6", 
         color = TRUE)
