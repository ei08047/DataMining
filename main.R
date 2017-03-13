

if(!exists("init", mode="function")) source("load.R")
init()
allTransactions <- DataFrameToTransaction()
print(class(allTransactions))







## get sample and calc dissimilarity
set.seed(1234)
s <- sample(trans, 20000)
d <- dissimilarity(s, method = "cosine")
clustering <- pam(d, k = 2)
plot(clustering)

slightSeverity <- subset(trans,items %in% "Accident_Severity=Slight")
seriousSeverity <- subset(trans,items %in% "Accident_Severity=Serious" | items %in% "Accident_Severity=Fatal")
seriousSeverity.freqItems <- apriori(seriousSeverity, parameter= list(support=0.4, target= "frequent itemsets"))
quality(seriousSeverity.freqItems)$lift <- interestMeasure(seriousSeverity.freqItems, measure="lift", trans = trans)

slightSeverity.OneCasuality <- subset(slightSeverity,items %in% "Number_of_Casualties=1" & items %in% "Accident_Severity=Slight")


itemFrequencyPlot(slightSeverity, topN=25,population=trans,  cex.names=.5)
itemFrequencyPlot(seriousSeverity, topN=25,population=trans,  cex.names=.5)


frequentItems <- apriori(trans, parameter= list(support=0.4, target= "frequent itemsets"))
frequentItems.sorted <- sort(frequentItems, by="support")
quality(frequentItems)$lift <- interestMeasure(frequentItems, measure="lift", trans = trans)
##inspect(head(sort(frequentItems, by = "lift"), n=10))


rules.sub_slight = apriori(slightSeverity,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
rules.sub_seriousSeverity = apriori(seriousSeverity,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))










### ti+ps+
## combine rules rComb <- c(r1, r2)
## union of rules rUnion <- union(r1,r2)


data("Groceries")
ct <- crossTable(Groceries, sort=TRUE)
ct[1:5, 1:5]
sp <- crossTable(Groceries, measure="support", sort=TRUE)
sp[1:5,1:5]
lift <- crossTable(Groceries, measure="lift", sort=TRUE)
lift[1:5,1:5]
chi2 <- crossTable(Groceries, measure="chiSquared", sort=TRUE)
chi2[1:5,1:5]

###