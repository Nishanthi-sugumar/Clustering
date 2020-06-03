crimedata<-read.csv(file.choose())
data(crimedata)
View(crimedata)
plot(crimedata)
mydata<-crimedata[1:50,c(1,2:5)]
View(mydata)

normalized_crimedata<-scale(mydata[,2:5])
View(normalized_crimedata)
wss = (nrow(normalized_crimedata)-1)*sum(apply(normalized_crimedata, 2, var))# Determine number of clusters by scree-plot
for (i in 2:5) wss[i] = sum(kmeans(normalized_crimedata, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

#model building
fit <- kmeans(normalized_crimedata, 3) # 5 cluster solution
fit$cluster
final2<- data.frame(mydata, fit$cluster) # append cluster membership
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit.cluster"))
View(final2)
aggregate(mydata[,2:5], by=list(fit$cluster), FUN=mean)
fit$size
str(fit$cluster)
