ewa<-read_xlsx(file.choose(),sheet=2)
data(ewa)
View(ewa)
str(ewa)
plot(ewa)
mydata<-ewa[1:3999,c(1,2:12)]
View(mydata)
normalized_data<-scale(mydata[,2:12])
View(normalized_data)
wss=(nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))
for(i in 2:12) wss[i]=sum(kmeans(normalized_data,centers = i)$withinss)
plot(1:12,wss,type = "b",xlab = "Number of clusters",ylab="within groups sum of squares")
title(sub = "k_Means clustering of scree plot ")
fit<-kmeans(normalized_data,3)
fit$cluster
final2<-data.frame(mydata,fit$cluster)
View(final2)
setcolorder<-final2,neworder=c["fit.cluster"]
View(final2)
aggregate(mydata[,2:12],by=list(fit$cluster),FUN=mean)
table(fit$cluster)

