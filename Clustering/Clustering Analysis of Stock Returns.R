# read data
data = read.csv("returns.csv")
returns = data[,3:122]

# Companies in Industry Sectors
library(dplyr)
data_byIndustry = data %>% group_by(Industry)
industry_size = data_byIndustry %>% summarise(CompanyNum=n())
as.data.frame(industry_size)

# plot average returns
data4plot = data[,c(2,25:60)]
avg_returns = aggregate(data4plot[,-1], by=list(Industry=data4plot$Industry), mean)
date = colnames(avg_returns[,-1])
date = sub('avg', '', date)

par(mfrow=c(5,2))
for (i in 1: dim(avg_returns)[1]){
  indus = avg_returns$Industry[i]
  indus_return = t(avg_returns[i, -1])
  plot(indus_return, type='l', xaxt='n', xlab='Month', ylab='Average Return', main=indus)
  axis(1, at=1:36, labels=as.factor(date))
}

# Hierarchical Clustering of the Average Returns
library(flexclust)
d = dist(returns)
hclust.mod = hclust(d, method="ward.D2")
par(mar=c(0.5,2.5,1,0.5))
plot(hclust.mod, labels=F, ylab="Dissimilarity", xlab = "", sub = "", main="")

# the scree plot
hc.dissim = data.frame(k=seq_along(hclust.mod$height),dissimilarity=rev(hclust.mod$height))
par(mar=c(4, 4, 1, 0.5))
plot(hc.dissim$k,hc.dissim$dissimilarity,type="l",xlim=c(0,20),ylim=c(2,6.5),
     xlab="k",ylab="Dissimilarity")
axis(side=1, at=1:10)

# Extract Cluster Assignments from the Hierarchical Clustering
## analyze clusters
h.clusters = cutree(hclust.mod, 8) # we choose k=8
data = cbind(cluster=h.clusters, data)
industry_counts = data.frame(Industry=sort(unique(data$Industry)))
for (k in 1:8){
  cluster_k = data[data$cluster == k, 1:3]
  indus_count = cluster_k %>% group_by(Industry) %>% summarise(CompanyNum = n())
  colnames(indus_count) = c("Industry", paste0("Cluster", k))
  industry_counts = merge(industry_counts, indus_count, all=TRUE)
}
industry_counts[is.na(industry_counts)] = 0
industry_counts

## calculate average returns
aggregate(returns, by=list(h.clusters), mean) %>% 
  select(Group.1, avg200810, avg200903)

## plot cluster returns
cluster_ret = aggregate(returns, by=list(h.clusters), mean) %>% select(-Group.1)

par(mfrow=c(4,2))
for (i in 1:8){
  main = paste("Hierarchical Cluster", i)
  plot(t(cluster_ret[i,]),type='l',xaxt='n',xlab='Month',ylab='Average Return',main=main)
  axis(1, at=seq(1, 120, 12), labels=paste0(2006:2015, "03"))
}

# K-Means Clustering of the Average Returns
set.seed(144)
km = kmeans(returns, centers = 8, iter.max=100)
km.clusters = km$cluster

# compute the number of companies in each cluster from each industry sector
data = cbind(cluster_km=km.clusters, data)
industry_counts = data.frame(Industry=sort(unique(data$Industry)))
for (k in 1:8){
  cluster_k = data[data$cluster_km == k, 1:4]
  indus_count = cluster_k %>% group_by(Industry) %>% summarise(CompanyNum = n())
  colnames(indus_count) = c("Industry", paste0("Cluster", k))
  industry_counts = merge(industry_counts, indus_count, all=TRUE)
}
industry_counts[is.na(industry_counts)] = 0
industry_counts

## sum of the number of companies in each cluster
apply(industry_counts[,-1], 2, sum)

## plot KCluster returns
cluster_ret = aggregate(returns, by=list(km.clusters), mean) %>% select(-Group.1)

par(mfrow=c(4,2))
for (i in 1:8){
  main = paste("K-Means Cluster", i)
  plot(t(cluster_ret[i,]),type='l',xaxt='n',xlab='Month',ylab='Average Return',main=main)
  axis(1, at=seq(1, 120, 12), labels=paste0(2006:2015, "03"))
}


