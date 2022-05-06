data("USArrests")

#Question 7
names(USArrests)

dist.data = dist(USArrests)
dist.cor = as.dist(1-cor(t(USArrests)))
par(mfrow=c(1,2))
plot(hclust(dist.data, method = "complete"),xlab = "",main = "Hierarchical Clustering using Euclidean Distance",cex=0.5)
plot(hclust(dist.cor, method = "complete"),xlab = "",main = "Hierarchical Clustering using Correlation",cex=0.5)


# Question 8

pr.out = prcomp(USArrests, scale. = TRUE)
pve.pr = pr.out$sdev**2 / sum(pr.out$sdev**2)


# Question 9

hc.out = hclust(dist.data, method = "complete")
par(mfrow=c(1,1))
plot(hc.out, main = "Complete Linkage", xlab = "",cex = 0.5)
hc.clusters = cutree(hc.out, 3)
hc.clusters

scUSarrests = scale(USArrests)
hc.out.scaled = hclust(dist(scUSarrests), method = "complete")
plot(hc.out.scaled, main = "Complete Linkage", xlab = "",cex = 0.5)
hc.clusters.scaled = cutree(hc.out.scaled, 3)
hc.clusters.scaled

table(hc.clusters, hc.clusters.scaled)

# Question 10

x = matrix(rnorm(60*50),nrow = 60,ncol = 50)
x[1:20,] = x[1:20,] + 5
x[20:40,] = x[20:40,] - 3

pr.out = prcomp(x)
plot(pr.out$x[,"PC1"], pr.out$x[,"PC2"])

km.out = kmeans(x, 3, nstart = 20)
table(km.out$cluster)

km.out = kmeans(x, 2, nstart = 20)
table(km.out$cluster)

km.out = kmeans(x, 4, nstart = 20)
table(km.out$cluster)

km.out = kmeans(pr.out$x[,1:2],3,nstart = 20)
table(km.out$cluster)

km.out = kmeans(scale(x),3,nstart = 50)
table(km.out$cluster)


# Question 11

data = read.csv("../Data/Ch10Ex11.csv", header = FALSE)
dim(data)
names(data)

dist.cor = as.dist(1-cor(t(data)))

hc.out.complete = hclust(dist.cor,method = "complete")
hc.out.average = hclust(dist.cor, method = "average")
hc.out.single = hclust(dist.cor, method = "single")

par(mfrow=c(1,3))
plot(hc.out.complete, main = "Hierarchical Clustering using Complete Linkage",
     xlab = "",cex = 0.5)
plot(hc.out.average, main = "Hierarchical Clustering using Average Linkage",
     xlab = "",cex = 0.5)
plot(hc.out.single, main = "Hierarchical Clustering using Single Linkage",
     xlab = "",cex = 0.5)

hc.cluster = cutree(hc.out.complete,2)
hc.cluster
table(hc.cluster)

row.names(data[hc.cluster==1,])
row.names(data[hc.cluster==2,])
