library(tidyverse)  # df manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualizationhc2 <- agnes(df, method = "complete")

folder = "/Users/xfang7/Google\ Drive/Courses/CSC591-004/hw/Clustering/"
df = read.csv(paste(folder,"xfang7.csv", sep=''),header = FALSE)
df = scale(df)

df.dis = dist(df, method = "euclidean")
hc.complete = hclust(df.dis,method = "complete")
#hc.ward = hclust(df.dis,method = "ward.D2")
plot(hc.complete,cex=0.6,hang= -1)

df.sub = df[1:50,]
df.sub.dis = dist(df.sub,method = "euclidean")
df.mx = data.matrix(df.sub.dis)
dim = ncol(df.mx)
image(1:dim, 1:dim, df.mx, axes = FALSE, xlab="", ylab="")
axis(1, 1:dim, 1:50, cex.axis = 0.5, las=3)
axis(2, 1:dim, 1:50, cex.axis = 0.5, las=1)
text(expand.grid(1:dim,1:dim),sprintf("%0.1f",df.mx),cex = 0.6)

# Average silhouette method for hierarchical clustering
fviz_nbclust(df, hcut, method = "silhouette",
             hc_method = "complete")

# Elbow method for hierarchical clustering
fviz_nbclust(df, hcut, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
cut4= cutree(hc.complete,k =4)
cut2 = cutree(hc.complete,k = 2)
library(rgl)
plot3d(df,xlab = "X",ylab = "Y",zlab = "Z",col=cut2,size =8)
plot3d(df,xlab = "X",ylab = "Y",zlab = "Z",col=cut4,size =8)

# task2 
set.seed(123)
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(df, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 4, lty =2)

set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
plot3d(df,xlab = "X",ylab = "Y",zlab = "Z",col=km.res$cluster,size =8)

cluster.stats(df.dis,db$cluster)$dunn

## Task 3

install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

ep3 = dbscan::kNNdistplot(df, k =  9)
abline(h = 0.6, lty = 2)
set.seed(123)
db3 = dbscan(df, 0.6,minPts = 9)
plot3d(df,xlab = "X",ylab = "Y",zlab = "Z",col=db3$cluster+1,size =8)
cluster.stats(df.dis,db3$cluster+1)$dunn

ep15 = dbscan::kNNdistplot(df, k =  15)
abline(h = 0.8, lty = 2)
set.seed(123)
db15 = dbscan(df, 0.8,minPts = 15)
plot3d(df,xlab = "X",ylab = "Y",zlab = "Z",col=db15$cluster+1,size =8)
cluster.stats(df.dis,db3$cluster+1)$dunn

## 3.2
set.seed(123)
ep = dbscan::kNNdistplot(df, k =  5)
abline(h = 0.53, lty = 2)
db5 = dbscan(df, 0.53,minPts = 5)
plot3d(df,xlab = "X",ylab = "Y",zlab = "Z",col=db5$cluster+1,size =8)

## task 4
library(fpc)
db = db3
cluster.stats(df.dis,cut2)$dunn
cluster.stats(df.dis,km.res$cluster)$dunn
cluster.stats(df.dis,db$cluster+1)$dunn

hc.res <- eclust(df, "hclust", k = 2, hc_method = "complete", graph = FALSE)
fviz_silhouette(hc.res)
kmm.res <- eclust(df, "kmeans", k = 4, graph = FALSE)
fviz_silhouette(kmm.res)

## extra credit
dd = read.csv(paste(folder,"xfang7.csv", sep=''),header = FALSE)
library("ggpubr")
names(dd) = c("x","y","z")
ggscatter(dd,x="x",y="z")+geom_density2d() # Add 2D density

library(mclust)
library(factoextra)

mod = Mclust(df)
plot(mod, what = "BIC", ylim = range(mod$BIC[,-(1:2)], na.rm = TRUE),legendArgs = list(x = "bottomright"))
abline(v = 6, lty = 2)
drmod = MclustDR(mod,lambda = 1)
summary(drmod)
plot(drmod, what = "contour")
cluster.stats(df.dis,mod$classification)$dunn







