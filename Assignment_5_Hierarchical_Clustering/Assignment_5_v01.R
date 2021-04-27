##Assignment 5

#Hierarchical Clustering Analysis

#1
library(cluster)
library(factoextra)
library(caret)

cereals.df <- read.csv("Cereals.csv")

#2
head(cereals.df)
str(cereals.df)
summary(cereals.df)
dim(cereals.df)

#3
colMeans(is.na(cereals.df))
cereals.df <- cereals.df[complete.cases(cereals.df),]
colMeans(is.na(cereals.df))

head(cereals.df)
summary(cereals.df)

cereals.short <- cereals.df
row.names(cereals.short) <- cereals.short[,1]
cereals.short <- cereals.short[,-c(1:3)]
dim(cereals.short)

#4
cereals.norm <- scale(cereals.short)
head(cereals.norm)
summary(cereals.norm)

#5
set.seed(123)
hc_s <- agnes(cereals.norm, method = "single")
hc_c <- agnes(cereals.norm, method = "complete")
hc_a <- agnes(cereals.norm, method = "average")
hc_w <- agnes(cereals.norm, method = "ward")

print(hc_s$ac)
print(hc_c$ac)
print(hc_a$ac)
print(hc_w$ac)

#6
#plot_s <- pltree(hc_s, cex = 0.6, hang = -1)
#plot_c <- pltree(hc_c, cex = 0.6, hang = -1)
#plot_a <- pltree(hc_a, cex = 0.6, hang = -1)
plot_w <- pltree(hc_w, cex = 0.6, hang = -1)

#7
fviz_nbclust(cereals.norm, FUN = hcut, method = "wss")
fviz_nbclust(cereals.norm, FUN = hcut, method = "silhouette")

#8
#clusters4 <- cutree(hc_w, k=4)
#clusters4
clusters5 <- cutree(hc_w, k=5)
clusters5
#clusters6 <- cutree(hc_w, k=6)
#clusters6

#clplot4 <- fviz_cluster(list(data = cereals.norm, cluster = clusters4))
clplot5 <- fviz_cluster(list(data = cereals.norm, cluster = clusters5))
#clplot6 <- fviz_cluster(list(data = cereals.norm, cluster = clusters6))

#9
set.seed(123)
datapart <- createDataPartition(cereals.norm, p=0.5, list=FALSE)
partitionA <- cereals.norm[datapart, ]
partitionB <- cereals.norm[-datapart, ]
summary(partitionA)
summary(partitionB)

#10
set.seed(123)
hc_w_A <- agnes(partitionA, method = "ward")
hc_w_B <- agnes(partitionB, method = "ward")
print(hc_w_A$ac)
print(hc_w_B$ac)
plot_w_A <- pltree(hc_w_A, cex = 0.6, hang = -1)
plot_w_B <- pltree(hc_w_B, cex = 0.6, hang = -1)

#11
cereals.clust <- cbind(clusters5, cereals.short)

median.table <- aggregate(cereals.short, list(clusters5), median)
median.table


-----------------------------------
d <- dist(cereals.norm, method = "euclidean")
d

hc_s <- hclust(d, method = "single")
plot_S <- plot(hc_s, cex = 0.6, hang = -1)
hc_c <- hclust(d, method = "complete")
plot_c <- plot(hc_c, cex = 0.6, hang = -1)
hc_a <- hclust(d, method = "average")
plot_a <- plot(hc_a, cex = 0.6, hang = -1)
hc_s <- hclust(d, method = "single")
plot_S <- plot(hc_s, cex = 0.6, hang = -1)