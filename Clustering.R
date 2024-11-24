install.packages("factoextra")
install.packages("NbClust")
install.packages("dbscan")
install.packages("fpc")


library(datasets)
library(NbClust) #determing number of cluster for k-means 
library(ggplot2)
library(dplyr)
library(cluster) #hierarchical clustering 
library(dbscan) #density-based clustering
library(fpc) #density-based clustering
library(factoextra) #plot density-based cluster

# Question 1: Data-set selection 

?USJudgeRatings


data("USJudgeRatings")
judge_Rating = USJudgeRatings
View(judge_Rating)


require(graphics)
pairs(USJudgeRatings, main = "USJudgeRatings data")


# Question 2: Perform clustering on the data using any 3 methods from among the following: k-means, k-medoids, hierarchical clustering, dbscan, optics


######### kmeans clustering #####################################################################

# step 1: scale the data
# making distance matrix unweighted since we are using euclidian distance

scaled_Judge_Rating = data.frame(scale(judge_Rating))
head(scaled_Judge_Rating)


# step 2: Measuring distance among observation 

dist_Judge_Rating = dist(scaled_Judge_Rating)


# step 3: Determine how many clusters it could make 
# visualize optimum number of cluster
# within sum square (WSS method)



fviz_nbclust(scaled_Judge_Rating, kmeans, method = "wss") + labs(subtitle = "Elbow method")

# the plot tells us there are 3 types of clusters we can use to merge our clusters together.
# afther point 3, the slop are almost zero.
# alternate method of finding optimum cluster number 

set.seed(1000)
NbClust(scaled_Judge_Rating, min.nc = 2, max.nc = 20, method = "kmeans")

# step 4: Multi-cluster plot visualization

k2 = kmeans(scaled_Judge_Rating, centers=2, nstart=100)
k3 = kmeans(scaled_Judge_Rating, centers=3, nstart=100)
k4 = kmeans(scaled_Judge_Rating, centers=4, nstart=100)
k5 = kmeans(scaled_Judge_Rating, centers=5, nstart=100)



# step 5: clustering visualization 

# two cluster

library(gridExtra)
grid.arrange(a,b,c,d, nrow=2)



x=judge_Rating %>% 
  mutate(cluster = k2$cluster,
         judgeName = row.names(judge_Rating)) 

km.cluster= k2$cluster

rownames(scaled_Judge_Rating)= paste(x$judgeName, 1:dim(x)[1],sep="_")
a = fviz_cluster(list(data=scaled_Judge_Rating, cluster=km.cluster))


# three cluster


x=judge_Rating %>% 
  mutate(cluster = k3$cluster,
         judgeName = row.names(judge_Rating)) 

km.cluster= k3$cluster

rownames(scaled_Judge_Rating)= paste(x$judgeName, 1:dim(x)[1],sep="_")
b = fviz_cluster(list(data=scaled_Judge_Rating, cluster=km.cluster))


# four clusters

x=judge_Rating %>% 
  mutate(cluster = k4$cluster,
         judgeName = row.names(judge_Rating)) 

km.cluster= k4$cluster
rownames(scaled_Judge_Rating)= paste(x$judgeName, 1:dim(x)[1],sep="_")
c = fviz_cluster(list(data=scaled_Judge_Rating, cluster=km.cluster))


# five clusters 

x=judge_Rating %>% 
  mutate(cluster = k5$cluster,
         judgeName = row.names(judge_Rating)) 

km.cluster= k5$cluster
rownames(scaled_Judge_Rating)= paste(x$judgeName, 1:dim(x)[1],sep="_")
d = fviz_cluster(list(data=scaled_Judge_Rating, cluster=km.cluster))



# Visualization with respect to two variables "DECI" & "INTG"

judge_Rating %>% 
  mutate(cluster = kmeansJudge$cluster,
         judgeName = row.names(judge_Rating)) %>%
  ggplot(aes(DECI, INTG, color=factor(cluster), label = judgeName))+
  geom_text()

##################### End of K-mean CLustering##################################



######################## K_medoid Clustering   #################################

#step-1
# Scaling the data

judge_data_std= scale(judge_Rating)


# step-2

# optimal number of clusters

fviz_nbclust(judge_data_std, FUNcluster = pam, method = "silhouette") + theme_classic()

fviz_nbclust(judge_data_std, FUNcluster = pam, method = "wss") + theme_classic()

fviz_nbclust(judge_data_std, FUNcluster = pam, method = "gap_stat") + theme_classic()

# so optimal number of cluster = 2 (from 3 above methods)


# step-3
# pam clustering to obtain medoids and available components 

my_cluster= pam(judge_data_std, 2, metric = "euclidean", stand = F)



# step-4
# visualization of the cluster 

fviz_cluster(my_cluster, data=judge_data_std, palette= c("red","blue"), geom="point",
             ellipse.type = "norm", ggtheme = theme_bw())


# comparison of different clustering plot to obtain optimum cluster number

par(mfrow = c(2, 2))


my_cluster3= pam(judge_data_std, 3, metric = "euclidean", stand = F)

fviz_cluster(my_cluster3, data=judge_data_std, palette= c("red","blue","orange"), geom="point",
             ellipse.type = "norm", ggtheme = theme_bw())



my_cluster4= pam(judge_data_std, 4, metric = "euclidean", stand = F)

fviz_cluster(my_cluster4, data=judge_data_std, palette= c("red","blue","orange","red"), geom="point",
             ellipse.type = "norm", ggtheme = theme_bw())


# step 5
# comparing by shilhoutte function 

pm2= silhouette(my_cluster$clustering, dist(judge_data_std))
fviz_silhouette(pm2)


pm3= silhouette(my_cluster3$clustering, dist(judge_data_std))
fviz_silhouette(pm3)


pm4= silhouette(my_cluster4$clustering, dist(judge_data_std))
fviz_silhouette(pm4)


####################  end of  K_medoid Clustering   #################################


##################    Hierarchial Clustering   #######################################


# Scaling the data

judge_data_std= scale(judge_Rating)

# measuring distance

judge.dist = dist(judge_data_std)

# Hierchial clustering algoritihm 

hc.out_judge=hclust(judge.dist, method = 'complete')

# Dendogram

plot(hc.out_judge)
rect.hclust(hc.out_judge, k = 5, border= 2:5)


# Clusters

judge.clusters = cutree(hc.out_judge, k=5)

print(judge.clusters)

length(judge.clusters) # each observation (43) are distrbuted to 5 clusters 

# visualize the cluster


a=judge_Rating %>% 
  mutate(
    judgeName = row.names(judge_Rating)) 


rownames(judge_data_std) = paste(a$judgeName, 1:dim(a)[1], sep = "-")

fviz_cluster(list(data=judge_data_std, cluster=judge.clusters))

table(judge.clusters, a$judgeName)


################## End of   Hierarchial Clustering   #######################################

################### density based clustering   ########################################




judgeDB = as.matrix(judge2)
head(judgeDB)
knnJudge =kNNdist(judgeDB, k = 2,search = "kd")

?kNNdist

kNNdistplot(judgeDB, k = 5)
# The knee is visible around a distance of .3

abline(h = 0.3, col = "blue")
?kNNdistplot

set.seed(100)
res = fpc::dbscan(judgeDB, eps = 0.3, MinPts = 4)


factoextra::fviz_cluster(res,judgeDB, geom = c("point","text"), 
                         labelsize = 6, pointsize = 1, 
                         main = "Judge judicial integrity & prompt decision Cluster (Density-based)")

dev.off()
##########################################################################################

