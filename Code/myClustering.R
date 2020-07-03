# Student Identity : 44814377
# File Description : this file is for the task 2 of the assignment, which is about the 
#                    clustering topic.



# Task2
# 2.1. Load the preprocessed data file from Task 1 into a data frame. Please
# note that for this set of clustering tasks, you should not include the Class
# column.

# read the data and remove the column 'Class'
breast_cancer_data <- readRDS("./Data/bcw_processed.Rda")
data_processed <- breast_cancer_data[,-c(10)]
data_processed <- subset(breast_cancer_data, select = -c(Class))


# 2.2. Cluster the data into 2 clusters using K-Means clustering, using the default
# parameters for the kmeans function. Plot the results of the clusters as a
# 2D plot where the x-axis is Clump Thickness and the y-axis is
# Uniformity of Cell Size.

# for reproducible result
set.seed(4377)

# cluster with K-means into nclust clusters
nclust = 2
(kmeans.result <- kmeans(data_processed,nclust))

# plot the samples
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],
     col = kmeans.result$cluster, xlab = "Clump Thickness", ylab = "Uniformity of Cell Size" )
title(paste("K-means Clustering: k= ",nclust,sep=""))

# plot the centers
points(kmeans.result$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],
       col = 1:nclust, pch = 8, cex = 2)


# 2.3. Plot another 2D plot with the same dimensions above, but color the points
# according to the Class column.

# plot the 2 dimension graph
plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],
     col = breast_cancer_data$Class, xlab = "Clump Thickness", ylab = "Uniformity of Cell Size" )
title(paste("K-means Clustering: k= ",nclust,sep=""))

# plot the centers
points(kmeans.result$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],
       col = unique(breast_cancer_data$Class), pch = 8, cex = 2)


# 2.4. Compare the 2 plots obtained in the previous two tasks – do the clusters
# visually represent the benign vs malignant classes?.

# Answer: According to the comparing of the 2 plots, the entire trend of cluster results represent
# the benign and malignant classes from the raw data, although some points assign to various class.


# 2.5. Cluster the data into more than 2 clusters (i.e., k = 3, 4, 5) using K-Means
# clustering and plot all the clustering results.

# initial 3 plots
par(mfrow=c(1,3))

# generate centers and clusters of various clust number, and plot points and centers
for(i in 3:5){
  nclust <- i
  (kmeans.result <- kmeans(data_processed,nclust))
  
  if(i==3){
    center3 <- kmeans.result$centers
    cluster3 <- kmeans.result$cluster
    sse3 <- kmeans.result$tot.withinss}
  if(i==4){
    center4 <- kmeans.result$centers
    cluster4 <- kmeans.result$cluster
    sse4 <- kmeans.result$tot.withinss}
  if(i==5){
    center5 <- kmeans.result$centers
    cluster5 <- kmeans.result$cluster
    sse5 <- kmeans.result$tot.withinss}
  
  plot(data_processed[,c("Clump.Thickness","Uniformity.of.Cell.Size")],
       col = kmeans.result$cluster, xlab = "Clump Thickness", ylab = "Uniformity of Cell Size" )
  title(paste("K-means Clustering: k= ",nclust,sep=""))
  points(kmeans.result$centers[,c("Clump.Thickness","Uniformity.of.Cell.Size")],
         col = unique(kmeans.result$cluster), pch = 8, cex = 2)
}


# 2.6. Compare the plots and SSEs obtained in the previous task, and provide
# your comments on the quality of clustering.
nclust <- 1
(kmeans.result <- kmeans(data_processed,nclust))
sse1 <- kmeans.result$tot.withinss

nclust <- 2
(kmeans.result <- kmeans(data_processed,nclust))
sse2 <- kmeans.result$tot.withinss
cluster_list <- c(sse1,sse2,sse3,sse4,sse5)

# Give the chart file a name.
png(file = "plots/2.6.jpeg")

# Plot the bar chart. 
plot(cluster_list,type = "o", main = "SSE of various K clusters")
text(1+0.3, sse1, round(sse1,digits=0))
text(2+0.3, sse2+0.3, round(sse2,digits=0))
text(3+0.3, sse3+0.3, round(sse3,digits=0))
text(4+0.3, sse4+0.3, round(sse4,digits=0))
text(5, sse5+0.5, round(sse5,digits=0))

# Save the file.
dev.off()


# 2.7. Apply hierarchical clustering to the data using the hclust function with
# default parameters and plot the corresponding dendrogram. Particularly,
# cluster the dendrogram into 2, 3, 4, and 5 clusters and plot all of them.

# for reproducible result
set.seed(4377)

# random generate 40 number belong [1,n] as index, and produce the sample
n = nrow(data_processed)
idx <- sample(1:n, 40)
data_Sample <- data_processed[idx,]

# hierarchical clustering
hc <- hclust(dist(data_Sample))

# cut the tree with different clust number
par(mfrow=c(2,2))
for(i in 2:5){
  plot(hc, hang = -1, labels= data_Sample$Class[idx])
  # Cut the dendrogram into nclust clusters
  nclust = i
  rect.hclust(hc,k=nclust)
  groups <- cutree(hc, k=nclust)
}

# 2.8. Compare the plots obtained in the previous task and provide your
# observations on the achieved clusters - should we have a new subtype of
# diseases?

# Answer: 
# Comparing the hierarchical clustering of 2 clusters with 3, 4, 5 clusters, only 262 and 422 on the 
# right side has be divide into a new category, and most of the data still belong to the same cluster. 
# Based on it, there is no need to have a new subtype of diseases.


# 2.9. Try different agglomeration methods in hierarchical clustering (i.e.,
# “single”, “complete”, and “average”). Plot the resulting dendrograms
# and provide your comments on the quality of clustering - is the data
# sensitive to the used agglomeration method? Based on your results, what
# do you think is the default agglomeration method used in Task 2.7?

# for reproducible result
set.seed(4377)

# random generate 40 number belong [1,n] as index, and produce the sample
n = nrow(data_processed)
idx <- sample(1:n, 40)
data_Sample <- data_processed[idx,]

# hierarchical clustering with different method
hc1 <- hclust(dist(data_Sample), method = "single")
hc2 <- hclust(dist(data_Sample), method = "complete")
hc3 <- hclust(dist(data_Sample), method = "average")

# hierarchical clustering with different method and plot the graphs
par(mfrow=c(1,3))
plot(hc1, hang = -1, labels= data_Sample$Class[idx])
plot(hc2, hang = -1, labels= data_Sample$Class[idx])
plot(hc3, hang = -1, labels= data_Sample$Class[idx])
