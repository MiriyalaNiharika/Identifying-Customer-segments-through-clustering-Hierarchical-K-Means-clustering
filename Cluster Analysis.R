setwd('C:/Users/Niharika/Documents/R/Project-1_EastWestAirlines') #Setting the working directory

df <- read.csv("EastWestAirlines.csv",header=TRUE) #Reading the required CSV file
dim(df) # Gives dimension of the dataframe in the file

# Removing the 'ID#' & 'Award' column from Airlines Dataset:
Airlines_Data <- df[c(-1,-12)] 
head(Airlines_Data)

#Hierarchical Clustering- Using Euclidean & ward's:

standardized_data <- scale(Airlines_Data) # Standardizing all the columns

distance <- dist(standardized_data, method = "euclidean") # Computing distance using Euclidean's method
Clusters <- hclust(distance, method="ward.D2") # Running hierarchical clustering using Ward's method
plot(Clusters) # displaying dendrogram


# cutting tree into 3 clusters
abline(h=88,col="red",lty=2)
groups <- cutree(Clusters, k=3) 
table(groups) #number of observations in each of 3 clusters

# Visualize
plot(Clusters, cex = 0.6) # plot tree
rect.hclust(Clusters, k = 3, border = 2:5) # add rectangle around the 3 clusters

#membership of each datapoint:
membership<-as.matrix(groups)
membership

#Data belonging to Clusters 1,2,3:
cluster1 <- subset(Airlines_Data,membership[,1]==1)
cluster1

cluster2 <- subset(Airlines_Data,membership[,1]==2)
cluster2

cluster3 <- subset(Airlines_Data,membership[,1]==3)
cluster3

#Output excel file
colnames(membership)[1] <- "Cluster ID"
Hierarchical_Clusters <- cbind(Airlines_Data,membership)
write.csv(Hierarchical_Clusters,file="Hierarchical_Clusters.csv")

#Finding the mean of all the values
Cluster_average <- aggregate(standardized_data,list(groups),mean)
Cluster_average

#Appending the size of clusters with the mean of all other values
Airlines_summary <- data.frame(Cluster=Cluster_average[,1],Cluster_count=as.vector(table(groups)),Cluster_average[,-1])

# transpose to change from horizontal to vertical
Airlines_Transpose <- t(Airlines_summary)

#rounding off the digits in the summary to 2:
round_df <- function(x, digits) {
numeric_columns    <- sapply(x, class) == 'numeric'
x[numeric_columns] <-  round(x[numeric_columns], digits)
x
}

Airlines_final_summmary <- round_df(Airlines_Transpose, 2)
Airlines_final_summmary

# To check the stability of clusters, remove a random 5% of the data (by 
#taking a random sample of 95% of the records), and repeat the analysis. 

install.packages("dplyr")
library(dplyr)

#Selecting random 95% of Data:
data_sample <- sample_n(df, 3800, replace = FALSE)

#Standardizing the data before performing 'Hierarchical Clustering'
sample_standardized <- scale(data_sample) # Standardizing all the columns

distance_S <- dist(sample_standardized, method = "euclidean") # Computing distance using Euclidean's method
Sample_Clusters <- hclust(distance_S, method="ward.D2") # Running hierarchical clustering using Ward's method
plot(Sample_Clusters) # displaying dendrogram

# cut tree into 3 clusters
abline(h=85,col="red",lty=2)
groups_Sample <- cutree(Clusters, k=3) 
table(groups_Sample) #number of observations in each of 3 clusters

# Visualize
plot(Sample_Clusters, cex = 0.6) # plot tree
rect.hclust(Sample_Clusters, k = 3, border = 2:5) # add rectangle around the 3 clusters

#K-means clustering
standardized_data <- scale(Airlines_Data) # Standardizing all the columns

fit <- kmeans(standardized_data, centers=3, iter.max=10, nstart=10) #setting initial centers to 3
fit

fit$cluster      # A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
fit$centers      # A matrix of cluster centres.
fit$totss        # The total sum of squares.
fit$withinss     # Vector of within-cluster sum of squares, one component per cluster.
fit$tot.withinss # Total within-cluster sum of squares, i.e. sum(withinss).
fit$betweenss    # The between-cluster sum of squares, i.e. totss-tot.withinss.
fit$size         # The number of points in each cluster.
fit$iter         # The number of (outer) iterations.

KMeansClusters <- cbind(Airlines_Data, fit$cluster) # append cluster membership
KMeansClusters

#Output excel file
write.csv(KMeansClusters,file="KMeans_Clusters.csv")

## Determining number of clusters using Elbow curve
Cluster_Variability <- matrix(nrow=6, ncol=1)
for (i in 1:6) Cluster_Variability[i] <- kmeans(standardized_data,centers=i, nstart=4)$tot.withinss
plot(1:6, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")


#From the elbow curve, we can see that the slope is changing at k=2 and increasing slightly after k=3.
#since 2 clusters won't be helpful in our analysis and upto 3 clusters is a optimum number, 3 clusters 
#would be ideal for our case

#Using factoextra package to determine ideal number of clusters:

install.packages("factoextra")
library(factoextra)

#Plotting a K-means Visualization taking k=3:
kmean_Visulization3 <- eclust(standardized_data, "kmeans", k = 3, nstart = 25, graph = TRUE)

#Plotting a K-means Visualization taking k=4:
kmean_Visulization4 <- eclust(standardized_data, "kmeans", k = 4, nstart = 25, graph = TRUE)
#We can see a clear overlap of clusters 2 & 3 when we considered k=4. Hence k=3 would be ideal

# appending cluster membership
NormDatasWithClusterMembership <- cbind(standardized_data, fit$cluster) 

#aggregating values based on mean
Kmeans_cluster_means<-aggregate(NormDatasWithClusterMembership, by=list(fit$cluster), FUN=mean) 


#Appending the size of clusters with the mean of all other values
##Kmeans_Airlines_summary <- data.frame(Cluster=Kmeans_cluster_means[,1],Cluster_count=as.vector(fit$size),Kmeans_cluster_means[,-1])
##Kmeans_Airlines_summary

Kmeans_cluster_means$clusterSize <- fit$size

# transpose to change from horizontal to vertical
Transpose_summary <- t(Kmeans_cluster_means)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

Kmeans_roundoff_summary <- round_df (Transpose_summary,2)
Kmeans_roundoff_summary

#From the summary above(Kmeans_roundoff_summary) on K-means clustering we observe the following:

#Cluster-1 : This cluster comprises of 34% of total customers. This segment of customers are oldest and 
#also have decent number of Bonus miles & transactions along with the balance accrual. But the flight miles
#travelled & non flight transactions in the past 12 months is comparatively less which tells us that 
#this group of customers are not very frequent travelers. But this segment has highest CC1_miles earned
#compared to other two segments which tells us that though they are not frequent travellers in the past 12
#months, since inception they were frequent travelers.

#Cluster-2 : Though this cluster comprises of least number of customer i.e., about 4%, it has the highest
#balance accrual when compared to other segments as it has 2nd highest number of old customers. Also, this
#segment of customers have highest frequent travellers in the last 12 months and also highest flight transactions.
#This segment also topped in earning CC miles through rewards credit card. This is the only segment that
#has customers who can be qualified for topflight status(Qualmiles).

#Cluster-3: This segment has highest number of customers(62%). This segment has least balance accrual & bonus
#miles that are earned compared to other segments. Also, this segment comprises of least frequent travellers
#in the last two months. Only positive point of this cluster is that, customers in this segment have next
#highest miles earned through rewards Credit.



