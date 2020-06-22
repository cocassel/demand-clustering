library(ggplot2)
library(caTools)
library(corrplot)
library(caret) 
library(cluster)  

############################### DATA PREPARATION ######################################

set.seed(0017)
data=read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 2/Demand_data.csv")
head(data)

split = sample.split(data$SKU, SplitRatio = 0.9) 
dataSubset = subset(data, split == TRUE) 

################################### PART 1 ##########################################

# Get general statistics of the dataset
summary(dataSubset)
sapply(dataSubset[4:33], var)
sapply(dataSubset[4:33], sd)
correlationMatrix = round(cor(dataSubset[5:33]),2)
correlationMatrix
table(dataSubset$Category)
table(dataSubset$Subcategory)
table(dataSubset$Innovation)

# Heatmap for correlation matrix 
corrplot(correlationMatrix, title ="Correlation Heatmap for Date Variables in Data Set", mar=c(0,0,3,0))

# Pie charts
categoryTable = table(dataSubset$Category)
lbls = paste(names(categoryTable), "\n", categoryTable, sep="")
colours = c("blue","violet","pink","orange")
pie(categoryTable, labels = lbls, main="Pie Chart of Categories", col=colours)

innovationTable = table(dataSubset$Innovation)
lbls = paste(c("Not Innovation","Innovation"), "\n", innovationTable, sep="")
colours = c("blue","violet")
pie(innovationTable, labels = lbls, main="Pie Chart of Innovation Products", col=colours)

# Subcategory bar chart
subcategoryTable = table(dataSubset$Subcategory)
subcategoryTable = sort(subcategoryTable)
barplot(subcategoryTable, main="Counts of SKUs in Sub-Categories", xlab="SKU Sub-Category", ylab = "Count of SKUs in Sub-Category", col=rgb(0.2,0.4,0.7,0.6), cex.names=0.5)

# Boxplot
Feb_1 = dataSubset$Feb_1
Mar_1 = dataSubset$Mar_1
Apr_1 = dataSubset$Apr_1
May_1 = dataSubset$May_1
Jun_1 = dataSubset$Jun_1
Jul_1 = dataSubset$Jul_1
Aug_1 = dataSubset$Aug_1
Sep_1 = dataSubset$Sep_1
Oct_1 = dataSubset$Oct_1
Nov_1 = dataSubset$Nov_1
Dec_1 = dataSubset$Dec_1
Jan_2 = dataSubset$Jan_2
Feb_2 = dataSubset$Feb_2
Mar_2 = dataSubset$Mar_2
Apr_2 = dataSubset$Apr_2
May_2 = dataSubset$May_2
Jun_2 = dataSubset$Jun_2
Jul_2 = dataSubset$Jul_2
Aug_2 = dataSubset$Aug_2
Sep_2 = dataSubset$Sep_2
Oct_2 = dataSubset$Oct_2
Nov_2 = dataSubset$Nov_2
Dec_2 = dataSubset$Dec_2
Jan_3 = dataSubset$Jan_3
Feb_3 = dataSubset$Feb_3
Mar_3 = dataSubset$Mar_3
Apr_3 = dataSubset$Apr_3
May_3 = dataSubset$May_3
Jun_3 = dataSubset$Jun_3

names = c("Feb_1", "Mar_1", "Apr_1", "May_1", "Jun_1", "Jul_1", "Aug_1", "Sep_1",
          "Oct_1", "Nov_1", "Dec_1", "Jan_2", "Feb_2", "Mar_2", "Apr_2", "May_2", 
          "Jun_2", "Jul_2", "Aug_2", "Sep_2", "Oct_2", "Nov_2", "Dec_2", "Jan_3", 
          "Feb_3", "Mar_3", "Apr_3", "May_3", "Jun_3")
par(mar=c(5.1, 6.1, 4.1, 2.1))
boxplot(Feb_1, Mar_1, Apr_1, May_1, Jun_1, Jul_1, Aug_1, Sep_1, Oct_1, Nov_1,
        Dec_1, Jan_2, Feb_2, Mar_2, Apr_2, May_2, Jun_2, Jul_2, Aug_2, Sep_2, 
        Oct_2, Nov_2, Dec_2, Jan_3, Feb_3, Mar_3, Apr_3, May_3, Jun_3, 
        names = names, las=2, main="Boxplot of Demand vs Month")
title(ylab="Demand", line=5)
title(xlab="Month", line=4)

# Line graph for mean demand in category
Feb_1 = tapply(dataSubset$Feb_1, dataSubset$Category, mean)
Mar_1 = tapply(dataSubset$Mar_1, dataSubset$Category, mean)
Apr_1 = tapply(dataSubset$Apr_1, dataSubset$Category, mean)
May_1 = tapply(dataSubset$May_1, dataSubset$Category, mean)
Jun_1 = tapply(dataSubset$Jun_1, dataSubset$Category, mean)
Jul_1 = tapply(dataSubset$Jul_1, dataSubset$Category, mean)
Aug_1 = tapply(dataSubset$Aug_1, dataSubset$Category, mean)
Sep_1 = tapply(dataSubset$Sep_1, dataSubset$Category, mean)
Oct_1 = tapply(dataSubset$Oct_1, dataSubset$Category, mean)
Nov_1 = tapply(dataSubset$Nov_1, dataSubset$Category, mean)
Dec_1 = tapply(dataSubset$Dec_1, dataSubset$Category, mean)
Jan_2 = tapply(dataSubset$Jan_2, dataSubset$Category, mean)
Feb_2 = tapply(dataSubset$Feb_2, dataSubset$Category, mean)
Mar_2 = tapply(dataSubset$Mar_2, dataSubset$Category, mean)
Apr_2 = tapply(dataSubset$Apr_2, dataSubset$Category, mean)
May_2 = tapply(dataSubset$May_2, dataSubset$Category, mean)
Jun_2 = tapply(dataSubset$Jun_2, dataSubset$Category, mean)
Jul_2 = tapply(dataSubset$Jul_2, dataSubset$Category, mean)
Aug_2 = tapply(dataSubset$Aug_2, dataSubset$Category, mean)
Sep_2 = tapply(dataSubset$Sep_2, dataSubset$Category, mean)
Oct_2 = tapply(dataSubset$Oct_2, dataSubset$Category, mean)
Nov_2 = tapply(dataSubset$Nov_2, dataSubset$Category, mean)
Dec_2 = tapply(dataSubset$Dec_2, dataSubset$Category, mean)
Jan_3 = tapply(dataSubset$Jan_3, dataSubset$Category, mean)
Feb_3 = tapply(dataSubset$Feb_3, dataSubset$Category, mean)
Mar_3 = tapply(dataSubset$Mar_3, dataSubset$Category, mean)
Apr_3 = tapply(dataSubset$Apr_3, dataSubset$Category, mean)
May_3 = tapply(dataSubset$May_3, dataSubset$Category, mean)
Jun_3 = tapply(dataSubset$Jun_3, dataSubset$Category, mean)

combinedMeans = rbind(Feb_1, Mar_1, Apr_1, May_1, Jun_1, Jul_1, Aug_1, Sep_1, Oct_1, Nov_1,
                      Dec_1, Jan_2, Feb_2, Mar_2, Apr_2, May_2, Jun_2, Jul_2, Aug_2, Sep_2, 
                      Oct_2, Nov_2, Dec_2, Jan_3, Feb_3, Mar_3, Apr_3, May_3, Jun_3)
monthNumber = c(1:29)
combinedMeans = cbind(monthNumber, combinedMeans)
combinedMeans = as.data.frame(combinedMeans)

ggplot() +
  geom_line(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$BPC, color="BPC")) +
  geom_point(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$BPC, color="BPC")) +
  geom_line(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$FOODS, color="FOODS")) +
  geom_point(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$FOODS, color="FOODS")) +
  geom_line(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$HHC, color="HHC")) +
  geom_point(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$HHC, color="HHC")) +
  geom_line(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$Refreshments, color="Refreshments")) +
  geom_point(data=combinedMeans, aes(x=monthNumber, y = combinedMeans$Refreshments, color="Refreshments")) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Mean Demand of SKUs by Category") +
  xlab("Month")  +
  ylab("Mean Demand of Category's SKUs") +
  theme(legend.position="right") +
  scale_color_discrete(name = "SKU Category")

################################### PART 2 ##########################################

# Normalize data
clusterNorm = dataSubset[,-c(1,2,3,4)]
for (i in colnames(clusterNorm)){
  meanDemand = mean(clusterNorm[[i]])
  stdDeviation = sd(clusterNorm[[i]])
  clusterNorm[[i]] = (clusterNorm[[i]] - meanDemand)/stdDeviation
}

# All variables now have mean 0
summary(clusterNorm)

# All variables now have standard deviation of 1
sapply(clusterNorm, sd)

################### Hierarchical Clustering ##################

# Get demand variable columns only
clusterData = dataSubset[,-c(1,2,3,4)]

# Function to get total sum of squared error given distances and Hclusters
getSumOfSquaredError = function(distanceMethod, agglomerationMethod) {
  y = vector()
  # Use K values between 2 and 15
  for (cluster in 2:15){
    distances = dist(clusterNorm, method=distanceMethod) 
    Hclusters = hclust(distances, method=agglomerationMethod) 
    clusterCut = cutree(Hclusters, k=cluster) 
    # Use original data (not normalized) to compute error
    ss = aggregate(clusterData, by=list(clusterCut), function(clusterData) sum(scale(clusterData,scale=FALSE)^2))
    ss = rowSums(ss) # Sum of squares for each cluster
    tss = sum(ss)  # Total sum of squares
    y = append(y, tss)
  }
  # Get mean of the total sum of squared error for the different values of K
  return(mean(y))
}

# Try different combinations of methods
errors = c(getSumOfSquaredError("euclidean", "ward.D"),
           getSumOfSquaredError("euclidean", "ward.D2"),
           getSumOfSquaredError("euclidean", "single"),
           getSumOfSquaredError("euclidean", "complete"),
           getSumOfSquaredError("euclidean", "average"),
           getSumOfSquaredError("euclidean", "mcquitty"),
           getSumOfSquaredError("euclidean", "median"),
           getSumOfSquaredError("euclidean", "centroid"),
           getSumOfSquaredError("manhattan", "ward.D"),
           getSumOfSquaredError("manhattan", "ward.D2"),
           getSumOfSquaredError("manhattan", "single"),
           getSumOfSquaredError("manhattan", "complete"),
           getSumOfSquaredError("manhattan", "average"),
           getSumOfSquaredError("manhattan", "mcquitty"),
           getSumOfSquaredError("manhattan", "median"),
           getSumOfSquaredError("manhattan", "centroid"),
           getSumOfSquaredError("maximum", "ward.D"),
           getSumOfSquaredError("maximum", "ward.D2"),
           getSumOfSquaredError("maximum", "single"),
           getSumOfSquaredError("maximum", "complete"),
           getSumOfSquaredError("maximum", "average"),
           getSumOfSquaredError("maximum", "mcquitty"),
           getSumOfSquaredError("maximum", "median"),
           getSumOfSquaredError("maximum", "centroid"))
which.min(errors)

# Euclidean and Ward.D2 give us the smallest mean total sum of squared error
distances = dist(clusterNorm, method="euclidean") 
Hclusters = hclust(distances, method="ward.D2") 
plot(Hclusters) 

# Cluster cut with chosen number of clusters from dendrogram
clusterCut = cutree(Hclusters, k=6) 
rect.hclust(Hclusters, k=6, border="red")
head(clusterCut); 
summary(clusterCut) 

# Number of SKUs in each cluster
table(clusterCut) 

# Number of SKUs in each cluster grouped by category
table(dataSubset$Category, clusterCut) 

# Number of SKUs in each cluster grouped by sub-category
table(dataSubset$Subcategory, clusterCut) 

# Number of SKUs in each cluster grouped by innovation
table(dataSubset$Innovation, clusterCut) 

# Get means of clusters in original (not normalized) data
Feb_1 = tapply(dataSubset$Feb_1, clusterCut, mean)
Mar_1 = tapply(dataSubset$Mar_1, clusterCut, mean)
Apr_1 = tapply(dataSubset$Apr_1, clusterCut, mean)
May_1 = tapply(dataSubset$May_1, clusterCut, mean)
Jun_1 = tapply(dataSubset$Jun_1, clusterCut, mean)
Jul_1 = tapply(dataSubset$Jul_1, clusterCut, mean)
Aug_1 = tapply(dataSubset$Aug_1, clusterCut, mean)
Sep_1 = tapply(dataSubset$Sep_1, clusterCut, mean)
Oct_1 = tapply(dataSubset$Oct_1, clusterCut, mean)
Nov_1 = tapply(dataSubset$Nov_1, clusterCut, mean)
Dec_1 = tapply(dataSubset$Dec_1, clusterCut, mean)
Jan_2 = tapply(dataSubset$Jan_2, clusterCut, mean)
Feb_2 = tapply(dataSubset$Feb_2, clusterCut, mean)
Mar_2 = tapply(dataSubset$Mar_2, clusterCut, mean)
Apr_2 = tapply(dataSubset$Apr_2, clusterCut, mean)
May_2 = tapply(dataSubset$May_2, clusterCut, mean)
Jun_2 = tapply(dataSubset$Jun_2, clusterCut, mean)
Jul_2 = tapply(dataSubset$Jul_2, clusterCut, mean)
Aug_2 = tapply(dataSubset$Aug_2, clusterCut, mean)
Sep_2 = tapply(dataSubset$Sep_2, clusterCut, mean)
Oct_2 = tapply(dataSubset$Oct_2, clusterCut, mean)
Nov_2 = tapply(dataSubset$Nov_2, clusterCut, mean)
Dec_2 = tapply(dataSubset$Dec_2, clusterCut, mean)
Jan_3 = tapply(dataSubset$Jan_2, clusterCut, mean)
Feb_3 = tapply(dataSubset$Feb_2, clusterCut, mean)
Mar_3 = tapply(dataSubset$Mar_2, clusterCut, mean)
Apr_3 = tapply(dataSubset$Apr_2, clusterCut, mean)
May_3 = tapply(dataSubset$May_2, clusterCut, mean)
Jun_3 = tapply(dataSubset$Jun_2, clusterCut, mean)

clusterCenters = rbind(Feb_1, Mar_1, Apr_1, May_1, Jun_1, Jul_1, Aug_1, Sep_1, Oct_1, Nov_1,
                       Dec_1, Jan_2, Feb_2, Mar_2, Apr_2, May_2, Jun_2, Jul_2, Aug_2, Sep_2, 
                       Oct_2, Nov_2, Dec_2, Jan_3, Feb_3, Mar_3, Apr_3, May_3, Jun_3)
monthNumber = c(1:29)
clusterCenters = cbind(monthNumber, clusterCenters)
clusterCenters = as.data.frame(clusterCenters)

clusterCenters

# Plot the cluster centers/means for original (not normalized) data
ggplot() +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[2]], color="Cluster 1")) + 
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[2]], color="Cluster 1")) + 
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[3]], color="Cluster 2")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[3]], color="Cluster 2")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[4]], color="Cluster 3")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[4]], color="Cluster 3")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[5]], color="Cluster 4")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[5]], color="Cluster 4")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[6]], color="Cluster 5")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[6]], color="Cluster 5")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[7]], color="Cluster 6")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[7]], color="Cluster 6")) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Cluster Centres for Hierarchical Clustering (K=6)") +
  xlab("Variables Used to Cluster (Month)")  +
  ylab("Variable's Mean Demand in the Cluster") +
  theme(legend.position="right") +
  scale_color_discrete(name = "Cluster #")


##################### K means clustering ############################

# Apply K-means clustering
KmeansClustering = kmeans(clusterNorm, centers=7, nstart=25) 

# Plot the clusters
clusplot(dataSubset, KmeansClustering$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 

# Number of SKUs in each cluster
table(KmeansClustering$cluster) 

# Number of SKUs in each cluster grouped by category
table(dataSubset$Category, KmeansClustering$cluster) 

# Number of SKUs in each cluster grouped by sub-category
table(dataSubset$Subcategory, KmeansClustering$cluster) 

# Number of SKUs in each cluster grouped by innovation
table(dataSubset$Innovation, KmeansClustering$cluster) 

# Get cluster centers of normalized data and transpose dataframe
clusterCentersNormalized = t(KmeansClustering$centers)
monthNumber = c(1:29)
clusterCentersNormalized = as.data.frame(cbind(monthNumber, clusterCentersNormalized))

# Plot the cluster centers/means for normalized data
ggplot() +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[2]], color="Cluster 1")) + 
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[2]], color="Cluster 1")) + 
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[3]], color="Cluster 2")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[3]], color="Cluster 2")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[4]], color="Cluster 3")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[4]], color="Cluster 3")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[5]], color="Cluster 4")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[5]], color="Cluster 4")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[6]], color="Cluster 5")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[6]], color="Cluster 5")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[7]], color="Cluster 6")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[7]], color="Cluster 6")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[8]], color="Cluster 7")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[8]], color="Cluster 7")) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Cluster Centres of Normalized Data for K-Means Clustering (K=7)") +
  xlab("Variables Used to Cluster (Month)")  +
  ylab("Variable's Mean Demand (Normalized) in the Cluster") +
  theme(legend.position="right") +
  scale_color_discrete(name = "Cluster #")

# Get means of clusters in original (not normalized) data
Feb_1 = tapply(dataSubset$Feb_1, KmeansClustering$cluster, mean)
Mar_1 = tapply(dataSubset$Mar_1, KmeansClustering$cluster, mean)
Apr_1 = tapply(dataSubset$Apr_1, KmeansClustering$cluster, mean)
May_1 = tapply(dataSubset$May_1, KmeansClustering$cluster, mean)
Jun_1 = tapply(dataSubset$Jun_1, KmeansClustering$cluster, mean)
Jul_1 = tapply(dataSubset$Jul_1, KmeansClustering$cluster, mean)
Aug_1 = tapply(dataSubset$Aug_1, KmeansClustering$cluster, mean)
Sep_1 = tapply(dataSubset$Sep_1, KmeansClustering$cluster, mean)
Oct_1 = tapply(dataSubset$Oct_1, KmeansClustering$cluster, mean)
Nov_1 = tapply(dataSubset$Nov_1, KmeansClustering$cluster, mean)
Dec_1 = tapply(dataSubset$Dec_1, KmeansClustering$cluster, mean)
Jan_2 = tapply(dataSubset$Jan_2, KmeansClustering$cluster, mean)
Feb_2 = tapply(dataSubset$Feb_2, KmeansClustering$cluster, mean)
Mar_2 = tapply(dataSubset$Mar_2, KmeansClustering$cluster, mean)
Apr_2 = tapply(dataSubset$Apr_2, KmeansClustering$cluster, mean)
May_2 = tapply(dataSubset$May_2, KmeansClustering$cluster, mean)
Jun_2 = tapply(dataSubset$Jun_2, KmeansClustering$cluster, mean)
Jul_2 = tapply(dataSubset$Jul_2, KmeansClustering$cluster, mean)
Aug_2 = tapply(dataSubset$Aug_2, KmeansClustering$cluster, mean)
Sep_2 = tapply(dataSubset$Sep_2, KmeansClustering$cluster, mean)
Oct_2 = tapply(dataSubset$Oct_2, KmeansClustering$cluster, mean)
Nov_2 = tapply(dataSubset$Nov_2, KmeansClustering$cluster, mean)
Dec_2 = tapply(dataSubset$Dec_2, KmeansClustering$cluster, mean)
Jan_3 = tapply(dataSubset$Jan_2, KmeansClustering$cluster, mean)
Feb_3 = tapply(dataSubset$Feb_2, KmeansClustering$cluster, mean)
Mar_3 = tapply(dataSubset$Mar_2, KmeansClustering$cluster, mean)
Apr_3 = tapply(dataSubset$Apr_2, KmeansClustering$cluster, mean)
May_3 = tapply(dataSubset$May_2, KmeansClustering$cluster, mean)
Jun_3 = tapply(dataSubset$Jun_2, KmeansClustering$cluster, mean)

clusterCenters = rbind(Feb_1, Mar_1, Apr_1, May_1, Jun_1, Jul_1, Aug_1, Sep_1, Oct_1, Nov_1,
                      Dec_1, Jan_2, Feb_2, Mar_2, Apr_2, May_2, Jun_2, Jul_2, Aug_2, Sep_2, 
                      Oct_2, Nov_2, Dec_2, Jan_3, Feb_3, Mar_3, Apr_3, May_3, Jun_3)
monthNumber = c(1:29)
clusterCenters = cbind(monthNumber, clusterCenters)
clusterCenters = as.data.frame(clusterCenters)
clusterCenters

# Plot the cluster centers/means for original (not normalized) data
ggplot() +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[2]], color="Cluster 1")) + 
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[2]], color="Cluster 1")) + 
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[3]], color="Cluster 2")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[3]], color="Cluster 2")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[4]], color="Cluster 3")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[4]], color="Cluster 3")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[5]], color="Cluster 4")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[5]], color="Cluster 4")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[6]], color="Cluster 5")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[6]], color="Cluster 5")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[7]], color="Cluster 6")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[7]], color="Cluster 6")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[8]], color="Cluster 7")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[8]], color="Cluster 7")) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Cluster Centres for K-Means Clustering (K=7)") +
  xlab("Variables Used to Cluster (Month)")  +
  ylab("Variable's Mean Demand in the Cluster") +
  theme(legend.position="right") +
  scale_color_discrete(name = "Cluster #")

# Elbow method
wss = sapply(1:15, function(k){ kmeans(clusterNorm, centers=k, nstart=25 )$tot.withinss })
plot(1:15, wss,
     type="b", pch = 19, 
     xlab="Number of Clusters K",
     ylab="Total Within-Clusters Sum of Squares",
     main="Elbow Plot")



# Apply K-means clustering with optimal number of clusters
KmeansClustering = kmeans(clusterNorm, centers=4, nstart=25) 

# Plot the clusters
clusplot(dataSubset, KmeansClustering$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 

# Number of SKUs in each cluster
table(KmeansClustering$cluster) 

# Number of SKUs in each cluster grouped by category
table(dataSubset$Category, KmeansClustering$cluster) 

# Number of SKUs in each cluster grouped by sub-category
table(dataSubset$Subcategory, KmeansClustering$cluster) 

# Number of SKUs in each cluster grouped by innovation
table(dataSubset$Innovation, KmeansClustering$cluster) 

# Get cluster centers of normalized data and transpose dataframe
clusterCentersNormalized = t(KmeansClustering$centers)
monthNumber = c(1:29)
clusterCentersNormalized = as.data.frame(cbind(monthNumber, clusterCentersNormalized))

# Plot the cluster centers/means for normalized data
ggplot() +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[2]], color="Cluster 1")) + 
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[2]], color="Cluster 1")) + 
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[3]], color="Cluster 2")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[3]], color="Cluster 2")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[4]], color="Cluster 3")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[4]], color="Cluster 3")) +
  geom_point(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[5]], color="Cluster 4")) +
  geom_line(data=clusterCentersNormalized, aes(x=monthNumber, y = clusterCentersNormalized[[5]], color="Cluster 4")) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Cluster Centres of Normalized Data for K-Means Clustering (K=4)") +
  xlab("Variables Used to Cluster (Month)")  +
  ylab("Variable's Mean Demand (Normalized) in the Cluster") +
  theme(legend.position="right") +
  scale_color_discrete(name = "Cluster #")

# Get means of clusters in original (not normalized) data
Feb_1 = tapply(dataSubset$Feb_1, KmeansClustering$cluster, mean)
Mar_1 = tapply(dataSubset$Mar_1, KmeansClustering$cluster, mean)
Apr_1 = tapply(dataSubset$Apr_1, KmeansClustering$cluster, mean)
May_1 = tapply(dataSubset$May_1, KmeansClustering$cluster, mean)
Jun_1 = tapply(dataSubset$Jun_1, KmeansClustering$cluster, mean)
Jul_1 = tapply(dataSubset$Jul_1, KmeansClustering$cluster, mean)
Aug_1 = tapply(dataSubset$Aug_1, KmeansClustering$cluster, mean)
Sep_1 = tapply(dataSubset$Sep_1, KmeansClustering$cluster, mean)
Oct_1 = tapply(dataSubset$Oct_1, KmeansClustering$cluster, mean)
Nov_1 = tapply(dataSubset$Nov_1, KmeansClustering$cluster, mean)
Dec_1 = tapply(dataSubset$Dec_1, KmeansClustering$cluster, mean)
Jan_2 = tapply(dataSubset$Jan_2, KmeansClustering$cluster, mean)
Feb_2 = tapply(dataSubset$Feb_2, KmeansClustering$cluster, mean)
Mar_2 = tapply(dataSubset$Mar_2, KmeansClustering$cluster, mean)
Apr_2 = tapply(dataSubset$Apr_2, KmeansClustering$cluster, mean)
May_2 = tapply(dataSubset$May_2, KmeansClustering$cluster, mean)
Jun_2 = tapply(dataSubset$Jun_2, KmeansClustering$cluster, mean)
Jul_2 = tapply(dataSubset$Jul_2, KmeansClustering$cluster, mean)
Aug_2 = tapply(dataSubset$Aug_2, KmeansClustering$cluster, mean)
Sep_2 = tapply(dataSubset$Sep_2, KmeansClustering$cluster, mean)
Oct_2 = tapply(dataSubset$Oct_2, KmeansClustering$cluster, mean)
Nov_2 = tapply(dataSubset$Nov_2, KmeansClustering$cluster, mean)
Dec_2 = tapply(dataSubset$Dec_2, KmeansClustering$cluster, mean)
Jan_3 = tapply(dataSubset$Jan_2, KmeansClustering$cluster, mean)
Feb_3 = tapply(dataSubset$Feb_2, KmeansClustering$cluster, mean)
Mar_3 = tapply(dataSubset$Mar_2, KmeansClustering$cluster, mean)
Apr_3 = tapply(dataSubset$Apr_2, KmeansClustering$cluster, mean)
May_3 = tapply(dataSubset$May_2, KmeansClustering$cluster, mean)
Jun_3 = tapply(dataSubset$Jun_2, KmeansClustering$cluster, mean)

clusterCenters = rbind(Feb_1, Mar_1, Apr_1, May_1, Jun_1, Jul_1, Aug_1, Sep_1, Oct_1, Nov_1,
                       Dec_1, Jan_2, Feb_2, Mar_2, Apr_2, May_2, Jun_2, Jul_2, Aug_2, Sep_2, 
                       Oct_2, Nov_2, Dec_2, Jan_3, Feb_3, Mar_3, Apr_3, May_3, Jun_3)
monthNumber = c(1:29)
clusterCenters = cbind(monthNumber, clusterCenters)
clusterCenters = as.data.frame(clusterCenters)
clusterCenters

# Plot the cluster centers/means for original (not normalized) data
ggplot() +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[2]], color="Cluster 1")) + 
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[2]], color="Cluster 1")) + 
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[3]], color="Cluster 2")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[3]], color="Cluster 2")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[4]], color="Cluster 3")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[4]], color="Cluster 3")) +
  geom_line(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[5]], color="Cluster 4")) +
  geom_point(data=clusterCenters, aes(x=monthNumber, y = clusterCenters[[5]], color="Cluster 4")) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Cluster Centres for K-Means Clustering (K=4)") +
  xlab("Variables Used to Cluster (Month)")  +
  ylab("Variable's Mean Demand in the Cluster") +
  theme(legend.position="right") +
  scale_color_discrete(name = "Cluster #")
