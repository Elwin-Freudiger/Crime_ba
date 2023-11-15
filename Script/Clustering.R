library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(cluster)
library(plotly)

#Load the dataset
Dep_data <- read_csv(here::here("data_end/Everything_by_dep.csv"))
View(Dep_data)

Data_to_cluster <- Dep_data[c(3, 4, 6, 7, 8, 9, 10, 11)]
Data_to_cluster
#distance matrix
Dep_dist <- dist(Dep_data[c(3, 4, 6, 7, 8, 9, 10, 11)], method= "euclidean")
Dep_dist

#build dendrogram
Dep_completelink <- hclust(Dep_dist, method = 'average')
summary(Dep_completelink)
plot(Dep_completelink)


#look who belong to what
Cluster_numbers <- cutree(Dep_completelink, k= 3)
sum(Cluster_numbers)

#add the data to the df
data_add_cluster <- cbind(Dep_data, Cluster_numbers)

#plot the data with unemployment and crime, add colors for clusters
ggplot(data_add_cluster, aes(x = Unemp_2019, y = Crime_rate_1k, color = factor(Cluster_numbers))) + geom_point()


#Run K-means

withins <- vector()

#scree plot for k-means
for (i in 1:15) {
  kclust <- kmeans(Data_to_cluster, centers = i, nstart = 25)
  withins[i] <- kclust$tot.withinss
}

#plot the scree plot
ggplot(mapping = aes(x = 1:15, y = withins)) + 
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  geom_vline(xintercept = 5, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Optimal number of clusters", x= "Number of clusters", y = "Total Within Sum Squares")

#same thing but with scaled data? 


#run the kmeans with 5 clusters
view(Data_to_cluster)
kcluster <- kmeans(Data_to_cluster, centers = 5, nstart = 25)
clust_number <- kcluster$cluster

#add what each observation belong to what
data_with_clust <- cbind(Dep_data, clust_number)

cluster_plot <- ggplot(data_with_clust, aes(x = Unemp_2019, y = Crime_rate_1k, color = factor(clust_number))) + 
  geom_point() + 
  geom_text(label= data_with_clust$Dep_name, nudge_x = -0.5, nudge_y = 0.25, check_overlap = TRUE) +
  labs(title = "Visualization of cluster by mapping Unemployement and Crime rate", x = "Unemployment", y = "Crime rate")

cluster_plot

cluster_plot + facet_wrap(~clust_number, nrow = 2) 







