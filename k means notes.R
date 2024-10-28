# Need to standardize since Euclidean distance

set.seed(1)
grade = read.csv("grades_km_input.csv")
head(grade)

# to identify suitable k value, plot out graphs
plot(grade[,2:4]) # look at individual graphs, can see around 3 to 4 clusters

kout <- kmeans(grade[,c("English", "Math", "Science")], centers = 3)
# pick relevant columns ^

# plot out the 2d graphs
plot(grade$English, grade$Math, col = kout$cluster)
plot(grade$English, grade$Science,  col = kout$cluster)
plot(grade$Math, grade$Science, col = kout$cluster)

# determine how close each points are --> use $withinss
kout$withinss

# Plot $withinss vs K to pick best K:
K = 15
wss <- numeric(K)
for (k in 1:K){
  wss[k] <- sum(kmeans(grade[,c("English", "Math", "Science")], centers = k)$withinss)
}
plot(1:K, wss, col = "blue", xlab = "Number of Clusters" , ylab = "Within Sum of squares", type = 'b') #type = b to connect dots  
wss
# Comments:
# WSS is greatly reduced when $k$ increases from 1 to 2. 
# Another substantial reduction in WSS occurs at $k = 3$.
# However, the improvement in WSS is fairly linear for $k > 3$.
# Therefore, the $k$-means analysis will be conducted for $k = 3$.
# The process of identifying the appropriate value of k is
# referred to as finding the ``elbow'' of the WSS curve

# functions:
kout$cluster # A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
kout$centers # A matrix of cluster centres.
kout$size # The number of points in each cluster.
kout$withinss # Vector of SS_k, one value per cluster
kout$tot.withinss # Total within-cluster sum of squares = WSS




