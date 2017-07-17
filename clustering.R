# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages("cluster")
install.packages("NbClust")
install.packages("RGtk2", depen=T, type="source")
install.packages("rattle", dependencies = T)


library(cluster)
library(rattle)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
View(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine2 <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
}

wssplot(wine2)

# Exercise 2:
#   * How many clusters does this method suggest?
#       Since the graph seems to bend at around 3 clusters, this method would suggest 3 clusters.

#   * Why does this method work? What's the intuition behind it?
#         This method works because it shows the degree to which adding more clusters reduces the 
#         sum of squares within each cluster. A lower sum of squares within the cluster means that
#         the cluster is more coherent and the data points have less deviation from the mean centroid.
#         With a sharp decrease of wss up until 3 clusters, and a lesser decrease afterwards, there is 
#         good reason to believe that there are 3 distinct groups that can be analyzed, and that adding
#         more clusters afterwards is not so useful (even though the wss will be marginally lower).

#   * Look at the code for wssplot() and figure out how it works
#         The code plots a function that specifies a maximum number of clusters and sets a seed for
#         reproducible consistency. 
#         The next part of the function defines the formula for calculating the within-group sums of
#         squares for each number of clusters allowed by the equation.
#         The following part codes for the actual k-means method of assigning clusters.
#         The last part charts the graph and assigns the labels.
#         





# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine2, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#             This method suggests 3 clusters.







# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine2, centers = 3, iter.max = 1000, nstart = 25)
str(fit.km)
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster)
table(wine$Type)

#It seems like the cluster sizes range from 51 to 65, while the actual wine type categories have
# slightly more variance in their sizes, from 48 to 71. 

# I would consider this a decent, but not ideal clustering based on the numbers.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(clara(wine2, 3))
clusplot(clara(wine, 3))
clusplot(pam(wine2, 3))
clusplot(pam(wine, 3))

#Based on the visualization, the clusters from the test dataset (with wine$Type removed) seem 
# even more distinct than the actual categories of wine type from the original set.

# From the way I understand this visualization and the way I plotted it, I believe this 
# is a good clustering if the goal is to categorize wine based on the variables listed in the table
# and possibly modify the way that wine types are categorized.