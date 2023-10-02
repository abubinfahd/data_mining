##Iris Data
data("iris")
head(iris)
View(iris)
attach(iris)
table(Species)

# install packages
# install.packages("ggplot2")
# install.packages("fpc")
# install.packages("cluster")
# install.packages("factoextra")

# load libraries
library(ggplot2)
library(cluster)
library(fpc)
library(factoextra)

#ggplot(data=iris)
ggplot(data=iris, aes(y=Sepal.Length,x=Petal.Length, col=Species, shape=Species))+
  geom_point()


#k-means clustering
mycluster<-kmeans(iris[,-5],centers=3)
plotcluster(iris[,-5],mycluster$cluster)
clusplot(iris[,-5],mycluster$cluster, color=T, shade=T, lines = 0, labels = 0)



fviz_cluster(mycluster, data = iris[,-5], palette=c("red","blue","orange"),
             geom = "point",ellipse.type = "euclid", 
             ggtheme =theme_bw())

##Noise
table(mycluster$cluster, iris$Species)

#Determining the number of cluster
#scree plot method
# totalwSS<-c()
# for (i in 1:15) {clusteriris<-kmeans(iris[,-5],centers=i)
# totalwSS<-clusteriris$tot.witinss}
# 
# plot(x=1:15, y=totalwSS, 
#      type= "b", xlab = "number of clusters", 
#      ylab="within group sum-of-squares")


# Initialize an empty vector to store totalwSS
totalwSS <- c()

# Loop through different numbers of clusters
for (i in 1:15) {
  clusteriris <- kmeans(iris[, -5], centers = i)
  totalwSS <- c(totalwSS, clusteriris$tot.withinss)  # Store the wSS in the vector
}

# Create a scree plot
plot(x = 1:15, y = totalwSS, 
     type = "b", xlab = "Number of clusters", 
     ylab = "Within-group sum-of-squares",
     main = "Scree Plot for K-means Clustering")


newcluster<-kmeans(iris[,-5],centers=6)
fviz_cluster(newcluster, data = iris[,-5], palette=c("red","blue","green","yellow","pink","black"),
             geom = "point",ellipse.type = "convex", 
             ggtheme =theme_bw())

##center checking of cluster
newcluster1<-kmeans(iris[,-5],centers=6)
newcluster1$centers
