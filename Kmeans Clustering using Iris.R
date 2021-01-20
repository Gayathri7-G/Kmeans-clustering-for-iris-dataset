#From the given 'Iris' dataset, predict the optimum number of clusters and represent it visually.
#1. Loading the Iris dataset
datasets::iris #to load iris dataset
str(iris) #to view structure of dataset
summary(iris) #statiscal summary of the dataset
head(iris) #to view top rows of dataset

#2. Preprocessing of data- Since the clusterng is an unsupervised learning method
#we will not require class label output during execution of algorithm and hence remove
#Class attribute 'Species'and store in another variable and normalize attributes between 0 and 1 
#using our own function

iris.newset<-iris[,c(1,2,3,4)]
iris.classset<-iris[,"Species"]
head(iris.newset)
head(iris.classset)

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
  }

iris.newset$Sepal.Length<-normalize(iris.newset$Sepal.Length)
iris.newset$Sepal.Width<-normalize(iris.newset$Sepal.Width)
iris.newset$Petal.Length<-normalize(iris.newset$Petal.Length)
iris.newset$Petal.Width<-normalize(iris.newset$Petal.Width)
head(iris.newset)


#3.Apply kmeans clustering algorithm
result<-kmeans(iris.newset,3) #kmeans with no.of centroids (k)=3
result$size #gives no of records in each cluster
result$cluster #gives cluster vector
result$withinss #gives within cluster sum of squares
result$tot.withinss #gives Total within-cluster sum of squares
result$centers #gives cluste center data pt value (3 centers for k)
result$iter #no of times iteration repeats



#4. CLustering verification

par(mfrow=c(2,2), mar=c(5,4,2,2))
par("mar")
plot(iris.newset[c(1,2)],col=result$cluster)
plot(iris.newset[c(1,2)],col=result$cluster) # Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.newset[c(1,2)],col=iris.classset) # Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.newset[c(3,4)],col=result$cluster) # Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.newset[c(3,4)],col=iris.classset)
table(result$cluster,iris.classset)

#REsults show cluster1= setosa; cluster2= Virginica; cluster3= Versicolor
#Total number of correctly classified instances are: 50 + 47 + 36= 133
#Total number of incorrectly classified instances are: 14 + 3= 17
#Accuracy = 133/(133+17) = 0.88 i.e our model has achieved 88% accuracy.

