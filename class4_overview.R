## Recap

# Class 1 - Data manipulation
# Class 2 - Regression
# Class 3 - Classification
# Class 4 - Unsuperivised learning




############################
# Prerequisites

install.packages(c("animation", "arules", "corrplot", "plotly"))


############################
# Clustering
# K means Clustering

library(animation)
library(tidyverse)
X1 <- runif(50)
X2 <- runif(50)

set.seed(500)
# Animation showing the mechanism of kmeans
kmeans.ani(x = cbind(X1 = X1, X2 = X2), centers = 3, 
           hints = c("Move centers!", "Find cluster?"), pch = 1:3, col = 1:3)


## K means in action

data(iris)

View(iris)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) + geom_point()

# apply kmeans
(cl <- kmeans(iris[c("Petal.Length","Petal.Width")], 3, nstart=50))

# Lets plot it 
iris %>% cbind(Cluster=cl$cluster) %>% select(Species, Cluster, Petal.Length, Petal.Width) %>%
  mutate(SpeciesXCluster=paste(Species, Cluster))-> iris_enriched

iris_enriched %>% select(Species, Cluster) %>% table()

ggplot(iris_enriched, aes(x=Petal.Width, y=Petal.Length, color=SpeciesXCluster)) + geom_point()

## Curse of dimensionality
# Adding more variables might not neccessarily improve clustering quality

(cl <- kmeans(iris[c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width")], 3, nstart=50))

iris %>% cbind(Cluster=cl$cluster) %>% select(Species, Cluster, Petal.Length, Petal.Width) %>%
  mutate(SpeciesXCluster=paste(Species, Cluster))-> iris_enriched

iris_enriched %>% select(Species, Cluster) %>% table()

ggplot(iris_enriched, aes(x=Petal.Width, y=Petal.Length, color=SpeciesXCluster)) + geom_point()

##########################
# Exploratory analysis

# The Sepal variables have less explanatory power
ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) + geom_point()

# Lets plot 3D too
library(plotly)
p <- plot_ly(iris, x = ~Petal.Width, y = ~Petal.Length, z = ~Sepal.Width, color = ~Sepal.Length, text=~Species) %>%
  add_markers() %>%
  add_text()
p

#######################
# Dimensionality Reduction
# Lets use PCA to find out the projection with the most variance
## PCA

iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% prcomp() -> pca_result

summary(pca_result) # the first two dimension explains most of the variance (97%)
plot(pca_result)
biplot(pca_result)


# Can also plot in ggplot
predict(pca_result, iris) %>% as.data.frame() %>% select(PC1, PC2) %>% cbind(iris) -> iris_with_pca
ggplot(iris_with_pca, aes(x=PC1, y=PC2, color=Species)) + geom_point()


## Further Reading
# http://setosa.io/ev/principal-component-analysis/


####################################
# Hierarchical Clustering
# hclust

# hclust works by recursively joining together nearby observations, starting from the nearest pair of observations
clusters <- hclust(dist(iris[,3:4]), method="average")
plot(clusters, labels=iris$Species, hang=-1)

iris %>% mutate(hclusters =cutree(clusters, 3),
                hclustersXSpecies = paste(hclusters, Species)
                ) -> iris_enriched
iris_enriched %>% select(hclusters, Species) %>% table()

ggplot(iris_enriched, aes(x=Petal.Length, y=Petal.Width, color=hclustersXSpecies)) +
  geom_point()



## hclust is often applied in correlation plot to reveal interesting pattern
# correlation plot

library(corrplot)
M <- cor(mtcars)
corrplot(M, method="circle")

hclust(dist(cor(M))) %>% plot()

corrplot(M, method="circle", order="hclust", addrect = 3)


##################################
## Association Rules

library(arules)

titanic_df <- read.csv("titanic.csv")


# Select those discrete variables and discover their relationship
titanic_df %>% select(pclass, survived, embarked, parch, sibsp, sex) %>% 
  mutate(pclass=as.factor(pclass),
         survived=as.factor(survived), 
         sibsp=as.factor(sibsp),
         parch=as.factor(parch)
         ) %>% apriori() -> rules

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10))
sort(inspect(rules), by=order('lift'))


#############
# Anomaly detection (Detect outliers)
library(mvoutlier)

data(iris)
# Assume a normally distributed set of variables, are there any abnormal observations (outliers?)
(outliers <- aq.plot(iris[,1:4]))

