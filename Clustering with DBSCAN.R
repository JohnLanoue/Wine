library(caret)
wine <- read.csv("wine.csv")
colnames(wine) <- c("Cultivator","Alcohol", "MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorIntensity","Hue","NOD280OD315OfDilutedWines","Proline")

set.seed(64)
partition <- createDataPartition(wine$Cultivator, p = .85, 
                                  list = FALSE, 
                                  times = 1)
wine_train <- wine[partition,]
wine_test <- wine[-partition,]
wine_train_dim2 <- wine_train[3:4]

plot(wine_train_dim2)


library(dbscan)
dbscan <- dbscan(wine_train_dim2, eps = .2, minPts = 3)
dbscan
dbscan$cluster


plot(wine_train_dim2, main= "DBSCAN", col = dbscan$cluster)


library(stats)
# Compute the distance matrix
dist_matrix <- dist(wine_train_dim2)

# Perform hierarchical clustering
hc <- hclust(dist_matrix)
cut_hc <- cutree(hc, k = 3)

plot(wine_train_dim2)
rect.hclust(hc , k = 2, border = 2:6)
rect.hclust(hc , k = 3, border = 2:6)
rect.hclust(hc , k = 4, border = 2:6)
rect.hclust(hc , k = 5, border = 2:6)
rect.hclust(hc , k = 6, border = 2:6)
rect.hclust(hc , k = 7, border = 2:6)
abline(h = 5, col = 'red')


#ggplot(cut_hc, aes(x=area, y = perimeter, color = factor(cluster))) + geom_point()
#seeds_df_cl <- mutate(wine, cluster = hc)
