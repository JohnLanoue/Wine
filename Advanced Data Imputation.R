wine <- read.csv("wine.csv")
library(caret)
library(randomForest)
library(zoo)
colnames(wine) <- c("Cultivator","Alcohol", "MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorIntensity","Hue","NOD280OD315OfDilutedWines","Proline")

set.seed(64)
partition <- createDataPartition(wine$Cultivator, p = .85, 
                                  list = FALSE, 
                                  times = 1)
wine_train <- wine[partition,]
wine_test <- wine[-partition,]

wine2 <- lapply(names(wine_train), function(col) {
  x <- wine_train[[col]]
  if (col != "Cultivator") {
    x[sample(seq_along(x), length(x) / 10)] <- NA
  }
  x
})

# Recombine the list into a data.frame
wine2 <- as.data.frame(wine2)
colnames(wine2) <- colnames(wine_train)


wine2



wine2$Cultivator <- na.locf(wine2$Cultivator)

wine.mean.imputed <- as.data.frame(wine2)
for(i in 1:ncol(wine.mean.imputed)) {
  wine.mean.imputed[ , i][is.na(wine.mean.imputed[ , i])] <- mean(wine.mean.imputed[ , i], na.rm=TRUE)
}

wine.rough.imputed <- na.roughfix(Cultivator ~., wine2)
wine.rf.imputed <- rfImpute(Cultivator ~., wine2)

xgboost <- function(train_data){
  
  xgb_grid_1 <- expand.grid(
    nrounds= 2400,
    eta=c(0.01,0.001,0.0001),
    lambda = 1,
    alpha =0
  )
  
  xgb_trcontrol <- trainControl(
    method="cv",
    number = 5,
    verboseIter = TRUE,
    returnData=FALSE,
    returnResamp = "all",
    allowParallel = TRUE,
    
  )
  
  xgb_train_1 <- caret::train(
    x = train_data[2:14],
    y= train_data$Cultivator,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid_1,
    method="xgbLinear"
  )
  #XG
  y_xg_pred <- round(predict(xgb_train_1,wine_test[2:14]))
  confusionMatrix(as.factor(y_xg_pred), as.factor(wine_test$Cultivator))
}

xgboost(train_data = wine.rf.imputed)
xgboost(train_data = wine.mean.imputed)
xgboost(train_data = wine_train)
