library(caret)
library(rpart)
library(randomForest)
library(mlr)
library(xgboost)

wine <- read.csv("wine.csv")
colnames(wine) <- c("Cultivator","Alcohol", "MalicAcid","Ash","AlcalinityOfAsh","Magnesium","TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins","ColorIntensity","Hue","NOD280OD315OfDilutedWines","Proline")

set.seed(64)
partition <- createDataPartition(wine$Cultivator, p = .85, 
                                  list = FALSE, 
                                  times = 1)
wine_train <- wine[partition,]
wine_test <- wine[-partition,]

wine_tree <- rpart(Cultivator ~ ., data = wine_train, method = "class" )
print(wine_tree)

#For building the decision tree 
d.tree.params <- makeClassifTask(
  data=wine_train, 
  target="Cultivator"
)

# Define Grid
control_grid = makeTuneControlGrid()

param_grid <- makeParamSet( 
  makeDiscreteParam("maxdepth", values=20:50))

# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)


# Define Measure
measure = acc

dt_tuneparam <- tuneParams(learner="classif.rpart", 
                           task=d.tree.params, 
                           resampling = resample,
                           measures = measure,
                           par.set=param_grid, 
                           control=control_grid, 
                           show.info = TRUE)

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam, partial.dep = TRUE)
best_parameters = setHyperPars(
  makeLearner("classif.rpart"), 
  par.vals = dt_tuneparam$x
)

d.tree.mlr.test <- makeClassifTask(
  data=wine_test, 
  target="Cultivator"
)


###Random Forest
predictors <- wine_train[, -wine_train$Cultivator]
wine_rf <-  randomForest(
  formula = Cultivator ~ .,
  data = wine_train
)
options(warn = -1) 
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
mtry <- 3
rf_default <- caret::train(Cultivator ~ ., data=wine_train, method="rf", metric="RMSE", tuneGrid=expand.grid(.mtry=mtry), trControl=control)
print(rf_default)


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
  x = wine_train[2:14],
  y= wine_train$Cultivator,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid_1,
  method="xgbLinear"
)
#Results
print(xgb_train_1)


#XG
y_xg_pred <- round(predict(xgb_train_1,wine_test[2:14]))
confusionMatrix(as.factor(y_xg_pred), as.factor(wine_test$Cultivator))


#RF 
rf_predict <- round(predict(rf_default, wine_test[2:14]))
confusionMatrix(as.factor(rf_predict), as.factor(wine_test$Cultivator))




