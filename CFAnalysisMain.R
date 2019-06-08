library(rpart)
library(rpart.plot)
library(caret)

f <- function(y, yi) {0.5*(y-yi)^2}
#przygotowanie danych do obr?bki
source("pRepare_Energy_data.R")

#definicja funkcji implementuj?cej boosting
source("OurGradBoost.R")

# Alan - WTF to na dole?
source("myxgb.R")
myxgb_model <- myxgb$new()
myxgb_model$fit(formula(lights ~ . - Appliances, data=tset), tset, 10, rpart.control(maxdepth = 3)) # dziala tak samo
rpart.plot(myxgb_model$models[[1]])
plot(myxgb_model$rmse)
plot(myxgb_model$weights)
plot(rsquared(tlab2, myxgb_model$predict(tset[,c(-1,-2)])))
#sprawdzenie w por?wnaniu z xgboostem
source("xgboost_implement.R")


##walidacja krzyzowa
source("myxgb.R")
folds <- createFolds(tlab2, k=10, list = TRUE, returnTrain = FALSE)
crossrmse    <- numeric()
predicts <- numeric()
models_list <- list()
for (i in  1:10){
  myxgb_model <- myxgb$new()
  myxgb_model$fit(formula(lights ~ . - Appliances, data=tset[-folds[[i]],]), tset[-folds[[i]],], 10, rpart.control(maxdepth = 3))
  predicts <- (myxgb_model$predict(lights ~ . - Appliances, tset[folds[[i]],]))
  models_list <- c(models_list, myxgb_model)
  error <- sum((tlab2[folds[[i]]] - predicts)^2)
  crossrmse <- c(crossrmse, sqrt(error/dim(tset[-folds[[i]],])[1]))
}
best_model_lights <- models_list[which.min(crossrmse)]
folds <- createFolds(tlab1, k=10, list = TRUE, returnTrain = FALSE)
crossrmse    <- numeric()
predicts <- numeric()
models_list <- list()
for (i in  1:10){
  myxgb_model <- myxgb$new()
  myxgb_model$fit(formula(Appliances ~ . - lights, data=tset[-folds[[i]],]), tset[-folds[[i]],], 10, rpart.control(maxdepth = 3))
  predicts <- (myxgb_model$predict(tset[folds[[i]],c(-1,-2)]))
  models_list <- c(models_list, myxgb_model)
  error <- sum((tlab1[folds[[i]]] - predicts)^2)
  crossrmse <- c(crossrmse, sqrt(error/dim(tset[-folds[[i]],])[1]))
}
best_model_appliances <- models_list[which.min(crossrmse)]
plot(crossrmse)
##
