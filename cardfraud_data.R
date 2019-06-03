rm(list=ls())
#setwd("/Users/arek/Documents/Studia/MGR/MOW/Projekt/od AD/mow/")
setwd("C:/Studia MGR/Semestr II/MOW/mowpartboostmain/mowpartboost")
cfdata = read.csv("C:/Studia MGR/Semestr II/MOW/mowpartboostmain/mowpartboost/datasets/cardfraud/creditcard.csv")
tlab <- cfdata$Class
tset <- cfdata[1:30]

library(rpart)
library(rpart.plot)
loss_matr <- matrix(c(0, sum(tlab), length(tlab) - sum(tlab), 0), nrow = 2, byrow = TRUE)
tree <- rpart(tlab ~ ., data=tset, method="anova", parms = list(loss = loss_matr))
rpart.plot(tree)
# maxtree <- rpart(tlab ~ ., data=tset, method="anova", cp = -1)
predictions <- rpart.predict(tree, tset)
error <- `^`(tlab - predictions, 2)
summary(error)

#gradient boosting
myxgb <- function(indata, outdata, niter) {# interfejs formuly
  models <- 1
  weights <- c(mean(outdata))
  f <- function(y, yi) {0.5*(y-yi)^2}
  result_so_far <- rep(weights[1], length(outdata))
  error <- sum((outdata - result_so_far)^2)
  loss_matr <- matrix(c(0, 55, 1, 0), nrow = 2, byrow = TRUE) # DO POPRAWIENIA/OPANOWANIA
  
  for (i in  1:niter) {
    residuum <- outdata - result_so_far
  # bez macierzy kar
    # models <- c(models, list(rpart(residuum ~ ., data=indata, method="anova"))) # poprawic
  # z macierza kar
    models <- c(models, list(rpart(residuum ~ ., data=indata, method="anova", parms = list(loss = loss_matr))))
    model_result <- rpart.predict(models[[length(models)]], tset)
    new_weight <- sum(model_result *(result_so_far + outdata))/sum(model_result*model_result) # eksperyment - wprowadzic 1
    if (sum(model_result^2) < 0.001) {
      new_weight <- 0.0
    }
    weights <- c(weights, new_weight)
    result_so_far <- result_so_far + new_weight * model_result
    error <- c(error, sum(f(outdata, result_so_far)))
  }
  model <- list("m" = models, "w" = weights, "e" = error)
  #wynik - list z models i weights
}

model <- myxgb(tset, tlab, 10)
plot(model$e)
sum(f(tlab1, rpart.predict(rpart(tlab1 ~ ., tset), tset))) # blad referencyjny

##walidacja krzy¿owa
folds <- createFolds(tlab, k=5, list = TRUE, returnTrain = FALSE)
source("PredictionF.R")
crossrmse    <- numeric()
predicts <- numeric()
models_list <- list()
for (i in  1:5){
  myxgb_model <- myxgb(tset[-folds[[i]],], tlab[-folds[[i]]], 5)
  predicts <- predict(myxgb_model,tset[folds[[i]]]) 
  models_list <- c(models_list, myxgb_model)
  error <- sum((tlab[folds[[i]]] - predicts)^2)
  crossrmse <- c(crossrmse, sqrt(error/dim(tset[-folds[[i]],])[1]))
}
best_model_cardFraud <- models_list[which.min(crossrmse)]
##walidacja krzy¿owa




