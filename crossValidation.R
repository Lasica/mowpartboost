library(caret)
source("myxgb.R")
rsquared = function (corrValue, predictValue){
  r2 <- cor(corrLab, predictLab ) ^ 2
}
crossValModels = function(frm, dataset, kfolds)
{
  fterms <- terms(frm, data=dataset)
  outdata <- as.matrix(dataset[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])])
  folds <- createFolds(outdata, k=kfolds, list = TRUE, returnTrain = FALSE)

  models_list <- list()
  
  for (i in  1:kfolds)
  {
      myxgb_model <- myxgb$new()
      
      
      myxgb_model$fit(frm, dataset[-folds[[i]],], 10)
      models_list <- c(models_list, myxgb_model)
      #browser()
  }
  return(models_list)
}

crossValAnalysis = function(frm, dataset, models)
{
  fterms <- terms(frm, data=dataset)
  outdata <- as.matrix(dataset[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])])
  validation_crossrmse    <- numeric()
  training_crossrmse  <- numeric()
  validation_predicts <- numeric()
  training_predicts   <- numeric()
  for (i in 1:length(models))
  {
    myxgb_model <- models[[i]]
    
    validation_predicts <- (myxgb_model$predict(frm, dataset[folds[[i]],]))
    error <- sum((outdata[folds[[i]]] - validation_predicts)^2)
    validation_crossrmse <- c(validation_crossrmse, sqrt(error/dim(dataset[-folds[[i]],])[1]))
    
    
    training_predicts <- (myxgb_model$predict(frm, dataset[-folds[[i]],]))
    error <- sum((outdata[-folds[[i]]] - training_predicts)^2)
    training_crossrmse <- c(training_crossrmse, sqrt(error/dim(dataset[-folds[[i]],])[1]))
  }
  best_model <- models_list[which.min(crossrmse)]
  plot(validation_crossrmse, color='red')
  line(training_crossrmse, color='green')
}