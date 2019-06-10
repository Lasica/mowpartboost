library(caret)
library(ggplot2)
source("myxgb.R")
rsquared = function (corrValue, predictValue){
  r2 <- cor(corrValue, predictValue ) ^ 2
}

crossValFolds = function(frm, dataset, kfolds)
{
  fterms <- terms(frm, data=dataset)
  outdata <- as.matrix(dataset[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])])
  folds <- createFolds(outdata, k=kfolds, list = TRUE, returnTrain = FALSE)
}

crossValModels = function(frm, dataset, folds, fit = 10, control = rpart.control())
{
  models_list <- list()
  
  for (i in  1:length(folds))
  {
      print(i)  
      print('myxgb$new')
      myxgb_model <- myxgb$new()
      print('myxgb_model$fit')
      myxgb_model$fit(frm, dataset[-folds[[i]],], fit, control)
      print('models_list')
      models_list <- c(models_list, myxgb_model)
  }
  return(models_list)
}

crossValAnalysis = function(frm, dataset, folds, models, title)
{
  fterms <- terms(frm, data=dataset)
  outdata <- as.matrix(dataset[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])])
  validation_crossrmse    <- numeric()
  training_crossrmse  <- numeric()
  
  validation_r2 <- numeric()
  training_r2   <- numeric()
  
  validation_predicts <- numeric()
  training_predicts   <- numeric()
  for (i in 1:length(models))
  {
    myxgb_model <- models[[i]]
    
    validation_predicts <- (myxgb_model$predict(frm, dataset[folds[[i]],]))
    error <- sum((outdata[folds[[i]]] - validation_predicts)^2)
    validation_crossrmse <- c(validation_crossrmse, sqrt(error/dim(dataset[-folds[[i]],])[1]))
    validation_r2 <- c(validation_r2, rsquared(outdata[folds[[i]]], validation_predicts))
    
    training_predicts <- (myxgb_model$predict(frm, dataset[-folds[[i]],]))
    error <- sum((outdata[-folds[[i]]] - training_predicts)^2)
    training_crossrmse <- c(training_crossrmse, sqrt(error/dim(dataset[-folds[[i]],])[1]))
    training_r2 <- c(training_r2, rsquared(outdata[-folds[[i]]], training_predicts))
  }
  
  plot(validation_crossrmse, col='red', ylab = '', xlab = '' )
  title(main = paste("RMSE", title, sep = " "), ylab = 'RMSE', xlab = 'Model')
  grid(col='black')
  par(new=T)
  plot(training_crossrmse, pch='x', col='green', xlab = '', ylab = '', axes = F)
  legend("topright",legend = c("Dane walidacyjne", "Dane treningowe"), bty = 'n', col = c("red", "green"),  
         pch = c('o','x'), cex=0.6)
  
  plot(validation_r2, col='red', ylab = '', xlab = '' )
  title(main = paste("R kwadrat", title, sep = " "), ylab = 'R^2', xlab = 'Model')
  grid(col='black')
  par(new=T)
  plot(training_r2, pch='x', col='green', xlab = '', ylab = '', axes = F)
  legend("topright",legend = c("Dane walidacyjne", "Dane treningowe"), bty = 'n', col = c("red", "green"),  
         pch = c('o','x'), cex=0.6)
  
  # Wyrysowanie krzywej ROC - do poprawienia - który wektor do predykcji?
  roc.estimate <- calculate_roc(validation_predicts, dataset$Class) # Do zmiany dla pozostałych zestawów danych
  single.rocplot <- ggroc(roc.estimate)
  plot_journal_roc(single.rocplot)
}

crossValAnaliseModels = function(frm, dataset, folds, models, title, subtitle)
{
  fterms <- terms(frm, data=dataset)
  outdata <- as.matrix(dataset[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])])
  
  title_locRMSE <- paste("RMSE dla kolejnych drzew modelu", title, "model", sep = " " )
  title_Weights <- paste("Wagi dla kolejnych drzew modelu", title, "model", sep = " " )
  
  for (i in 1:length(models))
  {
    myxgb_model <- models[[i]]
    
    plot(myxgb_model$rmse, ylab='', xlab='',col='red')
    grid(col='black')
    title(main = paste(title_locRMSE, i), sub= subtitle,
          ylab='RMSE', xlab = 'Iteracja')
    
    plot(myxgb_model$weights, ylab='', xlab='', col='red')
    grid(col='black')
    title(main = paste(title_Weights, i), sub= subtitle,
          ylab='Wagi', xlab = 'Model')
  
    
  }
  
}  
