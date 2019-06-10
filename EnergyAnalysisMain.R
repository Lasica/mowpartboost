library(rpart)
library(rpart.plot)
library(caret)

f <- function(y, yi) {0.5*(y-yi)^2}
rsquared = function (corrLab, predictLab){
  r2 <- cor(corrLab, predictLab ) ^ 2
}
#przygotowanie danych do obr?bki
source("pRepare_Energy_data.R")

#eksperymenty
maxdpt = 3
iter = 10
subtitle = paste('Maks. glebokosc:', maxdpt, 'Liczba iteracji', iter)
source("myxgb.R")
form <- lights ~ . - Appliances
myxgb_model <- myxgb$new()
myxgb_model$fit(form, tset, iter, rpart.control(maxdepth = maxdpt)) # dziala tak samo
#rpart.plot(myxgb_model$models[[1]])

plot(myxgb_model$rmse)
title(main = "RMSE dla kolejnych drzew dla energii z urzadzen", sub= subtitle,
      ylab='RMSE', xlab = 'Model')
plot(myxgb_model$weights)
title(main = "Wagi dla kolejnych drzew dla energii z urzadzen", sub= subtitle, ylab='Waga', xlab = 'Model')
plot(rsquared(tset[1], myxgb_model$predict(form, tset)))
title(main = "R^2 dla predykcji", sub= subtitle, ylab='R^2', xlab = 'Model')
#eksperymenty

#sprawdzenie w por?wnaniu z xgboostem
source("xgboost_implement.R")


##walidacja krzyzowa

source("crossValidation.R")
maxdpt = 5#DO ZMIANY DO EKSPERYMENTU
iter = 8#DO ZMIANY DO EKSPERYMENTU
subtitle = paste('Maks. glebokosc:', 'nieustalona', 'Liczba iteracji', iter)
EnergyTitle = paste("dla danych o zu¿yciu energii")#DO ZMIANY DO EKSPERYMENTU
form <- lights ~ . - Appliances#DO ZMIANY DO EKSPERYMENTU
folds <- crossValFolds(form, tset, 2)#DO ZMIANY DO EKSPERYMENTU
#models <- crossValModels(form, tset, folds, iter, rpart.control(maxdepth = maxdpt))
models <- crossValModels(form, tset, folds, iter)
crossValAnalysis(form, tset, folds, models, EnergyTitle, subtitle)
crossValAnaliseModels(form, tset, folds, models, EnergyTitle, subtitle)
##

## porównanie xbgoostem
library("xgboost")
fterms <- terms(form, data=tset)
outdata <- as.matrix(tset[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])])
indata <- model.matrix(fterms, data=tset)
#2,3,5
maxdpt = 5#DO ZMIANY DO EKSPERYMENTU
iter = 64#DO ZMIANY DO EKSPERYMENTU
subtitle = paste('Maks. glebokosc:',maxdpt, 'Liczba iteracji', iter)
bst5 <- xgboost(data = as.matrix(indata[folds[[1]],]), label = outdata[folds[[1]],], max.depth = 5, eta = 1, nrounds = iter, 
               nthread = 2)
bst3 <- xgboost(data = as.matrix(indata[folds[[1]],]), label = outdata[folds[[1]],], max.depth = 3, eta = 1, nrounds = iter, 
               nthread = 2)

bst2 <- xgboost(data = as.matrix(indata[folds[[1]],]), label = outdata[folds[[1]],], max.depth = 2, eta = 1, nrounds = iter, 
                nthread = 2)


plot(bst5$evaluation_log$train_rmse, col = 'red', ylab = '', xlab = '')
title(main = paste("XGBoost RMSE", "dla danych o zu¿yciu energii", sep = " "), ylab = 'RMSE', xlab = 'Model')
grid(col='black')
par(new=T)
plot(bst3$evaluation_log$train_rmse, col = 'green', ylab = '', xlab = '',axes = F, pch='x')
par(new=T)
plot(bst2$evaluation_log$train_rmse, col = 'blue', ylab = '', xlab = '',axes = F, pch=2)
legend(x = 20, y = 7.5, legend = c("maxdepth = 5", "maxdepth = 3", "maxdepth = 2"), bty = 'n', col = c("red", "green", "blue"),  
       pch = c(4,1,2), cex=0.6)

pred5 <- predict(bst5, as.matrix(indata[-folds[[1]],]))
pred3 <- predict(bst3, as.matrix(indata[-folds[[1]],]))
pred2 <- predict(bst2, as.matrix(indata[-folds[[1]],]))

err5 = sqrt((sum((outdata[-folds[[1]],] - pred5)^2))/length(folds[[2]]))
err3 = sqrt((sum((outdata[-folds[[1]],] - pred3)^2))/length(folds[[2]]))
err2 = sqrt((sum((outdata[-folds[[1]],] - pred2)^2))/length(folds[[2]]))

errs <- c(err5, err3, err2)
par(new=F)
plot(y=errs, x=c(5,3,2), ylab='RMSE regresji', xlab ='maxdept')
title(main = "RMSE regresji dla XGBost dla danych zu¿ycia energii")
grid(col='black')
## porównanie xgboostem
