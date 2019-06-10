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
maxdpt = 3#DO ZMIANY DO EKSPERYMENTU
iter = 10#DO ZMIANY DO EKSPERYMENTU
subtitle = paste('Maks. glebokosc:', maxdpt, 'Liczba iteracji', iter)
EnergyTitle = paste("dla danych o zu¿yciu energii")#DO ZMIANY DO EKSPERYMENTU
form <- lights ~ . - Appliances#DO ZMIANY DO EKSPERYMENTU
folds <- crossValFolds(form, tset, 2)#DO ZMIANY DO EKSPERYMENTU
models <- crossValModels(form, tset, folds, iter, rpart.control(maxdepth = maxdpt))
crossValAnalysis(form, tset, folds, models, EnergyTitle)
crossValAnaliseModels(form, tset, folds, models, EnergyTitle, subtitle)
##
