library(rpart)
library(rpart.plot)
library(caret)

#przygotowanie danych do obrobki
source("pRepare_CardFraud_data.R")

#Walidacja krzyżowa 
source("crossValidation.R")
  form <- targets[[1]]
  print('crossValFolds')
  folds <- crossValFolds(form, tset, 10)
  print('crossValModles')
  models <- crossValModels(form, tset, folds, 10,  rpart.control(maxdepth = 5))

  maxdpt = 5 #DO ZMIANY DO EKSPERYMENTU
  iter = 10 #DO ZMIANY DO EKSPERYMENTU
  
  subtitle = paste('Maks. glebokosc:', maxdpt, 'Liczba iteracji', iter)
  EnergyTitle = paste("dla danych o kradziezy z kart bankowych")#DO ZMIANY DO EKSPERYMENTU
  crossValAnalysis(form, tset, folds, models, EnergyTitle)
  crossValAnaliseModels(form, tset, folds, models, EnergyTitle, subtitle)
  ##