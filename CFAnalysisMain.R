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
  models <- crossValModels(form, tset, folds, 2,  rpart.control(maxdepth = 2))
  print('crossValAnalysis')
  crossValAnalysis(form, tset, folds, models, "Kradzierze z kart bankowych")