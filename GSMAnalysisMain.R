library(rpart)
library(rpart.plot)
library(caret)

#przygotowanie danych do obr?bki
source("pRepare_GSM_data.R")


# source("myxgb.R")
# myxgb_model <- myxgb$new()
# myxgb_model$fit(formula(lights ~ . - Appliances, data=tset), tset, 10, rpart.control(maxdepth = 3)) # dziala tak samo
# rpart.plot(myxgb_model$models[[1]])
# plot(myxgb_model$rmse)
# plot(myxgb_model$weights)
# plot(rsquared(tlab2, myxgb_model$predict(tset[,c(-1,-2)])))
# #sprawdzenie w por?wnaniu z xgboostem
# source("xgboost_implement.R")

source("crossValidation.R")
for (i in 1:length(targets)) {
  form <- targets[[i]]
  print(paste('Creating folds in', as.character(i), "target"))
  folds <- crossValFolds(form, tset, 2)
  print(paste('Building models in', as.character(i), "target"))
  models <- crossValModels(form, tset, folds)
  print(paste('Analysing models in', as.character(i), "target"))
  crossValAnalysis(form, tset, folds, models)
}