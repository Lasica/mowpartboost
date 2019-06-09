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
  print('crossValFolds')
  folds <- crossValFolds(form, tset, 10)
  print('crossValModles')
  models <- crossValModels(form, tset, folds, rpart.control(maxdepth = 2))
  print('crossValAnalysis')
  crossValAnalysis(form, tset, folds, models, "dla danych o zuzyciu energii")
}