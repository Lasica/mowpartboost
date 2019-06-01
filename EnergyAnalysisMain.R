library(rpart)
library(rpart.plot)

#setwd("/qarr/projects/studia/mow/mowpartboost")
setwd("C:/Studia MGR/Semestr II/MOW/mowpartboostmain/mowpartboost")
f <- function(y, yi) {0.5*(y-yi)^2}
#przygotowanie danych do obr?bki
source("pRepare_Energy_data.R")

#definicja funkcji implementuj?cej boosting
source("OurGradBoost.R")

#
rsquared = function (corrLab, predictLab){
  r2 <- cor(corrLab, predictLab ) ^ 2
}

tree <- rpart(tlab1 ~ ., data=tset, method="anova")
rpart.plot(tree)
maxtree <- rpart(tlab1 ~ ., data=tset, method="anova", cp = -1)
predictions <- rpart.predict(tree, tset)
error <- `^`(tlab1 - predictions, 2)
summary(error)
#

# model <- myxgbLegacy(tset, tlab1, 10)
# plot(model$e)
# errors <- sqrt(model$e/dim(tset)[1])
# min(errors)
# 
# model <- myxgbLegacy(tset, tlab2, 10)
# plot(model$e)
# errors <- sqrt(model$e/dim(tset)[1])
# min(errors)
# 
# sqrt(sum(f(tlab1, rpart.predict(rpart(tlab1 ~ ., tset), tset)))) # blad referencyjny


source("myxgb.R")
myxgb_model <- myxgb$new()
myxgb_model$fit(formula(lights ~ . - Appliances, data=tset), tset, 10, rpart.control(maxdepth = 3)) # dziala tak samo
rpart.plot(myxgb_model$models[[1]])
plot(myxgb_model$rmse)
plot(myxgb_model$weights)
plot(rsquared(tlab2, myxgb_model$predict(tset[,c(-1,-2)])))
#sprawdzenie w por?wnaniu z xgboostem
source("xgboost_implement.R")


#por?wnanie wynik?w za pomoc? funkcji predykcji
source("PredictionF.R")
predicts <-myxgb_predict(model, tset)
errors   <- `^`(tlab2 - predicts, 2)
plot(errors)

