library(rpart)
library(rpart.plot)

setwd("C:/Studia MGR/Semestr II/MOW/mowpartboostmain/mowpartboost")
f <- function(y, yi) {0.5*(y-yi)^2}
#przygotowanie danych do obróbki
source("pRepare_Energy_data.R")

#definicja funkcji implementuj¹cej boosting
source("OurGradBoost.R")

#
tree <- rpart(tlab1 ~ ., data=tset, method="anova")
rpart.plot(tree)
maxtree <- rpart(tlab1 ~ ., data=tset, method="anova", cp = -1)
predictions <- rpart.predict(tree, tset)
error <- `^`(tlab1 - predictions, 2)
summary(error)
#

model <- myxgb(tset, tlab1, 10)
plot(model$e)
errors <- sqrt(model$e/dim(tset)[1])
min(errors)

model <- myxgb(tset, tlab2, 10)
plot(model$e)
errors <- sqrt(model$e/dim(tset)[1])
min(errors)

sqrt(sum(f(tlab1, rpart.predict(rpart(tlab1 ~ ., tset), tset)))) # blad referencyjny

#sprawdzenie w porównaniu z xgboostem
source("xgboost_implement.R")


