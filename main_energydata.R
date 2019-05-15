setwd("/Users/arek/Documents/Studia/MGR/MOW/Projekt/od AD/mow/")
energydata = read.csv("datasets/energy/raw/energydata_complete.csv")
summary(energydata)

tset <- energydata[, seq(4,length(energydata)-2)]
tset$year <- as.POSIXlt(energydata[, 1])$year
tset$mon <- as.POSIXlt(energydata[, 1])$mon
tset$wday <- as.POSIXlt(energydata[, 1])$wday
tset$hour <- as.POSIXlt(energydata[, 1])$hour
tset$minutes <- as.POSIXlt(energydata[, 1])$min
tlab1 <- energydata$Appliances
tlab2 <- energydata$lights
library(rpart)
library(rpart.plot)
tree <- rpart(tlab1 ~ ., data=tset, method="anova")
rpart.plot(tree)
maxtree <- rpart(tlab1 ~ ., data=tset, method="anova", cp = -1)
predictions <- rpart.predict(tree, tset)
error <- `^`(tlab1 - predictions, 2)
summary(error)


# moving average example plot
#plot(seq(16,23,0.25), sapply(seq(16,23,0.25), function(k){a <- tset$T1>k-0.25 & tset$T1 <= k+0.25; mean(tlab1[a])}), type="l")

#gradient boosting
myxgb <- function(indata, outdata, niter) {
  models <- 1
  weights <- c(mean(outdata))
  f <- function(y, yi) {0.5*(y-yi)^2}
  result_so_far <- rep(weights[1], length(outdata))
  error <- sum((outdata - result_so_far)^2)
  for (i in  1:niter) {
    residuum <- outdata - result_so_far
    models <- c(models, list(rpart(residuum ~ ., data=indata, method="anova"))) # poprawic
    model_result <- rpart.predict(models[[length(models)]], tset)
    new_weight <- sum(model_result *(result_so_far + outdata))/sum(model_result*model_result)
    if (sum(model_result^2) < 0.001) {
      new_weight <- 0.0
    }
    weights <- c(weights, new_weight)
    result_so_far <- result_so_far + new_weight * model_result
    error <- c(error, sum(f(outdata, result_so_far)))
  }
  model <- list("m" = models, "w" = weights, "e" = error)
  #wynik - dataframe z models i weights
}

model <- myxgb(tset, tlab2, 10)
# 5-ta waga psuje
plot(model$e)

model <- myxgb(tset, tlab1, 10)
# 5-ta waga psuje
plot(model$e)

sum(f(tlab1, rpart.predict(rpart(tlab1 ~ ., tset), tset))) # blad referencyjny

#wynik - dataframe z models i weights
#return(models, weights, residuum)
# funkcja przyjmujaca model + dane -> wynik

# policzyc bledy







