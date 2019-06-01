myxgbLegacy <- function(indata, outdata, niter) {# interfejs formuly
  models <- 1
  weights <- c(mean(outdata))
  
  result_so_far <- rep(weights[1], length(outdata))
  error <- sum((outdata - result_so_far)^2)
  for (i in  1:niter) {
    residuum <- outdata - result_so_far
    models <- c(models, list(rpart(residuum ~ ., data=indata, method="anova"))) # poprawic
    model_result <- rpart.predict(models[[length(models)]], tset)
    new_weight <- sum(model_result *(result_so_far + outdata))/sum(model_result*model_result) # eksperyment - wprowadzic 1
    if (sum(model_result^2) < 0.001) {
      new_weight <- 0.0
    }
    weights <- c(weights, new_weight)
    result_so_far <- result_so_far + new_weight * model_result
    error <- c(error, sum(f(outdata, result_so_far)))
  }
  model <- list("m" = models, "w" = weights, "e" = error)
  #wynik - list z models i weights
}

