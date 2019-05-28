#zostawic dane wejsciowe razem z wyjsciem - podac odpowiednia formule
#gradient boosting
myxgb <- setRefClass("myxgb", 
            fields = list(bias = "numeric", weights = "numeric", rmse = "numeric", models="list"),
            methods = list(
              fit = function(fr, data, niter) {
                fterms <- terms(fr, data=data)
                outdata <- data[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])] # czemu to jest tak skomplikowane; +1 bo jak nie to wynikiem jest list
                #indata <- model.matrix(fterms, data=data)
                bias <<- mean(outdata[, 1])
                f <- function(y, yi) {0.5*(y-yi)^2}
                result_so_far <- rep(bias, length(outdata))
                error <- sum((outdata - result_so_far)^2)
                rmse <<- c(rmse, sqrt(error/dim(data)[1]))
                #################
                for (i in  1:niter) {
                  data["residuum"] <- outdata - result_so_far
                  new_formula <- update(fterms, residuum ~ .)
                  models <<- c(models, list(rpart(new_formula, data=data, method="anova"))) # poprawic
                  model_result <- rpart.predict(models[[length(models)]], tset)
                  new_weight <- sum(model_result *(result_so_far + outdata))/sum(model_result*model_result)
                  if (sum(model_result^2) < 0.001) {
                    new_weight <- 0.0
                  }
                  weights <<- c(weights, new_weight)
                  result_so_far <- result_so_far + new_weight * model_result
                  error <- sum(f(outdata, result_so_far))
                  rmse <<- c(rmse, sqrt(error/dim(data[1])))
                }
                #model <- list("m" = models, "w" = weights, "e" = error)
                #wynik - dataframe z models i weights
              },
              
              predict = function() {}
              )
            )

# example usage
#myxgb_model <- myxgb$new()
#myxgb_model$fit(formula(Appliances ~ . - lights, data=tset), tset, 10)
