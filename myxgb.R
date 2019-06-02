#zostawic dane wejsciowe razem z wyjsciem - podac odpowiednia formule
#gradient boosting
myxgb <- setRefClass("myxgb", 
            fields = list(bias = "numeric", weights = "numeric", rmse = "numeric", models="list"),
            methods = list(
              fit = function(fr, data, niter, control = rpart.control()) {
                bias    <<- numeric(0)
                weights <<- numeric(0)
                rmse    <<- numeric(0)
                models  <<- list()
                fterms <- terms(fr, data=data)
                outdata <- data[as.character(attr(fterms, "variables")[[attr(fterms, "response")+1]])] # czemu to jest tak skomplikowane; +1 bo jak nie to wynikiem jest list
                indata <- model.matrix(fterms, data=data)
                indata <- as.data.frame(indata[, 2:dim(indata)[2]])
                bias <<- mean(outdata[, 1])
                f <- function(y, yi) {0.5*(y-yi)^2}
                result_so_far <- rep(bias, length(outdata))
                error <- sum((outdata - result_so_far)^2)
                rmse <<- c(rmse, sqrt(error/dim(data)[1]))
                #################
                for (i in  1:niter) {
                  # browser()
                  residuum <- outdata[ ,1] - result_so_far
                  #new_formula <- update(fterms, residuum ~ .)
                  #models <<- c(models, list(rpart(new_formula, data=data, method="anova"))) # poprawic
                  
                  models <<- c(models, list(rpart(residuum ~ ., data=indata, method="anova", model = TRUE, control = control)))
                  model_result <- rpart.predict(models[[length(models)]], indata)
                  new_weight <- sum(model_result *(result_so_far + outdata))/sum(model_result*model_result)
                  if (sum(model_result^2) < 0.001) {
                    new_weight <- 0.0
                  }
                  weights <<- c(weights, new_weight)
                  result_so_far <- result_so_far + new_weight * model_result
                  error <- sum(f(outdata, result_so_far))
                  rmse <<- c(rmse, sqrt(error/dim(data)[1]))
                }
                #model <- list("m" = models, "w" = weights, "e" = error)
                #wynik - dataframe z models i weights
              },
              
              predict = function(data) {
                labels <- numeric(nrow(data))
                
                labels <- labels + bias
                
                for (i in 1:(length(models)))
                {
                  model_result <- rpart.predict(models[[i]], data)
                  labels <- labels + model_result  * weights[[i]] 
                }
                return(labels)
              },
              
              rsquared = function (corrLab, predictLab){
                r2 <- cor(corrLab, predictLab ) ^ 2
              }
              )
            )

# example usage
#myxgb_model <- myxgb$new()
#myxgb_model$fit(formula(Appliances ~ . - lights, data=tset), tset, 10)
