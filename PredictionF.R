library(rpart)
myxgb_predict <- function (model, data){
  #funkcja predykcji do u¿ycia przy naszym modelu
  
  labels <- numeric(nrow(data))
  
  for (i in 2:(length(model[[1]])))#uzupe³niæ
  {
       model_result <- rpart.predict(model[[1]][[i]], data)
       labels <- labels + model_result  * model[[2]][i] 
  }
  
}