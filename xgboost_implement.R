library("xgboost")

bst <- xgboost(data = as.matrix(tset), label = tlab1, max.depth = 4, eta = 1, nrounds = 15, 
               nthread = 2)

pred <- predict(bst, as.matrix(tset))
xgb.plot.tree(model = bst)

bst <- xgboost(data = as.matrix(tset), label = tlab2, max.depth = 4, eta = 1, nrounds = 15, 
               nthread = 2)


pred <- predict(bst, as.matrix(tset))
xgb.plot.tree(model = bst)