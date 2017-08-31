
library("xgboost")
library("tidyr")


# outcome should be 0/1 numeric
mkXGBoostModel <- function(trainDat, vars, outcomeCol, 
                           nTrain= 200000,
                           xval= TRUE,
                           ntrees= 1000,
                           eta = 0.1,
                           depth = 8,
                           nfold = 5) {
  # shuffle and control size
  print(paste("start mkXGBoostModel", base::date()))
  seed <- sample.int(1000000000,1)
  nthreads <- parallel::detectCores()
  trainDat <- trainDat[, c(vars, outcomeCol), drop=FALSE]
  complete <- complete.cases(trainDat)
  trainDat <- trainDat[complete, , drop=FALSE]
  trainDat <- trainDat[sample.int(nrow(trainDat), 
                                  size=min(nrow(trainDat),nTrain),
                                  replace=FALSE), , 
                       drop=FALSE]
  if(xval) {
    print(paste(" start mkXGBoostModel xgb.cv", base::date()))
    cv <- xgb.cv(data = as.matrix(trainDat[, vars, drop=FALSE]),
                 label =  ifelse(trainDat[[outcomeCol]]>0, 1, 0),
                 nrounds = ntrees,
                 nfold = nfold,
                 objective = "binary:logistic",
                 eta = eta,
                 depth = depth,
                 verbose = 0,
                 seed = seed,
                 nthreads = nthreads)
    # Get the evaluation log and call summary on it
    elog <- cv$evaluation_log
    plotD <- gather(elog, key=errtype, value=error, train_error_mean, test_error_mean) 
    title <- "xgboost loss"
    print(ggplot(plotD, aes(x=iter, y=error, color=errtype)) + geom_point() + geom_line() +
            scale_color_brewer(palette="Dark2") + ggtitle(title))
    ntrees <- which.min(elog$test_error_mean)
    cv <- NULL
    elog <- NULL
  }
  print(paste("xgboost: ntrees", ntrees))
  print(paste(" start mkXGBoostModel xgboost", base::date()))
  model <- xgboost(data = as.matrix(trainDat[, vars, drop=FALSE]),
                   label =  ifelse(trainDat[[outcomeCol]]>0, 1, 0),
                   nrounds = ntrees,
                   objective = "binary:logistic",
                   eta = eta,
                   depth = depth,
                   verbose = 0,
                   seed = seed,
                   nthreads = nthreads)
  # clear vars to prevent dangling refs
  trainDat <- NULL
  print(paste("done mkXGBoostModel", base::date()))
  # return prediction function
  function(dat) {
    preds <- predict(model,
                     newdata =  as.matrix(dat[, vars, drop=FALSE]), 
                     ntreelimit = ntrees)
    preds
  }
}

