
library("xgboost")
library("cdata")
library("ggplot2")

# outcome should be 0/1 numeric
# d <- data.frame(x = 0.1*as.numeric(1:500))
# d$y <- ifelse(sin(d$x)>0, 1, 0)
# mkXGBoostModelC(d, 'x', 'y')
mkXGBoostModelC <- function(trainDat, vars, outcomeCol, 
                           nTrain= 200000,
                           xval= TRUE,
                           ntrees= 1000,
                           eta = 0.1,
                           depth = 8,
                           nfold = 5,
                           buildPlot = TRUE) {
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
    if(buildPlot) {
      plotD <- moveValuesToRows(elog, 
                                nameForNewKeyColumn = 'errtype', 
                                nameForNewValueColumn = 'error', 
                                columnsToTakeFrom = c('train_error_mean', 'test_error_mean')) 
      title <- "xgboost loss"
      print(ggplot(plotD, aes(x=iter, y=error, color=errtype)) + geom_point() + geom_line() +
              scale_color_brewer(palette="Dark2") + ggtitle(title))
    }
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



# outcome should be numeric
# d <- data.frame(x = as.numeric(1:5), y = c(1,1,1,2,2))
# mkXGBoostModelR(d, 'x', 'y')
mkXGBoostModelR <- function(trainDat, vars, outcomeCol, 
                           nTrain= 200000,
                           xval= TRUE,
                           ntrees= 1000,
                           eta = 0.1,
                           depth = 8,
                           nfold = 5,
                           buildPlot = TRUE) {
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
                 label =  trainDat[[outcomeCol]],
                 nrounds = ntrees,
                 nfold = nfold,
                 eta = eta,
                 depth = depth,
                 verbose = 0,
                 seed = seed,
                 nthreads = nthreads)
    # Get the evaluation log and call summary on it
    elog <- cv$evaluation_log
    if(buildPlot) {
      plotD <- moveValuesToRows(elog, 
                                nameForNewKeyColumn = 'errtype', 
                                nameForNewValueColumn = 'rmse', 
                                columnsToTakeFrom = c('train_rmse_mean', 'test_rmse_mean')) 
      print(ggplot(plotD, aes(x=iter, y=rmse, color=errtype)) + geom_point() + geom_line() +
              scale_color_brewer(palette="Dark2") + ggtitle("xgboost cv regression"))
    }
    ntrees <- which.min(elog$test_rmse_mean)
    cv <- NULL
    elog <- NULL
  }
  print(paste("xgboost: ntrees", ntrees))
  print(paste(" start mkXGBoostModel xgboost", base::date()))
  model <- xgboost(data = as.matrix(trainDat[, vars, drop=FALSE]),
                   label =  trainDat[[outcomeCol]],
                   nrounds = ntrees,
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

