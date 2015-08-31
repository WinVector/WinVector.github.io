

# common functions we want available when we use parallel
# depends on:
#library('gbm')
#library('ROCR')

# compute normalized deviance
#' @param yVals logical
#' @param preds , length(preds)==length(yVals) 0<=preds<=1
#' @return deviance/nexamples
devianceNYP <- function(yVals,preds) {
  epsilon <- 1.e-5
  dev <- 0
  if(sum(yVals)>0) {
    dev <- dev + -2*sum(log(pmax(preds[yVals],epsilon)))
  }
  if(sum(yVals)<length(yVals)) {
    dev <- dev + -2*sum(log(pmax(1-preds[!yVals],epsilon)))
  }
  dev/length(yVals)
}

# compute AUC deviance
#' @param yVals logical
#' @param preds , length(preds)==length(yVals) 0<=preds<=1
#' @return AUC
aucYP <- function(yVals,preds) {
  pred <- ROCR::prediction(preds,yVals)
  as.numeric(ROCR::performance(pred,'auc')@y.values) # strip stuff
}




# repeat adjustment mapping on new data
mapIso <- function(trainOrig,trainAdjusted,newOrig) {
  # limit down to unique values
  breaks <- sort(unique(trainOrig))
  uposns <- match(breaks,trainOrig)
  trainOrig <- trainOrig[uposns]
  trainAdjusted <- trainAdjusted[uposns]
  newidx <- findInterval(newOrig,trainOrig)
  newidx <- pmax(1,newidx)
  trainOrig[[1+length(trainOrig)]] <- 1+trainOrig[[length(trainOrig)]] 
  trainAdjusted[[1+length(trainAdjusted)]] <- trainAdjusted[[length(trainAdjusted)]]
  lambda <- (newOrig-trainOrig[newidx])/(trainOrig[newidx+1]-trainOrig[newidx])
  newAdjusted <- (1-lambda)*trainAdjusted[newidx] + lambda*trainAdjusted[newidx+1]
  newAdjusted[newOrig<=trainOrig[[1]]] <- trainAdjusted[[1]]
  n <- length(trainOrig)
  newAdjusted[newOrig>=trainOrig[[n]]] <- trainAdjusted[[n]]
  newAdjusted           
}



# compute normalized deviance of a canonical transform of pred
# y numeric in range 0 to 1
# x numeric same length as y
# w numeric positive, same length as y (weights)
# return isotonicly adjusted x
# this is a vector of length x that is a function of x
# with at least as many order constraints as x and as close
# to y in probablity (by deviance) as possible.
solveIsotonicProblemW <- function(y,pred,w) {
  if(!is.numeric(y)) {
    stop("expect y logical")
  }
  if(!is.numeric(pred)) {
    stop("expect pred numeric")
  }
  if(!is.numeric(w)) {
    stop("expect y logical")
  }
  if(length(pred)!=length(y)) {
    stop("expect length(pred)==length(y)")
  }
  if(length(w)!=length(y)) {
    stop("expect length(w)==length(y)")
  }
  d <- data.frame(y=y,pred=pred,w=w)
  n <- nrow(d)
  if(n<=1) {
    return(as.numeric(y))
  }
  dord <- order(d$pred,d$y)
  invPerm <- 1:n
  invPerm[dord] <- 1:n
  d <- d[dord,]
  epsilon <- 1.0e-3
  Atot <- matrix(ncol=2,nrow=0,data=0)
  # build order relations to insist on a monotone function transform
  # first all order constraints
  Atot <- cbind(1:(n-1),2:n)
  # then any additional equality constraints to force result to be a
  # function of pred
  noIncrease <- which(d$pred[1:(n-1)]>=d$pred[2:n]-1.0e-6)
  if(length(noIncrease)>0) {
    Atot <- rbind(Atot,cbind(noIncrease+1,noIncrease))
  }
  # sum of squares objective 
  sqIso <- isotone::activeSet(Atot,y=d$y,weights=d$w)
  adjPred <- pmin(1-epsilon,pmax(epsilon,sqIso$x))
  # undo permutation
  adjPred <- adjPred[invPerm]
  adjPred
}


# compute normalized deviance of a canonical transform of pred
# y logical in range 0 to 1
# x numeric same length as y
# return isotonicly adjusted x
# this is a vector of length x that is a function of x
# with at least as many order constraints as x and as close
# to y in probablity (by deviance) as possible.
solveIsotonicProblem <- function(y,pred) {
  if(!is.logical(y)) {
    stop("expect y logical")
  }
  if(!is.numeric(pred)) {
    stop("expect pred numeric")
  }
  if(length(pred)!=length(y)) {
    stop("expect length(pred)==length(y)")
  }
  d <- data.frame(y=y,pred=pred,w=1.0)
  dA <- aggregate(cbind(y,w)~pred,data=d,sum)
  dA$y <- dA$y/dA$w
  dA$iso <- solveIsotonicProblemW(dA$y,dA$pred,dA$w)
  mapIso(dA$pred,dA$iso,pred)
}







# compute normalized deviance of a canonical transform of pred
# y logical
# x numeric same length as y
isoDeviance <- function(y,pred,maxSize=100) {
  nrow <- length(y)
  if(nrow>maxSize) {
    sample <- sample.int(nrow,maxSize)
    yS <- y[sample]
    predS <- pred[sample]
    adjPredS <- solveIsotonicProblem(yS,predS)
    adjPred <- mapIso(predS,adjPredS,pred)
  } else {
    adjPred <- solveIsotonicProblem(y,pred)
  }
  # adjPred(pred) == adjPred(f(pred)) for any monotone 1-1 f()
  # so we are now invariant over such f
  -2*(sum(log(adjPred[y]))+sum(log(1.0-adjPred[!y])))/length(y)
}





# convert truth and predictions to performance scores
#' @param yVals logical
#' @param preds , length(preds)==length(yVals) 0<=preds<=1
#' @return named list of perf scores
perfScores <- function(yVals,preds) {
  dev <- devianceNYP(yVals,preds)
  auc <- aucYP(yVals,preds)
  isodev <- isoDeviance(yVals,preds)
  list(ndeviance=dev,isodev=isodev,auc=auc)
}


# wrap dev calc for boot (inlined in case we use snow parallelism)
calcNDev <- function(data, indices) {
  yVals <- data[indices,'y']
  preds <- data[indices,'pred']
  epsilon <- 1.e-5
  dev <- 0
  if(sum(yVals)>0) {
    dev <- dev + -2*sum(log(pmax(preds[yVals],epsilon)))
  }
  if(sum(yVals)<length(yVals)) {
    dev <- dev + -2*sum(log(pmax(1-preds[!yVals],epsilon)))
  }
  dev/length(yVals)
}

# wrap AUC for boot (inlined in case we use snow parallelism)
calcAUC <- function(data, indices) {
  pred <- ROCR::prediction(data[indices,'pred',drop=TRUE],
                           data[indices,'y',drop=TRUE])
  as.numeric(ROCR::performance(pred,'auc')@y.values) # strip stuff
}

# convert truth and predictions to performance scores
# From: http://www.statmethods.net/advstats/bootstrapping.html
# boot.ci() errors out a lot (on 'stud' and 'pca')
#' @param yVals logical
#' @param preds , length(preds)==length(yVals) 0<=preds<=1
#' @return named list of perf scores ( names structed stat(.version) )
perfScoresB <- function(yVals,preds,nrep=200) {
  parallelism <- 'no' # explicit cluster start/stops can interfere with other parallelism
  perfs <- list()
  stats <- list('ndeviance'=calcNDev,
                'auc'=calcAUC)
  for(sName in names(stats)) {
    for(stratify in c(FALSE,TRUE)) {
      strata = rep(1,length(yVals))
      pName <- sName
      if(stratify) {
        strata = 1+as.numeric(yVals)
        pName <- paste(sName,'S',sep='.')
      }
      b <- boot::boot(data=data.frame(y=yVals,pred=preds),
                      statistic=stats[[sName]],
                      R=nrep,
                      strata=strata,
                      parallel=parallelism,
                      ncpus=1)
      perf <- as.numeric(b$t0)
      perf.lW <- perf
      perf.uW <- perf
      if(max(b$t)-min(b$t)>1.0e-5) {
        bc <- boot::boot.ci(b,type='norm')
        perf.lW <- as.numeric(bc$normal[1,2]) # strip stuff
        perf.uW <- as.numeric(bc$normal[1,3])
      }
      perfs[[pName]] <- perf
      perfs[[paste(pName,'lW',sep='.')]] <- perf.lW
      perfs[[paste(pName,'uW',sep='.')]] <- perf.uW
    }
  }
  perfs
}

  


