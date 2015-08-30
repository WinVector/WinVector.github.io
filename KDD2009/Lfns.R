

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

# compute normalized deviance of a canonical transform of pred
# y logical
# x numeric same length as y
isoDeviance <- function(y,pred) {
  if(!is.logical(y)) {
    stop("expect y logical")
  }
  if(!is.numeric(pred)) {
    stop("expect pred numeric")
  }
  if(length(pred)!=length(y)) {
    stop("expect length(pred)==length(y)")
  }
  adjPred <- isotone::gpava(pred,as.numeric(y))$x
  # adjPred(pred) == adjPred(f(pred)) for any monotone 1-1 f()
  # so we are now invariant over such f
  epsilon <- 1.0e-6
  adjPred <- pmin(1-epsilon,pmax(epsilon,adjPred))
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

  


