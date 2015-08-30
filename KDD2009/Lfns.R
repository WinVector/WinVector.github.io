

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
  d <- data.frame(y=y,pred=pred)
  n <- nrow(d)
  dord <- order(d$pred,d$y)
  invPerm <- 1:n
  invPerm[dord] <- 1:n
  d <- d[dord,]
  epsilon <- 1.0e-12
  # build order relations to insist on a monotone function transform
  # first all order constraints
  Atot <- cbind(1:(n-1),2:n)
  # then any additional equality constraints to force result to be a
  # function of pred
  noIncrease <- which(d$pred[1:(n-1)]>=d$pred[2:n]-1.0e-6)
  if(length(noIncrease)>0) {
    Atot <- rbind(Atot,cbind(noIncrease+1,noIncrease))
  }
  # sum of squares objective (as a good start for logistic attempt)
  sqIso <- isotone::activeSet(Atot,y=d$y,weights=rep(1,n))
  # deviance objective
  logit <- function(p) { log(p/(1-p))}
  sigmoid <- function(x) { 1/(1+exp(-x))}
  x0 <- logit(pmin(1-epsilon,pmax(epsilon,sqIso$x)))
  # robustify against optimizer failures
  bestSx <- d$pred   # init to identity function
  bestFx <- Inf
  # setting x0 in activeSet loses enforcement of constraints (bug in library) 
  fobj=function(x) {
    sx <- sigmoid(x+x0)
    sx <- pmin(1-epsilon,pmax(epsilon,sx))
    fx <- -2*(sum(log(sx[d$y]))+sum(log(1-sx[!d$y])))
    feasible <- all(sx[Atot[,1,drop=TRUE]]<=sx[Atot[,2,drop=TRUE]])
    if(feasible && (is.null(bestSx)||(fx<bestFx))) {
      bestSx <<- sx
      bestFx <<- fx
    }
    fx
  }
  # force an eval at zero
  fobj(numeric(length(x0)))
  isoSoln <- isotone::activeSet(Atot,isotone::fSolver,
                                fobj=fobj,
                                gobj=function(x) {
                                  sx <- sigmoid(x+x0)
                                  dx <- sx*(1-sx) # derivative of sx
                                  # gradient of fob by chain rule
                                  gx <- -2*ifelse(d$y,1/sx,-1/(1-sx))*dx
                                  gx[dx<=0] <- 0
                                  gx
                                },
                                y=d$y,weights=rep(1,n))
  # undo permutation
  adjPred <- bestSx
  adjPred <- adjPred[invPerm]
  adjPred
}

# compute normalized deviance of a canonical transform of pred
# y logical
# x numeric same length as y
isoDeviance <- function(y,pred) {
  adjPred <- solveIsotonicProblem(y,pred)
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

  


