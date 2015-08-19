
# functions needed for analysis (but don't need to distribute for parallel)
#load some libraries
# http://www.win-vector.com/blog/2014/08/vtreat-designing-a-package-for-variable-treatment/
# devtools::install_github("WinVector/vtreat")
library('vtreat')
# devtools::install_github("WinVector/WVPlots")
library('WVPlots')
library('parallel')
library('gbm')
library('ROCR')
library('data.table')
library('randomForest')
library('dplyr')
library('caret')
#library('deepnet')


# build a worker with an environment carrying the data we need to work with
# usefull when using parallel
mkWorker <- function(yName,chosenVars,treatedTrainM,
                     dsList,fitter) {
  force(yName) 
  force(chosenVars) 
  force(treatedTrainM)
  force(dsList)
  force(fitter)
  function(ci) {
    source('Lfns.R')
    fitter(yName,c(chosenVars,ci),
           treatedTrainM,
           dsList)
  }
}



# Fit and score logistic regression
doFitApplyLR <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  formulaS <- paste(yName,paste(selvars,collapse=' + '),sep=' ~ ')
  model <- glm(as.formula(formulaS),
               data=trainData[,c(selvars,yName),drop=FALSE],
               family=binomial(link='logit'))
  lapply(appDataList,function(d) {
    pred <- predict(model,newdata=d[,selvars,drop=FALSE],
                    type='response')
    if(bootScore) {
      perfScoresB(d[[yName]],pred)
    } else {
      perfScores(d[[yName]],pred)
    }
  })
}

# Fit and score GBM
doFitApplyGBM <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  formulaS <- paste(yName,paste(selvars,collapse=' + '),sep=' ~ ')
  modelGBMs <- gbm::gbm(as.formula(formulaS),
                        data=trainData[,c(selvars,yName),drop=FALSE],
                        distribution='bernoulli',
                        n.trees=500,
                        interaction.depth=3,
                        keep.data=FALSE,
                        cv.folds=5,
                        n.cores=1)
  nTrees <- gbm::gbm.perf(modelGBMs,plot.it=FALSE)
  lapply(appDataList,function(d) {
    pred <- gbm::predict.gbm(modelGBMs,newdata=d[,selvars,drop=FALSE],
                             type='response',
                             n.trees=nTrees)
    if(bootScore) {
      perfScoresB(d[[yName]],pred)
    } else {
      perfScores(d[[yName]],pred)
    }
  })
}


# Fit and score GAM logistic regression
doFitApplyGAMLR <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  manyLevels <- selvars[lapply(selvars,function(v) { length(unique(trainData[[v]]))})>6]
  mvars <- setdiff(selvars,manyLevels)
  if(length(manyLevels)>0) {
    mvars <- c(mvars,paste('s(',manyLevels,',k=3)',sep=''))
  }
  formulaS <- paste(yName,paste(mvars,collapse=' + '),sep=' ~ ')
  model <- mgcv::gam(as.formula(formulaS),
               data=trainData[,c(selvars,yName),drop=FALSE],
               family=binomial(link='logit'))
  lapply(appDataList,function(d) {
    pred <- mgcv::predict.gam(model,newdata=d[,selvars,drop=FALSE],
                    type='response')
    pred <- as.numeric(pred) # drop any attributes
    if(bootScore) {
      perfScoresB(d[[yName]],pred)
    } else {
      perfScores(d[[yName]],pred)
    }
  })
}

# Fit and score glmnet logistic regression
doFitApplyGLMNet <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  model <- glmnet::cv.glmnet(x=as.matrix(trainData[,selvars,drop=FALSE]),
                             y=as.factor(trainData[[yName]]),
                             alpha=0.5,
                             family='binomial')
  lapply(appDataList,function(d) {
    pred <- glmnet::predict.cv.glmnet(model,
                                      newx=as.matrix(d[,selvars,drop=FALSE]),
                                      type='response')
    pred <- as.numeric(pred) # drop attributes
    if(bootScore) {
      perfScoresB(d[[yName]],pred)
    } else {
      perfScores(d[[yName]],pred)
    }
  })
}

# Fit and score randomForest
doFitApplyRF <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  model <- randomForest(x=trainData[,selvars,drop=FALSE],
                        y=as.factor(as.character(trainData[[yName]])))
  lapply(appDataList,function(d) {
    # randomForest doesn't seem to export its predict function in a nice way
    pred <- predict(model,newdata=d[,selvars,drop=FALSE],
                    type='prob')[,'TRUE',drop=TRUE]
    if(bootScore) {
      perfScoresB(d[[yName]],pred)
    } else {
      perfScores(d[[yName]],pred)
    }
  })
}




# best constant model on train
doFitApplyNullModel <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  model <- mean(trainData[[yName]])
  lapply(appDataList,function(d) {
    # randomForest doesn't seem to export its predict function in a nice way
    pred <- rep(model,nrow(d))
    if(bootScore) {
      perfScoresB(d[[yName]],pred)
    } else {
      perfScores(d[[yName]],pred)
    }
  })
}


# best single variable model
# do not nest parallel::parLapply (seems to lose some results)
doFitApplySingleVarModel <- function(yName,selvars,trainData,appDataList,bootScore=FALSE,parallelCluster=c()) {
  mkWorkerS <- function(subfn,yName,trainData) {
    force(subfn)
    force(yName)
    force(trainData)
    function(vi) {
      source('Lfns.R')
      si <- subfn(yName,vi,trainData,list(train=trainData))
      si$train$ndeviance
    }
  }
  worker <- mkWorkerS(doFitApplyGAMLR,yName,trainData)
  if(is.null(parallelCluster)) {
    vscores <- lapply(selvars,worker)
  } else {
    vscores <- parallel::parLapply(parallelCluster,selvars,worker)
  }
  vscores <- as.numeric(vscores)
  bestI <- which.min(vscores)[[1]]
  bestV <- selvars[[bestI]]
  doFitApplyGAMLR(yName,bestV,trainData,appDataList,bootScore)
}


# fns to estimate Bayes rate on small fraction of data

# build some cut points for a numeric variable (no NA,Inf)
# not strict quantiles to deal with issue of very common values
mkCuts <- function(v,nTarget) {
  # corner case
  nv <- length(unique(v))
  if(nv<=2) {
    return(mean(unique(v)))
  }
  med <- median(v)
  cuts <- med
  if(nTarget>1) {
    lv <- v[v<med]
    if(length(lv)>1) {
      cuts <- c(cuts,mkCuts(lv,nTarget/2))
    }
    rv <- v[v>med]
    if(length(rv)>1) {
      cuts <- c(cuts,mkCuts(rv,nTarget/2))
    }
  }
  sort(unique(cuts))
}

docut <- function(v,breaks) {
  if(length(breaks)>1) {
    v <- pmin(max(breaks),pmax(min(breaks),v))
    cut(v,breaks,include.lowest=TRUE)
  } else {
    f <- factor(paste(c('<','>='),breaks[[1]]))
    ifelse(v<breaks[[1]],f[[1]],f[[2]])
  }
}

encodeCols <- function(d,selvars,colCuts) {
  ec <- lapply(selvars,
               function(v) {
                 docut(d[[v]],colCuts[[v]])
               })
  ec <- do.call(paste,ec)
  ec
}

# identical rows on subset of data, scored only on where finds dups
# a crude attempt to get a Bayes-rate style model
# The idea is it can get an artificially good score as it gets
# to choose not to score rows (and exclude skips from scoring).
# But it isn't a Bayes rate model, as it has to actually implement
# row grouping.
doFitApplyDupModelX <- function(yName,selvars,trainData,appDataList,
                                bootScore=FALSE,parallelCluster=c(),
                                outSample=FALSE) {
  colCuts <- lapply(selvars,
                    function(v) {
                      mkCuts(trainData[[v]],3)
                    })
  names(colCuts) <- selvars
  encodedCols <- encodeCols(trainData,selvars,colCuts)
  t <- table(encodedCols)
  t <- t[t>1]
  repeats <- names(t)
  pmap <- lapply(repeats,function(k) {
    vi <- trainData[[yName]][encodedCols==k]
    ni <- length(vi)
    nt <- sum(vi)
    list(num=nt,den=ni)
  })
  names(pmap) <- repeats
  lapply(appDataList,function(d) {
    # randomForest doesn't seem to export its predict function in a nice way
    pred <- pmap[encodeCols(d,selvars,colCuts)]
    goodPred <- vapply(seq_len(length(pred)),
                       function(i) {!is.null(pred[[i]])},
                       logical(1))
    predG <- pred[goodPred]
    yG <- d[[yName]][goodPred]
    num <- vapply(predG,function(r) r$num,numeric(1))
    den <- vapply(predG,function(r) r$den,numeric(1))
    if(outSample) {
      predP <- (num-yG+0.05)/(den-1+0.1) # out-sample smooted
    } else {
      predP <- (num+0.05)/(den+0.1) # in-sample smooted
    }
    if(bootScore) {
      perfScoresB(yG,predP)
    } else {
      perfScores(yG,predP)
    }
  })
}






# to debug:
# 1) run most of LStep.R
# selvars <- c('Var218_catB', 'Var225_catB', 'Var229_catB', 'Var126_isBAD')
# trainData <- treatedTrainM
# appDataList <- list(train=treatedTrainM,stepcal=treatedTrainS,test=treatedTest)
# scoredDatList <- doFitApplyLR(yName,selvars,trainData,appDataList)
# y = scoredDatList$test$y
# pred = scoredDatList$test$pred




# assumes groupKey is constant on eresult
plotResult <- function(eresult,plotRanges=TRUE) {
  tasks <- list('normalized.deviance'='\\.ndeviance',
                'AUC'='\\.auc',
                'normalized.deviance.stratified'='\\.ndeviance\\.S',
                'AUC.stratified'='\\.auc\\.S')
  plots <- list()
  for(testTitle in sort(names(tasks))) {
    testStr <- paste(tasks[[testTitle]],'(()|(\\.lW)|(\\.uW))$',sep='')
    testCols <- colnames(eresult)[grep(testStr,colnames(eresult))]
    if(length(testCols)>0) {
      title <- paste(testTitle,
                     '\nmodel=\'',eresult$fitterName[[1]],
                     '\', selector=\'',eresult$selectorName[[1]],
                     '\', eta=',eresult$eta[[1]],
                     sep='')
      pltTable <- eresult[,c('n',testCols)]
      pltTable <- reshape2::melt(pltTable,
                                 id.vars='n',
                                 value.name=testTitle,
                                 variable.name='test')
      pltTable$what <- 'observed'
      pltTable$what[grep('.lW$',pltTable$test)] <- 'lW'
      pltTable$what[grep('.uW$',pltTable$test)] <- 'uW'
      pltTable$what <- factor(pltTable$what)
      pltTable$test <- gsub('.lW$','',as.character(pltTable$test))
      pltTable$test <- factor(gsub('.uW$','',pltTable$test))
      pltTable <- dcast(pltTable,n + test ~ what,value.var=testTitle)
      plt <- ggplot(data=pltTable,aes(x=n,
                                      y=observed,
                                      color=test,
                                      fill=test))
      if(plotRanges&&('lW' %in% colnames(pltTable))&&
         ('uW' %in% colnames(pltTable))) {
        plt <- plt + geom_errorbar(aes(ymin=lW,ymax=uW),
                                   alpha=0.8,linetype=2,width=0.25) +
          geom_ribbon(aes(ymin=lW,ymax=uW),
                      alpha=0.2,color=NA)
      }
      plt <- plt +
        geom_line() +
        geom_point() +
        ylab(testTitle) +
        ggtitle(title)
      plots[[testTitle]] <- plt
    }
  }
  plots
}

allFitters <- list('logistic regression'=doFitApplyLR,
                   'gbm'=doFitApplyGBM,
                   'elastic net logistic regression'=doFitApplyGLMNet,
                   'GAM logistic regression'=doFitApplyGAMLR,
                   'random forest'=doFitApplyRF,
                   'null model'=doFitApplyNullModel,
                   'best single variable model'=doFitApplySingleVarModel)



plotResultRanges <- function(results,plotRanges=TRUE,
                             plotRestriction=c()) {
  tasks <- list('normalized.deviance'='\\.ndeviance',
                'AUC'='\\.auc',
                'normalized.deviance.stratified'='\\.ndeviance\\.S',
                'AUC.stratified'='\\.auc\\.S')
  plots <- list()
  for(testTitle in sort(names(tasks))) {
    testStr <- paste(tasks[[testTitle]],'(()|(\\.lW)|(\\.uW))$',sep='')
    testCols <- colnames(results)[grep(testStr,colnames(results))]
    if(length(testCols)>0) {
      title <- testTitle
      pltTable <- results[,c('model',testCols)]
      pltTable <- reshape2::melt(pltTable,
                                 id.vars='model',
                                 value.name=testTitle,
                                 variable.name='test')
      pltTable$what <- 'observed'
      pltTable$what[grep('.lW$',pltTable$test)] <- 'lW'
      pltTable$what[grep('.uW$',pltTable$test)] <- 'uW'
      pltTable$model <- factor(as.character(pltTable$model))
      pltTable$test <- gsub('.lW$','',as.character(pltTable$test))
      pltTable$test <- factor(gsub('.uW$','',pltTable$test))
      pltTable <- dcast(pltTable,model + test ~ what,value.var=testTitle)
      if(!is.null(plotRestriction)) {
        pltTable <- pltTable[grep(plotRestriction,pltTable$test),]
      }
      plt <- ggplot(data=pltTable,aes(y=model,color=model,
                                      x=observed)) +
        geom_point(size=3)
      if(plotRanges&&('lW' %in% colnames(pltTable))&&
         ('uW' %in% colnames(pltTable))) {
        plt <- plt + geom_errorbarh(aes(xmin=lW,xmax=uW))
      }
      plt <- plt + facet_wrap(~test,ncol=1) +
        xlab(testTitle) +
        ggtitle(title) +
        theme(legend.position="none")
      plots[[testTitle]] <- plt
    }
  }
  plots
}







# To debug:
# run most of LStep.R
# eta = 1.0e-2
# fitterName = 'logistic regression'
# selectorName = 'step cal deviance'
# eresults = runExperiment(fitterName,selectorName,eta)
# write.table(eresults,file='fitres.tsv',quote=FALSE,sep="\t",row.names=FALSE)
# or 
# chosenVars <- names(treatmentsC$varScores)[treatmentsC$varScores<1]
# scoreFList <- list(train=treatedTrainM,stepcal=treatedTrainS,test=treatedTest)
# bootScores <- doFitApplyGAMLR(yName,chosenVars,treatedTrainM,scoreFList,bootScore=TRUE)
# # source a lot of LStep.Rmd
# print(plotResult(eresults))

