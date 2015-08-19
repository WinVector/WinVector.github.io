
source('Lfns.R')
source('Afns.R')


# k-repetition cross validate fit
crossK <- 5
set.seed(729375)


# load the data as in the book
# change this path to match your directory structure
# dir = '~/Documents/work/PracticalDataScienceWithR/zmPDSwR/KDD2009/'
dir = './'

d = read.table(paste(dir,'orange_small_train.data.gz',sep=''),
               header=T,sep='\t',na.strings=c('NA',''), 
               stringsAsFactors=FALSE)
churn = read.table(paste(dir,'orange_small_train_churn.labels.txt',sep=''),
                   header=F,sep='\t')
d$churn = churn$V1
appetency = read.table(paste(dir,'orange_small_train_appetency.labels.txt',sep=''),
                       header=F,sep='\t')
d$appetency = appetency$V1
upselling = read.table(paste(dir,'orange_small_train_upselling.labels.txt',sep=''),
                       header=F,sep='\t')
d$upselling = upselling$V1
yName = 'churn'
yTarget = 1

nCoreEstimate <-  parallel::detectCores()
print(paste('core estimate',nCoreEstimate))
parallelCluster = parallel::makeCluster(nCoreEstimate)

crossValF <- c()
for(crossRep in seq_len(crossK)) {
  d$rgroup = runif(dim(d)[[1]])
  dTrainM = subset(d,rgroup<=0.7)  # set for building models
  dTrainC = subset(d,(rgroup>0.7) & (rgroup<=0.9)) # set for impact coding
  dTest = subset(d,rgroup>0.9) # set for evaluation
  outcomes = c('churn','appetency','upselling')
  vars = setdiff(colnames(dTrainM),
                 c(outcomes,'rgroup'))
  
  # build treatments on just the coding data
  treatmentsC = designTreatmentsC(dTrainC,
                                  vars,yName,yTarget,
                                  smFactor=2.0, 
                                  parallelCluster=parallelCluster,
                                  scoreVars=TRUE)
  
  # prepare data
  treatedTrainM = prepare(treatmentsC,
                          dTrainM,
                          pruneLevel=c())
  varSet = setdiff(colnames(treatedTrainM),yName)
  treatedTrainM[[yName]] = treatedTrainM[[yName]]==yTarget
  print(summary(treatedTrainM[[yName]]))
  
  treatedTest = prepare(treatmentsC,
                        dTest,
                        pruneLevel=c())
  treatedTest[[yName]] = treatedTest[[yName]]==yTarget
  print(summary(treatedTest[[yName]]))
  
  
  chosenVars <- names(treatmentsC$varScores)[treatmentsC$varScores<1]
  
  
  # debug
  # allFitters <- list('logistic regression'=doFitApplyLR,
  #                   'null model'=doFitApplyNullModel)
  
  
  # get performance on train and test
  scoreFList <- list(train=treatedTrainM,test=treatedTest)
  mkWorkerF1 <- function(allFitters,yName,chosenVars,treatedTrainM,scoreFList) {
    force(allFitters)
    force(yName)
    force(chosenVars)
    force(treatedTrainM)
    force(scoreFList)
    function(modelTitle) {
      source('Lfns.R')
      source('Afns.R')
      fitter <- allFitters[[modelTitle]]
      # not sure it is safe to pass parallel cluster to workers
      bootScores <- fitter(yName,chosenVars,treatedTrainM,scoreFList,
                           bootScore=FALSE,parallelCluster=c())
      cbind(data.frame(model=modelTitle,
                       stringsAsFactors=FALSE),
            data.frame(as.list(unlist(bootScores))))
    } 
  }
  w1 <- mkWorkerF1(allFitters,yName,chosenVars,
                   treatedTrainM,scoreFList)
  resListC <- parallel::parLapply(parallelCluster,names(allFitters),w1)
  if(length(resListC)!=length(allFitters)) {
    stop("not all results came back from parLapply")
  }
  resF <- data.frame(data.table::rbindlist(resListC))
  resF$crossRep <- crossRep
  crossValF <- rbind(crossValF,resF)  # only doing this a few times, so rbind() is okay
}


# shutdown, clean up
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster = NULL
}

save(list=ls(),file="csteps.RData")
