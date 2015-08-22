
source('Lfns.R')
source('Afns.R')

# load the data as in the book
# change this path to match your directory structure
#dir = '~/Documents/work/PracticalDataScienceWithR/zmPDSwR/KDD2009/'
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
set.seed(729375)
d$rgroup = runif(dim(d)[[1]])
dTrainM = subset(d,rgroup<=0.7)  # set for building models
dTrainC = subset(d,(rgroup>0.7) & (rgroup<=0.9)) # set for impact coding
dTest = subset(d,rgroup>0.9) # set for evaluation
rm(list=c('d','churn','appetency','upselling','dir'))
outcomes = c('churn','appetency','upselling')
vars = setdiff(colnames(dTrainM),
               c(outcomes,'rgroup'))
yName = 'churn'
yTarget = 1

set.seed(239525)
nCoreEstimate <-  parallel::detectCores()
print(paste('core estimate',nCoreEstimate))
parallelCluster = parallel::makeCluster(nCoreEstimate)

# build treatments on just the coding data
treatmentsC = designTreatmentsC(dTrainC,
                                vars,yName,yTarget,
                                smFactor=2.0, 
                                parallelCluster=parallelCluster)


# prepare data
treatedTrainM = prepare(treatmentsC,
                        dTrainM,
                        pruneSig=0.05)
varSet = setdiff(colnames(treatedTrainM),yName)
treatedTrainM[[yName]] = treatedTrainM[[yName]]==yTarget
print(summary(treatedTrainM[[yName]]))

treatedTest = prepare(treatmentsC,
                      dTest,
                      pruneSig=0.05)
treatedTest[[yName]] = treatedTest[[yName]]==yTarget
print(summary(treatedTest[[yName]]))


chosenVars <- names(treatmentsC$sig)[treatmentsC$sig<0.05]


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
                         bootScore=TRUE,parallelCluster=c())
    cbind(data.frame(model=modelTitle,
                     stringsAsFactors=FALSE),
          data.frame(as.list(unlist(bootScores))))
  } 
}
w1 <- mkWorkerF1(allFitters,yName,chosenVars,
                 treatedTrainM,scoreFList)
resList <- parallel::parLapply(parallelCluster,names(allFitters),w1)
if(length(resList)!=length(allFitters)) {
  stop("not all results came back from parLapply")
}



# get permuted training prerformance
n <- nrow(treatedTrainM)
yPerms <- lapply(1:10,function(i) {
  list(repNum=i,
       yP=treatedTrainM[[yName]][sample.int(n,n,replace=FALSE)])})
tasks <- list()
for(yP in yPerms) {
  for(modelTitle in names(allFitters)) {
    tasks[[1+length(tasks)]] <- list(repNum=yP$repNum,yP=yP$yP,modelTitle=modelTitle)
  }
}
mkWorkerF2 <- function(allFitters,yName,chosenVars,treatedTrainM) {
  force(allFitters)
  force(yName)
  force(chosenVars)
  force(treatedTrainM)
  function(task) {
    source('Lfns.R')
    source('Afns.R')
    modelTitle <- task$modelTitle
    yP <- task$yP
    repNum <- task$repNum
    fitter <- allFitters[[modelTitle]]
    treatedTrainP <- treatedTrainM
    treatedTrainP[[yName]] <- yP
    pScores <- fitter(yName,chosenVars,treatedTrainP,
                      list(xptrain=treatedTrainP))
    di <- cbind(data.frame(model=modelTitle,
                           repNum=repNum,
                           stringsAsFactors=FALSE),
                data.frame(as.list(unlist(pScores))))
  }
}
w2 <- mkWorkerF2(allFitters,yName,chosenVars,treatedTrainM)
resListP <- parallel::parLapply(parallelCluster,tasks,w2)
if(length(resListP)!=length(tasks)) {
  stop("not all results came back from parLapply")
}



# shutdown, clean up
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster = NULL
}

save(list=ls(),file="bsteps.RData")
