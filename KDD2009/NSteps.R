
# Plot effect of training y-noise on fit quality

source('Lfns.R')
source('Afns.R')

debug = FALSE

# load the data as in the book
# change this path to match your directory structure
dir = './'
if(debug) {
   dir = '~/Documents/work/PracticalDataScienceWithR/zmPDSwR/KDD2009/'
}

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
                                smFactor=2.0)


# TODO: need data prep to be post-noising as this problem is so sensitive to large categorical performance.

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

if(debug) {
  chosenVars <- chosenVars[1:5]
  treatedTrainM <- treatedTrainM[sample.int(nrow(treatedTrainM),200),]
  treatedTest <- treatedTest[sample.int(nrow(treatedTest),200),]
}




# get performance on test
scoreFList <- list(test=treatedTest,train=treatedTrainM)
# build work list
workList <- list()
for(rep in 1:2) {
  for(noiseLevel in c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35)) {
    # noise up some fraction of positions
    nnoise <- floor(noiseLevel*nrow(treatedTrainM))
    noiseIndices <- numeric(0)
    noiseValues <- logical(0)
    if(nnoise>0) {
      noiseIndices <- sample.int(nrow(treatedTrainM),nnoise)
      noiseValues <- rbinom(nnoise,1,prob=0.5)>=0.5
    }
    for(mn in names(allFitters)) {
      workList[[1+length(workList)]] <- list(modelTitle=mn,
                                             noiseLevel=noiseLevel,
                                             rep=rep,
                                             noiseIndices=noiseIndices,
                                             noiseValues=noiseValues)
    }
  }
}

mkWorkerN1 <- function(allFitters,yName,chosenVars,treatedTrainX,scoreFList) {
  force(allFitters)
  force(yName)
  force(chosenVars)
  force(treatedTrainX)
  force(scoreFList)
  function(workUnit) {
    source('Lfns.R')
    source('Afns.R')
    modelTitle <- workUnit$modelTitle
    noiseLevel <- workUnit$noiseLevel
    rep  <- workUnit$rep
    noiseIndices  <- workUnit$noiseIndices
    noiseValues  <- workUnit$noiseValues
    fitter <- allFitters[[modelTitle]]
    if(length(noiseIndices)>0) {
      treatedTrainX[[yName]][noiseIndices] <- noiseValues
    }
    scoreFList$ntrain <- treatedTrainX
    perfScores <- fitter(yName,chosenVars,treatedTrainX,scoreFList,
                         bootScore=FALSE,parallelCluster=c())
    cbind(data.frame(model=modelTitle,
                     noiseLevel=noiseLevel,
                     rep=rep,
                     stringsAsFactors=FALSE),
          data.frame(as.list(unlist(perfScores))))
  } 
}
w1 <- mkWorkerN1(allFitters,yName,chosenVars,
                 treatedTrainM,scoreFList)
if(!debug) {
  resList <- parallel::parLapply(parallelCluster,workList,w1)
} else {
  # run directly (without parLapply or lapply) to make tracing in easier
  resList <- vector(mode='list',length=length(workList))
  for(i in seq_len(length(workList))) {
    print(paste('start',i,date()))
    wi <- workList[[i]]
    resList[[i]] <- w1(wi)
    print(paste('done',i,date()))
  }
}
if(length(resList)!=length(workList)) {
  stop("not all results came back from parLapply")
}





# shutdown, clean up
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
  parallelCluster = NULL
}

rf = data.frame(data.table::rbindlist(resList))
saveRDS(rf,file='nResFrame.RData')

