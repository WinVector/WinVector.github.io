#' build a A/B test plan where the plan is to run an initial segment of B events and then either end (at maxSteps)
#' or switch permanently to A events for the remainder of the steps.
#' @param maxSteps integer >0, total number of events available to be split between B and A
#' @param pA exact value of an A event
#' @param alpha0 Beta prior for value of binomial B event (mean=alph0/(alpha0+beta0))
#' @param beta0 Beta prior for value of binomial B event (mean=alph0/(alpha0+beta0))
#' @return stratTable where stratTable[[nB+1]] is the minimal number of b-successes needed for the optimal play to be to run another B experiment
buildBayesPlan <- function(maxSteps,pA,
                           alpha0=0.5,beta0=0.5,pBbound=1.0) {
  buildBayesPlanG(maxSteps,pA,buildGraph=FALSE,
                  alpha0,beta0,pBbound)$stratTable
}

#' build a A/B test plan where the plan is to run an initial segment of B events and then either end (at maxSteps)
#' or switch permanently to A events for the remainder of the steps.
#' @param maxSteps integer >0, total number of events available to be split between B and A
#' @param pA exact value of an A event
#' @param buildGraph logical if TRUE build detailed graph explaining calculation
#' @param alpha0 Beta prior for value of binomial B event (mean=alph0/(alpha0+beta0))
#' @param beta0 Beta prior for value of binomial B event (mean=alph0/(alpha0+beta0))
#' @return list(stratTable,detailGraph) stratTable where stratTable[[nB+1]] is the minimal number of b-successes needed for the optimal play to be to run another B experiment
buildBayesPlanG <- function(maxSteps,pA,buildGraph,
                           alpha0=0.5,beta0=0.5,pBbound=1.0) {
  futureValues <- numeric(maxSteps+1)
  # for a given nB
  # futureValues[[b+1]] represents the estimated expected value of
  # continuing on an optimal strategy from 
  # nB attempts at source B, having seen b successes from source B each valued at 1.
  detailGraph <- c()
  if(buildGraph) {
    detailGraph <- list()
  }
  stratTable <- numeric(maxSteps)
  # stratTable[[nB+1]] is the minimal number of b-successes needed for the optimal 
  # play to be to run another B experiment
  
  # move total trials back to solve for earlier valuations
  for(nB in (maxSteps-1):0) {
    # vector of possible win counts
    winB <- 0:nB
    # posterior probabilty of b-rate given each observed win count
    pBEst <- pmin(pBbound,(winB+alpha0)/(nB+alpha0+beta0))
    # value of switching to A forever more
    v1 <- numeric(nB+1) + pA*(maxSteps-nB)
    # value of trying one more B
    v2 <- pBEst*(1+futureValues[2:(nB+2)]) + 
      (1-pBEst)*(0+futureValues[1:(nB+1)])
    # value of current states
    v <- pmax(v1,v2)
    # copy out details for possible reporting
    if(buildGraph) {
      details <- list()
      for(wB in winB) {
        details[[as.character(wB)]] <- list(step=nB,
                                            bwins=wB,
                                            pWinEst=pBEst[[wB+1]],
                                            vA=v1[[wB+1]],
                                            vB=v2[[wB+1]])
      }
      detailGraph[[as.character(nB)]] <- details
    }
    # pick first index where v2>v1, as our level to continue with b count
    # if there is no such index pick infinity
    # confirm (without pB bound) decision moves from FALSE to TRUE once (if it moves)
    switchCount <- match(TRUE,v2>v1+1.0e-5)
    if(is.na(switchCount)) {
      switchCount <- Inf
     }
    stratTable[[nB+1]] <- switchCount-1 # number of successes needed to justify a B move
    futureValues <- v
  }
  list(stratTable=stratTable,detailGraph=detailGraph)
}

#' @param stratTable where stratTable[[nB+1]] is the minimal number of b-successes needed for the optimal play to be to run another B experiment
#' @param pA exact value of an A event
#' @param pB expected value of a pB event (binomial distribution)
scorePlan <- function(stratTable,pA,pB) {
  maxSteps <- length(stratTable)
  futureValues <- numeric(maxSteps+1)
  # for a given nB
  # futureValues[[b+1]] represents the estimated expected value of
  # continuing on an optimal strategy from 
  # nB attempts at source B, having seen b successes from source B each valued at 1.
  
  # move total trials back to solve for earlier valuations
  for(nB in (maxSteps-1):0) {
    # value of switching to A forever more
    v1 <- numeric(nB+1) + pA*(maxSteps-nB)
    # value of trying one more B
    v2 <- pB*(1+futureValues[2:(nB+2)]) + 
      (1-pB)*(0+futureValues[1:(nB+1)])
    # get values of current states under declared strategy
    v <- v1
    # vector of decisions to go to B based on strategy
    bIndices <- (0:nB)>=stratTable[[nB+1]]
    v[bIndices] <- v2[bIndices]
    futureValues <- v
  }
  futureValues
}


numFormat <- function(x) {
  formatC(x,format="f",digits=2)
}

# need Rgraphviz
mkPlanGraph <- function(plan,title=c(),minBStep=0,smallLabel=FALSE) {
  baseColor <- 'royalblue'
  goColor <- 'green'
  unreachedColor <- 'grey'
  stopColor <- 'red'
  colorToLineType = list()
  colorToLineType[[baseColor]] <- 1
  colorToLineType[[goColor]] <- 1
  colorToLineType[[unreachedColor]] <- 2
  colorToLineType[[stopColor]] <- 1
  isReachable <- list()
  nodeToLabel <- list()
  nodeToColor <- list()
  edgeToLabel <- list()
  edgeToColor <- list()
  rEG <- new("graphNEL",  edgemode="directed")
  # add nodes (with reachablility calculation)
  lastStep <- length(plan$detailGraph)-1
  for(nB in 0:lastStep) {  # visit nodes in graded order
    nB <- as.character(nB)
    details <- plan$detailGraph[[nB]]
    for(wB in names(details)) {
      rec <- details[[wB]]
      key <- paste(nB,wB)
      if(!(key %in% names(isReachable))) {
        isReachable[[key]] <- FALSE
      }
      isStop <- FALSE
      valueB = "?"
      nodeToColor[[key]] <- unreachedColor
      if(rec$step<=minBStep) {
        isReachable[[key]] <- TRUE
      }
      if(rec$step<minBStep) {
         nodeToColor[[key]] <- baseColor
      } else {
        if(isReachable[[key]]) {
          if(rec$vB<=rec$vA) {
            isStop <- TRUE
            nodeToColor[[key]] <- stopColor
          } else {
            nodeToColor[[key]] <- goColor
          }
        }
        valueB = numFormat(rec$vB)
      }
      if(isReachable[[key]] && (!isStop) && (rec$step<lastStep)) {
        for(bDelta in 0:1) {
          key2 <- paste(as.character(rec$step+1),as.character(rec$bwins+bDelta))
          isReachable[[key2]] <- TRUE
        }
      }
      if(!smallLabel) {
         label <- paste('step',rec$step,'bwins',rec$bwins,
                     '\npbEst',numFormat(rec$pWinEst),
                     '\nvalueA',numFormat(rec$vA),
                     '\nvalueB',numFormat(valueB))
      } else {
        label <- paste(rec$step,rec$bwins)
      }
      nodeToLabel[[key]] <- label
      rEG <- addNode(key,rEG)
    }
  }
  # add edges
  for(nB in names(plan$detailGraph)) {
    details <- plan$detailGraph[[nB]]
    for(wB in names(details)) {
      rec <- details[[wB]]
      goNode <- rec$vB>rec$vA
      key1 <- paste(nB,wB)
      eLabels = c('miss','hit')
      for(bDelta in 0:1) {
        key2 <- paste(as.character(rec$step+1),as.character(rec$bwins+bDelta))
        if(key2 %in% names(isReachable)) {
           rEG <- addEdge(key1,key2,rEG)
           ekey <- paste(key1,key2,sep='~')
           edgeToLabel[[ekey]] <- eLabels[bDelta+1]
           edgeToColor[[ekey]] <- ifelse(nodeToColor[[key1]]==stopColor,
                                         unreachedColor,nodeToColor[[key1]])
        }
      }
    }
  }
  edgeAttrs=list()
  if(!smallLabel) {
    edgeAttrs$label=edgeToLabel
  }
  rEG <- layoutGraph(rEG,
                     nodeAttrs=list(label=nodeToLabel),
                     edgeAttrs=edgeAttrs)
  nri <- list(col=nodeToColor,
              lty=lapply(nodeToColor,function(x) colorToLineType[[x]]))
  stopNames <- names(nodeToColor)[nodeToColor==stopColor]
  if(length(stopNames)>0) {
    nri$fill <- list()
    for(ni in stopNames) {
      nri$fill[[ni]] <- 'indianred1'
    }
  }
  nodeRenderInfo(rEG) <- nri
  edgeRenderInfo(rEG) <- list(col=edgeToColor,
                              lty=lapply(edgeToColor,function(x) colorToLineType[[x]]))
  rEG
}

