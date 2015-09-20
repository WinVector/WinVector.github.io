
repo <- 'http://cran.cnr.berkeley.edu'
# once curl and devtools are on we can do just about anything
install.packages('curl',repos=repo)
install.packages('devtools',repos=repo)
# a bunch of usefull libraries
install.packages(c('ggplot2','reshape2','gbm','ROCR','data.table',
                   'dplyr','randomForest','mgcv','glmnet','caret','kernlab',
                   'isotone','snow'),repos=repo)
# a few of my own libraries, for experiments
install.packages('vtreat',repos=repo)
devtools::install_github("WinVector/WVPlots")
devtools::install_github("WinVector/daccum")
