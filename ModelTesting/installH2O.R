
# Setting up H2O for R from: http://www.h2o.ai/download/h2o/r

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
    if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turin/4/R")))
#library(h2o)
#localH2O = h2o.init(nthreads=-1)

# Finally, let's run a demo to see H2O at work.
#demo(h2o.kmeans)

# more sources:
# http://learn.h2o.ai/content/tutorials/deeplearning/
# http://www.h2o.ai/verticals/algos/deep-learning/

