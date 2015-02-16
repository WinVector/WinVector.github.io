# Generate some examples for the validating models slide
library(ggplot2)

set.seed(45345354)
 # -------------- example of bias --------------
N = 100
x = runif(N) - 0.5

d = data.frame(x=x, y=x^3 + 0.01*rnorm(N))
mod_d = lm(y~x, data=d)
d$pred_y = predict(mod_d, newdata=d)
d$model = "biased"
# unbiased
g = data.frame(x=x, y=0.1*x+0.01*rnorm(N))
mod_g = lm(y~x, data=g)
g$pred_y = predict(mod_g, newdata=g)
g$model = "unbiased"

perf = rbind(d[, c("y", "pred_y", "model")], g[, c("y", "pred_y", "model")])

ggplot(perf, aes(x=pred_y, y=pred_y-y)) +
  geom_point() + geom_abline(slope=0, color="blue") +
  scale_x_continuous("predicted outcome") + scale_y_continuous("predicted - true outcome") +
  facet_wrap(~model)

# --------------  variance: trees are high variance ------------------
library(rpart)
library(ggplot2)

set.seed(45345354)

generateSet = function(npts) {
  means = c(-1, 0, 1)
  names(means) = c("a", "b", "c")
  x1 = rnorm(npts)
  x2 = pi*(runif(npts)-0.5)
  x3 = sample(names(means), size=npts, replace=TRUE)
  x3_v= rnorm(npts) + means[x3]
  y = 2*x1 + sin(x2) + x3_v
  data.frame(x1=x1, x2=x2, x3=x3, y=y)
}

Ntrain = 100
Ntest=10
Nexp = 100

test = generateSet(Ntest)

experiments=NULL
for(i in seq_len(Nexp)) {
  train = generateSet(Ntrain)
  fmla = as.formula(y ~ x1+x2+x3)
  tfit = rpart(fmla, data=train)
  lfit = lm(fmla, data=train)

  # run it on test, and then collect the results
  # not efficient to do it this way, but whatevs
  tmp = rbind(data.frame(instance=seq_len(Ntest), model="high variance",
                         prediction=predict(tfit, newdata=test, type="vector")),
              data.frame(instance=seq_len(Ntest), model="low variance",
                         prediction=predict(lfit, newdata=test, type="response")))
  if(is.null(experiments)) {
    experiments=tmp
  } else {
    experiments=rbind(experiments, tmp)
  }
}


p75 = function(x) quantile(x, 0.75)
p25 = function(x) quantile(x, 0.25)


# there's probably a ddply way to do this
stats = aggregate(prediction ~ model+instance, data=experiments, FUN=mean)
colnames(stats) = c("model", "instance", "mean")
stats$min = aggregate(prediction~model+instance, data=experiments, FUN=min)[,3]
stats$max = aggregate(prediction~model+instance, data=experiments, FUN=max)[,3]
stats$q25 = aggregate(prediction~model+instance, data=experiments, FUN=p25)[,3]
stats$q75 = aggregate(prediction~model+instance, data=experiments, FUN=p75)[,3]

ggplot(stats, aes(x=as.factor(instance), y=mean)) +
  geom_linerange(aes(ymin=min, ymax=max)) +
  geom_crossbar(aes(ymin=q25, ymax=q75), fill="gray") +
  facet_wrap(~model)

#------ to make the test and training error graphs

set.seed(45345354)
N = 100
x = runif(N) - 0.5

g = data.frame(x=x, y=0.1*x+0.01*rnorm(N))
mod_g = lm(y~x, data=g)
g$pred_y = predict(mod_g, newdata=g)

ggplot(g, aes(x=pred_y, y=y)) +
  geom_point() + geom_abline(slope=1, color="blue") + coord_fixed()

# test set
xt = runif(N) - 0.5
f = data.frame(x=xt, y=0.1*xt+0.01*rnorm(N))
f$pred_y = predict(mod_g, newdata=f)

ggplot(f, aes(x=pred_y, y=y)) +
  geom_point() + geom_abline(slope=1, color="blue") + coord_fixed()

g$data="g"
f$data="f"
both = rbind(g, f)

ggplot(both, aes(x=pred_y, y=y, color=data)) +
  geom_point() + geom_abline(slope=1, color="blue") + coord_fixed()
