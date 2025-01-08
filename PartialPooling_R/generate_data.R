library("dplyr")
library("tidyr")

set.seed(46346)
# example data


# the radon data -- read it just to get similar stats
srrs = read.table("srrs2.dat", header=TRUE, sep=",", stringsAsFactor=FALSE)

# target: log of radon activity (activity)
# grouping variable: county
radonMN = filter(srrs, state=="MN") %>%
  select("county", "activity") %>%
  filter(activity > 0) %>%
  mutate(activity = log(activity),
         county = base::trimws(county)) %>%
  mutate(critical = activity>1.5)

stats = radonMN %>% group_by(county) %>%
  summarize(amean = mean(activity),
            sd_gp = sd(activity),
            counts = n())

summarize(stats, mu0 = mean(amean),
          sd_between = sd(sd_gp, na.rm=TRUE))

NGP = nrow(stats)  # same as MN data
prob = stats$counts/sum(stats$counts)  # the proportions of the MN data

mu0 = 0
sigma_within =  0.7
sigma_between =  0.5

mu_gp = rnorm(NGP, mean=mu0, sigma_between)
gpnames = paste0("gp", formatC(1:NGP, width=2, flag=0))
names(mu_gp) = gpnames
true_mu = data.frame(gp = names(mu_gp), mu_gp=mu_gp, stringsAsFactors=FALSE)


# prob=NULL: uniform
generate_data = function(nrows, mu_gp, sigma_within,  prob=NULL) {
  ngps = length(mu_gp)
  gps = 1:ngps
  gpnames = names(mu_gp)
  indices = sample(gps, size=nrows, replace=TRUE, prob=prob)

  data =  data.frame(gp = gpnames[indices],
                     y = rnorm(nrows, sd=sigma_within) + mu_gp[indices],
                     stringsAsFactors = FALSE)

  # any missing levels, add one observation, just to make the point
  missing = setdiff(gpnames, unique(data$gp))
  if(length(missing > 0)) {
    data = rbind(data, data.frame(gp=missing,
                                  y=rnorm(length(missing), sd=sigma_within) + mu_gp[missing],
                                  stringsAsFactors=FALSE))
  }

  data
}

df = generate_data(1000, mu_gp, sigma_within, prob)
saveRDS(df, "synthdata.rds")
saveRDS(true_mu, "truemu.rds")
