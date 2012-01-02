# Pick a prior...
pmean=170
pvar=25 # stated sd of 5 for our prior

# compute prior prob. of null hypothesis
probH = pnorm(175,pmean,sqrt(pvar))

# compute prior prob. of alternative hypothesis
probA = 1 - probH

prior.odds = probH/probA
# 5.302974

# So, we have odds ~5 to 1 that we believe the null hypothesis.
# This makes perfect sense, because we've only considered the prior, which reflects
# our belief that our weight is 170 (a full standard deviation below the null hypothesis cutoff).

# Now, we bring in evidence.

weights = c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)
scale_var = 3
ybar = mean(weights)
# sampling variance (
sigma2 = scale_var^2 / length(weights)

# normal density/normal prior updating formula
# posterior precision (inverse variance) of mu is the sum of the precisions of the data and the prior
post.precision = 1/sigma2+1/pvar
post.var = 1/post.precision

# Posterior mean of mu is the weighted average of the sample mean and prior mean, weighted by precision.
post.mean = (ybar / sigma2 + pmean / pvar) / post.precision
# posterior mean and standard deviation.
c(post.mean, sqrt(post.var))

# Posterior density (parameters to the normal distribution, mean/stddev)
#175.7915058   0.9320546

# Recalculate the odds of the null hypothesis
post.odds = pnorm(175, post.mean, sqrt(post.var)) / (1-pnorm(175, post.mean, sqrt(post.var)))

# Find the bayes factor for the null hypothesis:
BF = post.odds / prior.odds
# 0.04652139

# Posterior probability of the null hypothesis
# We can compute this with the Bayes Factor and the prior:
# posterior = (prior * BF) / (prior * BF + 1 - prior) # (1 - prior = probA, in our example)
postH = probH*BF / (probH*BF+probA)
# 0.1978835

# So, we conclude it is only ~19% likely the author's weight is <= 175lbs.
