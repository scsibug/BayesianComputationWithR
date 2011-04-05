# Chapter 2
require(LearnBayes)

defaultChartDim <- 500 # Default size of graphics
dir.create("ch2_img")

# 2.3 Using a Discrete Prior
# subjective belief:
# possible values for p
p <- seq(0.05, 0.95, by = 0.1)
# weights for the possible p values
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
png("ch2_img/discrete_prior.png", width=defaultChartDim, height=defaultChartDim)
plot(p, prior, type = "h", ylab="Prior Probability");
dev.off()

# data from the study (27 students, 11 had 8+ hours of sleep, 16 did not)
data <- c(11,16)
# compute the posterior from the prior, and our data
post <- pdisc(p, prior, data)
print(round(cbind(p, prior, post), 2))

# Make a pretty illustration
png("ch2_img/prior_and_posterior_discrete_distributions.png", width=defaultChartDim, height=defaultChartDim)
library(lattice)
PRIOR <- data.frame("prior",p,prior)
POST <- data.frame("posterior",p,post)
names(PRIOR) <- c("Type","P","Probability")
names(POST) <- c("Type","P","Probability")
data <- rbind(PRIOR,POST)
xyplot(Probability~P|Type,data=data,layout=c(1,2),type="h",lwd=3,col="black")
dev.off()

# 2.4 Using a Beta Prior
# prior belief is that p centered on 0.3, 90% confident p is less than 0.5
# prior distribution has median at 0.3, 90% less than 0.5
quantile2 <- list(p=.9,x=.5) # 90% less than 0.5
quantile1 <- list(p=.5,x=.3) # 50% less than 0.3
# use beta.select to find shape parameters that fit this type of prior knowledge
betas <- beta.select(quantile1,quantile2)
a <- betas[1]
b <- betas[2]
s <- 11 # successes
f <- 16 # failures

#Use dbeta to show densities of the beta distribution for prior, likelihood, and posterior
png("ch2_img/prior_and_posterior_beta_distributions.png", width=defaultChartDim, height=defaultChartDim)
curve(dbeta(x,a+s,b+f), from=0, to=1, xlab="p",ylab="Density",lty=1,lwd=4) #posterior
curve(dbeta(x,s+1,f+1),add=TRUE,lty=2,lwd=4) #likelihood
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4) #prior
legend(.7,4,c("Prior", "Likelihood", "Posterior"),lty=c(3,2,1),lwd=c(3,3,3))
dev.off()

# computing the likelihood that the proportion of heavy sleepers is greater than 0.5.
# this is done by using the cdf of the beta distribution with our posterior parameters.
# take the inverse because we want to know the likelihood greater than 0.5.
1-pbeta(0.5, a+s, b+f)
# result is 0.07, so not likely.

# to find a 90% estimate of the proportion of heavy sleepers, look at the 0.05 to 0.95 range.
# qbeta is the quantile function for the beta distribution
qbeta(c(0.05,0.95), a+s, b+f)

# previous work with beta has been exact, now we use a simulation
# produce 1,000 values from the posterior (beta) distribution
ps <- rbeta(1000, a+s, b+f)

png("ch2_img/posterior_beta_distribution_sampling.png", width=defaultChartDim, height=defaultChartDim)
histogram(ps,xlab="p",main="")                                                        
dev.off()

# same as before, estimate likelihood of p being greater than 0.5
sum(ps >= 0.5)/1000

# find range that is 90% likely the actual value for p is within
quantile(ps, c(0.05, 0.95))

# 2.5 Using a Histogram Prior
# here we represent an arbitrary prior with user-provided weights for each of ten subintervals
midpt <- seq(0.05, 0.95, by=0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior) # normalize

png("ch2_img/hist_prior_for_p.png", width=defaultChartDim, height=defaultChartDim)
curve(histprior(x,midpt,prior),from=0, to=1, xlab="p",ylab="Prior density",ylim=c(0,.3))
dev.off()

png("ch2_img/hist_posterior_for_p.png", width=defaultChartDim, height=defaultChartDim)
curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1),from=0, to=1, xlab="p",ylab="Posterior density")
dev.off()