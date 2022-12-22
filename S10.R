# posterior mean
posterior_mean = function(x, sigma, mu, tau){
  mu_post = ((length(x)/(sigma*sigma))*mean(x) + (1/(tau*tau))) / ((length(x)/(sigma*sigma)) + (1/(tau*tau)))
    
  return(mu_post)
}


n = 10
m = 100
tau_dev = 10
mu_priori = 5
sigma_priori = 6

priori_dist = rnorm(n, mean=mu_priori, sd=tau_dev)


x = rnorm(m, mean = posterior_mean(priori_dist, sigma_priori, mu_priori, tau_dev), sd = sigma_priori)

# posterior variance
posterior_variance = function(x, sigma, mu, tau){
  posterior_var = 1 / ( (length(x)/(sigma*sigma)) + (1/(tau*tau)))
  
  return(posterior_var)
}



# 1.2
n_1 = 2




##############################

#####################
##### Exercise 1 #####
#####################



post.mean = function(x,
                     sigma, # Known standard deviation
                     mu, # Prior mean
                     tau){ # Prior standard deviation
  n = length(x)
  a = n/sigma^2
  b = 1/tau^2
  mu.hat = (a * mean(x) + b * mu)/(a+b)
  return(mu.hat)
}



post.variance = function(x,
                         sigma,
                         mu,
                         tau){
  n = length(x)
  a = n/sigma^2
  b = 1/tau^2
  sigma.squared.hat = (a+b)^-1
  return(sigma.squared.hat )
}


#####################
##### Exercise 2 #####
#####################



n = 2000
x = rnorm(n, mean = 80, sd = 10)
values = seq(0,100,0.1)
plot(values, dnorm(values,
                   mean = post.mean(x, sigma = 10, mu = 50, tau = sqrt(25)),
                   sd = post.variance(x, sigma = 10, mu = 50, tau = sqrt(25))),
     type = "l")



#####################
##### Exercise 3 #####
#####################

sigma_val = 15
n = 500
x = rnorm(n, mean = 200, sd = 100)
values = c(185, 200, 225, 208, 194, 217, 220, 203, 206, 190)
values2 = seq(0, 500, 0.1)
plot(values2, dnorm(values2,
                    mean = post.mean(x, sigma = 15, mu = 200, tau = sqrt(100)),
                    sd = post.variance(x, sigma = 15, mu = 200, tau = sqrt(100))),
     type = "l")


1-pnorm(200, mean=post.mean(x, sigma = 15, mu = 200, tau = 100), sqrt(post.variance(x, sigma = 15, mu = 200, tau = 100)))


##### Prof sol.

#####################
##### Exercise 3#####
#####################
values = seq(0,500,0.1)
plot(values, dnorm(values, 200, 100), type = "l", col = "blue") # Prior density



x=c(185,200,225,208,194,217,220,203,206,190)
pMean = post.mean(x, sigma = 15, mu = 200, tau = 100) # Posterior mean
pVariance = post.variance(x, sigma = 15, mu = 200, tau = 100) # Posterior variance
plot(values, dnorm(values,
                   mean = pMean,
                   sd = sqrt(pVariance)),type = "l")



1-pnorm(200, pMean, sqrt(pVariance)) # Probability unsafe to swim


######################

### Ex. 4

n = 200
x = rnorm(n, mean = 80, sd = 100)
values = seq(0,100,0.1)
plot(values, dnorm(values,
                   mean = post.mean(x, sigma = 10, mu = 50, tau = sqrt(250)),
                   sd = post.variance(x, sigma = 10, mu = 50, tau = sqrt(250))),
     type = "l")

line(values, dnorm(values,
                   mean = post.mean(x, sigma = 10, mu = 50, tau = sqrt(2500)),
                   sd = post.variance(x, sigma = 10, mu = 50, tau = sqrt(2500))))


### prof solution

#####################
##### Exercise 4#####
#####################



# Convergence to improper prior
tau.sqaured = 1
values = seq(-50,50,0.1)
plot(values, dnorm(values,
                   mean = 0,
                   sd = sqrt(tau.sqaured)),
     type = "l",
     col = "blue",
     ylim = c(0,0.5))
tau.sqaured = 100
lines(values, dnorm(values,
                    mean = 0,
                    sd = sqrt(tau.sqaured)),
      type = "l",
      col = "red",
      ylim = c(0,0.5))
tau.sqaured = 10000
lines(values, dnorm(values,
                    mean = 0,
                    sd = sqrt(tau.sqaured)),
      type = "l",
      col = "black",
      ylim = c(0,0.5))
legend("topright", legend=c("tau^2 = 1", "tau^2 = 100", "tau^2 = 10000"),
       col=c("red", "blue", "black"), lty=1)


# 1.3 with improper prior
x=c(185,200,225,208,194,217,220,203,206,190)
pMean = mean(x) # Posterior mean
pVariance = 15^2/length(x) # Posterior variance
values = seq(0,500,0.1)
plot(values, dnorm(values,
                   mean = pMean,
                   sd = sqrt(pVariance)),type = "l")



1-pnorm(200, pMean, sqrt(pVariance)) # Probability unsafe to swim


##################

# Ex. 1.5
r1 = dbeta(30,11,30)
r2 = dbeta(30,25,8)

theta = 5
p = (0.5*1/r1)*theta^(11-1)*(1-theta)^(30-1) + (0.5*1/r2)*theta^(25-1) * (1-theta)^(8-1)


values = seq(0,1, 0.001)
plot(values, 0.5*dbeta(values,11,30)+0.5*dbeta(values,25,8), type="l", col="blue")


#### prof solution

#####################
##### Exercise 5#####
#####################



# Plot of distribution
values = seq(0,1,0.001)
plot(values, 0.5*dbeta(values,11,30)+0.5*dbeta(values,25,8), type = "l", col = "blue")



# Equal tail interval
n = 10000
sim = vector()
for(i in 1:n){
  temp = rbinom(n = 1, size = 1, prob = 0.5)
  if(temp == 1){
    sim = c(sim, rbeta(1, 11, 30))
  }else{
    sim = c(sim, rbeta(1, 25, 8))
  }
}



equTail = quantile(x = sim, probs = c(0.025, 0.975))
values = seq(equTail[1],equTail[2],0.001)
lines(values, 0.5*dbeta(values,11,30)+0.5*dbeta(values,25,8), type = "l", col = "red", lwd = "2")






# HPD
library(hdrcde)
HPD = hdr(sim, prob = 95)
values = seq(0,1,0.001)
values1 = seq(HPD$hdr[1],HPD$hdr[2],0.001)
values2 = seq(HPD$hdr[3],HPD$hdr[4],0.001)
plot(values, 0.5*dbeta(values,11,30)+0.5*dbeta(values,25,8), type = "l", col = "blue")
lines(values1, 0.5*dbeta(values1,11,30)+0.5*dbeta(values1,25,8), type = "l", col = "red", lwd = "2")
lines(values2, 0.5*dbeta(values2,11,30)+0.5*dbeta(values2,25,8), type = "l", col = "red", lwd = "2")