## ---- echo=FALSE, fig.height=3----
#data = read.csv("birthweight.csv")
#x = data$BWT
set.seed(3153)
x=2945  + (729.0224/sqrt(3))*rt(189,df=3)  
hist(x, freq = FALSE, breaks = 30)
summary(x)
sd(x)
length(x)
###################


## ----eval=FALSE------------------
## #data = read.csv("birthweight.csv")
## #x = data$BWT  #
## set.seed(3153)
## x=2945  + (729.0224/sqrt(3))*rt(189,df=3)  #3=df/(df-2)
## ##########################
## tau0 = 225
## mu0 = 3000
## 
## #data
## n=189
## sigma= 600
## xbar=2961.34
## 
## #posterior SD
## tau_n = 1 / sqrt(n / sigma ^ 2 + 1 / tau0 ^ 2)
## #posterior mean
## mu_n = mu0 * (1 / tau0 ^ 2) / (1 / tau_n ^ 2) + xbar * (n / sigma ^ 2) / (1 / tau_n ^ 2)


## ---- echo=FALSE-----------------
tau0 = 225
mu0 = 3000

#data 
n=189
sigma= 600
xbar=2945

#posterior SD
tau_n = 1 / sqrt(n / sigma ^ 2 + 1 / tau0 ^ 2)
#posterior mean
mu_n = mu0 * (1 / tau0 ^ 2) / (1 / tau_n ^ 2) + xbar * (n / sigma ^ 2) / (1 / tau_n ^ 2)
theta = seq(mu0 - 3 * tau0, mu0 + 3 * tau0, 0.01) # the grid is just for plotting

# prior
prior = dnorm(theta, mu0, tau0)
# likelihood
likelihood = dnorm(xbar, theta, sigma / sqrt(n))
# posterior
posterior = dnorm(theta, mu_n, tau_n)

# plot
ymax = max(c(prior, posterior))
scaled_likelihood = likelihood * ymax / max(likelihood)

plot(theta, prior, type='l', col='red', xlim= range(theta), ylim=c(0, ymax), ylab='', yaxt='n',lwd=3, frame.plot = FALSE)
par(new=T)
plot(theta, scaled_likelihood, type='l', col='blue', xlim=range(theta), ylim=c(0, ymax), ylab='',  yaxt='n',lwd=3, frame.plot = FALSE)
par(new=T)
plot(theta, posterior, type='l', col='seagreen', xlim=range(theta), ylim=c(0, ymax), ylab='', yaxt='n',lwd=3, frame.plot = FALSE)
legend("topleft", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("red", " blue", "green"),lwd=3,  bty = "n")


## --------------------------------
qnorm(c(0.025,0.975), 2962.742, 42.85)


## ---- eval=FALSE-----------------
## library(rjags)
## Nrep = 10000
## Nchains = 3
## 
## # data
## # data has already been loaded in previous code
## # x is the full sample
## # n is the sample size
## 
## # model
## model_string <- "model{
## 
##   # Likelihood
##   for (i in 1:n){
##     x[i] ~ dnorm(theta, 1 / sigma ^ 2)
##   }
##   sigma <- 600
## 
## 
##   # Prior
##   theta ~ dnorm(mu0, 1 / tau0 ^ 2)
##   mu0 <- 3000
##   tau0 <- 225
## 
## }"
## 
## # Compile the model
## dataList = list(x=x, n=n)
## 
## model <- jags.model(textConnection(model_string),
##                     data=dataList,
##                     n.chains=Nchains)


## ---- echo=FALSE-----------------
library(rjags)
Nrep = 10000
Nchains = 3

# data
# data has already been loaded in previous code

# model
model_string <- "model{

  # Likelihood
  for (i in 1:n){
    x[i] ~ dnorm(theta, 1 / sigma ^ 2)
  }
  sigma <- 600
  
  #OR simply dnorm(theta, n / sigma ^ 2)
  

  # Prior
  theta ~ dnorm(mu0, 1 / tau0 ^ 2)
  mu0 <- 3000  
  tau0 <- 225

}"

# Compile the model
dataList = list(x=x, n=n)

model <- jags.model(textConnection(model_string), 
                    data=dataList,
                    n.chains=Nchains)


## --------------------------------
update(model, 1000, progress.bar="none")

posterior_sample <- coda.samples(model, 
                                 variable.names=c("theta"),
                                 n.iter=Nrep,
                                 progress.bar="none")
# Summarize and check diagnostics
summary(posterior_sample)


## --------------------------------
plot(posterior_sample)


## ---- fig.height=3---------------
theta_sim = rnorm(100000, mu_n, tau_n)
x_sim = rnorm(100000, theta_sim, sigma)
hist(x_sim, freq = FALSE, breaks = 30,
 xlab = "Birthweight (grams)",
 main = "Posterior predictive distribution")


## --------------------------------
quantile(x_sim, c(0.025, 0.975))


## --------------------------------
(sum(x < quantile(x_sim, 0.025)) + sum(x > quantile(x_sim, 0.975))) / n


## ---- eval=FALSE-----------------
## # plot the observed data
## hist(x, freq = FALSE, breaks = 30) # observed data
## 
## # number of samples to simulate
## n_samples = 100
## 
## # simulate thetas from posterior
## theta = rnorm(n_samples, mu_n, tau_n)
## 
## # simulate samples
## for (r in 1:n_samples){
## 
##   # simulate values from N(theta, sigma) distribution
##   y_sim = rnorm(n, theta[r], sigma)
## 
##   # add plot of simulated sample to histogram
##   lines(density(y_sim),
##     col = rgb(135, 206, 235, max = 255, alpha = 25))
## }


## ---- echo=FALSE, fig.height=6----
# plot the observed data
hist(x, freq = FALSE, ylim=c(0,0.0009), breaks = 30) # observed data

# number of samples to simulate
n_samples = 100

# simulate thetas from posterior
theta = rnorm(n_samples, mu_n, tau_n)

# simulate samples
for (r in 1:n_samples){

  # simulate values from N(theta, sigma) distribution
  x_sim = rnorm(n, theta[r], sigma)

  # add plot of simulated sample to histogram
  lines(density(x_sim),
    col = rgb(135, 206, 235, max = 255, alpha = 25))
}

