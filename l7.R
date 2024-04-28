## ---- echo=FALSE---------------------
theta =seq(0,1,0.00001)
plot(theta, dbeta(theta, shape1=3, shape2=1), ylim=c(0,6), type="l", col="black", ylab="prior of theta", lty=1,  lwd=3, frame.plot = FALSE)
lines(theta, dbeta(theta, shape1=1, shape2=3), col="red", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=3, shape2=3), col="blue", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=6, shape2=3), col="orange", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=6, shape2=1), col= "green", lty=1, lwd=3)
legend("topleft", c("Prior 1", "Prior 2", "Prior 3", "Prior 4", "Prior 5"),       col = c("black", "red", "blue", "orange","green" ), lty = c(5, 1),lwd=3, bty="n")


## ---- echo=FALSE---------------------
theta =seq(0,1,0.0001)
plot(theta, dbeta(theta, shape1=34, shape2=5), ylim=c(0,9), type="l", col="black", ylab="Posterior of theta given x", lty=1,  lwd=3, frame.plot = FALSE)
abline(v=31/35,col="pink", lwd=3)
lines(theta, dbeta(theta, shape1=32, shape2=7), col="red", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=34, shape2=7), col="blue", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=37, shape2=7), col="orange", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=37, shape2=5), col= "green", lty=1, lwd=3)
legend("topleft", c("Beta(34,5)", "Beta(32,7)", "Beta(34,7)", "Beta(37,7)", "Beta(37,5)"),       col = c("black", "red", "blue", "orange","green" ), lty = c(5, 1),lwd=3, bty="n")


## ---- echo=FALSE, fig.width=7, fig.height=5.5----
theta =seq(0,1,0.0001)

par(mfrow=c(1,2))

plot(theta, dbeta(theta, shape1=3, shape2=1), ylim=c(0,6), type="l", col="black", ylab="prior of theta", lty=1,  lwd=3, frame.plot = FALSE)
lines(theta, dbeta(theta, shape1=1, shape2=3), col="red", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=3, shape2=3), col="blue", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=6, shape2=3), col="orange", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=6, shape2=1), col= "green", lty=1, lwd=3)
legend("topleft", c("Prior 1", "Prior 2", "Prior 3", "Prior 4", "Prior 5"),       col = c("black", "red", "blue", "orange","green" ), lty = c(5, 1),lwd=3, bty="n")

plot(theta, dbeta(theta, shape1=34, shape2=5), ylim=c(0,9), type="l", col="black", ylab="Posterior of theta given x", lty=1,  lwd=3, frame.plot = FALSE)
abline(v=31/35,col="pink", lwd=3)
lines(theta, dbeta(theta, shape1=32, shape2=7), col="red", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=34, shape2=7), col="blue", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=37, shape2=7), col="orange", lty=1, lwd=3)
lines(theta, dbeta(theta, shape1=37, shape2=5), col= "green", lty=1, lwd=3)
legend("topleft", c("Beta(34,5)", "Beta(32,7)", "Beta(34,7)", "Beta(37,7)", "Beta(37,5)"),       col = c("black", "red", "blue", "orange","green" ), lty = c(5, 1),lwd=3, bty="n")


## ------------------------------------
# prior
alpha_prior = 3; beta_prior = 1; 
prior = dbeta(theta, alpha_prior, beta_prior)
# data
n = 35
x = 31
# likelihood
likelihood = dbinom(x, n, theta)
# posterior
alpha_post = alpha_prior + x
beta_post = beta_prior + n - x
#posterior = dbeta(theta, alpha_post, beta_post)
# posterior credible interval
qbeta(c(0.025, 0.975), alpha_post, beta_post)

