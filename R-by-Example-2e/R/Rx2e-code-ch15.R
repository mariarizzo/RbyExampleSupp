# R by Example 2e
# Code for Chapter 15

## -------------------------------------------------------------
sam <- runif(1000, 10, 11.5)
annie <- runif(1000, 10.5, 12)


## -------------------------------------------------------------
prob <- sum(annie < sam) / 1000
prob


## -------------------------------------------------------------
plot(sam, annie, cex=.5)
polygon(c(10.5, 11.5, 11.5, 10.5),           
        c(10.5, 10.5, 11.5, 10.5), density=10, angle=135)


## -------------------------------------------------------------
sqrt(prob * (1 - prob) / 1000)


## -------------------------------------------------------------
difference <- annie - sam


## -------------------------------------------------------------
mc.est <- mean(difference)
se.est <- sd(difference) / sqrt(1000)
c(mc.est, se.est)


## -------------------------------------------------------------
sim.median <- function(n){
   median(rexp(n))
}


## -------------------------------------------------------------
M <- replicate(10000, sim.median(21))


## -------------------------------------------------------------
samp.med <- function(m, n){
   con <- factorial(n) / factorial((n - 1) / 2)^2
   con * pexp(m)^((n - 1) / 2) * (1 - pexp(m))^((n - 1) / 2) * dexp(m)
}


## -------------------------------------------------------------
hist(M, prob=TRUE, main="")
curve(samp.med(x, 21), add=TRUE, col="red", lwd = 2.5)


## -------------------------------------------------------------
(quantile(M, c(0.05, 0.95)) -> q95)


## -------------------------------------------------------------
y <- c(6.11, 5.04, 4.83, 4.89, 4.97, 5.15, 7.98, 4.62,
     6.19, 4.58, 6.34, 4.54, 6.37, 5.64, 4.53, 4.68,
     5.17, 4.72, 5.06, 4.96, 4.70)
median(y)


## -------------------------------------------------------------
c(4.97 - 1.118365, 4.97 - 0.398709)


## -------------------------------------------------------------
taxi <- function(N, n){
   y <- sample(N, size=n, replace=TRUE)
   N1 <- max(y)
   N2 <- 2 * mean(y)
   c(N1 = N1, N2 = N2)
}


## -------------------------------------------------------------
(taxi(100, 5) -> sim1)


## -------------------------------------------------------------
EST <- replicate(1000, taxi(100, 5))


## -------------------------------------------------------------
df_N1 <- data.frame(Estimate = "N1",
                      Value = EST["N1", ])
df_N2 <- data.frame(Estimate = "N2",
                      Value = EST["N2", ])
df_EST <- rbind(df_N1, df_N2)


## -------------------------------------------------------------
library(dplyr, warn.conflicts=FALSE)
(df_EST |> 
  group_by(Estimate) |> 
  summarize(Bias = mean(Value) - 100,
            SE = sd(Value) / sqrt(1000)) |> 
  mutate(LB = round(Bias - 2 * SE, 3 ),
         UB = round(Bias + 2 * SE, 3)) -> ci_output)


## -------------------------------------------------------------
library(ggplot2)
df_EST |> 
  mutate(ABS_ERROR = abs(Value - 100)) -> df_EST
ggplot(df_EST, aes(ABS_ERROR, Estimate)) +
  geom_boxplot()


## -------------------------------------------------------------
df_EST |> 
  group_by(Estimate) |> 
  summarize(Mean_Abs_Error = mean(ABS_ERROR),
            SE = sd(ABS_ERROR) / sqrt(1000))


## -------------------------------------------------------------
wald <- function(y ,n, prob){
  p <- y / n
  z <- qnorm(1 - (1 - prob) / 2)
  lb <- p - z * sqrt(p * (1 - p) / n)
  ub <- p + z * sqrt(p * (1 - p) / n)
  cbind(lb, ub)
}


## -------------------------------------------------------------
wald(5, 20, 0.95)


## -------------------------------------------------------------
y <- c(2, 4, 6, 8)
wald(y, 20, 0.90)


## -------------------------------------------------------------
mc.coverage <- function(p, n, prob, iter=10000){
  y <- rbinom(iter, n, p)
  c.interval <- wald(y, n, prob)
  mean((c.interval[ ,1] < p) & (p < c.interval[ ,2]))
}


## -------------------------------------------------------------
(mc.coverage(0.15, 20, 0.90) -> mcc)


## -------------------------------------------------------------
(sqrt(mcc * (1 - mcc) / 10000) -> se.mcc)


## -------------------------------------------------------------
many.mc.coverage <- function(p.vector, n, prob){
  sapply(p.vector, mc.coverage, n, prob)
}


## -------------------------------------------------------------
curve(many.mc.coverage(x, 100, 0.90), from=0.001, to=0.999,
   xlab="p", ylab="Coverage Probability",
   main=paste("n=", 100, ", prob=", 0.90),
   ylim=c(0.7, 1))
abline(h=.9)


## -------------------------------------------------------------
simulate.markov.chain <- function(P, starting.state, steps){
  n.states <- dim(P)[1]
  state <- starting.state
  STATE <- rep(0, steps)
  for(j in 1:steps){
    state <- sample(n.states, size=1, prob=P[state, ])
    STATE[j ] <- state
  }
  return(STATE)
}


## -------------------------------------------------------------
P <- matrix(c(0.50, 0.25, 0, 0, 0,0.25,
            0.25, 0.50, 0.25, 0, 0, 0,
            0, 0.25, 0.50, 0.25, 0, 0,
            0, 0, 0.25, 0.50, 0.25, 0,
            0, 0, 0, 0.25, 0.50, 0.25,
            0.25, 0, 0, 0, 0.25, 0.50),
            nrow=6, ncol=6, byrow=TRUE)
P


## -------------------------------------------------------------
s <- simulate.markov.chain(P, 3, 10000)


## -------------------------------------------------------------
table(s)


## -------------------------------------------------------------
table(s) / 10000


## -------------------------------------------------------------
w = c(1, 1, 1, 1, 1, 1) / 6


## -------------------------------------------------------------
w %*% P


## -------------------------------------------------------------
log.samp.med <- function(y, n){
   (n - 1) / 2 * log(pexp(y)) + 
   (n - 1) / 2 * log(1 - pexp(y)) +
   log(dexp(y))
}


## -------------------------------------------------------------
metrop.hasting.exp <- function(logf, current, iter, ...){
  S <- rep(0, iter)
  n.accept <- 0
  for(j in 1:iter){
    candidate <- rexp(1, current)
    prob <- exp(logf(candidate, ...) - logf(current, ...) +
       dexp(current, candidate, log=TRUE) -
       dexp(candidate, current, log=TRUE))
    accept <- ifelse(runif(1) < prob, "yes", "no")
    current <- ifelse(accept == "yes", candidate, current)
    S[j] <- current
    n.accept <- n.accept + (accept == "yes")
  }
  list(S=S, accept.rate=n.accept / iter)
}


## -------------------------------------------------------------
mcmc.sample <- metrop.hasting.exp(log.samp.med, 1, 10000, 21)


## -------------------------------------------------------------
mcmc.sample$accept.rate


## -------------------------------------------------------------
plot(mcmc.sample$S, cex=.5, pch=20)


## -------------------------------------------------------------
plot(density(mcmc.sample$S), lwd=2, main="", xlab="M")
curve(samp.med(x, 21), add=TRUE, lwd=2, lty=2)
legend("topright", c("Simulated", "Exact"), 
       lty=c(1, 2),lwd=c(2, 2))


## -------------------------------------------------------------
metrop.hasting.rw <- function(logf, current, C, iter, ...){
  S <- rep(0,  iter); n.accept <- 0
  for(j in 1:iter){
    candidate <- rnorm(1, mean=current, sd=C)
    prob <- exp(logf(candidate, ...) - logf(current, ...))
    accept <- ifelse(runif(1) < prob, "yes", "no")
    current <- ifelse(accept == "yes", candidate, current)
    S[j] <- current; n.accept <- n.accept + (accept == "yes")
  }
list(S=S, accept.rate=n.accept / iter)
}


## -------------------------------------------------------------
mcmc.sample1 <- metrop.hasting.rw(log.samp.med, 
                                 1, 1, 10000, 21)
mcmc.sample2 <- metrop.hasting.rw(log.samp.med, 
                                 1, 0.05, 10000, 21)


## -------------------------------------------------------------
plot(mcmc.sample1$S, type="l", main=paste(
   "Scale C = 1, Acceptance rate =",
   round(mcmc.sample1$accept.rate,2)))
plot(mcmc.sample2$S, type="l", main=paste(
   "Scale C = 0.05, Acceptance rate =",
   round(mcmc.sample2$accept.rate,2)))


## -------------------------------------------------------------
random.coin.gibbs <- function(p=0.5, m=1000){
  S <- matrix(0, m, 2)
  dimnames(S)[[2]] <- c("p", "y")
  for(j in 1:m){
    y <- rbinom(1, size=12, prob=p)
    p <- rbeta(1, y+4, 20-y)
    S[j, ] <- c(p, y)
  }
 return(S)
}


## -------------------------------------------------------------
sim.values <- random.coin.gibbs()


## -------------------------------------------------------------
plot(sim.values, cex=.5)


## -------------------------------------------------------------
table(sim.values[ ,"y"])

