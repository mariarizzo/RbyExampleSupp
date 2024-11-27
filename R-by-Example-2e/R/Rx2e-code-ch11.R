# R by Example 2e
# Code for Chapter 11

set.seed(1)

library(RbyExample)
head(wasterunup)
#stack the data, create group var.
waste <- stack(wasterunup) 
waste <- na.omit(waste)
str(waste)

names(waste)[2] <- "plant"
summary(waste)

table(waste$plant)

with(waste,
     by(values, plant, summary))

boxplot(values ~ plant, data = waste)
stripchart(values ~ plant, data = waste, vertical=TRUE)

L <- lm(values ~ plant, data = waste)
plot(L, which=1:2)  #the first two residual plots

oneway.test(values ~ plant, data = waste)

rand.oneway <- function(response, group, R=199) {
  test <- oneway.test(response ~ group)
  observed.F <- test$statistic
  stats <- replicate(R, {
    random.labels <- sample(group)
    oneway.test(response ~ random.labels)$statistic
    })
  p <- sum(stats >= observed.F) / (R+1)
  test$method <- "Randomization test for equal means"
  test$p.value <- p
  test
}

rand.oneway(response=waste$values, 
            group=waste$plant, R=999)

Fstat <- function(dat, i) {
  oneway.test(values ~ plant[i], data=dat)$statistic
}

oneway.test(values ~ plant, data=waste)$statistic
Fstat(waste, 1:NROW(waste))

replicate(3, expr={
  k <- sample(1:NROW(waste))
  Fstat(waste, k)
})

library(boot)
boot.out <- boot(waste, statistic=Fstat, R=999, sim="permutation")
boot.out

(pval <- (1 + sum(boot.out$t >= boot.out$t0)) / 1000)

MASS::truehist(boot.out$t, xlab="F")
abline(v = boot.out$t0, lwd=2)

dat <- with(density(boot.out$t), data.frame(x, y))

library(ggplot2)
s <- sprintf("Pr(F > %6.3f) = %5.3f", boot.out$t0, pval)
ggplot(data = dat, aes(x, y)) + 
  geom_line() +
  geom_area(aes(x = ifelse(x>boot.out$t0, x, boot.out$t0)), fill = 2, alpha = .2) + 
  annotate("text", x = boot.out$t0, y = .1, label = s, hjust = "left") +
  labs(x = "statistic", y = "density")

library(ggplot2)
web <- RbyExample::webhits
ggplot(web, aes(Week, Hits)) +
  geom_point() +
  geom_smooth()

cor.test(web$Week, web$Hits, method="spearman")

rand.correlation <- function(x, y, R=199) {
    ranks.x <- rank(x)
    ranks.y <- rank(y)
    observed.r <- cor(ranks.x, ranks.y)
    stats <- replicate(R, {
      random.ranks <- sample(ranks.y)
      cor(ranks.x, random.ranks)
    })
    r0 <- abs(observed.r)
    p.value <- (sum(stats < -r0) + sum(stats > r0)) / (R + 1)
    list(observed.r = observed.r, p.value = p.value)
}

(rand.correlation(web$Week, web$Hits, R=1000) -> ptest)

Sstat <- function(dat, i) {
  # the statistic to replicate
  ranks.x <- rank(dat$Week)
  ranks.y <- rank(dat$Hits, ties.method="random")[i]
  cor(ranks.x, ranks.y)
}

boot.out2 <- boot(web, statistic=Sstat, R=999, sim="permutation")
boot.out2
r0 <- abs(boot.out2$t0)
pval2 <- (1 + (sum(boot.out2$t < -r0) + sum(boot.out2$t > r0))) / (1000)
pval2

MASS::truehist(boot.out2$t, xlab = "r")
lines(density(boot.out2$t), col = 2, lwd = 2)
abline(v = boot.out2$t0, lwd = 2)
abline(v = -boot.out2$t0, lwd = 2)

library(ggplot2)
dat2 <- with(density(boot.out2$t), data.frame(x, y))
r0 <- abs(boot.out2$t0)

ggplot(data = dat2, aes(x, y)) + 
  geom_line() + 
  geom_area(aes(x = ifelse(x < -r0, x, -r0)), 
            fill = 4, alpha=.2) +
  geom_area(aes(x = ifelse(x > r0, x, r0)), 
            fill = 4, alpha=.2) +
  geom_vline(xintercept=c(-r0, r0), linetype="dotted") +
  annotate("point", x = r0, y = 0) +
  annotate("text", x = r0, y = 1, 
           label = round(boot.out2$t0, 3)) +
  labs(x = "statistic", y = "density")

library(energy)
dcor.test(web$Week, web$Hits, R=999) -> dctest
dctest

dcor.test(rank(web$Week), rank(web$Hits), R=999)

MASS::truehist(dctest$replicates, xlab="replicates")
lines(density(dctest$replicates), lwd=2, col=2)
abline(v=dctest$statistic)
