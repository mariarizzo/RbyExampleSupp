# R by Example 2e
# Code for Chapter 7

set.seed(1)

sleep <- c(7.75, 8.5, 8, 6, 8, 6.33, 8.17, 7.75, 7, 6.5, 
          8.75, 8, 7.5, 3, 6.25, 8.5, 9, 6.5, 9, 9.5, 
          9, 8, 8, 9.5)

nine.hours <- ifelse(sleep >= 9, "yes", "no")
table(nine.hours)

y <- 5; n <- 24
Test <- prop.test(y, n, p=0.5, alternative="two.sided", correct=FALSE)

names(Test)

Test$estimate

Test$statistic
Test$p.value

Test$conf.int

y <- 5; n <- 24
Test.adj <- prop.test(y, n, p=0.5, 
                      alternative="two.sided",
                      correct=TRUE)
c(Test.adj$stat, p.value=Test.adj$p.value)

Test.exact <- binom.test(y, n, p=0.5)
c(Test.exact$stat, p.value=Test.exact$p.value)

2 * pbinom(5, size=24, prob=0.5)

Test.exact$conf.int

agresti.interval <- function(y, n, conf=0.95){
  n1 <- n + 4
  y1 <- y + 2
  phat <- y1 / n1
  me <- qnorm(1 - (1 - conf) / 2) * sqrt(phat * (1 - phat) / n1)
  c(phat - me, phat + me)
}

agresti.interval(y, n)

cnames <- c("Wilson Score Interval", "Clopper-Pearson",
 "Agresti-Coull")
cfunctions <- c("prop.test", "binom.test", "agresti.interval")
intervals <- rbind(Test$conf.int, Test.exact$conf.int,
   agresti.interval(y, n))

data.frame(Name=cnames, Function=cfunctions, 
           LO=intervals[ , 1], HI=intervals[ , 2])

hist(sleep)
qqnorm(sleep)
qqline(sleep)

plot(sleep)
sleep.new <- sleep[-14]

t.test(sleep.new, mu=8, conf.level=0.90)

wilcox.test(sleep, mu=8, conf.int=TRUE, conf.level=0.90) -> W

names(W)

W$statistic
W$p.value
W$conf.int

library(RbyExample)
str(twins)

log.wages <- log(twins$HRWAGEH)

College <- ifelse(twins$EDUCH > 12, "yes", "no")

boxplot(log.wages ~ College, 
        horizontal=TRUE, xlab="log Wage",
        names=c("High School", "Some College"))

t.test(log.wages ~ College)

t.test(log.wages ~ College, var.equal=TRUE)$p.value

t.test(log.wages ~ College, var.equal=TRUE)$statistic^2
oneway.test(log.wages ~ College, var.equal=TRUE)

wilcox.test(log.wages ~ College, conf.int=TRUE)

log.wages <- log(twins$HRWAGEH)
College <- ifelse(twins$EDUCH > 12, "yes", "no")

table(College)

resample <- function()
  t.test(log.wages ~ sample(College))$statistic

many.T <- replicate(1000, 
                   resample())

T.obs <- t.test(log.wages ~ College)$statistic
T.obs

hist(many.T)
abline(v=T.obs)

2 * mean(many.T < T.obs)

twins.diff <- subset(twins, EDUCL != EDUCH)

twins.diff <- twins.diff[complete.cases(twins.diff), ]

log.wages.low <- with(twins.diff,
   ifelse(EDUCL < EDUCH, log(HRWAGEL), log(HRWAGEH)))
log.wages.high <- with(twins.diff,
   ifelse(EDUCL < EDUCH, log(HRWAGEH), log(HRWAGEL)))

head(cbind(log.wages.low, log.wages.high))

hist(log.wages.high - log.wages.low)

t.test(log.wages.low, log.wages.high, paired=TRUE)
