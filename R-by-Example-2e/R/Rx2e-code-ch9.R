# R by Example 2e
# Code for Chapter 9

library(RbyExample)
str(flicker)

library(RbyExample)
str(flicker)

is.factor(flicker$Colour)
levels(flicker$Colour)

unclass(flicker$Colour)

with(flicker, {
  boxplot(Flicker ~ Colour, ylab = "Flicker")
  stripchart(Flicker ~ Colour, vertical=TRUE)
})

with(flicker,
  by(Flicker, Colour, FUN=mean))

meansd <- function(x) c(mean = mean(x), sd = sd(x))
with(flicker,
  by(Flicker, Colour, FUN = meansd))

oneway.test(Flicker ~ Colour, data = flicker)

oneway.test(Flicker ~ Colour, 
            data = flicker, var.equal=TRUE)

L <- lm(Flicker ~ Colour, data = flicker)
L

predict(L)

M <- aov(Flicker ~ Colour, data = flicker)
model.tables(M, type="means")

model.tables(M)

options(show.signif.stars=FALSE)
anova(L)

plot(L$fit, L$res)
abline(h=0)   #add horizontal line through 0

#Normal-QQ plot of residuals with reference line
qqnorm(L$res)
qqline(L$res)

MSE <- 2.3944
t97.5 <- qt(.975, df=16) #97.5th percentile of t
with(flicker, {
  table(Colour)                  #sample sizes
  means <- by(Flicker, Colour, mean)  #treatment means
  outer(means, means, "-")
})

(n <- table(flicker$Colour))
t97.5 * sqrt(MSE * outer(1/n, 1/n, "+"))

with(flicker, 
  pairwise.t.test(Flicker, Colour, data = flicker))

qtukey(.95, nmeans=3, df=16)

L <- lm(Flicker ~ Colour, data = flicker)
anova(L)

M <- aov(Flicker ~ Colour, data = flicker)
M

summary(M)

TukeyHSD(M)

plot(TukeyHSD(M))

resist.dat <- SiRstv
str(resist.dat)
head(resist.dat)

#Instrument is not a factor
is.factor(resist.dat$Instrument)

#convert Instrument to factor
resist.dat$Instrument <- as.factor(resist.dat$Instrument)

str(resist.dat)      #check our result

boxplot(Resistance ~ Instrument, data = resist.dat)
stripchart(Resistance ~ Instrument, data = resist.dat,  vertical=TRUE)

with(resist.dat, { 
by(Resistance, Instrument, 
   FUN=function(x) {c(mean(x), sd(x))})
  })

L2 <- aov(Resistance ~ Instrument, data = resist.dat)
par(mfrow=c(2, 2), mai=c(.5,.1,.5,.1)) #4 plots on one screen
plot(L2) #residual plots

plot(TukeyHSD(L2), cex.axis=.7)  #plot the Tukey CI's

anova(L)

n <- table(flicker$Colour) #sample sizes
a <- length(n)        #number of levels
N <- sum(n)           #total sample size
SS.total <- (N - 1) * var(flicker$Flicker)
#within-sample variances
vars <- by(flicker$Flicker, flicker$Colour, var)   
SSE <- sum(vars * (n - 1))

#degrees of freedom
print(c(a - 1, N - a, N - 1)) 

#treatment means
means <- by(flicker$Flicker, flicker$Colour, mean)
grandmean <- sum(flicker$Flicker) / N
SST <- sum(n * (means - grandmean)^2)
print(SST)


MST <- SST / (a - 1)
MSE <- SSE / (N - a)
statistic <- MST / MSE
p.value <- pf(statistic, df1=a-1, df2=N-a,
              lower.tail=FALSE)

print(as.data.frame(
   list(MST = MST, MSE = MSE, 
        F = statistic, p = p.value)))
