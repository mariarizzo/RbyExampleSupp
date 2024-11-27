# R by Example 2e
# Code for Chapter 8

plot(cars)    

lm(dist ~ speed, data = cars)

L1 <- lm(dist ~ speed, data = cars)
print(L1)

plot(cars, main = "dist = -17.579 + 3.932 speed", xlim = c(0, 25))
#line with intercept=-17.579, slope=3.932
abline(-17.579, 3.932)
curve(-17.579 + 3.932*x, add = TRUE)  #same thing

plot(L1, which=1, add.smooth=FALSE)

L2 <- lm(dist ~ 0 + speed, data = cars)
L2

plot(cars, main = "dist = 2.909 speed", xlim = c(0, 25))
#line with intercept=0, slope=2.909
abline(0, 2.909)

Trees <- trees
names(Trees)[1] <- "Diam"

pairs(Trees)
cor(Trees)

M1 <- lm(Volume ~ Diam, data = Trees)
print(M1)

with(Trees, {
  plot(Diam, Volume) #response vs predictor
  abline(M1$coef) #add fitted line
})

new <- data.frame(Diam = 16)
predict(M1, new)

plot(M1, which=1:2)

M2 <- lm(Volume ~ Diam + Height, data = Trees)
print(M2)

M3 <- lm(Volume ~ Diam + I(Diam^2) + Height, data = Trees)
print(M3)

plot(M3, which = 1:2)

summary(M3)

(broom::tidy(M3) -> t1)

t1[3, 5] #the p-value for the quadratic term

anova(M3)

anova(M1, M2, M3)

new <- data.frame(Diam = 16, Height = 70)
predict(M3, newdata = new)

predict(M3, newdata = new, interval = "pred")

predict(M3, newdata = new, interval = "conf")

diameter <- 16
height <- seq(65, 70, 1)
new <- data.frame(Diam=diameter, Height=height)
predict(M3, newdata=new, interval="conf")

library(RbyExample)

luna <- lunatics
names(luna) <- tolower(names(lunatics))

scatter.smooth(luna$dist, luna$phome)
cor(luna$dist, luna$phome)

library(ggplot2)
ggplot(luna, aes(dist, phome)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

luna$rdist <- 1 / luna$dist
plot(luna$rdist, luna$phome)
cor(luna$rdist, luna$phome)

M <- lm(phome ~ rdist, data = luna)
M

plot(luna$rdist, luna$phome)
abline(M)

plot(luna$dist, luna$phome)
curve(M$coef[1] + M$coef[2] / x, add = TRUE)

fn <- function(x) {M$coef[1] + M$coef[2] / x}

ggplot(luna, aes(dist, phome)) +
  geom_point() +
  geom_function(fun = fn)

plot(M$fitted, M$resid, xlab = "fitted", ylab = "residuals")
abline(h = 0, lty = 2)

luna[13, ]

str(CPUspeed)
summary(CPUspeed)

years <- CPUspeed$time - 1994
speed <- CPUspeed$speed
log2speed <- CPUspeed$log10speed / log10(2)

plot(years, speed)
plot(years, log2speed)

L <- lm(log2speed ~ years)
print(L)

plot(years, speed)
curve(2^(-3.6581 + 0.5637 * x), add = TRUE)

plot(years, log2speed)
abline(L)

plot(L, which = 1:2)

CPUspeed[c(16, 26, 27), ]

summary(L)$r.squared

new <- data.frame(years = 2005 + 316.5 / 365 - 1994)
lyhat <- predict(L, newdata=new)
lyhat

2^lyhat
