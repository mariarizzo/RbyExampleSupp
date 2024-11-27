# R by Example 2e
# Code for Chapter 2

library(RbyExample)
draftnums <- draftlottery

names(draftnums)

draftnums$Jan[15]

months <- draftnums[2:13]
sapply(months, median)

sapply(months, median, na.rm=TRUE)

medians <- sapply(months, median, na.rm=TRUE)
plot(medians, type="b", xlab="month number")

months <- draftnums[2:13]
boxplot(months)

library(MASS)  #load the package
# data()         #display available datasets

head(mammals)

summary(mammals)

boxplot(mammals)
boxplot(log(mammals), names=c("log(body)", "log(brain)"))

plot(mammals)

plot(log(mammals$body), log(mammals$brain),
  xlab="log(body)", ylab="log(brain)")

summary(log(mammals))

cor(log(mammals))

cor(log(mammals$body), log(mammals$brain))

plot(log(mammals$body), log(mammals$brain),
  xlab="log(body)", ylab="log(brain)")
x <- log(mammals$body); y <- log(mammals$brain)
abline(lm(y ~ x))

library(RbyExample)
head(twinIQ)

summary(twinIQ)

boxplot((Foster - Biological) ~ Social, data=twinIQ)

status <- as.integer(twinIQ$Social)
plot(Foster ~ Biological, data=twinIQ, pch=status, col=status)
legend("topleft", c("high","low","middle"),
  pch=1:3, col=1:3, inset=.02)
abline(0, 1)

coplot(Foster ~ Biological|Social, data=twinIQ)

library(lattice)
xyplot(Foster ~ Biological|Social, data=twinIQ, 
       pch = 20, col = 1)

library(RbyExample)  # ?brainsize for description
str(brainsize)

summary(brainsize)

mean(brainsize$Weight)

mean(brainsize$Weight, na.rm = TRUE)

by(data = brainsize[, 2:7], INDICES = brainsize$Gender, 
   FUN = colMeans, na.rm = TRUE)

with(brainsize, {
  gender <- as.integer(factor(Gender))  
  plot(Weight, MRI_Count, pch = gender, col = gender)
  legend("topleft", c("Female", "Male"), 
         pch = 1:2, col = 1:2, inset = .02)
  })

pairs(brainsize[, 2:7], cex=.5, gap=0)

brainsize2 <- brainsize
medIQ <- median(brainsize2$FSIQ)
brainsize2$Group <- ifelse(brainsize2$FSIQ < medIQ, "low", "high")
head(brainsize2$Group)

table(brainsize2$Gender, brainsize2$Group)
by(brainsize2[2:4], brainsize2$Group, 
   FUN = colMeans, na.rm = TRUE)

round( cor(brainsize[, 2:7]), 2)

round( cor(brainsize[, 2:7], 
           use = "pairwise.complete.obs"), 2)

mri <- brainsize$MRI_Count / brainsize$Weight
  cor(brainsize$FSIQ, mri, use="pairwise.complete.obs")

cor.test(brainsize$FSIQ, brainsize$MRI_Count)

cor.test(brainsize$FSIQ, mri)$p.value

which(is.na(brainsize), arr.ind=TRUE)

brainsize[c(2, 21), ]

i <- which(brainsize$Gender == "Male")
maleWt <- mean(brainsize$Weight[i], na.rm = TRUE)
maleHt <- mean(brainsize$Height[i], na.rm = TRUE)
brainsize[2, 5] <- maleWt
brainsize[21, 5] <- maleWt 
brainsize[21, 6] <- maleHt 

brainsize[c(2, 21), ]

nhtemp

plot(nhtemp)

plot(nhtemp, ylab="Mean annual temperatures")
abline(h = mean(nhtemp), lty=2)
lines(lowess(nhtemp), col=4)

diff(nhtemp)

d <- diff(nhtemp)
plot(d, ylab="First differences of mean annual temperatures")
abline(h = 0, lty=3, col=2)
lines(lowess(d), col=3)

colMeans(randu)
var(randu)

 diag(var(randu))

cor(randu)

library(lattice)
cloud(z ~ x + y, data=randu)

means <- rowMeans(randu)

hist(means)

plot(density(means))

truehist(means)
curve(dnorm(x, 1/2, sd=sqrt(1/36)), add=TRUE)

qqnorm(means)
qqline(means)

mammals2 <- mammals
m <- median(mammals2$body)
mammals2$size <- ifelse(mammals2$body >= m, 
                        "large", "small")

head(mammals2)

mammalsL <- subset(mammals2, size == "large")

head(mammalsL)

which(mammals2$body > 2000)

mammals2[c(19, 33), ]

max(mammals2$body)

which.max(mammals2$body)
mammals2[33, ]

which.min(mammals2$body)
mammals2[14, ]

x <- mammals[1:5, ]   #the first five
x

o <- order(x$body)
o

x[o, ]

o <- order(mammals$body)
sorted.data <- mammals[o, ]

tail(sorted.data, 3)

data(mammals)

x <- mammals[1:5, ]
round( dist(x), 2)

round( as.matrix(dist(x)), 2)

plot(log(mammals$body), log(mammals$brain),
     xlab="log(body)", ylab="log(brain)")

plot(log(mammals$body), log(mammals$brain),
     xlab="log(body)", ylab="log(brain)")
y <- log(mammals[c("Chimpanzee", "Cow", "Human"), ])
polygon(y)
text(y, rownames(y), adj=c(1, .5), col=2)

dist(y)

library(ggplot2)

logmammals <- log(mammals)
ggplot(logmammals, aes(x=body, y=brain)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('log(body size)') + 
  ylab('log(brain size)')

ggplot(twinIQ, aes(x=Social, y=Foster-Biological)) + 
  geom_boxplot()

ggplot(twinIQ, 
       aes(x=Biological, y=Foster, shape=Social, color=Social)) +
       geom_point() +
       geom_abline(slope=1, intercept=0)

ggplot(twinIQ, aes(Biological, Foster)) +
  geom_point() + 
  facet_grid(. ~ Social)
