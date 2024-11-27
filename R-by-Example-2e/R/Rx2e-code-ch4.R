# R by Example 2e
# Code for Chapter 4

library(RbyExample)
dat <- college

colleges <- subset(dat, complete.cases(dat))

stripchart(Retention ~ Tier, method="stack", pch=19,
           xlab="Retention Percentage",
           ylab="Tier", xlim=c(50, 100), data=colleges)

# identify(colleges$Retention, colleges$Tier, n=2,
#    labels=colleges$School)

 b.output <- boxplot(Retention ~ Tier, data=colleges, 
                     horizontal=TRUE, ylab="Tier",
                     xlab="Retention")

b.output$stats

b.output$out

b.output$group

plot(colleges$Retention, colleges$Grad.rate,
  xlab="Retention", ylab="Graduation Rate")

fit <- line(colleges$Retention, colleges$Grad.rate)
coef(fit)

plot(colleges$Retention, colleges$Grad.rate, 
     xlab="Retention", ylab="Graduation Rate")
abline(coef(fit))

plot(colleges$Retention, fit$residuals,
  xlab="Retention", ylab="Residual")
abline(h=0)

BGSU <- RbyExample::bgsu

plot(BGSU$Year, BGSU$Enrollment)

fit <- lm(Enrollment ~ Year, data=BGSU)
plot(BGSU$Year, BGSU$Enrollment)
abline(fit)

plot(BGSU$Year, fit$residuals)
abline(h=0)

BGSU$log.Enrollment <- log(BGSU$Enrollment)

plot(BGSU$Year, BGSU$log.Enrollment)
fit2 <- lm(log.Enrollment ~ Year, data=BGSU)
abline(fit2)

plot(BGSU$Year, fit2$residuals)
abline(h=0)

college1 <- subset(colleges, Tier==1)

stem(college1$Top.10)

f = c(0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95)
ff = f - (1 - f)
froot = sqrt(2  * f) - sqrt(2 * (1 - f))
flog = 1.15 * log10(f) - 1.15 * log10(1 - f)
D = data.frame(f, ff, froot, flog)
matplot(t(as.matrix(D)), 1:4, type="l", lty=1, lwd=1,
  xlab="Fraction", ylab="Transformed Fraction",
  xlim=c(-1.8, 2), ylim=c(0.5, 4.3))
matplot(t(as.matrix(D[c(1, 4, 7), ])),
  1:4, type="l", lwd=3, lty=1, add=TRUE)
lines(D[c(1, 7), 1], c(1, 1), lwd=2)
lines(D[c(1, 7) ,2], 2 * c(1, 1), lwd=2)
lines(D[c(1, 7), 3], 3 * c(1, 1), lwd=2)
lines(D[c(1, 7), 4], 4 * c(1, 1), lwd=2)
text(c(1.8, 1.5, 1.3, 1.3, 0, 0.5 ,1), 
  c(4, 3, 2, 1, 0.8, 0.8, 0.8),
  c("flog", "froot", "ff", "f", "f=.05", "f=.5", "f=.95"))

froot <- sqrt(college1$Top.10) - sqrt(100 - college1$Top.10)
flog <- log(college1$Top.10 + 0.5) - log(100 - college1$Top.10 + 0.5)

stem(froot)

stem(flog)

college34 <- subset(colleges, Tier==3 | Tier==4)
college34$froot <- sqrt(college34$Top.10) - 
                   sqrt(100 - college34$Top.10)

boxplot(froot ~ Tier, data=college34, horizontal=TRUE,
        xlab="Froot(Top 10 Pct)", ylab="Tier")

library(ggplot2)
ggplot(colleges, aes(y=factor(Tier), x=Retention, fill=factor(Tier))) +
  geom_dotplot(dotsize=.5, binwidth=1) +
  labs(fill="Tier", x="Retention Percentage", y="Tier")

library(ggplot2)
ggplot(colleges, aes(x=Retention, y=Tier, group=Tier)) +
  geom_boxplot() + ylab("Tier")
ggplot(colleges, aes(x=Retention, y=Tier, group=Tier)) +
  geom_violin() + ylab("Tier")

fit <- line(colleges$Grad.rate ~ colleges$Retention)
library(ggplot2)

ggplot(colleges, aes(x=Retention, y=Grad.rate)) + 
  geom_point() + ylab("Graduation Rate") +
  geom_abline(intercept=fit$coef[1], slope=fit$coef[2], color=4)

ggplot(colleges, aes(x=Retention, y=fit$residuals)) +
  geom_point() +
  geom_hline(yintercept=0, color=4)
