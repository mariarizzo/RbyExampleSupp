# R by Example 2e
# Code for Chapter 3

tosses <- c("H", "T", "H", "H", "T", "H", "H",
           "T", "H", "H", "T", "T", "H", "T",
           "T", "T", "H", "H", "H", "T")

table(tosses)

table(tosses) / length(tosses)

prop.tosses <- table(tosses) / length(tosses)

plot(prop.tosses)
barplot(prop.tosses)

y <- c(1, 4, 3, 5, 4, 2, 4)

possible.rolls <- c(1, 2, 3, 4, 5, 6)

labels.rolls <- c("one", "two", "three", "four", "five", "six")

fy <- factor(y, levels=possible.rolls, labels=labels.rolls)

fy

table(fy)

k <- 0:12
p <- dbinom(k, size=12, prob=1/3)

Binom <- round(26306 * p)
names(Binom) <- k

Weldon <- c(185, 1149, 3265, 5475, 6114, 5194, 3067,
   1331, 403, 105, 14, 4, 0)
names(Weldon) <- k

counts <- cbind(Binom, Weldon)
barplot(counts, beside=TRUE)

plot(k, Binom, type="h", lwd=2, lty=1, ylab="Count")
lines(k + .2, Weldon, type="h", lwd=2, lty=2)
legend("topright", legend=c("Binomial", "Weldon"),
       lty=c(1,2), lwd=c(2,2), bty="n")

cWeldon <- c(Weldon[1:10], sum(Weldon[11:13]))
cWeldon

probs <- c(p[1:10], 1 - sum(p[1:10]))
chisq.test(cWeldon, p=probs)

test <- chisq.test(cWeldon, p=probs)
plot(0:10, test$residuals,
   xlab="k", ylab="Residual")
abline(h=0)

library(RbyExample)
twn <- twins

table(twn$EDUCL)
table(twn$EDUCH)

c.EDUCL <- cut(twn$EDUCL, breaks=c(0, 12, 15, 16, 24),
   labels=c("High School", "Some College", "College Degree",
   "Graduate School"))
c.EDUCH <- cut(twn$EDUCH, breaks=c(0, 12, 15, 16, 24),
   labels=c("High School", "Some College", "College Degree",
   "Graduate School"))

table(c.EDUCL)
prop.table(table(c.EDUCL))

barplot(prop.table(table(c.EDUCL)), cex.names=.7)

mosaicplot(table(c.EDUCL), main="")

table(c.EDUCL, c.EDUCH)

T1 <- table(c.EDUCL, c.EDUCH)
diag(T1)

sum(diag(T1)) / sum(T1)

plot(T1, main="")

c.wage <- cut(twn$HRWAGEL, c(0, 7, 13, 20, 150))

table(c.wage)

table(c.EDUCL, c.wage)

T2 <- table(c.EDUCL, c.wage)
prop.table(T2, margin = 1)

P <- prop.table(T2, 1)
barplot(t(P), ylim=c(0, 1.55), ylab="Proportion",
   cex.names=.75,
   legend.text=dimnames(P)$c.wage,  
   args.legend=list(x = "topright", bty="n"))

barplot(t(P), beside=TRUE, ylab="PROPORTION",  
   legend.text=dimnames(P)$c.wage,
   args.legend=list(x="topleft", bty="n"))

T2 <- table(c.EDUCL, c.wage)

S <- chisq.test(T2)

print(S)

S$expected

sum((T2 - S$expected) ^ 2 / S$expected)

1 - pchisq(54.57759, df=9)


names(S)

S$residuals

mosaicplot(T2, shade=FALSE)
mosaicplot(T2, shade=TRUE)

library(ggplot2)

df2 <- data.frame(
          group=factor(rep(c("Binomial","Weldon"),each=13)),
          k=rep(0:12, times=2), 
          n=c(Binom, Weldon))

ggplot(df2, aes(x=group, y=n, fill=group)) +
  geom_col(color='black', position='dodge2')

ggplot(df2, aes(x=k, y=n)) + 
  geom_col(color=1, fill=5) +
  facet_wrap(.~group)

ggplot(df2, aes(x=k, y=n, fill=group, color=group)) +  
  geom_col(position='dodge')

P <- prop.table(T2, 1)
df3 <- as.data.frame(P)
names(df3) <- c("Education", "Wage", "Proportion")
str(df3)

ggplot(df3, aes(x=Education, y=Proportion, fill=Wage)) +
  geom_col(color=1) +
  scale_fill_brewer() +
  labs(fill="Wage")
