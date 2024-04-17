###########################
# Chapter 1 - Introduction
###########################

x = c(109, 65, 22, 3, 1)
x <- c(109, 65, 22, 3, 1)

y = rpois(200, lambda=.61)
y <- rpois(200, lambda=.61)

temps = c(51.9, 51.8, 51.9, 53)

temps

temps - 32

(5/9) * (temps - 32)

CT = c(48, 48.2, 48, 48.7)
temps - CT

winner = c(185, 182, 182, 188, 188, 188, 185, 185, 177,
  182, 182, 193, 183, 179, 179, 175)
opponent = c(175, 193, 185, 187, 188, 173, 180, 177, 183,
  185, 180, 180, 182, 178, 178, 173)

length(winner)

year = seq(from=2008, to=1948, by=-4)

year = seq(2008, 1948, -4)

winner[4] = 189
winner[5] = 189

winner[4:5] = 189

winner

mean(winner)

mean(opponent)

difference = winner - opponent

data.frame(year, winner, opponent, difference)

data.frame(year, winner, opponent, difference)

taller.won = winner > opponent
taller.won

table(taller.won)

barplot(rev(difference), xlab="Election years 1948 to 2008",
 ylab="Height difference in cm")

plot(winner, opponent)

k = c(0, 1, 2, 3, 4)
x = c(109, 65, 22, 3, 1)

barplot(x, names.arg=k)

p = x / sum(x)
p

r = sum(p * k)
r

v = sum(x * (k - r)^2) / 199
v

f = r^k * exp(- r) / factorial(k)
f

f = dpois(k, r)
f

floor(200*f) #expected counts

x

cbind(k, p, f)

y = rpois(200, lambda=.61)
kicks = table(y) #table of sample frequencies
kicks

Theoretical = dpois(0:3, lambda=.61)
Sample = kicks / 200
cbind(Theoretical, Sample)

mean(y)

var(y)

example(mean)

var.n = function(x){
  v = var(x)
  n = NROW(x)
  v * (n - 1) / n
}

temps = c(51.9, 51.8, 51.9, 53)

var(temps)

var.n(temps)

f = function(x, a=1, b=1)
  x^(a-1) * (1-x)^(b-1)

x = seq(0, 1, .2) #sequence from 0 to 1 with steps of 0.2
f(x, a=2, b=2)

integrate(f, lower=0, upper=1, a=2, b=2)

beta(2, 2)

curve(x*(1-x), from=0, to=1, ylab="f(x)")

probs = c(.45, .05, .01, .48, .70, .50, .07, .25, .49)
P = matrix(probs, nrow=3, ncol=3)
P

rownames(P) <- colnames(P) <- c("lower", "middle", "upper")
P

rowSums(P)

apply(P, MARGIN=1, FUN=sum)

P2 = P %*% P
P2

P2[1, 3]

P2[1, ]

P4 = P2 %*% P2
P8 = P4 %*% P4
P8

Q = matrix(c(  0.45, 0.48, 0.07,
               0.05, 0.70, 0.25,
               0.01, 0.50, 0.49), nrow=3, ncol=3, byrow=TRUE)
Q

head(USArrests)

NROW(USArrests)
dim(USArrests) #dimension

names(USArrests)

str(USArrests)

arrests = as.matrix(USArrests)
str(arrests)

any(is.na(USArrests))

summary(USArrests)

USArrests["California", "Murder"]
USArrests["California", ]

USArrests$Assault

hist(USArrests$Assault)

library(MASS)
truehist(USArrests$Assault)

attach(USArrests)
murder.pct = 100 * Murder / (Murder + Assault + Rape)
head(murder.pct)

with(USArrests, expr={
  murder.pct = 100 * Murder / (Murder + Assault + Rape)
})
murder.pct

plot(UrbanPop, Murder)

pairs(USArrests)

cor(UrbanPop, Murder)

cor(USArrests)

y1 = c(22, 26)
y2 = c(28, 24, 29)
y3 = c(29, 32, 28)
y4 = c(23, 24)
y = c(y1, y2, y3, y4)
Model = c(rep("A", 2), rep("B", 3), rep("C", 3), rep("D", 2))
mileages = data.frame(y, Model)

str(mileages)
mileages

lunatics = read.table("lunatics.txt", header=TRUE)

str(lunatics)

lunatics

dat = read.table("college.txt", header=TRUE, sep="\t")

pidigits = read.table(
  "http://www.itl.nist.gov/div898/strd/univ/data/PiDigits.dat",
  skip=60)
head(pidigits)

table(pidigits)

prop = table(pidigits) / 5000  #proportions
prop

sqrt(.1 * .9 / 5000)

se.hat = sqrt(prop * (1-prop) / 5000)
round(rbind(prop, se.hat, prop-2*se.hat, prop+2*se.hat), 4)

barplot(prop, xlab="digit", ylab="proportion")
abline(h = .1)

library()

law
data(law)

install.packages("bootstrap")

data(package="bootstrap")

library(bootstrap)

library(help=bootstrap)
help(package=bootstrap)

law

mean(law)
cor(law$LSAT, law$GPA)

ls()

###############################
# Chapter 2 - Quantitative Data
###############################

library(MASS) #load the package
data() #display available datasets

?mammals

head(mammals)

is.matrix(mammals)
is.data.frame(mammals)

summary(mammals)

boxplot(mammals)

plot(mammals)

plot(log(mammals$body), log(mammals$brain),
 xlab="log(body)", ylab="log(brain)")

summary(log(mammals))

boxplot(log(mammals), names=c("log(body)", "log(brain)"))

cor(log(mammals))

cor(log(mammals$body), log(mammals$brain))

plot(log(mammals$body), log(mammals$brain),
  xlab="log(body)", ylab="log(brain)")
x = log(mammals$body); y = log(mammals$brain)
abline(lm(y ~ x))

twins = read.table("twinIQ.txt", header=TRUE)

head(twins)

summary(twins)

boxplot(Foster - Biological ~ Social, twins)

attach(twins)
status = as.integer(Social)
status

plot(Foster ~ Biological, data=twins, pch=status,  col=status)

legend("topleft", c("high","low","middle"),
 pch=1:3, col=1:3, inset=.02)

abline(0, 1)

coplot(Foster ~ Biological|Social, data=twins)

library(lattice)
xyplot(Foster ~ Biological|Social, data=twins, pch=20,  layout=c(2,2))

brain = read.table("brainsize.txt", header=TRUE)
summary(brain)

mean(brain$Weight)

mean(brain$Weight, na.rm=TRUE)

by(data=brain[, -1], INDICES=brain$Gender, FUN=mean, na.rm=TRUE)

attach(brain)
gender = as.integer(Gender) #need integer for plot symbol, color
plot(Weight, MRI_Count, pch=gender, col=gender)

legend("topleft", c("Female", "Male"), pch=1:2, col=1:2, inset=.02)

pairs(brain[, 2:7])

round(cor(brain[, 2:7]), 2)

round(cor(brain[, 2:7], use="pairwise.complete.obs"), 2)

mri = MRI_Count / Weight
cor(FSIQ, mri, use="pairwise.complete.obs")

cor.test(FSIQ, MRI_Count)

cor.test(FSIQ, mri)$p.value

which(is.na(brain), arr.ind=TRUE)

brain[c(2, 21), ]

brain[2, 5] = mean(brain$Weight, na.rm=TRUE)
brain[21, 5:6] = c(mean(brain$Weight, na.rm=TRUE),
  mean(brain$Height, na.rm=TRUE))

brain[c(2, 22), ]

nhtemp

plot(nhtemp)

plot(nhtemp, ylab="Mean annual temperatures")
abline(h = mean(nhtemp))
lines(lowess(nhtemp))

diff(nhtemp)

d = diff(nhtemp)
plot(d, ylab="First differences of mean annual temperatures")
abline(h = 0, lty=3)
lines(lowess(d))

draftnums = read.table("draft-lottery.txt", header=TRUE)

names(draftnums)

draftnums$Jan[15]

months = draftnums[2:13]
sapply(months, median)

sapply(months, median, na.rm=TRUE)

medians = sapply(months, median, na.rm=TRUE)
plot(medians, type="b", xlab="month number")

months = draftnums[2:13]
boxplot(months)

mean(randu)
var(randu)

diag(var(randu))

cor(randu)

means = apply(randu, MARGIN=1, FUN=mean)

hist(means)

hist(means, prob=TRUE)

plot(density(means))

truehist(means)
curve(dnorm(x, 1/2, sd=sqrt(1/36)), add=TRUE)

qqnorm(means)
qqline(means)

m = median(mammals$body)
mammals$size = ifelse(mammals$body >= m, "large", "small")

head(mammals)

subset(mammals, size=="large")

which(mammals$body 2000)
mammals[c(19, 33), ]

max(mammals$body)

which.max(mammals$body)

mammals[33, ]

which.min(mammals$body)

x = mammals[1:5, ] #the first five
x

o = order(x$body)

x[o, ]

o = order(mammals$body)
sorted.data = mammals[o, ]

tail(sorted.data, 3)

data(mammals)

x = mammals[1:5, ]

dist(x)

as.matrix(dist(x))

plot(log(mammals$body), log(mammals$brain),
 xlab="log(body)", ylab="log(brain)")

y = log(mammals[c("Grey wolf", "Cow", "Human"), ])
polygon(y)

text(y, rownames(y), adj=c(1, .5))

dist(y)

d = dist(log(mammals))
h = hclust(d, method="complete")

big = subset(mammals, subset=(body > median(body)))

d = dist(log(big))
h = hclust(d, method="complete")

plot(h)

head(h$merge)

rownames(mammals)[c(22, 28)]

log(mammals)[c(22, 28), ]


##########################################
#  Chapter 3 - Categorical Data
##########################################

tosses = scan(what="character")
H T H H T H H T H H T T
H T T T
H H H T

table(tosses)

table(tosses) / length(tosses)

prop.tosses = table(tosses) / length(tosses)

plot(prop.tosses)

barplot(prop.tosses)

#############################

y = c(1, 4, 3, 5, 4, 2, 4)

possible.rolls = c(1, 2, 3, 4, 5, 6)

labels.rolls = c("one", "two", "three", "four", "five", "six")

fy = factor(y, levels=possible.rolls, labels=labels.rolls)

fy

table(fy)

#############################

k = 0:12
p = dbinom(k, size=12, prob=1/3)

Binom = round(26306 * p)
names(Binom) = k

Weldon = c(185, 1149, 3265, 5475, 6114, 5194, 3067,
1331, 403, 105, 14, 4, 0)
names(Weldon) = k

data.frame(Binom, Weldon, Diff=Weldon - Binom)

counts = cbind(Binom, Weldon)
barplot(counts, beside=TRUE)

plot(k, Binom, type="h", lwd=2, lty=1, ylab="Count")
lines(k + .2, Weldon, type="h", lwd=2, lty=2)
legend(8, 5000, legend=c("Binomial", "Weldon"),
  lty=c(1,2), lwd=c(2,2))

cWeldon = c(Weldon[1:10], sum(Weldon[11:13]))
cWeldon

probs = c(p[1:10], 1 - sum(p[1:10]))
chisq.test(cWeldon, p=probs)

test = chisq.test(cWeldon, p=probs)
plot(0:10, test$residuals,
  xlab="k", ylab="Residual")
abline(h=0)

twn = read.table("twins.dat.txt", header=TRUE,
  sep=",", na.strings=".")

table(twn$EDUCL)
table(twn$EDUCH)

c.EDUCL = cut(twn$EDUCL, breaks=c(0, 12, 15, 16, 24),
  labels=c("High School", "Some College", "College Degree",
  "Graduate School"))
c.EDUCH = cut(twn$EDUCH, breaks=c(0, 12, 15, 16, 24),
  labels=c("High School", "Some College", "College Degree",
  "Graduate School"))

table(c.EDUCL)

prop.table(table(c.EDUCL))

barplot(prop.table(table(c.EDUCL)))

mosaicplot(table(c.EDUCL))

table(c.EDUCL, c.EDUCH)

T1=table(c.EDUCL, c.EDUCH)
diag(T1)

sum(diag(T1)) / sum(T1)

plot(T1)

c.wage = cut(twn$HRWAGEL, c(0, 7, 13, 20, 150))

table(c.wage)

table(c.EDUCL, c.wage)

T2 = table(c.EDUCL, c.wage)

prop.table(T2, margin=1)

P = prop.table(T2, 1)
barplot(t(P), ylim=c(0, 1.3), ylab="PROPORTION",
  legend.text=dimnames(P)$c.wage,
  args.legend=list(x = "top"))

barplot(t(P), beside=T, legend.text=dimnames(P)$c.wage,
  args.legend=list(x="topleft"), ylab="PROPORTION")

T2 = table(c.EDUCL, c.wage)

S = chisq.test(T2)

print(S)

S$expected

sum((T2 - S$expected)^2 / S$expected)

1 - pchisq(54.57759, df=9)

names(S)

S$residuals

mosaicplot(T2, shade=FALSE)

mosaicplot(T2, shade=TRUE)

##########################################
#  Chapter 4 - Presentation Graphics
##########################################

hitting.data = read.table("batting.history.txt", header=TRUE,
sep="\t")
attach(hitting.data)

plot(Year, HR)

plot(Year, HR, xlab="Season", ylab="Avg HR Hit Per Team Per Game",
main="Home Run Hitting in the MLB Across Seasons")

plot(Year, HR, xlab="Season", type="b",
ylab="Avg. Home Runs Hit by a Team in a Game",
main="Home Run Hitting in the MLB Across Seasons")

row = rep(1:3, each=7)
col = rep(1:7, times=3)
plot(2, 3, xlim=c(.5,3.5), ylim=c(.5,7.5),
type="n", xaxt = "n", yaxt = "n", xlab="", ylab="")
points(row, col, pch=0:20, cex=3)
text(row, col, 0:20, pos=4, offset=2, cex=1.5)
title("Plotting Symbols with the pch Argument")

plot(Year, HR, xlab="Season", cex=1.5, pch=19,
ylab="Avg. Home Runs Hit by a Team in a Game",
main="Home Run Hitting in the MLB Across Seasons")

plot(Year, HR, xlab="Season",
ylab="Avg. Home Runs Hit by a Team in a Game",
main="Home Run Hitting in the MLB Across Seasons")
lines(lowess(Year, HR))

plot(0, 0, type="n", xlim=c(-2, 2), ylim=c(-2, 2),
xaxt="n", yaxt="n", xlab="", ylab="")

y = seq(2, -3, -1)
for(j in 1:6)
abline(a=y[j], b=1, lty=j, lwd=2)
legend("topleft", legend=c("solid", "dashed", "dotted",
"dotdash", "longdash", "twodash"), lty=1:6, lwd=2)
title("Line Styles with the lty Argument")

plot(Year, HR, xlab="Season",
ylab="Avg. Home Runs Hit by a Team in a Game",
main="Home Run Hitting in the MLB Across Seasons")
lines(lowess(Year, HR), lwd=2)
lines(lowess(Year, HR, f=1 / 3), lty="dashed", lwd=2)
lines(lowess(Year, HR, f=1 / 12), lty="dotdash", lwd=2)

legend("topleft", legend=c("f = 2/3", "f = 1/3",
"f = 1/12"), lty=c(1, 2, 4), lwd=2, inset=0.05)

colors()

plot(1:10, c(5, 4, 3, 2, 1, 2, 3, 4, 3, 2),
pch=19, cex=5,
col=c("red", "blue", "green", "beige", "goldenrod",
"turquoise", "salmon", "purple", "pink", "seashell"))

palette()

plot(0, 0, type="n", xlim=c(-2, 2), ylim=c(-2, 2),
xaxt="n", yaxt="n", xlab="", ylab="")
 y = c(-1, 1, 0, 50000)

for (j in 1:4)
abline(a=0, b=y[j], lty=j, lwd=4)

plot(0, 0, type="n", xlim=c(-1, 6), ylim=c(-0.5, 4),
xaxt="n", yaxt ="n", xlab="", ylab="",
main="Font Choices Using, font, family and srt Arguments")
text(2.5, 4, "font = 1 (Default)")
text(1, 3, "font = 2 (Bold)", font=2, cex=1.0)
text(1, 2, "font = 3 (Italic)", font=3, cex=1.0)
text(1, 1, "font = 4 (Bold Italic), srt = 20", font=4,
cex=1.0, srt=20)
text(4, 3, 'family="serif"', cex=1.0, family="serif")
text(4, 2, 'family="sans"', cex=1.0, family="sans")
text(4, 1, 'family="mono"', cex=1.0, family="mono")
text(2.5, 0, 'family = "HersheyScript"', cex=2.5,
family="HersheyScript", col="red")

fit = lowess(Year, HR, f=1 / 12)
Residual = HR - fit$y
plot(Year, Residual)
abline(h=0)
identify(Year, Residual, n=2, labels=Year)

par(mfrow=c(2, 1))
plot(Year, HR, xlab="Season",
ylab="Avg HR Hit Per Team Per Game",
main="Home Run Hitting in the MLB Across Seasons")
lines(fit, lwd=2)
plot(Year, Residual, xlab="Season",
main="Residuals from Lowess Fit")
abline(h=0)

par(mfrow=c(1, 1))
n = 20; p = 0.2
y = 0:20
py = dbinom(y, size=n, prob=p)
plot(y, py, type="h", lwd=3,
xlim=c(0, 15), ylab="Prob(y)")

mu = n * p; sigma = sqrt(n * p * (1 - p))
curve(dnorm(x, mu, sigma), add=TRUE, lwd=2, lty=2)

text(10, 0.15, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
e^{frac(-(y-mu)^2, 2*sigma^2)})), cex = 1.5)

title("Binomial probs with n=2, p=0.2, and matching normal curve")

locs = locator(2)
arrows(locs$x[1], locs$y[1], locs$x[2], locs$y[2])

snow.yr1 = c(85.9, 71.4, 68.8, 58.8, 34.4)
snow.yr2 = c(150.9, 102.0, 86.2, 80.1, 63.8)

windows(width=5, height=7)
layout(matrix(c(1, 2), ncol=1), heights=c(6, 4))

par("plt")

par(plt=c(0.20, 0.80, 0, 0.88), xaxt="n")
plot(snow.yr1, snow.yr2, xlim=c(30, 100), ylim=c(30, 155),
ylab="2010-11 Snowfall (in)", pch=19,
main="Snowfall in Five New York Cities")
abline(a=0, b=1)
 text(80, 145, "Syracuse")

tm = par("yaxp")
ticmarks = seq(tm[1], tm[2], length=tm[3]+1)
axis(4, at=ticmarks,
labels=as.character(round(2.54 * ticmarks, -1)))
mtext("2010-11 Snowfall (cm)", side=4, line=3)

par(plt=c(0.20, 0.80, 0.35, 0.95), xaxt="s")
plot(snow.yr1, snow.yr2 - snow.yr1, xlim=c(30, 100),
xlab="2009-10 Snowfall (in)", pch=19,
ylab="Increase (in)")
text(80, 60, "Syracuse")
tm=par("yaxp")
ticmarks=seq(tm[1], tm[2], length=tm[3] + 1)
axis(4, at=ticmarks,
labels=as.character(round(2.54 * ticmarks, -1)))
mtext("Increase (cm)", side=4, line=3)

plot.new()
plot.window(xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pty="s")
theta = seq(0, 2*pi, length=100)
lines(cos(theta), sin(theta))
theta=seq(0, 2*pi, length=7)[-7]
points(cos(theta), sin(theta), cex=3, pch=19)
pos = locator(6)
text(pos, labels=1:6, cex=2.5)
box()

pdf("homerun.pdf")
plot(Year, HR, xlab="Season",
ylab="Avg. Home Runs Hit by a Team in a Game",
main="Home Run Hitting in the MLB Across Seasons")
lines(lowess(Year, HR))
lines(lowess(Year, HR, f=1 / 3), lty=2)
lines(lowess(Year, HR, f=1 / 6), lty=3)
legend("topleft", legend=c("f = 2/3", "f = 1/3", "f = 1/6"), lty=c(1, 2, 3))
dev.off()

library(lattice)
xyplot(mpg ~ wt, data=mtcars, xlab="Weight",
ylab="Mileage",
main="Scatterplot of Weight and Mileage for 32 Cars")

xyplot(mpg ~ wt | cyl, data=mtcars, pch=19, cex=1.5,
xlab="Weight", ylab="Mileage")

densityplot(~ yvar, group=gvar, data)

densityplot(~ wt, groups=cyl, data=mtcars,
auto.key=list(space="top"))

library(lattice)
cloud(z ~ x + y, data = randu)

mile = read.csv("world.record.mile.csv")
mile2 = subset(mile, Year >= 1950)

library(ggplot2)
p = ggplot(mile2, aes(x = Year, y = seconds,
color = Gender, shape = Gender))

p + geom_point(size = 4) + geom_smooth()

##################################################
#  Chapter 5 - Exploratory Data Analysis
##################################################

dat = read.table("college.txt", header=TRUE, sep="\t")

college = subset(dat, complete.cases(dat))

stripchart(college$Retention, method="stack", pch=19,
xlab="Retention Percentage")

stripchart(Retention ~ Tier, method="stack", pch=19,
xlab="Retention Percentage",
ylab="Tier", xlim=c(50, 100), data=college)

identify(college$Retention, college$Tier, n=2,
labels=college$School)

b.output = boxplot(Retention ~ Tier, data=college, horizontal=TRUE,
ylab="Tier", xlab="Retention")

b.output$stats

b.output$out

b.output$group

plot(college$Retention, college$Grad.rate,
xlab="Retention", ylab="Graduation Rate")

fit = line(college$Retention, college$Grad.rate)
coef(fit)

plot(college$Retention, fit$residuals,
xlab="Retention", ylab="Residual")
abline(h=0)

identify(college$Retention, fit$residuals, n=2,
labels=college$School)

bgsu = read.table("bgsu.txt", header=TRUE, sep="\t")
plot(bgsu$Year, bgsu$Enrollment)

fit = lm(Enrollment ~ Year, data=bgsu)
abline(fit)
plot(bgsu$Year, fit$residuals)
abline(h=0)

bgsu$log.Enrollment = log(bgsu$Enrollment)

plot(bgsu$Year, bgsu$log.Enrollment)
fit2 = lm(log.Enrollment ~ Year, data=bgsu)
fit2$coef
abline(fit2)
plot(bgsu$Year, fit2$residuals)
abline(h=0)

college1 = subset(college, Tier==1)

stem(college1$Top.10)

f = c(0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95)
ff = f - (1 - f)
froot = sqrt(2 * f) - sqrt(2 * (1 - f))
flog = 1.15 * log10(f) - 1.15 * log10(1 - f)
D = data.frame(f, ff, froot, flog)
matplot(t(as.matrix(D)), 1:4, type="l", lty=1, lwd=1,
xlab="FRACTION", ylab="TRANSFORMATION",
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

froot = sqrt(college1$Top.10) - sqrt(100 - college1$Top.10)
flog = log(college1$Top.10 + 0.5) - log(100 - college1$Top.10 + 0.5)

stem(froot)
stem(flog)

college34 = subset(college, Tier==3 | Tier==4)
froot = sqrt(college34$Top.10) - sqrt(100 - college34$Top.10)
boxplot(froot ~ Tier, data=college34, horizontal=TRUE,
xlab="Froot(Top 10 Pct)", ylab="Tier")

##################################################
#  Chapter 6 - Basic Inference Methods
##################################################

sleep = c(7.75, 8.5, 8, 6, 8, 6.33, 8.17, 7.75,
7, 6.5, 8.75, 8, 7.5, 3, 6.25, 8.5, 9, 6.5,
9, 9.5, 9, 8, 8, 9.5)

nine.hours = ifelse(sleep >= 9, "yes", "no")
table(nine.hours)

y = 5; n = 24
Test = prop.test(y, n, p=0.5, alternative="two.sided",
                 correct=FALSE)
Test

names(Test)

Test$estimate

Test$statistic

Test$p.value

Test$conf.int

y = 5; n = 24
Test.adj = prop.test(y, n, p=0.5, alternative="two.sided",
correct=TRUE)
c(Test.adj$stat, p.value=Test.adj$p.value)

Test.exact = binom.test(y, n, p=0.5)
c(Test.exact$stat, p.value=Test.exact$p.value)

2 * pbinom(5, size=24, prob=0.5)

Test.exact$conf.int

agresti.interval = function(y, n, conf=0.95){
n1 = n + 4
y1 = y + 2
phat = y1 / n1
me = qnorm(1 - (1 - conf) / 2) * sqrt(phat * (1 - phat) / n1)
c(phat - me, phat + me)
}

agresti.interval(y, n)

cnames = c("Wilson Score Interval", "Clopper-Pearson",
"Agresti-Coull")
cfunctions = c("prop.test", "binom.test", "agresti.interval")
intervals = rbind(Test$conf.int, Test.exact$conf.int,
 agresti.interval(y, n))
data.frame(Name=cnames, Function=cfunctions,
LO=intervals[ , 1], HI=intervals[ , 2])

hist(sleep)
qqnorm(sleep)
qqline(sleep)

plot(sleep)
sleep.new = sleep[-14]

t.test(sleep.new, mu=8, conf.level=0.90)

W = wilcox.test(sleep, mu=8, conf.int=TRUE, conf.level=0.90)

W

names(W)

W$statistic
W$p.value

W$conf.int

twins = read.table("twins.txt", header=TRUE)

log.wages = log(twins$HRWAGEH)

college = ifelse(twins$EDUCH > 12, "yes", "no")

boxplot(log.wages ~ college, horizontal=TRUE,
names=c("High School", "Some College"), xlab="log Wage")

t.test(log.wages ~ college)

t.test(log.wages ~ college, var.equal=TRUE)$p.value

wilcox.test(log.wages ~ college, conf.int=TRUE)

log.wages = log(twins$HRWAGEH)
college = ifelse(twins$EDUCH > 12, "yes", "no")

table(college)

resample = function()
t.test(log.wages ~ sample(college))$statistic

many.T = replicate(1000, resample())

T.obs = t.test(log.wages ~ college)$statistic
T.obs

hist(many.T)
abline(v=T.obs)

2 * mean(many.T < T.obs)

twins.diff = subset(twins, EDUCL != EDUCH)

twins.diff = twins.diff[complete.cases(twins.diff), ]

log.wages.low = with(twins.diff,
ifelse(EDUCL < EDUCH, log(HRWAGEL), log(HRWAGEH)))
log.wages.high = with(twins.diff,
ifelse(EDUCL < EDUCH, log(HRWAGEH), log(HRWAGEL)))

head(cbind(log.wages.low, log.wages.high))

t.test(log.wages.low, log.wages.high, paired=TRUE)

###############################
# Chapter 7 - Regression
###############################

attach(cars) #attach the data
?cars #display the help page for cars data
plot(cars) #construct scatterplot

lm(dist ~ speed)

L1 = lm(dist ~ speed)
print(L1)

plot(cars, main="dist = -17.579 + 3.932 speed", xlim=c(0, 25))
#line with intercept=-17.579, slope=3.932
abline(-17.579, 3.932)
curve(-17.579 + 3.932*x, add=TRUE) #same thing

plot(L1, which=1, add.smooth=FALSE)

L2 = lm(dist ~ 0 + speed)
L2

plot(cars, main="dist = 2.909 speed", xlim=c(0,25))
#line with intercept=0, slope=2.909
abline(0, 2.909)

Trees = trees
names(Trees)[1] = "Diam"
attach(Trees)

pairs(Trees)
cor(Trees)

M1 = lm(Volume ~ Diam)
print(M1)

plot(Diam, Volume) #response vs predictor
abline(M1$coef) #add fitted line

new = data.frame(Diam=16)
predict(M1, new)

plot(M1, which=1:2)

M2 = lm(Volume ~ Diam + Height)
print(M2)

plot(M2, which=1:2)

M3 = lm(Volume ~ Diam + I(Diam^2) + Height)
print(M3)

plot(M3, which=1:2)

summary(M3)

anova(M3)

anova(M1, M2, M3)

new = data.frame(Diam=16, Height=70)
predict(M3, newdata=new)

predict(M3, newdata=new, interval="pred")

diameter = 16
height = seq(65, 70, 1)
new = data.frame(Diam=diameter, Height=height)
predict(M3, newdata=new, interval="conf")

lunatics = read.table("lunatics.txt", header=TRUE)
attach(lunatics)

plot(DIST, PHOME)
cor(DIST, PHOME)

RDIST = 1/DIST
plot(RDIST, PHOME)
cor(RDIST, PHOME)

M = lm(PHOME ~ RDIST)
M

abline(M)

plot(DIST, PHOME)
curve(M$coef[1] + M$coef[2] / x, add=TRUE)

plot(M$fitted, M$resid, xlab="fitted", ylab="residuals")

abline(h=0, lty=2)

lab = abbreviate(COUNTY)
identify(M$fitted.values, M$residuals, n=1, labels=lab)

lunatics[13, ]

detach(lunatics)

CPUspeed = read.table("CPUspeed.txt", header=TRUE)

head(CPUspeed)

years = CPUspeed$time - 1994
speed = CPUspeed$speed
log2speed = CPUspeed$log10speed / log10(2)

plot(years, speed)
plot(years, log2speed)

L = lm(log2speed ~ years)
print(L)

plot(years, speed)
curve(2^(-3.6581 + 0.5637 * x), add=TRUE)

plot(years, log2speed)
abline(L)

plot(L, which=1:2)

CPUspeed[c(16, 26, 27), ]

summary(L)$r.squared

new = data.frame(years = 2005 + 316.5 / 365 - 1994)
lyhat = predict(L, newdata=new)
lyhat

2^lyhat

##################################################
#  Chapter 8 - ANOVA I
##################################################

flicker = read.table(file=
 "http://www.statsci.org/data/general/flicker.txt",
 header=TRUE)

flicker = read.table("flicker.txt", header=TRUE)

is.factor(flicker$Colour)
levels(flicker$Colour)

unclass(flicker$Colour)

attach(flicker)

boxplot(Flicker ~ Colour, ylab = "Flicker")

stripchart(Flicker ~ Colour, vertical=TRUE)

by(Flicker, Colour, FUN=mean)

meansd = function(x) c(mean=mean(x), sd=sd(x))

by(Flicker, Colour, FUN=meansd)

oneway.test(Flicker ~ Colour)

oneway.test(Flicker ~ Colour, var.equal=TRUE)

L = lm(Flicker ~ Colour)
L

predict(L)

M = aov(Flicker ~ Colour)
model.tables(M, type="means")

model.tables(M)

options(show.signif.stars=FALSE)
anova(L)

#plot residuals vs fits
plot(L$fit, L$res)
abline(h=0) #add horizontal line through 0

#Normal-QQ plot of residuals with reference line
qqnorm(L$res)
qqline(L$res)

MSE = 2.3944
t97.5 = qt(.975, df=16) #97.5th percentile of t
n = table(Colour) #sample sizes
means = by(Flicker, Colour, mean) #treatment means
outer(means, means, "-")

t97.5 * sqrt(MSE * outer(1/n, 1/n, "+"))

pairwise.t.test(Flicker, Colour)

qtukey(.95, nmeans=3, df=16)

L = lm(Flicker ~ Colour)
anova(L)

M = aov(Flicker ~ Colour)
M

summary(M)

TukeyHSD(M)

plot(TukeyHSD(M))

dat = read.table("SiRstv.txt", header=TRUE)
head(dat)

#Instrument is not a factor
is.factor(dat$Instrument)

#convert Instrument to factor
dat$Instrument = as.factor(dat$Instrument)
attach(dat)
str(dat) #check our result

boxplot(Resistance ~ Instrument)
stripchart(Resistance ~ Instrument, vertical=TRUE)
by(Resistance, Instrument, FUN=function(x) c(mean(x), sd(x)))

L = aov(Resistance ~ Instrument)

summary(L)
par(mfrow=c(2, 2)) #4 plots on one screen
plot(L) #residual plots

par(mfrow=c(1, 1)) #restore display to plot full window
plot(TukeyHSD(L)) #plot the Tukey CI's

file1 = "http://www.itl.nist.gov/div898/strd/anova/SiRstv.dat"
dat = read.table(file1, skip=60)

read.table("PATIENT.DAT")

times = read.table("PATIENT.DAT", sep="\t")
names(times) = c("stomach","bronchus","colon","ovary","breast")

times1 = stack(times)
names(times1)

names(times1) = c("time", "organ")
times1 = na.omit(times1)

head(times1, 3)
tail(times1, 3)
is.factor(times1$organ)
summary(times1)

times = read.csv("PATIENT.csv")
head(times)

##################################################
#  Chapter 9 - ANOVA II
##################################################

library(bootstrap)
head(scor)

sapply(scor, mean, data=scor)

scor.long = stack(scor)
block = factor(rep(1:88, times=5))
scor.long = data.frame(scor.long, block)

head(scor.long) #top
tail(scor.long) #bottom

names(scor.long) = c("score", "exam", "student")

str(scor.long)

L = aov(score ~ exam + student, data=scor.long)
summary(L)

model.tables(L, cterms="exam")

model.tables(L, cterms="exam", type="mean")

CIs = TukeyHSD(L, which=1)
CIs

plot(CIs, las=1)

plot(L, which=1:2)

boxplot(score ~ student,
 xlab="Student Number", ylab="Score", data=scor.long)

poison = read.csv("poison.csv")

str(poison)

L = aov(Time ~ Poison * Treatment, data = poison)
anova(L)

with(data=poison, expr={
  interaction.plot(Poison, Treatment, response=Time)
  interaction.plot(Treatment, Poison, response=Time)
 })

model.tables(L, type="means")

TukeyHSD(L, which=c("Poison", "Treatment"))

plot(TukeyHSD(L, which=c("Poison", "Treatment")))

pairwise.t.test(poison$Time, poison$Poison)

pairwise.t.test(poison$Time, poison$Treatment)

##################################################
#  Chapter 10 - Randomization Tests
##################################################

waste = read.table(
 file="wasterunup.txt",
 header=TRUE, na.strings="*")
head(waste) #top of the data set

waste = stack(waste) #stack the data, create group var.
waste = na.omit(waste)

summary(waste)

table(waste$ind)

names(waste)[2] = "plant"
names(waste)

attach(waste)
by(values, plant, summary)

boxplot(values ~ plant)
stripchart(values ~ plant, vertical=TRUE)

L <- lm(values ~ plant)
plot(L, which=1:2) #the first two residual plots

oneway.test(values ~ plant)

rand.oneway = function(response, group, R=199) {
test = oneway.test(response ~ group)
observed.F <- test$statistic
stats = replicate(R, {
random.labels = sample(group)
oneway.test(response ~ random.labels)$statistic})
p = sum(stats >= observed.F) / (R+1)
test$method = "Randomization test for equal means"
test$p.value = p
test
}

rand.oneway(response=values, group=plant, R=999)

library(coin)
oneway_test(values ~ plant, distribution=approximate(B=999))

detach(package:coin)

web = read.table("webhits.txt", header=TRUE)
plot(web$Week, web$Hits)

cor.test(web$Week, web$Hits, method="spearman")

rand.correlation = function(x, y, R=199) {
ranks.x = rank(x)
ranks.y = rank(y)
observed.r = cor(ranks.x, ranks.y)
stats = replicate(R, {
random.ranks = sample(ranks.y)
cor(ranks.x, random.ranks)
})
p.value = sum(stats >= observed.r) / (R + 1)
list(observed.r = observed.r, p.value = p.value)
}

rand.correlation(web$Week, web$Hits, R=1000)

##################################################
#  Chapter 11 - Simulation Experiments
##################################################

options(width=60)
sample(c(-1, 1), size=50, replace=TRUE)

win = sample(c(-1, 1), size=50, replace=TRUE)
cum.win = cumsum(win)
cum.win

par(mfrow=c(2, 2))
for(j in 1:4){
win = sample(c(-1, 1), size=50, replace=TRUE)
plot(cumsum(win), type="l" ,ylim=c(-15, 15))
abline(h=0)}

peter.paul=function(n=50){
win = sample(c(-1, 1), size=n, replace=TRUE)
sum(win)
}

peter.paul()

F = replicate(1000, peter.paul())

table(F)

par(mfrow=c(1, 1))
plot(table(F))

dbinom(25, size=50, prob=0.5)

peter.paul=function(n=50){
win=sample(c(-1, 1), size=n, replace=TRUE)
cum.win = cumsum(win)
c(F=sum(win), L=sum(cum.win > 0), M=max(cum.win))
}

peter.paul()

S = replicate(1000, peter.paul())
dim(S)

times.in.lead = S["L", ]

plot(prop.table(table(times.in.lead)))

maximum.lead = S["M", ]
plot(table(maximum.lead))

sum(maximum.lead >= 10) / 1000

n = 10
hats = 1:n

mixed.hats = sample(hats)

hats

mixed.hats

hats == mixed.hats

correct = sum(hats == mixed.hats)
correct

correct = sum(hats = mixed.hats)
correct

scramble.hats = function(n){
hats = 1:n
mixed.hats = sample(n)
sum(hats == mixed.hats)
}

scramble.hats(30)

matches = replicate(1000, scramble.hats(10))

table(matches)

table(matches) / 1000

mean(matches)

prop.no.matches = function(n){
matches = replicate(10000, scramble.hats(n))
sum(matches == 0) / 10000
}

prop.no.matches(20)

many.probs = sapply(2:20, prop.no.matches)
plot(2:20, many.probs,
xlab="Number of Men", ylab="Prob(no matches)")
abline(h=0.37)

cards = c("Mantle", "Aaron", "Gehrig", "Ruth", "Schmidt",
"Mays", "Cobb", "DiMaggio", "Williams", "Foxx")

samp.cards = sample(cards, size=20, replace=TRUE)
samp.cards

unique(samp.cards)

length(unique(samp.cards))

collector=function(n,m){
samp.cards = sample(1:n, size=m, replace=TRUE)
ifelse(length(unique(samp.cards)) == n, "yes", "no")
}

collector(586, 3000)

table(replicate(100, collector(586, 3000)))

collect2 = function(n.purchased){
cost.rcard = 0.05
cost.ncard = 0.25
samp.cards = sample(1:586, size=n.purchased, replace=TRUE)
n.cards = length(unique(samp.cards))
n.missed = 586 - n.cards
n.purchased * cost.rcard + n.missed * cost.ncard
}

costs = replicate(500, collect2(800))
summary(costs)

expected.cost = function(n.purchased)
mean(replicate(100, collect2(n.purchased)))

N=500:1500
ECOST = sapply(N, expected.cost)
plot(N, ECOST, xlab="Cards Purchased",
ylab="Expected Cost in Dollars")
grid(col="black")

y = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1)

where = (c(0, y, 0) == 0)
n = length(y)
loc.zeros = (0:(n+1))[where]
loc.zeros

streak.lengths = diff(loc.zeros) - 1
streak.lengths = streak.lengths[streak.lengths > 0]
streak.lengths

longest.streak=function(y){
where = c(0, y, 0) == 0
n = length(y)
loc.zeros = (0:(n+1))[where]
streak.lengths = diff(loc.zeros) - 1
streak.lengths = streak.lengths[streak.lengths > 0]
max(streak.lengths)
}

dat = read.table("utley2006.txt", header=TRUE, sep="\t")
utley = as.numeric(dat$H > 0)
longest.streak(utley)

random.streak=function(y){
mixed.up.y = sample(y)
longest.streak(mixed.up.y)
}

L = replicate(100000, random.streak(utley))
plot(table(L))
abline(v=35, lwd=3)
text(38, 10000, "Utley")


##################################################
#  Chapter 12 - Bayesian Modeling
##################################################

curve(dgamma(x, shape=331.6, rate= 270.3), from=0.9, to=1.6)
curve(dgamma(x, shape=324, rate=262), add=TRUE, lty=2, lwd=2)
legend("topright", c("Posterior", "Likelihood"), lty=c(1, 2),
lwd=c(1, 2))

qgamma(0.5, shape=331.6, rate=270.3)

qgamma(c(0.05, 0.95), shape=331.6, rate=270.3)

sim.post = rgamma(1000, shape=331.6, rate=270.3)

hist(sim.post, freq=FALSE, main="", xlab="Lambda")
curve(dgamma(x, shape=331.6, rate=270.3), add=TRUE)

quantile(sim.post,c(0.05, 0.5, 0.95))

mylogpost = function(lambda, shape, rate)
dgamma(lambda, shape, rate, log=TRUE)

metrop.hasting.rw = function(logpost, current, C, iter, ...){
S = rep(0, iter); n.accept = 0
for(j in 1:iter){
candidate = runif(1, min=current - C, max=current + C)
prob = exp(logpost(candidate, ...) - logpost(current, ...))
accept = ifelse(runif(1) < prob, "yes", "no")
current = ifelse(accept == "yes", candidate, current)
S[j] = current; n.accept = n.accept + (accept == "yes")
}
list(S=S, accept.rate=n.accept / iter)
}

sim = metrop.hasting.rw(mylogpost, 1, 0.2, 1000, 331.6, 270.3)

mylogpost = function(lambda){
dgamma(lambda, shape=324, rate=262, log=TRUE) +
dnorm(lambda, mean=2.0, sd=0.2, log=TRUE)
}

lambda = rgamma(1, shape=331.6, rate=270.3)
ys = rpois(1, lambda)

lambda = rgamma(1, shape=331.6, rate=270.3)
ys = rpois(262, lambda)

table(ys)

sim.y = function(){
lambda = rgamma(1, shape=331.6, rate=270.3)
ys = rpois(262, lambda)
H = hist(ys, seq(-.5,9.5,1), plot=FALSE)
data.frame(y=H$mids, f=H$counts)
}

D = data.frame(Type="Obs", y=c(0:6),
f = c(90, 93, 42, 17, 8, 9, 3))
for(j in 1:8){
sim.out = sim.y()
D = rbind(D, data.frame(Type=paste("Sim",j),
y=sim.out$y, f=sim.out$f))
}
library(lattice)
xyplot(f ~ y | Type, data=D, type="h", lwd=3)

metrop.hasting.rw2 = function(logpost2, curr, iter, scale, ...){
S = matrix(0, iter, 2)
n.accept = 0; cand = c(0, 0)
for(j in 1:iter){
cand[1] = runif(1, min=curr[1] - scale[1],
max=curr[1] + scale[1])
cand[2] = runif(1, min=curr[2] - scale[2],
max=curr[2] + scale[2])
prob = exp(logpost2(cand, ...) - logpost2(curr, ...))
accept = ifelse(runif(1) < prob, "yes", "no")
if(accept == "yes") curr=cand
S[j, ] = curr
n.accept = n.accept + (accept == "yes")
}
list(S=S, accept.rate=n.accept / iter)
}

lognbpost = function(theta, data){
sum(data$f * dnbinom(data$y, size=exp(theta[1]),
prob=exp(theta[1]) / sum(exp(theta)), log=TRUE)) +
sum(theta) - 2 * log(1 + exp(theta[1])) -
2 * log(1 + exp(theta[2]))
}

dat = list(y=0:6, f=c(90, 93, 42, 17, 8, 9, 3))

sim.fit = metrop.hasting.rw2(lognbpost, c(1, 0), 10000, c(1.0, 0.2), dat)

sim.fit$accept.rate

smoothScatter(sim.fit$S, xlab="log a", ylab="log mu")

a = exp(sim.fit$S[ ,1])

plot(density(a), xlab="a", ylab="Density", main="")

quantile(a, c(0.05, 0.95))

mu = exp(sim.fit$S[ ,2])

quantile(mu, c(0.05, 0.95))

##################################################
#  Chapter 13 - Monte Carlo Methods
##################################################

sam = runif(1000, 10, 11.5)
annie = runif(1000, 10.5, 12)

prob = sum(annie < sam) / 1000
prob

plot(sam, annie)
polygon(c(10.5, 11.5, 11.5, 10.5),
c(10.5, 10.5, 11.5, 10.5), density=10, angle=135)

sqrt(prob * (1 - prob) / 1000)

difference = annie - sam

mc.est = mean(difference)
se.est = sd(difference) / sqrt(1000)
c(mc.est, se.est)

sim.median = function(n)
median(rexp(n))

M = replicate(10000, sim.median(21))

hist(M, prob=TRUE, main="")

samp.med = function(m, n){
con = factorial(n) / factorial((n - 1) / 2)^2
con * pexp(m)^((n - 1) / 2) * (1 - pexp(m))^((n - 1) / 2) * dexp(m)
}

curve(samp.med(x, 21), add=TRUE, col="red")

quantile(M, c(0.05, 0.95))

y = c(6.11, 5.04, 4.83, 4.89, 4.97, 5.15, 7.98, 4.62,
6.19, 4.58, 6.34, 4.54, 6.37, 5.64, 4.53, 4.68,
5.17, 4.72, 5.06, 4.96, 4.70)
median(y)

c(4.97 - 1.118365, 4.97 - 0.398709)

taxi = function(N, n){
y = sample(N, size=n, replace=TRUE)
estimate1 = max(y)
estimate2 = 2 * mean(y)
c(estimate1=estimate1, estimate2=estimate2)
}

taxi(100, 5)

EST = replicate(1000, taxi(100, 5))

c(mean(EST["estimate1", ]) - 100, sd(EST["estimate1", ]) / sqrt(1000))
c(mean(EST["estimate2", ]) -100, sd(EST["estimate2", ]) / sqrt(1000))

absolute.error = abs(EST - 100)
boxplot(t(absolute.error))

apply(t(absolute.error), 2, mean)

apply(t(absolute.error), 2, sd) / sqrt(1000)

wald = function(y ,n, prob){
p = y / n
z = qnorm(1 - (1 - prob) / 2)
lb = p - z * sqrt(p * (1 - p) / n)
ub = p + z * sqrt(p * (1 - p) / n)
cbind(lb, ub)
}

wald(5, 20, 0.95)

y = c(2, 4, 6, 8)
wald(y, 20, 0.90)

mc.coverage = function(p, n, prob, iter=10000){
y = rbinom(iter, n, p)
c.interval = wald(y, n, prob)
mean((c.interval[ ,1] < p) & (p < c.interval[ ,2]))
}

mc.coverage(0.15, 20, 0.90)

sqrt(0.8005 * (1 - 0.8005) / 10000)

many.mc.coverage = function(p.vector, n, prob)
sapply(p.vector, mc.coverage, n, prob)

curve(many.mc.coverage(x, 100, 0.90), from=0.001, to=0.999,
xlab="p", ylab="Coverage Probability",
main=paste("n=", 100, ", prob=", 0.90),
ylim=c(0.7, 1))
abline(h=.9)

simulate.markov.chain = function(P, starting.state, steps){
n.states = dim(P)[1]
state = starting.state
STATE = rep(0, steps)
for(j in 1:steps){
state = sample(n.states, size=1, prob=P[state, ])
STATE[j ] = state
}
return(STATE)
}

P = matrix(c(0.50, 0.25, 0, 0, 0,0.25,
0.25, 0.50, 0.25, 0, 0, 0,
0, 0.25, 0.50, 0.25, 0, 0,
0, 0, 0.25, 0.50, 0.25, 0,
0, 0, 0, 0.25, 0.50, 0.25,
0.25, 0, 0, 0, 0.25, 0.50),
nrow=6, ncol=6, byrow=TRUE)
P

s = simulate.markov.chain(P, 3, 10000)

table(s)

table(s) / 10000

w = c(1, 1, 1, 1, 1, 1) / 6

log.samp.med = function(y, n){
(n - 1) / 2 * log(pexp(y)) + (n - 1) / 2 * log(1 - pexp(y)) +
log(dexp(y))
}

metrop.hasting.exp = function(logf, current, iter, ...){
S = rep(0, iter); n.accept = 0
for(j in 1:iter){
candidate = rexp(1, current)
prob = exp(logf(candidate, ...) - logf(current, ...) +
dexp(current, candidate, log=TRUE) -
dexp(candidate, current, log=TRUE))
accept = ifelse(runif(1) < prob, "yes", "no")
current = ifelse(accept == "yes", candidate, current)
S[j] = current; n.accept = n.accept + (accept == "yes")
}
list(S=S, accept.rate=n.accept / iter)
}

mcmc.sample = metrop.hasting.exp(log.samp.med, 1, 10000, 21)

mcmc.sample$accept.rate

plot(mcmc.sample$S)

plot(density(mcmc.sample$S), lwd=2, main="", xlab="M")
curve(samp.med(x, 21), add=TRUE, lwd=2, lty=2)
legend("topright", c("Simulated", "Exact"), lty=c(1, 2),lwd=c(2, 2))

metrop.hasting.rw = function(logf, current, C, iter, ...){
S = rep(0, iter); n.accept = 0
for(j in 1:iter){
candidate = rnorm(1, mean=current, sd=C)
prob = exp(logf(candidate, ...) - logf(current, ...))
accept = ifelse(runif(1) < prob, "yes", "no")
current = ifelse(accept == "yes", candidate, current)
S[j] = current; n.accept = n.accept + (accept == "yes")
}
list(S=S, accept.rate=n.accept / iter)
}

mcmc.sample1 = metrop.hasting.rw(log.samp.med, 1, 1, 10000, 21)
mcmc.sample2 = metrop.hasting.rw(log.samp.med, 1, 0.05, 10000, 21)

plot(mcmc.sample1$S, type="l", main=paste(
"Scale C = 1, Acceptance rate =",
round(mcmc.sample1$accept.rate,2)))
plot(mcmc.sample2$S, type="l", main=paste(
"Scale C = 0.05, Acceptance rate =",
round(mcmc.sample2$accept.rate,2)))

random.coin.gibbs = function(p=0.5, m=1000){
S = matrix(0, m, 2)
dimnames(S)[[2]] = c("p", "y")
for(j in 1:m){
y = rbinom(1, size=12, prob=p)
p = rbeta(1, y+4, 20-y)
S[j, ] = c(p, y)
}
return(S)
}

sim.values = random.coin.gibbs()

plot(sim.values)

table(sim.values[ ,"y"])

















