# R by Example 2e
# Code for Chapter 1

set.seed(1)

library(RbyExample)

head(bgsu)

temps <- c(51.9, 51.8, 51.9, 53)
temps

(temps <- c(51.9, 51.8, 51.9, 53))

temps - 32

(5/9) * (temps - 32)

CT <- c(48, 48.2, 48, 48.7)
temps - CT

winner <- c(185, 182, 182, 188, 188, 188, 185, 185, 177, 182, 182, 193, 183, 179, 179, 175)
opponent <- c(175, 193, 185, 187, 188, 173, 180, 177, 183, 185, 180, 180, 182, 178, 178, 173)

length(winner)

year <- seq(from = 2008, to = 1948, by = -4)

year <- seq(2008, 1948, -4)

mean(winner)
mean(opponent)

difference <- winner - opponent

head(data.frame(year, winner, opponent, difference))

taller.won <- winner > opponent
taller.won

table(taller.won)
table(taller.won) / 16 * 100

barplot(rev(difference), 
        xlab = "Election years 1948 to 2008", 
        ylab = "Height difference in cm")

k <- c(0, 1, 2, 3, 4)
x <- c(109, 65, 22, 3, 1)

barplot(x, names.arg = k)

p <- x / sum(x)
p

(r <- sum(p * k))

(v <- sum(x * (k - r)^2) / 199)

deaths <- rep(k, times = x)
table(deaths)

mean(deaths)
var(deaths)

f <- r^k * exp(- r) / factorial(k)
f

f <- dpois(k, lambda = r)
f

floor(200 * f)  #expected counts
x             #observed counts

cbind(k, p, f)

f <- function(x) {
  return( x * (1-x) )
}

x <- runif(5)  #a vector of Uniform(0, 1) numbers
x
f(x)

var.n <- function(x) {
  v <- var(x)
  n <- NROW(x)
  v * (n - 1) / n
  }

temps <- c(51.9, 51.8, 51.9, 53)
var(temps)
var.n(temps)

fn <- function(x, a = 1, b = 1)
    x^(a-1) * (1-x)^(b-1)

x <- seq(0, 1, .2)  #sequence from 0 to 1 with steps of .2
fn(x, a = 2, b = 2)

integrate(fn, lower = 0, upper = 1, a = 2, b = 2)

beta(2, 2)

integrate(function(x) {x * (1-x)}, 
          lower = 0, upper = 1)

curve(x*(1-x), from = 0, to = 1, ylab = "f(x)")

normaldat <- rnorm(200)

hist(normaldat, prob = TRUE, breaks = "scott", 
     main = "", xlab = "")
curve(dnorm(x), add = TRUE, col = "red")
lines(density(normaldat))

probs <- c(.45, .05, .01, .48, .70, .50, .07, .25, .49)
P <- matrix(probs, nrow = 3, ncol = 3)
P

Q <- matrix(c( 0.45,  0.48,  0.07,
               0.05,  0.70,  0.25,
               0.01,  0.50,  0.49), 
            nrow = 3, ncol = 3, byrow = TRUE)
Q

rownames(P) <- colnames(P) <- c("lower", "middle", "upper")

P

rowSums(P)

apply(P, MARGIN = 1, FUN = sum)

P2 <- P %*% P
P2

P2[1, 3]

P2[1, ]

P4 <- P2 %*% P2
P8 <- P4 %*% P4

P8

library(RbyExample)
str(brainsize)

names(brainsize)
class(brainsize$Gender)
sapply(brainsize, class)

dim(brainsize)
NROW(brainsize); nrow(brainsize)
NCOL(brainsize); ncol(brainsize)

head(brainsize)
tail(brainsize, 2)

brainsize[4:8, ]

any(is.na(brainsize))
anyNA(brainsize)

summary(brainsize)

str(USArrests)
head(USArrests)

summary(USArrests)

colMeans(USArrests)
sapply(USArrests, "sd")
sapply(USArrests, "median")

range(USArrests$UrbanPop)

sapply(USArrests, "range")

USArrests[3, "Murder"]
USArrests[3, ]

USArrests$Assault[1:3]

with(USArrests,
     plot(UrbanPop, Murder))

head(rownames(USArrests))

states <- rownames(USArrests)
USArrests2 <- data.frame(State = states,
                         USArrests,
                         row.names = NULL)
str(USArrests2)

Arrests <- USArrests[order(USArrests$UrbanPop), ]
head(Arrests)

# alternately precompute order
o <- order(USArrests$UrbanPop)
Arrests <- USArrests[o, ]
tail(Arrests)

library(dplyr, warn.conflicts = FALSE)

USArrests |>
  arrange(UrbanPop) -> Arrests
head(Arrests)

library(ggplot2)
ggplot(data = USArrests, aes(x = UrbanPop, y = Murder)) +
  geom_point()

year <- seq(1948, 2008, 4)
diff <- rev(winner - opponent)
df <- data.frame(year = year, diff = diff)

ggplot(data = df, aes(x = year, y = diff)) +
  geom_col() +
  labs(x = "Election years 1948 to 2008", y = "Height difference in cm")

library(ggplot2)
df <- data.frame(x = normaldat)
ggplot(df, aes(x = x)) +
  geom_histogram()

nbin <- nclass.scott(df$x)
ggplot(df, aes(x = x)) +
  geom_histogram(bins = nbin, fill = "white", color = "black")
