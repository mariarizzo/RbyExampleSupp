# R by Example 2e
# Code for Chapter 13

set.seed(12345)
sample(c(-1, 1), size=50, replace=TRUE)


set.seed(12348)
win <- sample(c(-1, 1), size=50, replace=TRUE)
cum.win <- cumsum(win)
cum.win

library(ggplot2)
df <- NULL
for(j in 1:4){
  win <- sample(c(-1, 1), size=50, replace=TRUE)
  df_j <- data.frame(Label = paste("Simulation", j),
                     Index = 1:50,
                     CumSum = cumsum(win))
  df <- rbind(df, df_j)
}
ggplot(df, aes(Index, CumSum)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ Label)

peter.paul <- function(n = 50){
  win <- sample(c(-1, 1), size = n, replace = TRUE)
  sum(win)
}

peter.paul()

F <- replicate(1000, peter.paul())

table(F)

require(ggplot2)
ggplot(data.frame(Total_Fortune = F), 
       aes(Total_Fortune)) +
  geom_bar()

dbinom(25, size=50, prob=0.5)

peter.paul <- function(n=50){
  win <- sample(c(-1, 1), size=n, replace=TRUE)
  cum.win <-  cumsum(win)
  c(F=sum(win), L=sum(cum.win > 0), M=max(cum.win))
}

(peter.paul() -> pp.sim)

S <- replicate(1000, peter.paul())
dim(S)

times.in.lead <- S["L", ]


plot(prop.table(table(times.in.lead)),
     ylab = "Count")

ggplot(data.frame(Maximum_Lead = S["M", ]),
       aes(Maximum_Lead)) +
  geom_bar()

n <- 10
hats <- 1:n

mixed.hats <- sample(hats)

hats

hats == mixed.hats

correct <- sum(hats == mixed.hats)
correct

scramble.hats <- function(n){
  hats <- 1:n
  mixed.hats <- sample(n)
  sum(hats == mixed.hats)
}

(scramble.hats(30) -> y)

matches <- replicate(1000, scramble.hats(10))

table(matches)

mean(matches)

prop.no.matches <- function(n){
  matches <- replicate(10000, scramble.hats(n))
  sum(matches == 0) / 10000
}

(prop.no.matches(20) -> p0)

many.probs <- sapply(2:20, prop.no.matches)
plot(2:20, many.probs,
  xlab="Number of Men", ylab="Prob(no matches)")
abline(h=0.37)

cards <- c("Mantle", "Aaron", "Gehrig", "Ruth", "Schmidt",
          "Mays", "Cobb", "DiMaggio", "Williams", "Foxx")

samp.cards <- sample(cards, size=20, replace=TRUE)
samp.cards

unique(samp.cards)

length(unique(samp.cards))

collector <- function(n, m){
  samp.cards <- sample(1:n, size=m, replace=TRUE)
  ifelse(length(unique(samp.cards)) == n, "yes", "no")
}

collector(586, 3000)

(table(replicate(100, collector(586, 3000))) -> out)

collect2 <- function(n.purchased){
  cost.rcard <- 0.05
  cost.ncard <- 0.25
  samp.cards <- sample(1:586, size=n.purchased, replace=TRUE)
  n.cards <- length(unique(samp.cards))
  n.missed <- 586 - n.cards
  n.purchased * cost.rcard + n.missed * cost.ncard
}

costs <- replicate(500, collect2(800))
summary(costs)

expected.cost <- function(n.purchased){
  mean(replicate(100, collect2(n.purchased)))
}

N <- 500:1500
ECOST <- sapply(N, expected.cost)
plot(N, ECOST, xlab="Cards Purchased",
   ylab="Expected Cost in Dollars", cex=.5)
grid(col="black")

y <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1)

(out <- rle(y))

max(out$lengths[out$values == 1])

longest.streak <- function(y){
  out <- rle(y)
  max(out$lengths[out$values == 1])
}

library(RbyExample)
utley <- as.numeric(utley2006$H > 0)
longest.streak(utley)

random.streak <- function(y){
  mixed.up.y <- sample(y)
  longest.streak(mixed.up.y)
}

L <- replicate(10000, random.streak(utley))
ggplot(data.frame(Longest_Streak = L),
       aes(Longest_Streak)) +
  geom_bar() +
  geom_vline(xintercept = 35, linewidth = 2) +
  annotate(geom = "text", x = 37, y = 1000,
           label = "Utley")

