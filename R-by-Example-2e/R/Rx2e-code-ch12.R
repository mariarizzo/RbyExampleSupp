# R by Example 2e
# Code for Chapter 12

library(tidyverse)

library(knitr, quietly = TRUE)

library(ggbiplot)
str(crime)

numvars <- sapply(crime, "is.numeric")
crime1 <- subset(crime, select = numvars)
head(crime1, 2)

crime |>
  select(where(is.numeric)) -> crime2 
head(crime2, 2)

identical(crime1, crime2)

anyNA(crime)

colMeans(crime1)

sapply(crime1, median)

crime |> 
  summarize(across(where(is.numeric), mean))

crime1 |> 
  summarize(across(everything(), median))

crime |>
  summarize(across(where(is.numeric), median))

crime |>
  group_by(region) |>
  summarize(across(where(is.numeric), mean))

knitr::kable(round(cor(crime1), 2))

means <- colMeans(crime1)
cdata <- sweep(crime1, MARGIN = 2, STATS = means)

round(colMeans(cdata), 7)

s <- sapply(crime1, FUN="sd")
scaled <- sweep(crime1, MARGIN = 2, STATS = s, FUN = "/")

sapply(scaled, "sd")

means <- colMeans(crime1)
s <- sapply(crime1, FUN = "sd")
crime1 |>
  sweep(MARGIN = 2, STATS = means, FUN = "-") |>
  sweep(MARGIN = 2, STATS = s, FUN = "/") -> crime1_01

center_scale <- function(x) {
  means <- colMeans(x, na.rm = TRUE)
  s <- apply(x, MARGIN=2, FUN="sd", na.rm = TRUE)
  x |>
    sweep(MARGIN = 2, STATS = means, FUN = "-") |>
    sweep(MARGIN = 2, STATS = s, FUN = "/") -> y
  return(y)
}

crime1_01 <- center_scale(crime1)

round(colMeans(crime1_01), 7)
sapply(crime1_01, FUN = "sd")

cor_before <- cor(crime1)
cor_after <- cor(crime1_01)
all.equal(cor_before, cor_after)

# center columns to mean 0
crime1 |> 
  mutate(across(everything(), ~ .x - mean(.x))) -> ccrime1

# scale columns to variance 1
crime1 |> 
  mutate(across(everything(), ~ .x / sd(.x))) -> scrime1

# center and scale within columns
crime1 |> 
  mutate(across(everything(), ~ (.x - mean(.x)) / sd(.x))) -> cscrime1

ccrime1 |>
  summarize(across(everything(), ~ round(mean(.x), 7)))
scrime1 |>
  summarize(across(everything(), sd))
cscrime1 |>
  summarize(across(everything(), ~ round(mean(.x), 7)))
cscrime1 |>
  summarize(across(everything(), sd))

crimes <- crime[c(2:8, 10)] 

pairs(crimes[1:4], gap=0, pch=20, col=4)

library(lattice)

splom(crimes[1:4], 
      pscales = 0, cex = .5, lwd = 2, 
      type = c("p", "r"))

splom(~crimes[1:4] | region, data = crimes, 
      pscales = 0, cex = .5, type=c("p", "smooth"),
      varname.cex = .75)

library(corrplot)
corrplot(cor(USArrests))
corrplot(cor(USArrests), 
         addCoef.col = 'black', number.cex = 2, 
         tl.pos = 'd')

library(corrplot)

x <- crime[, 2:8]
corx <- cor(x)
corrplot(corx, method = "ellipse")

corrplot(corx, type="upper",
         method = "square",
         diag = FALSE, tl.pos = "td")
corrplot(corx, add=TRUE, 
         type ="lower", 
         method = "number", 
         diag = FALSE, tl.pos = "l")

library(FactoMineR)
data(decathlon)
names(decathlon)

corrdeca <- cor(decathlon[1:10])
corrplot(corrdeca)

corrplot(corrdeca, order="FPC")

table(decathlon$Competition)
events <- subset(decathlon, 
                 decathlon$Competition == "OlympicG",
                 select = 1:10)

decathlon |>
  filter(Competition == "OlympicG") |>
  select(1:10) -> events

events |> 
  mutate(across(everything(), ~ .x / sd(.x))) -> events1

prcomp(events, scale. = TRUE, rank. = 5) -> pca1
summary(pca1)

S <- cov(events1)
ev <- eigen(S)
dim(ev$vectors)
round(ev$values, 3)

lambda <- ev$values
prop_var <- lambda / sum(lambda)
cum_prop <- cumsum(lambda) / sum(lambda)
rbind(prop_var[1:5], cum_prop[1:5])

screeplot(pca1)
screeplot(pca1, type = "lines")

round( pca1$rotation[, 1:3], 3)

biplot(pca1, cex=c(.5,.5))

pca <- prcomp(events, scale. = TRUE)
ggbiplot(pca, varname.color = "blue")

library(FactoMineR)
PCA(events)

# plots only
PCA(events, graph=FALSE) -> pca
plot(pca, choix = "ind")
plot(pca, choix = "var")

pca2 <- princomp(events1) 
summary(pca2)

library(MASS, warn.conflicts = FALSE)
data(mammals)
d <- dist(log(mammals))
(h <- hclust(d, method = "complete"))

bigmammals <- subset(mammals, 
                     subset = (body > median(body)))

d <- dist(log(bigmammals))
h <- hclust(d, method = "complete")

plot(h)

head(h$merge)

rownames(bigmammals)[c(10, 12)]

log(bigmammals)[c(10, 12), ]
