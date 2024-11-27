# R by Example 2e
# Code for Chapter 5

library(RbyExample)
head(bball.women, 3)

plot(bball.women$Year, bball.women$`FT%`)

library(ggplot2)
ggplot(bball.women, aes(Year, `FT%`)) +
           geom_point()

plot(bball.women$Year, bball.women$`FT%`, 
     xlab="Season", 
     ylab="Free-Throw Shooting Percentage",
     main="Free-throw shooting graph with axis labels and title added")

ggplot(bball.women, aes(Year, `FT%`)) +
           geom_point() +
     labs(x = "Season",
          y = "Free-Throw Shooting Percentage",
          title = "Free-throw shooting graph with axis labels and title added")

plot(bball.women$Year, bball.women$`FT%`, 
     xlab="Season", 
     ylab="Free-Throw Shooting Percentage",
     main="Free-throw shooting graph of NCAA women.")
lines(lowess(bball.women$Year, 
             bball.women$`FT%`,
             f = 1 / 6))

ggplot(bball.women, aes(Year, `FT%`)) +
           geom_point() +
           geom_smooth(method = "loess",
                       formula = "y ~ x",
                       se = FALSE,
                       span = 1 / 4) +
     labs(x = "Season",
          y = "Free-Throw Shooting Percentage",
          title = "Free-throw shooting graph with smooth added")

row = rep(1:3, each=7)
col = rep(1:7, times=3)
plot(2, 3, xlim=c(.5,3.5), ylim=c(.5,7.5),
    type="n", xaxt = "n", yaxt = "n", xlab="", ylab="")
points(row, col, pch=0:20, cex=2)
text(row, col, 0:20, pos=4, offset=2, cex=1.5)
title("Plotting Symbols with the pch Argument")

plot(0, 0, type="n", xlim=c(-8, 8),  ylim=c(-8, 12),
           xaxt="n",  yaxt="n", xlab="", ylab="")
y = seq(0, -6, -1)
for(j in 1:6){
  abline(a=y[j], b=1, lty=j, lwd=2)
}
legend("topleft", legend=c("solid", "dashed", "dotted",
  "dotdash", "longdash", "twodash"), lty=1:6, lwd=2, bty="n")
title("Line Styles with the lty Argument")

bball.men <- bball.men[bball.men$Year >= 1980, ]

plot(bball.women$Year, bball.women$`FT%`, 
     xlab="Season", 
     ylab="Free-Throw Shooting Percentage",
     main="Free-throw shooting graph of NCAA men and women.",
     ylim = c(64, 72),
     pch = 19)
lines(lowess(bball.women$Year, 
             bball.women$`FT%`,
             f = 1 / 6),
      lwd = 2, lty = "solid")
points(bball.men$Year, bball.men$`FT%`,
      pch = 17)
lines(lowess(bball.men$Year, 
             bball.men$`FT%`,
             f = 1 / 6),
       lwd = 2, lty = "dashed")
legend("topleft", 
       legend = c("Women", "Men"),
       pch = c(19, 17),
       lty = c("solid", "dashed"),
       lwd = 2)

ggplot(bball[bball$Year >= 1980, ], 
       aes(Year, `FT%`, 
           shape = Gender,  
           linetype = Gender)) +
  geom_point(size = 3) +
  geom_smooth(span = .25, se = FALSE,
              method = "loess",
              formula = "y ~ x") +
  labs(x = "Season",
       y = "Free-Throw Shooting Percentage",
       title = "Free-throw shooting of NCAA men and women")

plot(bball.women$Year, bball.women$`FT%`, 
     xlab="Season", 
     ylab="Free-Throw Shooting Percentage",
     main="Free-throw shooting graph of NCAA women.")
lines(lowess(bball.women$Year, 
             bball.women$`FT%`,
             f = 2 / 3), lwd=2)
 lines(lowess(bball.women$Year, 
              bball.women$`FT%`, 
              f= 1 / 6), lty=2, lwd=2)
 lines(lowess(bball.women$Year, 
              bball.women$`FT%`, 
              f=1 / 12), lty=4, lwd=2)
 legend("topleft", legend=c("f = 2/3", "f = 1/6",
     "f = 1/12"), lty=c(1, 2, 4), lwd=2,  inset=0.05)

colors()

y <- c(5, 4, 3, 2, 1, 2, 3, 4, 3, 2)
plot(1:10, y,
  pch=19, cex=5,
  col=c("red", "blue", "green", "beige", "goldenrod",
      "turquoise", "salmon", "purple", "pink", "seashell"))

plot(bball.women$Year, bball.women$`FT%`, 
     xlab="Season", 
     ylab="Free-Throw Shooting Percentage",
     main="Free-throw shooting graph of NCAA men and women.",
     ylim = c(64, 72),
     pch = 19,
     col = "red")
points(bball.men$Year, bball.men$`FT%`,
      pch = 19, col = "blue")
legend("topleft", 
       legend = c("Women", "Men"),
       pch = c(19, 19),
       col = c("red", "blue"))

ggplot(bball[bball$Year >= 1980, ], 
       aes(Year, `FT%`, color = Gender)) +
  geom_point(size = 3) +
  geom_smooth(span = .25, se = FALSE,
              method = "loess",
              formula = "y ~ x") +
  labs(x = "Season",
       title = "Free-throw shooting of NCAA men and women")

bball$`2PA` <- bball$FGA - bball$`3PA`
bball$`2P%` <- 100 * (bball$FG -  bball$`3P`) / bball$`2PA`

df1 <- data.frame(Success_Pct = bball$`2P%`,
                  Shot_Gender = "2 Point Shot",
                  Gender = bball$Gender, Year = bball$Year)
df2 <- data.frame(Success_Pct = bball$`3P%`,
                  Shot_Gender = "3 Point Shot",
                  Gender = bball$Gender, Year = bball$Year)
df3 <- data.frame(Success_Pct = bball$`FT%`,
                  Shot_Gender = "Free Throw",
                  Gender = bball$Gender, Year = bball$Year)
df <- rbind(df1, df2, df3)
df <- df[df$Year >= 1980, ]

ggplot(df, aes(Year, Success_Pct, color = Gender)) +
  geom_point() +
  geom_smooth(span = 0.25, se = FALSE) +
  facet_wrap(~ Shot_Gender, scales = "free_y",
             ncol = 2)

ggplot(bball[bball$Gender == "W", ], 
       aes(Year, `2P%`, size = `2PA` )) +
  geom_point() 

library(RbyExample)
bball_avgs <- batting_avg_2021

bball_avgs <- bball_avgs[order(bball_avgs$AVG, 
                               decreasing = TRUE), ]
topten <- head(bball_avgs, 10)

dotchart(rev(topten$AVG),
         labels = rev(topten$Player),
         xlab = "AVG")

topten$Player <- factor(topten$Player,
               levels = topten$Player[order(topten$AVG)])
ggplot(topten,  aes(x = AVG, y = Player)) +
  geom_point()

mu <- mean(bball_avgs$AVG)
sigma <- sd(bball_avgs$AVG)

hist(bball_avgs$AVG, freq = FALSE)
curve(dnorm(x, mu, sigma), add = TRUE,
      col = "red", lwd = 2.5)
text(.18, 10, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                   e^{frac(-(y-mu)^2, 2*sigma^2)})), 
     cex = 1.5, col = "red")

ggplot(bball_avgs, aes(AVG)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "black",
                 fill = "grey",
                 bins = 9) +
  geom_function(fun = dnorm,
                args = list(mean = mu, sd = sigma),
                color = "red",
                linewidth = 1.5) +
  annotate(geom = "text", .18, 10, parse = TRUE,
           label = "frac(1, sigma * sqrt(2 * pi)) *
           e ^ {-(x - mu)^2 / (2 * sigma)}",
           size = 6,
           color = "red")

library(RbyExample)
library(ggplot2)
four_players[1, c("PX", "PZ", "BIP", "H", "H_Rate")]

add_zone <- function(){
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  geom_path(aes(.data$x, .data$y), data=kZone,
              lwd=1)
}
ggplot(four_players) +
  geom_tile(aes(x = px, y = pz, fill = Z_H)) +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~ Player, ncol = 2) +
  add_zone() +
  coord_fixed()

library(ggplot2)
library(gridExtra)
p <- ggplot(bball.women, aes(Year, `FT%`)) +
           geom_point() +
           xlab("Season") + 
           ylab("Free-Throw Pct")
p1 <- p + geom_smooth(method = "lm") + 
  ggtitle("lm fit")
p2 <- p + geom_smooth(method = "loess", span = 2/3) +
  ggtitle("loess fit with span = 2/3")
p3 <- p + geom_smooth(method = "loess", span = 1/2) +
  ggtitle("loess fit with span = 1/2")
p4 <- p + geom_smooth(method = "loess", span = 1/4) +
  ggtitle("loess fit with span = 1/4")

grid.arrange(p1, p2, p3, p4, 
             nrow=2, ncol=2)

L <- lm(`FT%` ~ Year, data=bball.women)
plot(L)

par(mfrow=c(2,2), mai=c(.5,.5,.5,.5))
L <- lm(`FT%` ~ Year, data=bball.women)
plot(L)

## pdf("freethrow.pdf") #uncomment to save the figure

plot(bball.women$Year, 
     bball.women$`FT%`, xlab="Season",
     ylab="Free-throw shooting percentage",
     main="Free-throw shooting graph of NCAA women")
lines(lowess(bball.women$Year, bball.women$`FT%`))
lines(lowess(bball.women$Year, bball.women$`FT%`, 
             f=1 / 3), lty=2)
lines(lowess(bball.women$Year, bball.women$`FT%`, 
             f=1 / 6), lty=3)
legend("topleft", legend=c("f = 2/3", "f = 1/3", "f = 1/6"), lty=c(1, 2, 3))

dev.off()
