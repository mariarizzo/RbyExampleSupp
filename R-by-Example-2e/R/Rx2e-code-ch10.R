# R by Example 2e
# Code for Chapter 10

library(bootstrap)
head(scor)

sapply(scor, mean, data=scor)

scor.long <- stack(scor)
block <- factor(rep(1:88, times=5))
scor.long <- data.frame(scor.long, block)

str(scor.long)

names(scor.long) <- c("score", "exam", "student")

str(scor.long)

L <- aov(score ~ exam + student, data=scor.long)
summary(L)

model.tables(L, cterms="exam")

model.tables(L, cterms="exam", type="mean")

CIs <- TukeyHSD(L, which=1)
CIs

plot(CIs, las=1)

plot(L, which=1:2)

boxplot(score ~ student,
        xlab="Student Number", ylab="Score", data=scor.long)

library(RbyExample)
str(poison)

L <- aov(Time ~ Poison * Treatment, data = poison)
anova(L)

with(data=poison, expr={
  interaction.plot(Poison, Treatment, response=Time)
  interaction.plot(Treatment, Poison, response=Time)
})

model.tables(L, type="means")

TukeyHSD(L, which=c("Poison", "Treatment"))

broom::tidy(TukeyHSD(L, which=c("Poison", "Treatment")))

pairwise.t.test(poison$Time, poison$Poison)

L <- aov(score ~ exam + student, data=scor.long)
df <- data.frame(residuals=L$resid, fitted.values=L$fitted.values)

library(ggplot2)
ggplot(df, aes(fitted.values, residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values", y = "Residuals")
ggplot(df, aes(sample=residuals)) +
  stat_qq() + 
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

L <- aov(score ~ exam + student, data=scor.long)
cis <- broom::tidy(TukeyHSD(L, which=1))
cis

ggplot(cis, aes(x=estimate, y=reorder(contrast, estimate), group=contrast)) +
  geom_point() +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=.25)) +
  geom_vline(xintercept=0, linetype="dotted") +
  labs(x="Differences in mean levels of exams", y="") +
  ggtitle("95% family-wise confidence level")

L <- aov(Time ~ Poison * Treatment, data = poison)

mtabs <- model.tables(L, type="mean")$tables
df <- as.data.frame(as.table(mtabs$`Poison:Treatment`))
str(df)

library(ggplot2)
ggplot(df, aes(x=Poison, y=Freq, group=Treatment, color=Treatment)) +
  geom_line(linewidth=2) + labs(y="mean of Time")

ggplot(df, aes(x=Treatment, y=Freq, group=Poison)) +
  geom_line(aes(linetype=Poison)) + labs(y="mean of Time")
