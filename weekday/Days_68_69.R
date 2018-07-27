# Solution exercises One Way Analysis of Variance
# from https://www.r-exercises.com/2016/09/30/one-way-analysis-of-variance-exercises/
library(psych)
library(ggplot2)
library(car)

# Exercise 1
cancer_df <- read.csv('data/cancer-survival.csv')
str(cancer_df)

# Exercise 2
describeBy(cancer_df$Survival, cancer_df$Organ)

# Exercise 3
ggplot(cancer_df, aes(x = Organ, y = Survival)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=24, size=3)

# Exercise 4
with(cancer_df, tapply(Survival, Organ, shapiro.test))

# Exercise 5
leveneTest(Survival ~ Organ, data = cancer_df)

# Exercise 6
cancer_df$log_survival <- log(cancer_df$Survival)
with(cancer_df, tapply(log_survival, Organ, shapiro.test))
leveneTest(log_survival ~ Organ, data = cancer_df)

# Exercise 7
fit <- aov(log_survival ~ Organ, data = cancer_df)
summary(fit)

# Exercise 8
TukeyHSD(fit)

# Exercise 9
plot(TukeyHSD(fit))

# Exercise 10
kruskal.test(log_survival ~ Organ, cancer_df)