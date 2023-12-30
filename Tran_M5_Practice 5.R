#Trang Tran, ALY6010, Module 5, Practice 5
cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)
library(corrplot)

df <- read.csv("Life Expectancy Data.csv")
summary(df)
head(df)

df <- na.omit(df)
df <- df |> select(-Country, -Status)
cor(df)

#correlation table
corr <- cor(df[, c("Life.expectancy", "Alcohol", "BMI", "GDP", "Schooling")], method = "pearson")
corr

#correlation chart
corrplot(corr, method="circle", type="upper", tl.col = "black", tl.srt = 45)

#Look at distribution of target variable
ggplot(df, aes(x = Life.expectancy)) +
  geom_histogram(aes(y =..density..), fill = "orange", bins = 20) +
  geom_density() + geom_vline(aes(xintercept = mean(df$Life.expectancy)),
                              color="blue", linetype="dashed", size=1)

#Check for outliers
ggplot(df, aes(x = "", y = Life.expectancy)) +
  geom_boxplot()

ggplot(df, aes(x = "", y = Alcohol)) +
  geom_boxplot()

ggplot(df, aes(x = "", y = BMI)) +
  geom_boxplot()

ggplot(df, aes(x = "", y = GDP)) +
  geom_boxplot()

ggplot(df, aes(x = "", y = Schooling)) +
  geom_boxplot()

#Filter data
df <- df |> mutate(GDP = log(GDP))

ggplot(df, aes(x = "", y = GDP)) +
  geom_boxplot()

#Run OLS regression
reg1 <- lm(Life.expectancy ~ Alcohol + BMI + GDP + Schooling, data = df)
summary(reg1)

#Plot results
#load car package
library(car)

#produce added variable plots
avPlots(reg1)
