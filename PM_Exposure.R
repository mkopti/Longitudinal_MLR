df <- read.table("BreathingZonePM.txt", header=T)
head(df)
library(tidyverse)
library(tinytex)
library(tidyverse)
library(forecast)
library(multcomp)
library(nlme)
library(car)

# Histogram of Aerosol Measurements:
ggplot(data = df, aes(x = Aerosol)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Aerosol Measurements", y = "Frequency", title = "Distribution of Aerosol Measurements")

# Create a new variable with the logged aerosol measurements
df$log_aerosol <- log(df$Aerosol)

# Make ID variable a factor
df$ID <- factor(df$ID)

# Create a histogram of logged aerosol measurements
ggplot(df, aes(x=log_aerosol)) +
  geom_histogram(binwidth=0.2, color="black", fill="blue") +
  xlab("Logged Aerosol Measurements") +
  ylab("Frequency") +
  ggtitle("Histogram of Logged Aerosol Measurements")

ggplot(data = df, aes(x = log(Stationary), y = log(Aerosol))) +
  geom_point() +
  labs(x = "Stationary Measurements", y = "Logged Aerosol Measurements", title = "Relationship between Logged Stationary and Logged Aerosol Measurements")

cor(log(df$Stationary), log(df$Aerosol))

ggplot(data = df, aes(x = Activity, y = log(Aerosol))) +
  geom_boxplot()

AR1.model <- gls(model=log(Aerosol) ~ Activity*ID + log(Stationary)*ID , data=df, 
                 correlation=corAR1(form=~Minute|ID), method="ML")


# Fit an independent model without the Time variable
lmobj <- lm(log(Aerosol) ~ log(Stationary) + Activity, data = df)

# Check the assumption of linearity using added-variable plots
avPlots(lmobj, layout = c(5,2))

source("stdres.gls.R")

# Obtain the decorrelated residuals
sres <- stdres.gls(AR1.model)

# Extract the residuals
residuals <- matrix(sres, nrow = 100, ncol = 59, byrow = TRUE)

# Calculate the correlation matrix
R <- cor(residuals)

# Print the correlation matrix
round(R[1:5, 1:5], 2)

# Create a histogram of the decorrelated residuals
hist(sres, main = "Histogram of Decorrelated Residuals")

# Fitted values and decorrelated residuals plot
plot(fitted(AR1.model), sres, xlab = "Fitted Values", ylab = "Decorrelated Residuals", main = "Residuals vs. Fitted Values Plot")

# Fit the model

# Calculate the fitted values
fitted <- predict(AR1.model)

Aerosol <- log(df$Aerosol)
# Calculate the correlation of observed and fitted values
corr <- cor(Aerosol, fitted)

# Calculate the pseudo R-squared
pseudo_R2 <- corr^2

# Print the pseudo R-squared
print(pseudo_R2)

# alternative method 
cor(fitted(AR1.model),log(df$Aerosol))^2

# Does stationary measurement alone do a good job explaining PM exposure
stationary.model <- gls(model=log(Aerosol) ~ log(Stationary), data=df, 
                        correlation=corAR1(form=~Minute|ID), method="ML")
cor(log(df$Aerosol), fitted(stationary.model))^2


# Do the activities explain more of the aerosol intake then just stationary alone?
new.model <- gls(model=log(Aerosol) ~ log(Stationary) + Activity, data=df, 
                 correlation=corAR1(form=~Minute|ID), method="ML")

# Finding the R2
cor(fitted(new.model),log(df$Aerosol))^2

# Are the effects of activity/stationary child-specific?
# reduced model (no interaction ID:log(Stationary) or ID:Activity terms)

red.model <- gls(model=log(Aerosol) ~ log(Stationary) + Activity + ID, data=df, 
                 correlation=corAR1(form=~Minute|ID), method="ML")
# ANOVA F-test to compare models
anova(red.model, AR1.model)


# What activities (on average) lead to higher PM exposure
# AR1.model$coefficients[1:8]
# summary(AR1.model)
# intervals(AR1.model)

# Variability in the effects from child to child 

# histogram of interaction terms from ID:ActivityHomework 
hist(AR1.model$coefficients[c(seq(109, 795, 7))], main = "Histogram of ID:ActivityHomework Interaction Terms", xlab = "ID:ActivityHomework")
# on the phone
hist(AR1.model$coefficients[c(seq(110, 796, 7))], main = "Histogram of ID:OnPhone Interaction Terms", xlab = "ID:OnPhone")
# playing on the floor
hist(AR1.model$coefficients[c(seq(111, 797, 7))], main = "Histogram of ID:PlayingOnFloor Interaction Terms", xlab = "ID:PlayingOnFloor")
# playing on furniture
hist(AR1.model$coefficients[c(seq(112, 798, 7))], main = "Histogram of ID:PlayingOnFurniture Interaction Terms", xlab = "ID:PlayingOnFurniture")
# video games
hist(AR1.model$coefficients[c(seq(113, 799, 7))], main = "Histogram of ID:VideoGames Interaction Terms", xlab = "ID:VideoGames")
# walking 
hist(AR1.model$coefficients[c(seq(114, 800, 7))], main = "Histogram of ID:Walking Interaction Terms", xlab = "ID:Walking")
# watching TV
hist(AR1.model$coefficients[c(seq(115, 801, 7))], main = "Histogram of ID:WatchingTV Interaction Terms", xlab = "ID:WatchingTV")

# log(Stationary)
hist(AR1.model$coefficients[c(802:900)], main = "Histogram of ID:log(Stationary)", xlab = "ID:log(Stationary")