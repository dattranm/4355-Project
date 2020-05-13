# STAT 4355.001 Course Project 
# Dat Tran - dmt170030

# Preparation: Install library and read data
install.packages("nloptr")
install.packages("faraway")
library(faraway)
library(leaps)
library(MASS)
library(car)
library(ggplot2)
library(reshape2)
data(sat)
n <- nrow(sat)

## Part 2
# regsubset input
all.possible <- regsubsets(expend ~ ratio+salary+takers+verbal+math, data=sat)
summary(all.possible)
summary(all.possible)$which
names(summary(all.possible))

# Selection criteria
ap.mse <- summary(all.possible)$rss/(n-(2:6))
ap.adjr2 <- summary(all.possible)$adjr2
ap.cp <- summary(all.possible)$cp
ap.bic <- summary(all.possible)$bic
ap.criteria <- cbind(ap.mse, ap.adjr2, ap.cp, ap.bic)
colnames(ap.criteria) <- c("MSE", "Adj R2", "Cp", "BIC")
rownames(ap.criteria) <- 2:6
ap.criteria

# Visualizing selection criteria
par(mfrow=c(1,2))
plot(all.possible, scale="r2", main = "Exhaustive: R2")
plot(all.possible, scale="adjr2", main = "Exhaustive: adjusted R2")

par(mfrow=c(1,2))
plot(2:6, ap.mse, col = "blue", type = "l", xlab = "p", ylab = "MSE")
plot(2:6, ap.adjr2, col = "blue", type = "l", xlab = "p", ylab = "Adj R2")

par(mfrow=c(1,2))
plot(all.possible, scale="Cp", main = "Exhaustive: Cp")
plot(all.possible, main="Exhaustive: BIC")

par(mfrow=c(1,2))
plot(2:6, ap.cp, col = "blue", xlab = "p", ylab = "Cp", pch=16, cex=1)
abline(a=0,b=1, col = "red")
plot(2:6, ap.bic, col = "blue", type = "l", xlab = "p", ylab = "BIC")

# Stepwise selection
full <- lm(expend~ ratio+salary+takers+verbal+math, data=sat)
summary(full)
bwd.aic <- stepAIC(full, direction="both")

# Model fitting
final.model <- lm(expend ~ ratio + salary, data = sat)
summary(final.model)

# Linear regression plotting
sat2 <- melt(sat[, c(1, 2:3)], id.vars = "expend")
ggplot(sat2) +
  geom_jitter(aes(value,expend, colour=variable),) +
  geom_smooth(aes(value,expend, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x")

# Normal probability plot
par(mfrow=c(1,2))
hist(studres(final.model), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
qqPlot(final.model)

# Residual Plot
par(mfrow=c(1,1))
residualPlot(final.model, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

#Influence analysis
myInf <- influence.measures(final.model)
summary(myInf)

influenceIndexPlot(final.model, vars=c("Cook", "Studentized", "hat"))

## Part 3
# Transformation

par(mfrow=c(1,1))
boxcox(sat$expend ~ sat$ratio, lambda=seq(-2,2,1/10))
boxcox(sat$expend ~ sat$salary, lambda=seq(-2,2,1/10))
boxcox(sat$expend ~ sat$takers, lambda=seq(-2,2,1/10))
boxcox(sat$expend ~ sat$verbal, lambda=seq(-2,2,1/10))
boxcox(sat$expend ~ sat$math, lambda=seq(-2,2,1/10))

# Through this, can choose y'= y^-0.5 to test (expend)
# regsubset input
all.possible.2 <- regsubsets(expend^-0.5 ~ ratio+salary+takers+verbal+math, data=sat)
summary(all.possible.2)
summary(all.possible.2)$which
names(summary(all.possible.2))

# Selection criteria
ap.mse.2 <- summary(all.possible.2)$rss/(n-(2:6))
ap.adjr2.2 <- summary(all.possible.2)$adjr2
ap.cp.2 <- summary(all.possible.2)$cp
ap.bic.2 <- summary(all.possible.2)$bic
ap.criteria.2 <- cbind(ap.mse.2, ap.adjr2.2, ap.cp.2, ap.bic.2)
colnames(ap.criteria.2) <- c("MSE", "Adj R2", "Cp", "BIC")
rownames(ap.criteria.2) <- 2:6
ap.criteria.2

# Visualizing selection criteria
par(mfrow=c(1,2))
plot(all.possible.2, scale="r2", main = "Exhaustive: R2")
plot(all.possible.2, scale="adjr2", main = "Exhaustive: adjusted R2")

par(mfrow=c(1,2))
plot(2:6, ap.mse.2, col = "blue", type = "l", xlab = "p", ylab = "MSE")
plot(2:6, ap.adjr2.2, col = "blue", type = "l", xlab = "p", ylab = "Adj R2")

par(mfrow=c(1,2))
plot(all.possible.2, scale="Cp", main = "Exhaustive: Cp")
plot(all.possible.2, main="Exhaustive: BIC")

par(mfrow=c(1,2))
plot(2:6, ap.cp.2, col = "blue", xlab = "p", ylab = "Cp", pch=16, cex=1)
abline(a=0,b=1, col = "red")
plot(2:6, ap.bic.2, col = "blue", type = "l", xlab = "p", ylab = "BIC")

# Stepwise selection
full.2 <- lm(expend^-0.5~ ratio+salary+takers+verbal+math, data=sat)
summary(full.2)
bwd.aic <- stepAIC(full.2, direction="both")

# Model fitting
final.model.2 <- lm(expend^-0.5 ~ ratio + salary + verbal + math, data = sat)
summary(final.model.2)

# Linear regression plotting
sat2.2 <- melt(sat[, c(1, 2:6)], id.vars = "expend")
sat2.2$expend <- sat2.2$expend^-0.5
ggplot(sat2.2) +
  geom_jitter(aes(value,expend, colour=variable),) +
  geom_smooth(aes(value,expend, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x")

# Normal  probability plot
par(mfrow=c(1,2))
hist(studres(final.model.2), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
qqPlot(final.model.2)

# Resisdual plot
par(mfrow=c(1,1))
residualPlot(final.model.2, type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

# Influence analysis
myInf.2 <- influence.measures(final.model.2)
summary(myInf.2)

influenceIndexPlot(final.model.2, vars=c("Cook", "Studentized", "hat"))
