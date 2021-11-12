setwd("F:/Subbu/RMIT/sem 2/Analysis of Categorical data/project/phase 1")
assign_ds <- read.csv("Project Groups11_data.csv")

library(ggplot2)

mod_fit <- glm(formula = DEATH_EVENT ~ age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking,
    family = binomial(link = logit), data = assign_ds)
mod_fit1 <- glm(formula = DEATH_EVENT ~ age+creatinine_phosphokinase+ejection_fraction+serum_creatinine+serum_sodium,
                family = binomial(link = logit), data = assign_ds)
mod_fit$residuals
summary(mod_fit1)
vcov(mod_fit)

w.n1


####
#Serum_Creatinine
w<-aggregate(formula = DEATH_EVENT ~ serum_creatinine, data = assign_ds,
               FUN = sum)
n<-aggregate(formula = DEATH_EVENT ~ serum_creatinine, data = assign_ds,
               FUN = length)
w.n<-data.frame(Serum_Creatinine = w$serum_creatinine, Death = w$DEATH_EVENT,
                  trials = n$DEATH_EVENT, proportion = round(w$DEATH_EVENT/n$DEATH_EVENT,4))
head(w.n)

####
mod_fit_group <- glm(formula = Death/trials ~ Serum_Creatinine,weights = trials,
               family = binomial(link = logit), data = w.n)
pi.hat <- predict(mod_fit_group, type = "response")
p.res <- residuals(mod_fit_group, type = "pearson")
s.res <- rstandard(mod_fit_group, type = "pearson")
lin.pred <- mod_fit_group$linear.predictors
w.n <- data.frame(w.n, pi.hat,p.res, s.res, lin.pred)

#residuals
#continous
plot(x = w.n$Serum_Creatinine, y = w.n$s.res, xlab = "Serum_Creatinine",
     ylab = "Standardized Pearson residuals", main =
       "Standardized residuals vs. \n X")

abline(h = c(3, 2, 0, -2, -3), lty = 3, col = "blue")
# Add loess model to help visualize trend
smooth.stand <- loess(formula = s.res ~ Serum_Creatinine, data =
                        w.n, weights = trials)
# Make sure that loess estimates are ordered by "X" for
#the plots, so that they are displayed properly
order.SC <- order(w.n$Serum_Creatinine)
lines(x = w.n$Serum_Creatinine[order.SC], y =
        predict(smooth.stand)[order.SC], lty = 3, col =
        "red", lwd = 3)

#ejection_fraction
w1<-aggregate(formula = DEATH_EVENT ~ ejection_fraction, data = assign_ds,
              FUN = sum)
n1<-aggregate(formula = DEATH_EVENT ~ ejection_fraction, data = assign_ds,
              FUN = length)
w.n1<-data.frame(ejection_fraction = w1$ejection_fraction, Death = w1$DEATH_EVENT,
                 trials = n1$DEATH_EVENT, proportion = round(w1$DEATH_EVENT/n1$DEATH_EVENT,4))
head(w.n1)

####
mod_fit_group1 <- glm(formula = Death/trials ~ ejection_fraction,weights = trials,
                      family = binomial(link = logit), data = w.n1)
pi.hat <- predict(mod_fit_group1, type = "response")
p.res <- residuals(mod_fit_group1, type = "pearson")
s.res <- rstandard(mod_fit_group1, type = "pearson")
lin.pred <- mod_fit_group1$linear.predictors
w.n1 <- data.frame(w.n1, pi.hat,p.res, s.res, lin.pred)

#residuals
#continous
plot(x = w.n1$ejection_fraction, y = w.n1$s.res, xlab = "ejection_fraction",
     ylab = "Standardized Pearson residuals", main =
       "Standardized residuals vs. \n X")

abline(h = c(3, 2, 0, -2, -3), lty = 3, col = "blue")
# Add loess model to help visualize trend
smooth.stand <- loess(formula = s.res ~ ejection_fraction, data =
                        w.n1, weights = trials)
# Make sure that loess estimates are ordered by "X" for
#the plots, so that they are displayed properly
order.EF <- order(w.n1$ejection_fraction)
lines(x = w.n1$ejection_fraction[order.EF], y =
        predict(smooth.stand)[order.EF], lty = 3, col =
        "red", lwd = 3)

#Response Analysis
EF <- assign_ds %>% group_by(DEATH_EVENT) %>% summarise(Ejection_Fraction = mean(ejection_fraction))
p1 <- ggplot(data=EF, aes(x=DEATH_EVENT, y=Ejection_Fraction)) +
  geom_bar(stat="identity", fill = 'Steel Blue')+
  theme_minimal()
p1

SS <- assign_ds %>% group_by(DEATH_EVENT) %>% summarise(serum_sodium = mean(serum_sodium))
p2 <- ggplot(data=SS, aes(x=DEATH_EVENT, y=serum_sodium)) +
  geom_bar(stat="identity", fill = 'Grey')+
  theme_minimal()
p2

CP <- assign_ds %>% group_by(DEATH_EVENT) %>% summarise(creatinine_phosphokinase = mean(creatinine_phosphokinase))
p3 <- ggplot(data=CP, aes(x=DEATH_EVENT, y=creatinine_phosphokinase)) +
  geom_bar(stat="identity", fill = 'black')+
  theme_classic()
p3

install.packages("ResourceSelection")
library("ResourceSelection")
hoslem.test(mod_fit1$y,mod_fit1$fitted.values)

mod_fit1$deviance / mod_fit1$df.residual

confint(mod_fit1)

library(car)
Anova(mod_fit,test= "LR")

exp(mod_fit1$coefficients[5])
1/exp(5*mod_fit1$coefficients[5])





















