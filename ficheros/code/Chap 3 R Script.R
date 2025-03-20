# Chap 3 R Script
# Remove all objects
rm(list = ls(all=TRUE))

# The following user-written packages need to be installed first 
# Use install.packages(" ") and then load it with library()
# library(foreign)
# library(rcompanion)
# library(ResourceSelection)
# library(margins)
# library(ggeffects)               # It is already installed for Chapter 2
# library(stargazer)               # It is already installed for Chapter 2

# Import GSS 2016 Stata data file
library(foreign)
chp3.lr <- read.dta("C:/CDA/gss2016.dta")
chp3.lr$educ <- as.numeric(chp3.lr$educ)
chp3.lr$wrkfull <- as.numeric(chp3.lr$wrkfull)
chp3.lr$maritals <- as.numeric(chp3.lr$maritals)
attach(chp3.lr)
head(chp3.lr)
table(healthy)
table(wrkfull)

# Cross tabulation
tab <- table (healthy, wrkfull)
summary(tab)
tab
ftable(tab)
addmargins(tab)

# Simple logistic regression
LR.1 <- glm(healthy ~ wrkfull, data = chp3.lr, family = binomial(link = "logit"))
summary(LR.1)
coef(LR.1)
confint(LR.1)
confint.default(LR.1)
exp(coef(LR.1))
exp(confint(LR.1))

library(rcompanion)
nagelkerke(LR.1)
1-2203.2/2258.9
LR.0 <- glm(healthy ~ 1, data = chp3.lr, family = binomial(link = "logit"))
LLM <- logLik(LR.1)
LL0 <- logLik(LR.0)
McFadden <- 1-(LLM/LL0)
McFadden
CS <- 1-exp(2*(LL0-LLM)/1873)
CS
NG <- CS/(1-exp(2*LL0/1873))
NG

AIC(LR.1)
BIC(LR.1)

anova(LR.1, update(LR.1, ~1), test="Chisq")

# Multiple logistic regression
LR.2 <- glm(healthy ~ wrkfull + maritals + female + educ, data = chp3.lr, 
          family = binomial(link = "logit"))
summary(LR.2)
coef(LR.2)
confint(LR.2)
exp(coef(LR.2))
exp(confint(LR.2))
cbind(exp(coef(LR.2)), exp(confint(LR.2)))

nagelkerke(LR.2)
1-2103.4/2258.9
AIC(LR.2)
BIC(LR.2)
AIC(LR.1, LR.2)
BIC(LR.1, LR.2)

# Hosmer-Lemeshow goodness-of-fit test
library(ResourceSelection)
hoslem.test(healthy, fitted(LR.2), g = 10)
hlt2 <- hoslem.test(healthy, fitted(LR.2), g = 10)
cbind(hlt2$observed,hlt2$expected)

anova(LR.2, update(LR.2, ~1), test="Chisq")

# Model comparison using the log likelihood ratio test
anova(LR.1, LR.2, test = "Chisq")

# Marginal effects
library(margins)
marg2 <- margins(LR.2)
summary(marg2)

# Predicted probabilities
newdf <- data.frame(educ=c(12,14,16),
                  maritals=rep(mean(maritals),3),
                  wrkfull=rep(mean(wrkfull),3),
                  female=rep(mean(female),3))
newdf
newdf[,c('pred.prob')] <- predict(LR.2, newdata=newdf, type="response")
newdf

# Predicted probabilities with ggpredict() in ggeffects
library(ggeffects)
margins <- ggpredict(LR.2, terms="educ[12, 14, 16]")
margins
as.data.frame(margins)
sqrt(diag(vcov(margins)))
plot(margins)

margins.ew <- ggpredict(LR.2, terms=c("educ[12, 14, 16]","wrkfull"))
margins.ew
plot(margins.ew)

# Probit regression
PR.2 <- glm(healthy ~ wrkfull + maritals + female + educ, data = chp3.lr, 
          family = binomial(link = "probit"))
summary(PR.2)

# Making tables to display the results
library(stargazer)
stargazer(LR.1, LR.2, type="text", align=TRUE, out="lr2mod.txt")
stargazer(LR.1, LR.2, type="html", align=TRUE, out="lr2mod.htm")

detach(chp3.lr)
