library(gmodels)

CrossTable(df_all$Treat, df_all$Response, digits = 2)


chisq.test(df_all$Treat, df_all$Response)
chisq.test(df_all$Period, df_all$Response)
chisq.test(df_all$Seq, df_all$Response)

library(sjstats)
# V de Cramer
# Fuerza de la asociación

cramer(Response ~ Treat, data = df_all)
cramer(Response ~ Period, data = df_all)
cramer(Response ~ Seq, data = df_all)

library(stargazer)

m1 <- lm(as.numeric(Response) ~ Treat, data = df_response)
m2 <- lm(as.numeric(Response) ~ Treat * Period, data = df_response)

stargazer(m1, m2)

df <- df_response %>% mutate(Response = factor(Response == 5))


m1 <- df %>%
    glm(Response ~ Treat, data = ., family = binomial(link = "logit"))
summary(m1)
# deviance residuals, which are similar to the residuals in a linear regression model
# The null deviance is the deviance for the null model with the intercept only
# The residual deviance is the deviance for the fitted model, which is defined as −2(log likelihood of the current model − log likelihood of the saturated model)
# The difference between the residual deviance and the null deviance can be used to evaluate the significance of the fitted model
# the residual deviance is not the same as the deviance residual. The deviance residual is the residual for each case, whereas the residual deviance is the total of the deviance residuals multiplied by 2


coef(m1)
confint(m1) # based on the chi-square distribution of the likelihood ratio test statistic
confint.default(m1) # based on the Wald test statistic

exp(coef(m1)[1]) # odss response 5 for A treat
exp(coef(m1)[2]) # OR response 5 B/A

exp(coef(m1))
exp(confint(m1))
AIC(m1)
BIC(m1)
# pag 222
anova(m1, update(m1, ~1, data = df), test = "Chisq")

m2 <- df %>%
    glm(Response ~ Treat * Period, data = ., family = binomial(link = "logit"))
summary(m2)
cbind(exp(coef(m2)), exp(confint(m2)))
anova(m1, m2, test = "Chisq")
# pag 238
library(margins)
marg2 <- margins(m2, data = df)
summary(marg2)
# pag 239
predict(m2)

library(ggeffects)
margins <- ggpredict(m2, terms = ~ Period + Treat)

margins
plot(margins)

# pag 275
library(ordinal)


m1 <- clm(Response ~ Treat, data = df_response)

summary(m1)
coef(m1)
confint(m1)

exp(coef(m1))
exp(confint(m1))

m0 <- clm(Response ~ 1, data = df_response)
summary(m0)


anova(m0, m1)

# pag 283
library(rcompanion)
nagelkerke(m1)

nominal_test(m1)


# pag 287
options(contrasts = rep("contr.treatment", 2))
m2 <- clm(Response ~ Treat + Period, data = df_response)
summary(m2)

# pag292
margins.e <- ggpredict(m2, terms = ~Treat)
margins.e

plot(margins.e)

anova(m1, m2)
nagelkerke(m2)

nominal_test(m2)

options(contrasts = rep("contr.sum", 2))
m3 <- clm(Response ~ Treat + Period + Seq, data = df_response)
summary(m3)

nominal_test(m3)

m2 <- clm(Response ~ Treat + Period, data = df_response)
summary(m2)

anova(m2, m3)

predict(m3, type = "class")


stargazer(m1, m2, m3, type = "text", align = TRUE)

m4 <- clm(Response ~ Period, nominal = ~ Treat + Seq, data = df_response)
summary(m4)
nominal_test(m4)
m4$Theta

scale_test(m3)
m5 <- clm(Response ~ Treat + Period + Seq, scale = ~ Treat + Seq, data = df_response)
summary(m5)
convergence(m3)

predict(m5, type = "class")


library(VGAM)


# pag 300

model1 <- vglm(Response ~ Treat, cumulative(parallel = TRUE, reverse = F), data = df_response)
summary(model1)

model2 <- vglm(Response ~ Treat + Period + Seq, cumulative(parallel = TRUE, reverse = T), data = df_response)
summary(model2)

apply(predict(model2, type = "response"), 1, which.max)

model2c <- vglm(Response ~ Treat + Period + Seq, cumulative(parallel = F, reverse = T), data = df_response)
summary(model2c)


lrtest(model2, model2c)
lrtest(model1, model2)

coef(model2c, matrix = TRUE)

nagelkerke(model2c)


apply(predict(model2, type = "response"), 1, which.max)

model3c <- vglm(Response ~ Treat + Period + Seq, cumulative(parallel = F ~ Treat, reverse = T), data = df_response)
summary(model3c)

lrtest(model3c, model2c)

library(texreg)

texreg(model3c)

crmodel2 <- vglm(Response ~ Treat + Period + Seq, sratio(parallel = TRUE, reverse = FALSE), data = df_response)
summary(crmodel2)

apply(predict(crmodel2, type = "response"), 1, which.max)

crmodel3 <- vglm(Response ~ Treat + Period + Seq, sratio(parallel = F, reverse = FALSE), data = df_response)
summary(crmodel3)

apply(predict(crmodel3, type = "response"), 1, which.max)

acmodel2 <- vglm(Response ~ Treat + Period + Seq, acat(parallel = TRUE, reverse = FALSE), data = df_response)
summary(acmodel2)

apply(predict(acmodel2, type = "response"), 1, which.max)


acmodel3 <- vglm(Response ~ Treat + Period + Seq, acat(parallel = F, reverse = FALSE), data = df_response)
summary(acmodel3)

sum(apply(predict(model3c, type = "response"), 1, which.max) == df_response$Response)
sum(apply(predict(acmodel3, type = "response"), 1, which.max) == df_response$Response)


mulmodel2 <- vglm(Response ~ Treat + Period + Seq, multinomial(refLevel = 1), data = df_response)
summary(mulmodel2)

apply(predict(mulmodel2, type = "response"), 1, which.max)

# pag 759
mpo.1 <- clmm(Response ~ 1 + (1 | Subject), data = df_response, Hess = TRUE, nAGQ = 7)
summary(mpo.1)

exp(coef(mpo.1))

mpo.2 <- clmm(Response ~ Treat + (1 | Subject), data = df_response, Hess = TRUE, nAGQ = 7)
summary(mpo.2)

anova(mpo.1, mpo.2)


mpo.3 <- clmm2(Level ~ Treat + Period + Seq + Item, random = Subject, data = df_response, Hess = TRUE, nAGQ = 1)
summary(mpo.3)
sum(predict(mpo.3) >= 0.5)

library(brms)
olr.brm <- brm(Response ~ Treat + Period + Seq, family = cumulative("logit"), data = df_response, warmup = 500, iter = 1000)
summary(olr.brm)

mpo.brm <- brm(Response ~ Treat + Period + Seq + (1 | Subject) + (1 | Item), family = cumulative, data = df_response, warmup = 500, iter = 1000)
print(mpo.brm, digits = 3)

plot(mpo.brm)
conditional_effects(mpo.brm, categorical = TRUE)
