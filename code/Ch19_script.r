# Modern Analysis of Customer Satisfaction Surveys - Wiley 2011
# Chapter 19 - L Grilli & C Rampichini: Multilevel models for ordinal data
# R script for the analysis in Sect. 19.4

# call the 'ordinal' package
library(ordinal)

# call the 'lattice' and 'gplots' packages (for caterpillar graph)
library(lattice)
library(gplots)

# data input
cols <- c("course", "satisfaction", "fulltime", "exam", "knowledge", "interest")
evaluation <- read.table("code/data_ch19.txt", header = F, na.strings = ".", col.names = cols)
evaluation <- as.data.frame(evaluation)
attach(evaluation)
evaluation[1:10, ]

# observed frequencies
table(satisfaction) * 100 / length(satisfaction)

# generate the constant
const <- rep(1, length(satisfaction))

###############################################################
# CUMULATIVE LOGIT MODELS WITHOUT COVARIATES (NULL MODELS)

# standard cumulative logit model - no covariates
m00 <- clm(as.factor(satisfaction) ~ 1, link = "logit")
summary(m00)

# compute marginal probability P(Y=3)
pio3 <- ((1 / (1 + exp(-m00$coeff["3|4"]))) - (1 / (1 + exp(-m00$coeff["2|3"]))))
pio3

# random intercept cumulative logit model - no covariates
m0 <- clmm(as.factor(satisfaction) ~ 1 + (1 | as.factor(course)), nAGQ = 8, Hess = TRUE, link = "logit")
summary(m0)

# LRT comparing model m0 (random effects) with model m00 (no random effects)
anova(m0, m00)

# compute conditional probability P(Y=3|u=0)
pi3 <- ((1 / (1 + exp(-m0$coeff["3|4"]))) - (1 / (1 + exp(-m0$coeff["2|3"]))))
pi3

# compute the (residual) ICC for the latent response
icc0 <- m0$coef[length(m0$coef)]^2 / (m0$coef[length(m0$coef)]^2 + pi^2 / 3)
icc0

# compute conditional probability P(Y>=3|u=0) for u=+/-1.96sigmau
u <- -1.96 * m0$coef[length(m0$coef)]
badcourse <- 1 - 1 / (1 + exp(-m0$coeff["2|3"] + u))
badcourse
goodcourse <- 1 - 1 / (1 + exp(-m0$coeff["2|3"] + (-u)))
goodcourse

###############################################################
# CUMULATIVE LOGIT MODELS WITH COVARIATES

# center covariates on the value 3
exam_c3 <- exam - 3
knowledge_c3 <- knowledge - 3
interest_c3 <- interest - 3

summary(cbind(exam_c3, knowledge_c3, interest_c3, fulltime))

# standard cumulative logit model
m1 <- clm(as.factor(satisfaction) ~ fulltime + exam_c3 + knowledge_c3 + interest_c3, link = "logit")
summary(m1)

# random intercept cumulative logit model (Table 19.1)
m2 <- clmm(as.factor(satisfaction) ~ fulltime + exam_c3 + knowledge_c3 + interest_c3 + (1 | factor(course)), nAGQ = 8, Hess = TRUE, link = "logit")
summary(m2)

# compute the (residual) ICC for the latent response
icc <- m2$coef[length(m2$coef)]^2 / (m2$coef[length(m2$coef)]^2 + pi^2 / 3)
icc

##########
# compute predicted (conditional) cumulative probabilities (last four columns of Table 19.1)
# baseline: fulltime=0, exam_c3=knowledge_c3=interest_c3=0, u=0

linpred <- 0
### (PROB) compute and print category probabilities for linpred ###
cum1 <- 1 / (1 + exp(-m2$coeff[1] + linpred))
cum2 <- 1 / (1 + exp(-m2$coeff[2] + linpred))
cum3 <- 1 / (1 + exp(-m2$coeff[3] + linpred))
pr1 <- cum1
pr2 <- cum2 - cum1
pr3 <- cum3 - cum2
pr4 <- 1 - cum3
cbind(pr1, pr2, pr3, pr4)
###############################################################


## repeat lines in (PROB) above to print category probabilities after each of the following linpred ##
linpred <- m2$coeff["fulltime"]
linpred <- m2$coeff["exam_c3"]
linpred <- m2$coeff["knowledge_c3"]
linpred <- m2$coeff["interest_c3"]
linpred <- -1.96 * m2$coef[length(m2$coef)]
linpred <- +1.96 * m2$coef[length(m2$coef)]

##########
# compute predicted (conditional) cumulative probabilities
# Pr(Y>=3| fulltime=1, exam=knowledge=3, interest, u)
# for varying values of the covariate 'interest' and the random effect
# (these probabilities are needed for Figure 19.2)

# u = -1.96sigmau
u <- -1.96 * m2$coef[length(m2$coef)]
above2_low_x1 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (-2) * m2$coeff["interest_c3"] + u))
above2_low_x2 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (-1) * m2$coeff["interest_c3"] + u))
above2_low_x3 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + u))
above2_low_x4 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (+1) * m2$coeff["interest_c3"] + u))
cbind(above2_low_x1, above2_low_x2, above2_low_x3, above2_low_x4)

# u = 0
u <- 0
above2_med_x1 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (-2) * m2$coeff["interest_c3"] + u))
above2_med_x2 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (-1) * m2$coeff["interest_c3"] + u))
above2_med_x3 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + u))
above2_med_x4 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (+1) * m2$coeff["interest_c3"] + u))
cbind(above2_med_x1, above2_med_x2, above2_med_x3, above2_med_x4)

# u = +1.96sigmau
u <- +1.96 * m2$coef[length(m2$coef)]
above2_hig_x1 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (-2) * m2$coeff["interest_c3"] + u))
above2_hig_x2 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (-1) * m2$coeff["interest_c3"] + u))
above2_hig_x3 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + u))
above2_hig_x4 <- 1 - 1 / (1 + exp(-m2$coeff[2] + m2$coeff["fulltime"] + (+1) * m2$coeff["interest_c3"] + u))
cbind(above2_hig_x1, above2_hig_x2, above2_hig_x3, above2_hig_x4)

##########
# graph of predicted cumulative probabilities (Figure 19.2)

hig <- cbind(above2_hig_x1, above2_hig_x2, above2_hig_x3, above2_hig_x4)
med <- cbind(above2_med_x1, above2_med_x2, above2_med_x3, above2_med_x4)
low <- cbind(above2_low_x1, above2_low_x2, above2_low_x3, above2_low_x4)
x <- c(1, 2, 3, 4)

plot(x, med, xaxt = "n", cex = 2, ylim = c(0, 1), xlim = c(1, 4), ylab = "Probability of positive evaluation", xlab = "Interest in the subject")
points(x, low, pch = 6, cex = 2)
points(x, hig, pch = 5, cex = 2)
axis(1, x, c("1", "2", "3", "4"))
lines(x, low, lty = 1)
lines(x, med, lty = 1)
lines(x, hig, lty = 1)
legend(2.9, 0.2, legend = c('"good" course', '"mean" course', '"bad" course'), pch = c(5, 1, 6), lty = c(1, 1, 1))

##########
# prediction of random effects (conditional mode of the posterior distribution)
m2$ranef

# conditional variances of random effects (inverse Hessian at the conditional mode)
m2$condVar

# print cluster size, prediction of random effects and their std.dev. (increasing order of predicted value)
clust.size <- tapply(course, course, length)
u_pred <- cbind(clust.size, m2$ranef, sqrt(m2$condVar))
u_pred[order(m2$ranef), ]

##########
# caterpillar plot of ordered predicted random effects with 95% bars (Figure 19.3)
k <- 1:length(m2$ranef)
u_k <- u_pred[order(m2$ranef), ]
plotCI(x = k, y = u_k[, 2], 1.96 * u_k[, 3], lwd = 2, xlab = "Course ranking", ylab = "Predicted random effects", pch = 19, pt.bg = par("bg"))
lines(c(0, length(m2$ranef) + 1), c(0, 0), lwd = 2, lty = 2)

# end of script
