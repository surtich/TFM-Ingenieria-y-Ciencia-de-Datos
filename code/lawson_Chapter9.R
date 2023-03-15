# example 1 p. 353-354
library(daewr)
library(car)
modl <- lm( pl ~ Subject + Period + Treat, data = antifungal,
            contrasts = list(Subject = contr.sum, Period = contr.sum,
                             Treat = contr.sum))
Anova(modl, type = "III" )

# Example 2 p. 354
library(emmeans)
lsmeans(modl, pairwise ~ Treat)

# Example 3 p. 355
alpha <- 0.05
sigma2 <- 326
delta <- 10
n <- seq( 40, 80, by = 2)
stderrdiff <- sqrt( 2 * sigma2 / n)
df <- n - 2
t1 <- qt( 1 - alpha / 2, df )
gamma <- delta / stderrdiff
power <- 1 - pt(t1, df, gamma)
data.frame( alpha = alpha, n = n, delta = delta, power = power)

# Example 4 p. 356
library(lme4)
c1 <- c( .5, -.5)
mod4 <- lmer( pl ~ 1 + Group + (1|Subject:Group) + Period +
                Treat, contrasts = list(Group = c1, Period = c1, Treat = c1),
              data = antifungal)
summary(mod4)

# Example 5 p. 360
library(daewr)
data(bioequiv)
head(bioequiv,12)

# Example 6 p. 360
modc<-lm(y~Subject+Period+Treat+Carry,data=bioequiv)
anova(modc)

# Example 7 p. 361
library(lsmeans)
lsmeans(modc, ~ Treat)

# Example 8 p. 362
library(crossdes)
wdes3 <- williams(3)
rownames(wdes3) <- paste("seqGroup", 1:6, sep = "")
colnames(wdes3) <- paste("Period", 1:3, sep = "")
wdes3

# Example 9 p. 363
library(daewr)
data(chipman)
mod3a <- lm(Time ~ Subject + Period + Treat + Carry, data=chipman)
mod3b <- lm(Time ~ Subject + Period + Carry + Treat, data=chipman)

# Example 10 p. 364
anova(mod3b)
anova(mod3a)


# Example 11 p. 365
with(chipman, tapply(Time, Treat, mean))
sqrt(with(chipman, tapply(Time, Treat, var)))
with(chipman, tapply(Time, Carry, mean))
sqrt(with(chipman, tapply(Time, Carry, var)))

# Figure 9.2 p. 368
interaction.plot(strung$week, strung$Diet, strung$protein, type="b",pch=c(18,24,22),
                 leg.bty="o" ,main="Comparison of Trends in Protein over Time"
                 ,xlab="Week",ylab="Average Protein")

# Example 12 p. 368-369
library(daewr)
data(strung)
library(GAD)
D <- as.fixed(strung$Diet)
W <- as.fixed(strung$week)
C <- as.random(strung$Cow)
model <- lm(protein ~ D + C%in%D + W + D*W, data = strung)
gad(model)

# Example 13 p. 369
library(lme4)
rmodel <- lmer(protein ~ 1 + Diet*week + (1|Cow:Diet),
               data = strung)
anova(rmodel)

# Example 14 p. 370
pr1 <- strung$protein[1:30]
pr2 <- strung$protein[31:60]
pr3 <- strung$protein[61:90]
pr4 <- strung$protein[91:120]
dairy <- data.frame(Diet = as.factor(strung$Diet[1:30]),
                    pr1, pr2, pr3, pr4)

# Example 15 p. 370
mod.w <- lm(cbind(pr1 ,pr2, pr3, pr4) ~ Diet, data = dairy) 

# Example 16 p. 371
library(car)
time <- factor(c(1:4))
idata <- data.frame(time)
(mod.i <- Anova(mod.w, idata = idata, idesign = ~ time))
summary(mod.i,multivariate=FALSE)

# Figure 9.3 p. 374
library(daewr)
data(residue)
res30 <- residue[residue$temp == 30, ]
res30L <- res30[res30$moisture == 'L', ]
res30LP <- res30L[res30L$soil =='P', ]
lc <- log(c(res30LP[1,4],res30LP[1,5],res30LP[1,6],res30LP[1,7],res30LP[1,8],
            res30LP[2,4],res30LP[1,5],res30LP[2,6],res30LP[2,7],res30LP[2,8]))
day <- rep(c(0,7,14,30,60), 2)
fl <- lm(lc~day)
plot(lc~day, ylab="log concentration", main="Log Concentration by Day for Temp.=30 Deg. Moisture=L Soil=P")
abline(coef(fl))

# Example 17 p. 375
library(daewr)
data(residue)
sp <- 7 * log(residue$X2) + 14 * log(residue$X3) +
  30 * log(residue$X4) + 60 * log(residue$X5)
sm <- log(residue$X1) + log(residue$X2) + log(residue$X3) + log(residue$X4) + log(residue$X5)
num <- 5 * sp - 111 * sm
den <- 5 * 4745 - (111)**2
k <- num / den
half_life <- -log(2)/k
logHL <- log(half_life)
residue <- cbind(residue, k, half_life, logHL)
options(digits=3)
residue[ , c(1:3,9:11)]

# Example 18 p. 376
growthc <- aov(logHL ~ temp*moisture*soil, data = residue)
summary(growthc)

# Example 19 p. 376
model.tables(growthc)

