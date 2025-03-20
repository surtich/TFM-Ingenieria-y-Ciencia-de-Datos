# Analysis of Example 4.3 Cross-over Trials in Clinical Research

# This example cannot be analysed by SPlus or R unless the
# Venables and Ripley MASS library is used

# This analysis not illustrated in book

# This version has SPlus commands that need to be modified commented out and their R versions added

library(MASS) # Enable use of the Venables and Ripley MASS library

# Input data values
# n1 is number of values in first sequence,
# n2 is number in second sequence, n is total
# sequence is factor of sequence labels
n1 <- 12
n2 <- 12
n <- n1 + n2
sequence <- factor(c(rep("forsal", n1), rep("salfor", n2)))
patient <- factor(c(
    3, 4, 7, 8, 9, 11, 15, 16, 19, 20, 22, 23,
    1, 2, 5, 6, 10, 12, 13, 14, 17, 18, 21, 24
))
EffOrd1 <- c(
    4, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 3,
    4, 4, 4, 4, 4, 4, 3, 2, 2, 3
)
EffOrd2 <- c(
    4, 1, 1, 3, 4, 3, 3, 1, 3, 1, 3, 2, 4, 4,
    4, 4, 4, 4, 4, 3, 4, 4, 4, 4
)
# Calculate binary categories
# EffBin1<-cut(EffOrd1,breaks=c(-0.5,3.5,4.5)) # AS: in S-Plus
EffBin1 <- cut(EffOrd1, breaks = c(-0.5, 3.5, 4.5), labels = c(0, 1)) # AS: in R

# EffBin2<-cut(EffOrd2,breaks=c(-0.5,3.5,4.5)) # AS: in S-Plus
EffBin2 <- cut(EffOrd2, breaks = c(-0.5, 3.5, 4.5), labels = c(0, 1)) # AS: in R

# Calculate change scores
# ChBin<-factor(cut((EffBin1-EffBin2),breaks=c(-1.5,-0.5,0.5,1.5)),levels=c(1,2,3)) # AS: in S-Plus
ChBin <- factor(cut((as.numeric(as.character(EffBin1)) - as.numeric(as.character(EffBin2))), breaks = c(-1.5, -0.5, 0.5, 1.5), labels = c(1, 2, 3))) # AS: in R

# ChOrd<-factor(cut((EffOrd1-EffOrd2),breaks=c(-3.5,-2.5,-0.5,0.5,2.5,3.5)),levels=c(1,2,3,4,5)) # AS: in S-Plus
ChOrd <- factor(cut((EffOrd1 - EffOrd2), breaks = c(-3.5, -2.5, -0.5, 0.5, 2.5, 3.5), labels = c(1, 2, 3, 4, 5))) # AS: in R

Change.frame <- data.frame(sequence, patient, EffOrd1, EffOrd2, ChBin, ChOrd)
Change.frame

# Carry out logistic regression for categorical variables
# on the change score using approach of Venabales and Ripley
# Note that estimates and SEs appear to be half what they would
# be with SAS paramaterisation

# Use proportional odds model on first change score
# fit1<-polr(ChBin~sequence) # AS: in S-Plus
fit1 <- polr(ChBin ~ sequence, Hess = TRUE) # AS: in R (Hess = TRUE to be able to call summary())
summary(fit1)

# Use proportional odds model on second change score

## ChOrd should be ordered:
## Change.frame$ChOrdt<-ordered(Change.frame$ChOrd, levels=2:5)
# fit2<-polr(ChOrd~sequence) # AS: in S-Plus
fit2 <- polr(ChOrd ~ sequence, Hess = TRUE) # AS: in R
summary(fit2)