ftable(df_all %>% dplyr::select(Treat, Response, Item))

library(vcd)

stable <- structable(df_all %>% dplyr::select(Treat, Cluster, Response))
stable
plot(stable)

spineplot(Response ~ Item, data = df_all)


df_response %>%
    group_by(Treat, Item) %>%
    summarize(Response = mean(Response_v)) %>%
    ungroup() %>%
    xyplot(Response ~ Item | Treat,
        data =
            ., type = c("h", "p"), pch = 16, lwd = 4, cex = 1.25
    )

# Probabilidad de respuesta
prop.table(ftable(df_all %>% dplyr::select(Treat, Period, Response)), margin = 1) %>%
    data.frame() %>%
    xyplot(Freq ~ Response | Treat + Period,
        data =
            ., type = c("h", "p"), pch = 16, lwd = 4, cex = 1.25
    )

# Test de independencia (pag 123)
summary(loddsratio(~ Treat + Period + Response_l, data = df_response))
summary(loddsratio(~ Response_l + Period, data = df_response))

library(vcdExtra)

# Tests for ordinal variables pag 125 (el importante es cmeans)
CMHtest(~ Response + Treat, data = df_response)
CMHtest(~ Response + Period, data = df_response)

CMHtest(~ Response + Period + Treat, data = df_response)

CMHtest(~ Response + Treat + Item, data = df_response)

# Página 134
fourfold(xtabs(~ Treat + Seq + Response, data = df_all))

fourfold(xtabs(~ Treat + Period + Response, data = df_all))

fourfold(xtabs(~ Treat + Period + Response + Cluster, data = df_all))

# 145
assoc(~ Response + Period, data = df_all, shade = TRUE, gp_axis = gpar(lty = 5))

# 162

df_all %>%
    dplyr::select(Response, Treat, Cluster) %>%
    mosaic(Response ~ Treat + Cluster, data = ., labeling = labeling_values)

# http://ddar.datavis.ca/pages/extra/titanic-tree-ex.pdf
library(party)
plot(ctree(Response ~ Treat + Cluster + Seq + Period, data = df_all),
    tp_args = list(fill = c("blue", "lightgray")),
    ip_args = list(fill = c("lightgreen"))
)

plot(ctree(Response ~ Treat + gender, data = df_all),
    tp_args = list(fill = c("blue", "lightgray")),
    ip_args = list(fill = c("lightgreen"))
)

library(MASS)
m1.polr <- polr(Response ~ Treat, data = df_response, Hess = TRUE)
summary(m1.polr)
m1.o <- clm(Response ~ Treat, data = df_response, Hess = TRUE)
summary(m1.o)
# pag 327
library(car)
Anova(m1)

library(VGAM)
m1.po <- vglm(Response ~ Treat, data = df_response, cumulative(parallel = TRUE))
summary(m1.po)

m1.npo <- vglm(Response ~ Treat, data = df_response, cumulative(parallel = FALSE))
summary(m1.npo)

coef(m1.po, matrix = TRUE)
coef(m1.npo, matrix = TRUE)

VGAM::lrtest(m1.npo, m1.po)


library(rms)
m1.po2 <- lrm(Response ~ Treat + Item, data = df_response)
m1.po2

plot.xmean.ordinaly(Response ~ Treat, data = df_response, lwd = 2, pch = 16, subn = FALSE)


# pag 331
# No usado. Necesita una varible continua
plotdat <- cbind(df_response, predict(m1.polr, type = "probs")) %>%
    pivot_longer(cols = c(`1`, `2`, `3`, `4`, `5`), names_to = "Response_level", values_to = "Probability")

# pag 333
library(effects)
plot(Effect("Treat", m1.polr))
plot(Effect("Treat", m1.polr), style = "stacked")

# pag 343
library(nnet)
m1.multi <- multinom(Response ~ Treat * Period, data = df_response, HESS = TRUE)

Anova(m1.multi)

library(brant)
brant(m1.polr)

# pag 378
library(corrplot)


library(brms)
library(bmmb)

model_ordinal <- brms::brm(Response ~ Treat + (1 | Subject) + (1 | Item), data = df_response, family = "cumulative")
bmmb::short_summary(model_ordinal)

# make latent variable predictions
predictions_latent <- fitted(model_ordinal, scale = "linear", re_formula = NA)

# inspect first 6 predictions
head(predictions_latent)

# get fixed effects
fixed_effects <- brms::fixef(model_ordinal)

# predict probability of category membership
predictions_latent <- fitted(model_ordinal, re_formula = NA)

# see first six for each category
head(predictions_latent[, 1, ])

brms::fixef(model_ordinal)

samples <- brms::fixef(model_ordinal, summary = FALSE)
colMeans(samples)
