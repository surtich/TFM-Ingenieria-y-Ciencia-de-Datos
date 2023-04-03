# https://rpubs.com/jboscomendoza/alfa_cronbach_r
# https://ladal.edu.au/surveys.html

df_clean %>%
    pivot_wider(names_from = Question, values_from = Response_v, id_cols = c(Treat, Subject)) %>%
    dplyr::select(-c(Treat, Subject)) %>%
    psych::alpha()

efa <- df_clean %>%
    pivot_wider(names_from = Question, values_from = Response_v, id_cols = c(Treat, Subject)) %>%
    dplyr::select(-c(Treat, Subject)) %>%
    psych::fa(cor = "poly", fm = "mle", rotate = "none", nfactor = 2)

plot(efa$loadings)

text(efa$loadings, labels = rownames(efa$loadings), pos = 3)
