df_improve <- df_clean %>%
    pivot_wider(id_cols = c(Subject, Question, Seq), names_from = Treat, values_from = Response) %>%
    filter(A != 0 & B != 0) %>%
    mutate(Improve = A > B, Improve_level = (A %in% c(4, 5)) & (B %in% c(1, 2)))

qlogis(tapply(df_improve$Improve, df_improve$Seq, mean))

m4 <- brm(Improve ~ 1 + Seq + (1 | Subject) + (1 | Question), family = "bernoulli", data = df_improve)
m4

options(contrasts = c("contr.treatment", "contr.treatment"))
options(contrasts = c("contr.sum", "contr.sum"))
m5 <- brm(Improve ~ 1 + Seq + (1 | Subject) + (1 | Question), family = "bernoulli", data = df_improve)
m5

brms::fixef(m5)

samples <- brms::fixef(m5, summary = FALSE)

head(samples)
colMeans(samples)

new_parameters <- cbind(
    AB_mean = samples[, "Intercept"] + samples[, "Seq1"],
    BA_mean = samples[, "Intercept"] - samples[, "Seq1"]
)

brms::posterior_summary(new_parameters)

short_hypothesis(
    m5,
    c(
        "Intercept = 0", # overall mean
        "Intercept + Seq1 = 0",
        "Intercept - Seq1 = 0"
    )
)


question_effects_hat <- ranef(m5, summary = FALSE)$Question[, , "Intercept"]
str(question_effects_hat)

Intercept_hat <- fixef(m5, summary = FALSE)[, "Intercept"]

str(Intercept_hat)

question_means_hat <- brms::posterior_summary(Intercept_hat + question_effects_hat)
question_effects_hat <- brms::posterior_summary(question_effects_hat)

# find average improve for each question and seq
question_means <- tapply(df_improve$Improve, df_improve[, c("Seq", "Question")], mean)

# find average improve for each question
question_means <- colMeans(question_means)
Intercept <- mean(question_means)
question_effects <- question_means - Intercept


short_hypothesis(m5, "Intercept = 0")
short_hypothesis(m5, "Intercept = 0", scope = "ranef", group = "Question")
question_effects_hat

brm_treat.period.subject.question <- brm(
    Response ~ Treat * Period + (1 + Treat | Subject) + (1 + Treat | Question),
    data = df_clean,
    family = cumulative("logit"),
    sample_prior = TRUE,
    file = "models/brm_treat.period.subject.question",
    file_refit = "on_change",
    iter = 4000
)

bmmb::short_summary(brm_treat.period.subject.question)

varcorr_information <- brms::VarCorr(brm_treat.period.subject.question)

str(varcorr_information)
bmmb::get_sds(brm_treat.period.subject.question)

bmmb::getcorrs(brm_treat.period.subject.question, factor = "Question")

model_sum_coding <- brms::add_criterion(brm_treat.period.subject.question, criterion = "waic")
