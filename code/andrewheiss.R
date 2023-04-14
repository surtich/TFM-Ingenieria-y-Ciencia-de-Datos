library(tidyverse)
library(brms)
library(tidybayes)
library(marginaleffects)
library(broom.mixed)
library(kableExtra)
library(scales)
library(ggtext)
library(patchwork)


library(viridisLite)
library(hrbrthemes)
library(ggrepel)

# Southern Utah colors
clrs <- NatParksPalettes::natparks.pals("BryceCanyon")

# Custom ggplot themes to make pretty plots
# Get Noto Sans at https://fonts.google.com/specimen/Noto+Sans
theme_nice <- function() {
    theme_bw(base_family = "Noto Sans") +
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold"),
            strip.background = element_rect(fill = "grey80", color = NA),
            legend.title = element_text(face = "bold")
        )
}

d <- readr::read_rds("https://www.andrewheiss.com/blog/2022/11/29/conditional-marginal-marginaleffects/df_example_lognormal.rds")

fit <- brm(
    bf(y ~ 1 + TX + (1 | cluster)),
    family = lognormal(),
    data = d,
    chains = 4, iter = 5000, warmup = 1000, seed = 4445
)

r_fit <- fit %>%
    tidy() %>%
    mutate(term = janitor::make_clean_names(term)) %>%
    split(~term)

B0 <- r_fit$intercept$estimate
B1 <- r_fit$tx$estimate
sigma_y <- r_fit$sd_observation$estimate
sigma_0 <- r_fit$sd_intercept$estimate

fit %>%
    tidy() %>%
    mutate(Parameter = c(
        "\\(\\beta_0\\)", "\\(\\beta_1\\)",
        "\\(\\sigma_0\\)", "\\(\\sigma_y\\)"
    )) %>%
    mutate(Description = c(
        "Global average gambling losses across all individuals",
        "Effect of treatment on gambling losses for all individuals",
        "Between-cluster variability of average gambling losses",
        "Within-cluster variability of gambling losses"
    )) %>%
    mutate(
        term = glue::glue("<code>{term}</code>"),
        estimate = round(estimate, 3)
    ) %>%
    select(Parameter, Term = term, Description, Estimate = estimate) %>%
    kbl(escape = FALSE) %>%
    kable_styling(full_width = FALSE)

predictions(
    fit,
    newdata = datagrid(TX = c(0, 1)),
    by = "TX",
    re_formula = NA
)
newdata <- df_clean %>% modelr::data_grid(Period, Treat, Question)

posterior_predict(brm_treat.period.subject.question, newdata, re_formula = ~ (1 + Treat | Question)) %>% head()



tidy_pred <- brm_treat.period.subject.question %>%
    predictions(newdata = newdata, re_formula = ~ (1 + Treat | Question))
tidy_pred



posterior_predict(brm_treat.period.subject.question, re_formula = ~ (1 + Treat | Question), newdata) %>% head()



conditional_preds <- brm_treat.period.subject.question %>%
    predictions(newdata = newdata, re_formula = ~ (1 + Treat | Question), by = c("Treat", "Question")) %>%
    posteriordraws()

brm_treat.period.subject.question %>% comparisons(newdata = newdata, variable = "Question", re_formula = ~ (1 + Treat | Question))


tidy_pred <- brm_treat.period.subject.question %>%
    predicted_draws(newdata = newdata, re_formula = ~ (1 + Treat | Question))
tidy_pred

tidy_epred <- brm_treat.period.subject.question %>%
    epred_draws(newdata = newdata, re_formula = ~ (1 + Treat | Question))
tidy_epred


newdata <- df_clean %>% modelr::data_grid(Period, Treat, Question)
pred_brm <- brm_treat.period.subject.question %>%
    epred_draws(newdata = newdata, re_formula = ~ (1 + Treat | Question), by = c("Treat", "Question"), category = "Response") %>%
    select(Period, Treat, Question, Response, .epred) %>%
    group_by(Period, Treat, Question, Response) %>%
    summarize(
        lo = quantile(.epred, 0.025),
        Probability = median(.epred),
        hi = quantile(.epred, 0.975)
    ) %>%
    ungroup()

pred_brm %>% ggplot(aes(x = Treat, y = Probability, colour = Response)) +
    geom_point(aes(shape = Period)) +
    geom_line(aes(group = interaction(Period, Response), linetype = Period)) +
    geom_ribbon(data = pred_brm, aes(ymin = lo, ymax = hi, fill = Response, group = Response), colour = NA, alpha = 0.2) +
    facet_grid(Question ~ Response, labeller = label_both) +
    theme(panel.spacing = grid::unit(0, "lines"))

pred2_brm <- brm_treat.period.subject.question %>%
    epred_draws(ndraws = 100, newdata = newdata, re_formula = ~ (1 + Treat | Question), by = c("Treat", "Question"), category = "Response") %>%
    select(Period, Treat, Question, Response, Probability = .epred, .draw) %>%
    ungroup() %>%
    group_by(.draw, Response) %>%
    mutate(indices = cur_group_id()) %>%
    ungroup()

colors <- viridis(
    option = "plasma",
    begin = 0,
    end = 0.9,
    direction = -1,
    n = 5
)

questions_vector <- setNames(levels(df_clean$Question_lr), levels(df_clean$Question))

question_labeller <- function(string) paste0(string, ": ", questions_vector[string])

# Plotting the fitted draws
p <- pred2_brm %>%
    ggplot(aes(
        x = Treat,
        y = Probability,
        color = Response,
        # Don't forget the indices!
        group = indices
    )) +
    facet_wrap(~Question, nrow = 6, labeller = as_labeller(question_labeller)) +
    geom_line(alpha = 0.2) +
    scale_color_manual(values = colors) +
    # We won't need these
    guides(
        color = FALSE,
        label = FALSE,
        scale = "none"
    ) +
    theme_ipsum_ps()


p +
    labs(
        x = "Treat",
        y = NULL
    ) +
    # Note we extend the right margin to make space for our labels(the order is top, right, bottom, left)
    theme(
        plot.margin = margin(30, 100, 30, 100),
        # You'll have to add this element_textbox_simple call to make the formatting work
        plot.subtitle = element_textbox_simple(
            margin = ggplot2::margin(10, 0, 20, 0)
        )
    ) +
    # This allows any labels or data to go past the grid
    coord_cartesian(clip = "off") +
    # Finally, our labels. We filter the data to avoid having a million of them
    geom_text_repel(
        data = pred2_brm %>% filter(Question %in% c("Q02", "Q05", "Q08", "Q11", "Q14", "Q17") & Period == 2, Treat == "B") %>% distinct(Treat,
            Period, Question, Response,
            .keep_all = TRUE
        ) %>% mutate(Response_l = ordered(Response, labels = levels(df_clean$Response_l))),
        aes(label = Response_l),
        direction = "y",
        hjust = 0,
        segment.size = 0.2,
        # Move the labels to the right
        nudge_x = 0.4,
        na.rm = TRUE,
        # Expand limits so that the label doesn't get stuck
        xlim = c(-10, 10),
        # Adjust size as needed!
        size = 3.5
    )
