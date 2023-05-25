library(scico)

options(contrasts = rep("contr.sum", 2))
brm_null <- brm(
    Response ~ 1,
    data = df_response,
    family = cumulative("logit"),
    iter = 4000,
    cores = 4,
    sample_prior = TRUE,
    file = "models/brm_null",
    file_refit = "on_change"
)

summary(brm_null)

brm_null_draws <- as_draws_df(brm_null)
str(brm_null_draws)

brm_null_draws <-
    brm_null_draws %>%
    select(.draw, `b_Intercept[1]`:`b_Intercept[4]`)

# compute the posterior means for each threshold
brm_null_means <-
    brm_null_draws %>%
    summarise_at(vars(`b_Intercept[1]`:`b_Intercept[4]`), mean) %>%
    pivot_longer(everything(),
        values_to = "mean"
    )

# wrangle
brm_null_draws %>%
    pivot_longer(-.draw, values_to = "threshold") %>%
    group_by(.draw) %>%
    mutate(theta_bar = mean(threshold)) %>%
    ggplot(aes(x = threshold, y = theta_bar, color = name)) +
    geom_vline(
        data = brm_null_means,
        aes(xintercept = mean, color = name),
        linetype = 2
    ) +
    geom_point(alpha = 1 / 10) +
    scale_color_scico_d(palette = "lajolla", begin = .25) +
    ylab("mean threshold") +
    theme(legend.position = "none")


library(GGally)

sl <- scico(palette = "lajolla", n = 9)

my_upper <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
        geom_point(size = 1 / 5, alpha = 1 / 5, color = sl[7])
}

my_diag <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
        geom_density(linewidth = 0, fill = sl[3]) +
        scale_x_continuous(NULL, breaks = NULL) +
        scale_y_continuous(NULL, breaks = NULL)
}

my_lower <- function(data, mapping, ...) {
    # get the x and y data to use the other code
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    # compute the correlations
    corr <- cor(x, y, method = "p", use = "pairwise")

    # plot the cor value
    ggally_text(
        label = formatC(corr, digits = 2, format = "f") %>% str_replace(., "0\\.", "."),
        mapping = aes(),
        color = "black",
        size = 4
    ) +
        scale_x_continuous(NULL, breaks = NULL) +
        scale_y_continuous(NULL, breaks = NULL)
}

as_draws_df(brm_null) %>%
    select(contains("b_Intercept")) %>%
    set_names(str_c("theta[", 1:4, "]")) %>%
    # plot!
    ggpairs(
        upper = list(continuous = my_upper),
        diag = list(continuous = my_diag),
        lower = list(continuous = my_lower),
        labeller = label_parsed
    )


library(tidybayes)

brm_null_draws %>%
    select(-.draw) %>%
    mutate_all(.funs = ~ exp(.) / (1 + exp(.))) %>%
    transmute(
        `p[Y==1]` = `b_Intercept[1]`,
        `p[Y==2]` = `b_Intercept[2]` - `b_Intercept[1]`,
        `p[Y==3]` = `b_Intercept[3]` - `b_Intercept[2]`,
        `p[Y==4]` = `b_Intercept[4]` - `b_Intercept[3]`,
        `p[Y==5]` = 1 - `b_Intercept[4]`
    ) %>%
    set_names(1:5) %>%
    pivot_longer(everything(), names_to = "Y") %>%
    ggplot(aes(x = value, y = Y)) +
    stat_halfeye(
        point_interval = mode_hdi, .width = .95,
        fill = sl[4], color = sl[8], size = 1 / 2, height = 2.5
    ) +
    scale_x_continuous(expression(italic(p) * "[" * Y == italic(i) * "]"),
        breaks = 0:5 / 5,
        expand = c(0, 0), limits = c(0, 1)
    )


bayesplot::color_scheme_set(sl[2:7])

set.seed(23)

pp_check(brm_null, type = "bars", ndraws = 1000, fatten = 2) +
    scale_x_continuous("y", breaks = 1:5) +
    scale_y_continuous(NULL, breaks = NULL, expand = expansion(mult = c(0, 0.05))) +
    theme(
        legend.background = element_blank(),
        legend.position = c(.9, .8)
    )


brm_treat <- brm(
    Response ~ 1 + Treat,
    data = df_response,
    family = cumulative("logit"),
    iter = 4000,
    cores = 4,
    sample_prior = TRUE,
    file = "models/brm_treat",
    file_refit = "on_change"
)

summary(brm_treat)


options(contrasts = rep("contr.sum", 2))
brm_treat.period.subject.question_thres <- brm(
    Response | thres(gr = Item) ~ Treat * Period + (1 + Treat | Subject) + (1 + Treat | Item),
    data = df_response,
    family = cumulative("logit"),
    iter = 4000,
    sample_prior = TRUE,
    chains = 4,
    cores = 4,
    file = "models/brm_treat.period.subject.question_thres",
    file_refit = "on_change",
    control = list(adapt_delta = 0.99, max_treedepth = 12)
)

summary(brm_treat.period.subject.question_thres)

pp_check(brm_treat.period.subject.question_thres, type = "bars_grouped", group = "Item", ndraws = 1000)


pred2_brm <- brm_treat.period.subject.question_thres %>%
    epred_draws(ndraws = 50, newdata = newdata, re_formula = ~ (1 + Treat | Item), by = c("Treat", "Item"), category = "Response") %>%
    select(Period, Treat, Item, Response, Probability = .epred, .draw) %>%
    mutate(Median = ave(Probability, FUN = function(x) quantile(x, .5, type = 3, na.rm = TRUE))) %>%
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

questions_vector <- setNames(levels(df_response$Item_lr), levels(df_response$Item))

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
    facet_wrap(~Item, nrow = 6, labeller = as_labeller(question_labeller)) +
    geom_line(alpha = 0.4) +
    scale_color_manual(values = colors) +
    # We won't need these
    guides(
        color = FALSE,
        label = FALSE,
        scale = "none"
    ) +
    theme_ipsum_ps(base_family = NULL)
p +
    labs(
        x = "Treat",
        y = "Probability"
    ) +
    theme(
        plot.margin = margin(0, 100, 0, 0),
    ) +
    # This allows any labels or data to go past the grid
    coord_cartesian(clip = "off") +
    # Finally, our labels. We filter the data to avoid having a million of them
    geom_text_repel(
        data = pred2_brm %>% filter(Probability == Median & Item %in% c("Q02", "Q05", "Q08", "Q11", "Q14", "Q17") & Period == 2, Treat == "B") %>% distinct(Treat,
            Period, Item, Response,
            .keep_all = TRUE
        ) %>% mutate(Response_l = ordered(Response, labels = levels(df_response$Response_l))),
        aes(label = Response_l),
        direction = "y",
        hjust = 0,
        segment.size = 0.2,
        # Move the labels to the right
        nudge_x = 0.1,
        na.rm = TRUE,
        # Expand limits so that the label doesn't get stuck
        xlim = c(0, 5),
        # Adjust size as needed!
        size = 3.5
    ) + scale_x_discrete(expand = c(0, 0), limits = c("A", "B"))


library(marginaleffects)
options(marginaleffects_posterior_center = mean)
predictions(brm_treat.period.subject.question, newdata = newdata, re_formula = ~ (1 + Treat | Item), by = c("Item", "Treat"))

avg_comparisons(brm_treat.period.subject.question, newdata = newdata, re_formula = ~ (1 + Treat | Item), variables = c("Treat"))
avg_comparisons(brm_treat.period.subject.question, newdata = newdata, re_formula = ~ (1 + Treat | Item), by = c("Item"), variables = c("Treat"))
