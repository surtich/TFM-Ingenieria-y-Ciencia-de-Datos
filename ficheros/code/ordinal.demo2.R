library(triangle)
library(ggplot2)
library(plotly)
library(dplyr)
library(ordinal)

ord_fit <- function(x, y) {
    fit <- clm(y ~ x)
    m <- matrix()
    y <- 0:5
    x <- 0:90
    coefs <- c(-Inf, coef(fit)[1:4], Inf)
    m <- outer(coefs, x, FUN = function(coef, x) coef - coef(fit)[5] * x)
    df_cum <- data.frame(x = rep(x, each = length(y)), y = rep(y, times = length(x)), cum.log.odds = c(m))

    df_cum$cum.prob <- plogis(df_cum$cum.log.odds)
    df_cum$cum.log.odds <- ifelse(df_cum$y == 5, 4.5, ifelse(df_cum$y == 0, -5.5, df_cum$cum.log.odds))
    df_cum$color <- ifelse(df_cum$y == 5, "grey", "blue")

    return(df_cum)
}

set.seed(420)
N <- 200

nsteps <- 7

data <- list()
for (step in 1:nsteps) {
    x <- rnorm(N, step * 10, 10)
    y <- factor(floor(sapply(x, function(c) rtriangle(n = 1, a = 1, b = 5.99, c = min(5.99, max(1, c %/% 10))))), levels = 1:5)
    yfreq <- table(y) %>% data.frame()
    colnames(yfreq) <- c("y", "n")
    yfreq$cum <- cumsum(yfreq$n)
    fit <- ord_fit(x, y)

    data[[step]] <- list(
        visible = F,
        name = paste0("v = ", step),
        c = step * 10,
        x = x,
        y = y,
        yfreq = yfreq,
        fit = fit
    )
}

data[1][[1]]$visible <- TRUE

steps <- list()
fig1 <- plot_ly()
fig1 <- add_segments(fig1,
    x = data[1][[1]]$yfreq$y, xend = data[1][[1]]$yfreq$y, y = 0, yend = data[1][[1]]$yfreq$n, line = list(width = 4, color = "blue"),
    visible = data[1][[1]]$visible
)
fig1 <- add_trace(fig1,
    x = data[1][[1]]$yfreq$y, y = data[1][[1]]$yfreq$n,
    type = "scatter", mode = "markers", marker = list(color = "blue", size = 15),
    visible = data[1][[1]]$visible
)
fig1 <- add_segments(fig1,
    x = data[1][[1]]$yfreq$y, xend = data[1][[1]]$yfreq$y, y = data[1][[1]]$yfreq$n, yend = data[1][[1]]$yfreq$cum, line = list(width = 4, color = "red", dash = "dash"),
    visible = data[1][[1]]$visible
)
fig1 <- add_trace(fig1,
    x = data[1][[1]]$yfreq$y, y = data[1][[1]]$yfreq$cum,
    type = "scatter", mode = "markers", marker = list(color = "red", size = 15),
    visible = data[1][[1]]$visible
)

for (i in 1:nsteps) {
    step <- list(
        args = list("visible", rep(FALSE, length(data))),
        method = "restyle"
    )

    step$args[[2]][i] <- TRUE
    steps[[i]] <- step
}

fig1 <- fig1 %>% layout(
    showlegend = FALSE,
    xaxis = list(zeroline = F, range = c(-0.5, 5)),
    yaxis = list(range = c(0, 210))
)

x_labels <- as.character(-4.5:4.5)
x_labels[length(x_labels)] <- "Inf"

fig2 <- plot_ly(data[[1]]$fit, x = ~cum.log.odds, y = ~cum.prob) %>%
    add_segments(x = ~cum.log.odds, xend = ~cum.log.odds, y = 0, yend = ~cum.prob, frame = ~x, line = list(dash = "dash"), color = ~color) %>%
    add_segments(x = -5, xend = ~cum.log.odds, y = ~cum.prob, yend = ~cum.prob, frame = ~x, line = list(dash = "dash"), color = ~color) %>%
    layout(
        showlegend = FALSE,
        yaxis = list(range = c(-0.05, 1.2), tickvals = seq(0, 1, 0.2)),
        xaxis = list(zeroline = F, range = c(-5, 5))
    ) %>%
    add_lines(x = c(-5, -5), y = c(0, 1), line = list(color = "black", width = 1)) %>%
    add_trace(x = ~cum.log.odds, y = ~cum.prob, type = "scatter", mode = "lines+markers+text", frame = ~x, line = list(color = "blue", shape = "spline"), marker = list(color = ~color, size = 20), text = ~y, textposition = "middle center", textfont = list(color = "white", size = 12)) %>%
    animation_opts(frame = 50, redraw = T, easing = "linear") %>%
    animation_slider(value = "10")

fig3 <- plot_ly(data[[1]]$fit, x = ~cum.log.odds, y = ~cum.prob)

data[[1]]$fit %>%
    arrange(x, y) %>%
    mutate(prob = cum.prob - lag(cum.prob)) %>%
    filter(y != 0) %>%
    group_by(y) %>%
    plot_ly(x = ~x, y = ~prob, split = ~y, type = "scatter", mode = "lines")
