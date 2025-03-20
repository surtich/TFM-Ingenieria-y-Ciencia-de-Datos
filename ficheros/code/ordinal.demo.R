library(triangle)
library(ggplot2)
library(plotly)
library(dplyr)
library(ordinal)

set.seed(420)
N <- 200
c <- 30 # 0:70
x <- rnorm(N, c, 10)
y <- factor(floor(sapply(x, function(c) rtriangle(n = 1, a = 1, b = 5.99, c = min(5.99, max(1, c %/% 10))))), levels = 1:5)
df <- data.frame(x = x, y = y)

p <- df %>%
    group_by(y) %>%
    count() %>%
    ggplot(aes(x = y, y = n)) +
    geom_segment(aes(xend = y, yend = 0), color = "blue", size = 2) +
    geom_point(size = 6, shape = 21, fill = "blue", color = "white") +
    theme(
        panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

p <- df %>%
    group_by(y) %>%
    count() %>%
    ungroup() %>%
    mutate(n = cumsum(n)) %>%
    ggplot(aes(x = y, y = n)) +
    geom_segment(aes(xend = y, yend = 0), color = "blue", size = 2) +
    geom_point(size = 6, shape = 21, fill = "blue", color = "white") +
    theme(
        panel.background = element_rect(fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

fit <- clm(y ~ x, data = df)


# Definir los vectores

# Crear una matriz con el mismo número de filas que x y columnas que y
m <- matrix()

# Sumar cada elemento de y con cada elemento de x usando la función outer
y <- 0:5
x <- 0:90

coefs <- c(-Inf, coef(fit)[1:4], Inf)

m <- outer(coefs, x, FUN = function(coef, x) coef - coef(fit)[5] * x)

# Crear un dataframe con los vectores x, y y la matriz de suma
df_cum <- data.frame(x = rep(x, each = length(y)), y = rep(y, times = length(x)), cum.log.odds = c(m))

df_cum$cum.prob <- plogis(df_cum$cum.log.odds)
df_cum$cum.log.odds <- ifelse(df_cum$y == 5, 4.5, ifelse(df_cum$y == 0, -5.5, df_cum$cum.log.odds))
df_cum$color <- ifelse(df_cum$y == 5, "grey", "blue")
# Mostrar el dataframe resultante
df_cum

x_labels <- as.character(-4.5:4.5)
x_labels[length(x_labels)] <- "Inf"

p <- plot_ly(df_cum, x = ~cum.log.odds, y = ~cum.prob) %>%
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

p
