library(patchwork)
x <- seq(-4, 4, length.out = 500)
y <- dnorm(x, 0, 1.5)

x_cuts <- c(-2.5, -1.1, 1.8)
x_labels <- c(expression(tau[1]), expression(tau[2]), expression(tau[3]))

data <- tibble(x, y)

g <- data %>% ggplot(aes(x = x, y = y)) +
    geom_line() +
    theme(
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = -3)
    ) +
    ylab(NULL)

cumulative <- g + geom_vline(xintercept = x_cuts) +
    geom_label(data = data.frame(x = x_cuts - 0.4, y = rep(0.22, 3), label = paste("Y = ", 1:3, sep = "")), aes(x = x, y = y, label = label)) +
    geom_label(x = 2.5, y = 0.22, label = "Y = 4") +
    scale_x_continuous(breaks = x_cuts, labels = x_labels) +
    xlab(expression(tilde(Y))) +
    coord_fixed(ratio = 5)


sequencial <- list()
for (i in 1:3) {
    p <- g + geom_vline(xintercept = x_cuts[i]) +
        geom_label(x = x_cuts[i] - 1, y = 0.22, label = paste("Y = ", i, sep = "")) +
        geom_label(x = x_cuts[i] + 1, y = 0.22, label = paste("Y > ", i, sep = "")) +
        scale_x_continuous(breaks = x_cuts[i], labels = x_labels[i]) +
        xlab(bquote(tilde(Y)[.(i)])) +
        coord_fixed(ratio = 30)
    sequencial[[i]] <- p
}
