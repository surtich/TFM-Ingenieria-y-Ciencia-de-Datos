data <- read.table("code/Envir.dat", header = TRUE)
t <- as_tibble(data) %>%
    mutate(question = factor(question, labels = c("Drive Less", "Recycle")), y = factor(y, labels = c("Always", "Often", "Sometimes", "Never"))) %>%
    pivot_wider(
        names_from = question, values_from =
            y
    ) %>%
    xtabs(~
        Recycle + `Drive Less`, data = .)


# Definimos las dimensiones de la matriz
n <- nrow(t)
m <- n * (n - 1) / 2
# Creamos la matriz inicial con todos los valores en cero
dt <- data.frame()

for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
        r <- matrix(0, nrow = 1, ncol = n + 3)
        r[1, i] <- 1
        r[1, j] <- -1
        r[1, n + 1] <- j - i
        r[1, n + 2] <- t[i, j]
        r[1, n + 3] <- t[j, i]
        dt <- rbind(dt, r)
    }
}

colnames(dt) <- c(row.names(t), "x", "nij", "nji")


fit <- glm(nij / (nij + nji) ~ -1 + x, family = binomial, weights = nij + nji, data = dt)
summary(fit)
