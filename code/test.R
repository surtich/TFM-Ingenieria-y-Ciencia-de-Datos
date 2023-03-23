library(cluster)

distancias <- df %>%
    xtabs(~ Question + Response, data = .) %>%
    daisy(x = ., metric = "euclidean")

# Realizar un clustering con el algoritmo de Ward
cluster <- agnes(distancias, method = "average", diss = TRUE)

# Asignar las preguntas a grupos
grupos <- cutree(cluster, k = 3)

# Crear un nuevo data frame con la columna Question y su grupo correspondiente
(preguntas_grupo <- data.frame(Question = levels(df$Question), Grupo = grupos))
