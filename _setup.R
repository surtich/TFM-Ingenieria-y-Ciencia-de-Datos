#| warning: false
#| include: false
#| echo: false
#| cache: false

library(car)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(lme4)
# library(lmerTest)
library(magrittr)
library(multcomp)
library(ordinal)
library(purrr)
library(stringr)
library(readr)
library(testit)
library(tidyr)
library(tidyverse)
library(broom)
library(emmeans)
library(vcdExtra)
library(gt)
library(gtExtras)
library(likert)
library(patchwork)
library(colorspace)
library(vcd)
library(party)
library(caret)
library(dunnr)
library(VGAM)
library(janitor)
library(broom.mixed)
library(bmmb)
library(brms)
library(tidybayes)
library(viridisLite)
library(hrbrthemes)
library(ggrepel)
library(modelr)
library(ggtext)

## ggplot options
theme_set(theme_bw())
theme_update(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
)
knitr::opts_chunk$set(dev.args = list(bg = "transparent"))

# Leemos el tibble preprocesado
test_all_df <- read_delim(
    "./data/preprocess/test_all.csv",
    delim = ",", show_col_types = FALSE
)

# Eliminamos aquellos usuarios que no han hecho uno de los test
test_df <- test_all_df %>%
    group_by(User) %>%
    mutate(Rows = n()) %>%
    filter(Rows > 1) %>%
    ungroup()

##### SAVE TO FILE #####
write_csv(test_df, "./data/preprocess/test.csv")

df <- test_df %>%
    mutate(
        Period = as.factor(
            if_else(Test == "01", 1, 2)
        ),
        Treat = as.factor(
            if_else(Group == "A" & Test == "01" | Group == "B" & Test == "02", "A", "B")
        ),
        Seq = as.factor(
            if_else(Group == "A", "AB", "BA")
        ),
        Subject = as.factor(User)
    ) %>%
    dplyr::select(
        Seq, Period, Treat, Subject,
        gender, year_of_birth, level_of_education, level_of_knowledge, starts_with("Q")
    ) %>%
    mutate_at(
        vars(starts_with("Q")), ~ (. + 1) %% 6
    ) %>%
    pivot_longer(
        cols = all_of(starts_with("Q")),
        names_to = "Question",
        values_to = "Response"
    ) %>%
    mutate(
        Question = relevel(as.factor(Question), ref = "Q18"),
        Response = factor(Response, ordered = TRUE)
    ) %>%
    arrange(Subject, Period, Question)

response_labels <- c(
    "No sé / No contesto",
    "Muy en desacuerdo",
    "En desacuerdo",
    "Neutral",
    "De acuerdo",
    "Muy de acuerdo"
)

question_labels <- c(
    "Los subtítulos del vídeo cumplen en general con los requisitos de accesibilidad.",
    "La posición de los subtítulos.",
    "El número de líneas por subtítulo.",
    "La disposición del texto respecto a la caja donde se muestran los subtítulos.",
    "El contraste entre los caracteres y el fondo.",
    "La corrección ortográfica y gramatical.",
    "La literalidad.",
    "La identificación de los personajes.",
    "La asignación de líneas a los personajes en los diálogos.",
    "La descripción de efectos sonoros.",
    "La sincronización de las entradas y salidas de los subtítulos.",
    "La velocidad de exposición de los subtítulos.",
    "El máximo número de caracteres por línea.",
    "La legibilidad de la tipografía.",
    "La separación en líneas diferentes de sintagmas nominales, verbales y preposicionales.",
    "La utilización de puntos suspensivos.",
    "La escritura de los números.",
    "Las incorrecciones en el habla."
)

question_labels_reduced <- c(
    "Valoración general",
    "Posición",
    "Número líneas",
    "Texto dentro caja",
    "Contraste",
    "Corrección",
    "Literalidad",
    "Identificación personajes",
    "Líneas/personajes",
    "Efectos sonoros",
    "Sincronización",
    "Velocidad",
    "Caracteres x línea",
    "Tipografía",
    "Separación sintagmas",
    "Puntos suspensivos",
    "Escritura números",
    "Incorrecciones habla"
)

df <- df %>% mutate(
    Response_v = as.numeric(Response) - 1,
    Response_l = ordered(Response_v, labels = response_labels),
    Question_l = factor(Question, labels = question_labels),
    Question_lr = factor(Question, labels = question_labels_reduced)
)

dist <- df %>%
    xtabs(~ Question + Response, data = .) %>%
    dist(x = ., method = "euclidean")

cluster <- hclust(dist, method = "complete")
cuts <- factor(cutree(cluster, k = 3))

cuts <- c(1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)

# Añadimos la columna cluster al dataframe
df <- inner_join(
    df,
    data.frame(
        Question = factor(levels(df$Question),
            levels = levels(df$Question)
        ),
        Cluster = as.factor(cuts)
    ),
    by = "Question"
)

write_csv(df, "./data/preprocess/test_lg.csv")
df_all <- df

df_all$Y <- model.matrix(~ Response - 1, data = df_all)

df_clean <- df %>% filter(Response != 0)
df_clean <- df_clean %>% mutate(
    Response = factor(Response, levels = levels(Response)[-1]),
    Response_l = ordered(Response_l, levels = levels(Response_l)[-1]),
    Level = as.ordered(
        ifelse(
            Response %in% c(1, 2),
            "Negative",
            ifelse(
                Response %in% c(4, 5),
                "Positive",
                "Neutral"
            )
        )
    )
)
df_0 <- df %>% filter(Response == 0)

df_improve <- df_clean %>%
    pivot_wider(id_cols = c(Subject, Question, Seq), names_from = Treat, values_from = Response) %>%
    filter(A != 0 & A != 3 & B != 0 & B != 3) %>%
    mutate(
        Improve = A > B, Improve_level = (A %in% c(4, 5)) & (B %in% c(1, 2)),
        Worse_level = (A %in% c(1, 2)) & (B %in% c(4, 5))
    )
