library(readr)
library(purrr)
library(dplyr)
library(magrittr)
library(stringr)
library(forcats)
library(testit)
library(tidyr)

##### GRADE #####
## Usuarios que no quieren participar
no_want_users <- read_lines("data/original/ids_a_eliminar.txt")

# Leemos todos los archivos de grade CSV
grade_files <- list.files("data/original", pattern = ".*grade.*.csv", full.names = TRUE)
grade_df <- map_dfr(
    grade_files, ~ read_delim(.x, delim = ";", show_col_types = FALSE) %>%
        mutate(Userid = row_number() + 1) %>% # Añadimos el número de fila para mantener la trazabilidad
        relocate(Userid, .before = 2) %>% # Movemos las columnas de identificación de fila a la primera posición
        rename_with(~ str_to_title(.), everything()) %>% # Renombramos las columnas para que empiecen con mayúsculas
        rename("Cohort" = "Cohort Name") %>% # Renombramos para que sea más fácil procesar el campo Cohort Name
        filter(!is.na(Cohort) & !Username %in% no_want_users) # Eliminamos valores nulos y los que no quieren participar
)

assert("Comprobamos que no hay usuarios duplicados", grade_df %>% nrow() == grade_df %>%
    distinct(Username) %>%
    nrow())

# Creamos un tibble que tiene un campo con letras en lugar del valor de Cohorte
(groups <- grade_df %>%
    distinct(Cohort) %>%
    arrange(Cohort) %>%
    mutate(Group = LETTERS[1:n()]))

# Unimos los tibbles para asignar en grupo como letra en lugar de la cohorte
grade_df <- left_join(grade_df, groups) %>% dplyr::select(Username, Userid, Group)

##### PROFILE #####
profile_files <- list.files("data/original", pattern = ".*student_profile.*.csv", full.names = TRUE)
profile_df <- map_dfr(
    profile_files, ~ read_delim(.x, delim = ";", show_col_types = FALSE)
)

grade_df <- left_join(grade_df, profile_df %>% dplyr::select(-cohort), by = join_by(Username == username))


##### CONOC #####
conoc_files <- list.files("data/original", pattern = ".*conoc.*.csv", full.names = TRUE)
conoc_df <- map_dfr(
    conoc_files, ~ read_delim(.x, delim = ";", show_col_types = FALSE)
)

conoc_df <- conoc_df %>%
    filter(Tries == 1) %>%
    rowwise() %>%
    mutate(level_of_knowledge = sum(c_across(starts_with(paste("Q", 1:10, "C", sep = ""))) == "correct")) %>%
    dplyr::select(User, level_of_knowledge)

grade_df <- left_join(grade_df, conoc_df, by = join_by(Username == User))

##### TEST #####
# Leemos todos los archivos de test CSV
test_files <- list.files("data/original", pattern = ".*test.*.csv", full.names = TRUE)

# Leer todos los archivos de test y los combinamos en un dataframe
test_df <- map_dfr(
    test_files, ~ read_delim(.x, delim = ";", show_col_types = FALSE) %>%
        mutate(Row = row_number() + 1) %>% # Añadimos un número de fila para mantener la trazabilidad
        mutate(Test = sprintf("%02d", as.integer(str_extract(.x, "(?<=test)\\d+")))) %>% # Añadimos la columna del número de test
        relocate(c(Test, Row), .before = 2) # Movemos las columnas de identificación de test y fila a la primera posición
) %>% filter(!User %in% no_want_users) # eliminamos los usuarios que no quieren participar

num_questions <- 18
questions_original <- paste("Q", seq(from = 1, by = 2, length.out = num_questions), "R", sep = "") # Nombre de los campos que contienen las respuestas al test
comments_original <- paste("Q", seq(from = 2, by = 2, length.out = num_questions - 1), "R", sep = "") # Nombre de los campos que contienen las respuestas al test

questions <- sprintf("Q%02d", seq(from = 1, by = 1, length.out = num_questions)) # Nombre de los campos que se usarán para renombrar los campos de respuesta al test
comments <- sprintf("C%02d", seq(from = 1, by = 1, length.out = num_questions - 1))
columns <- c("Row", "Test", "User", "LastTry", questions_original, comments_original)

# Procesamos el dataframe
test_df %<>% # Con este operador del paquete magrittr hacemos las transformaciones in situ
    filter(Tries > 0) %>% # Eliminamos las filas que no contienen información
    mutate(LastTry = strptime(LastTry, format = "%Y-%m-%dT%H:%M:%SZ")) %>% # Convertimos LastTry a formato fecha
    dplyr::select(all_of(columns)) %>% # Seleccionamos las columnas que nos interesan
    mutate(across(questions_original, ~ if_else(startsWith(.x, "choice_"), as.integer(str_extract(.x, "\\d+")), NA_integer_))) %>% # Extraemos la puntuación númerica de la pregunta
    rename(setNames(questions_original, questions), setNames(comments_original, comments)) %>% # Renombramos los respuestas para que sean secuenciales
    arrange("Test", "Row") # nos aseguramos de que el orden filas es el mismo que el de los ficheros.

# Guardamos el número de filas para posterior comprobación
n_test <- test_df %>% nrow()

# Unimos los dataframes para tener el grupo y el UserID secuencial
test_df <- inner_join(test_df, grade_df, by = join_by(User == Username)) %>% relocate(Group, .before = 2)

# Cambiamos los valores del campo User por los del UserID
test_df %<>%
    mutate(User = Userid) %>%
    dplyr::select(-Userid) %>%
    arrange(User, Test) # Ordenamos por usuario y test


##### CHECKS #####
assert("Comprobamos que no hay preguntas duplicadas en el dataframe de test", n_test == test_df %>%
    distinct(Group, Test, User) %>%
    nrow())

assert("Comprobamos que no hay valores nulos", test_df %>% dplyr::select(-c(comments, year_of_birth, gender, level_of_education, level_of_knowledge)) %>% filter(if_any(everything(), is.na)) %>% nrow() == 0)


assert("Comprobamos que no hay respuestas con valores incorrectos", sum(sort(unique(unlist(test_df %>% dplyr::select(all_of(questions))))) == 0:5) == 6)


comments_df <- test_df %>%
    pivot_longer(cols = starts_with(c("Q", "C")), names_to = c(".value", "Question"), names_pattern = "(Q|C)(.*)") %>%
    rename(Response = Q, Comment = C) %>%
    filter(!is.na(Comment) & grepl("[a-zA-Z]", Comment)) %>%
    dplyr::select(Test, Row, Group, User, Question, Response, Comment) %>%
    arrange(Test, Group, Response, Row)


write_csv(comments_df, "./data/preprocess/comments.csv")

##### SAVE TO FILE #####

write_csv(test_df %>% dplyr::select(-all_of(comments)), "./data/preprocess/test.csv")
