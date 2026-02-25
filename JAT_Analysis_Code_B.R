###############################################################################
# R CODE FOR JAT FOLLOW-UP SURVEY ANALYSIS
# Jóvenes Atiempo 2025 - Ex-Mentors Follow-up Survey
# Vía Educación
###############################################################################

# Note: This code is equivalent to the Python analysis conducted.
# To run in R, you need: tidyverse, readxl, psych packages

# ============================================================================
# 1. SETUP AND DATA LOADING
# ============================================================================
library(tidyverse)
library(readxl)
library(psych)

# Define coding function
code_responses <- function(text, keywords) {
  pattern <- paste(keywords, collapse = "|")
  pattern_match <- as.numeric(str_detect(pattern, tolower(text)))
  return(pattern_match)
}

# Keys for renaming
# Likert columns (indices 39-46)
# Open-ended questions (indices 63-66)
key_renames <- c(
  "Sexo" = 8,
  "Edad" = 12,
  "Estado" = 13,
  "Nivel_estudios" = 23,
  "Estatus_ocupacional" = 24,
  "Quiere_seguir_estudiando" = 29,
  "Nivel_estudiando_solo" = 31,
  "Institucion_estudiando_solo" = 32,
  "Nivel_estudiando_trabajo" = 33,
  "Institucion_estudiando_trabajo" = 34,
  "L1_motivacion_estudiar" = 39,
  "L2_seguridad_decisiones" = 40,
  "L3_habilidades_retos" = 41,
  "L4_red_apoyo" = 42,
  "L5_obstaculos_superados" = 43,
  "L6_vision_proyecto_vida" = 44,
  "L7_integracion_espacios" = 45,
  "L8_responsabilidades_comunidad" = 46,
  "Expectativas_Red" = 63,
  "Mas_valioso" = 64,
  "Habilidades" = 65,
  "Voluntariado" = 66
)

# Likert variables
likert_map <- c(
  "Totalmente en desacuerdo" = 1,
  "En desacuerdo" = 2,
  "De acuerdo" = 3,
  "Totalmente de acuerdo" = 4
)

likert_cols <- c(
  "L1_motivacion_estudiar",
  "L2_seguridad_decisiones",
  "L3_habilidades_retos",
  "L4_red_apoyo",
  "L5_obstaculos_superados",
  "L6_vision_proyecto_vida",
  "L7_integracion_espacios",
  "L8_responsabilidades_comunidad"
)

# Text analysis variables
text_categories <- list(
  "val" = list(
    "Amistades/Relaciones" = c(
      "amig",
      "amistad",
      "compañer",
      "personas",
      "convivencia",
      "convivir"
    ),
    "Mentitos/Mentoría" = c("mentito", "mentoria", "mentoría"),
    "Habilidades sociales" = c(
      "habilidad",
      "social",
      "comunicación",
      "expresar"
    ),
    "Liderazgo" = c("líder", "liderazgo", "responsabilidad"),
    "Aprendizajes" = c("aprender", "enseñanza", "aprendizaje"),
    "Experiencia general" = c("experiencia", "tiempo", "momentos", "todo")
  ),
  "skill" = list(
    "Liderazgo" = c("liderazgo", "líder", "liderar"),
    "Comunicación" = c("comunicación", "hablar", "expresar"),
    "Trabajo en equipo" = c("equipo", "trabajo en equipo"),
    "Responsabilidad" = c("responsabilidad", "responsable"),
    "Empatía" = c("empatía", "empatia", "comprensión")
  )
)

text_cols = list("val" = "Mas_valioso", "skill" = "Habilidades")

# Load data
df_raw <- read_excel("final_seguimiento_JAT_Jan_2026_-_anon.xlsx")

df_recode <- df_raw %>%
  # Rename key columns (column indices from Excel)
  rename(all_of(key_renames)) %>%
  # Create derived variables
  mutate(
    Sexo_label = case_when(
      Sexo == "M" ~ "Mujer",
      Sexo == "H" ~ "Hombre",
      TRUE ~ NA_character_
    ),
    Continua_estudiando = as.integer(
      Estatus_ocupacional %in% c("Estudiando", "Estudiando y trabajando")
    ),
    Nivel_actual_estudios = coalesce(
      Nivel_estudiando_solo,
      Nivel_estudiando_trabajo
    ),
    Institucion_actual = coalesce(
      Institucion_estudiando_solo,
      Institucion_estudiando_trabajo
    )
  ) %>%
  # Convert Likert to numeric
  mutate(across(
    all_of(likert_cols),
    ~ case_when(
      . == "Totalmente en desacuerdo" ~ 1,
      . == "En desacuerdo" ~ 2,
      . == "De acuerdo" ~ 3,
      . == "Totalmente de acuerdo" ~ 4
    ),
    .names = "{.col}_num"
  )) %>%
  # Composite indices
  mutate(
    Likert_total = rowMeans(select(., ends_with("_num")), na.rm = TRUE),
    Satisfaction_index = rowMeans(
      select(., ends_with("_num"), -starts_with("L5")),
      na.rm = TRUE
    )
  )

# ============================================================================
# 2. DEMOGRAPHICS
# ============================================================================

# Total N
cat("Total respondents:", nrow(df_recode), "\n")

# Gender distribution
table(df_recode$Sexo_label) %>%
  prop.table(table(df_recode$Sexo_label)) *
  100

# State distribution
table(df_recode$Estado)

# Age statistics
summary(df_recode$Edad)
sd(df_recode$Edad, na.rm = TRUE)

# Cross-tabulation: State x Gender
table(df_recode$Estado, df_recode$Sexo_label)

# Age by State and Gender
df_recode %>%
  group_by(Estado, Sexo_label) %>%
  summarise(
    mean_age = mean(Edad, na.rm = TRUE),
    sd_age = sd(Edad, na.rm = TRUE),
    n = n()
  )

# Education level
table(df_recode$Nivel_estudios)

# Occupational status by Gender
table(df_recode$Estatus_ocupacional, df_recode$Sexo_label)

# ============================================================================
# 3. EDUCATIONAL TRAJECTORIES
# ============================================================================

# Overall continuation rate
sum(df_recode$Continua_estudiando) / nrow(df_recode) * 100

# By State and Gender
list(
  "Estado" = "Estado",
  "Género" = "Sexo_label",
  "Estado y género" = c("Estado", "Sexo_label")
) %>%
  map(function(group_col) {
    df_recode %>%
      group_by(across(group_col)) %>%
      summarise(
        n_studying = sum(Continua_estudiando),
        n_total = n(),
        pct = mean(Continua_estudiando) * 100
      )
    df_recode %>%
      group_by(across(group_col)) %>% 
      filter(Continua_estudiando == 1) %>%
      count(Nivel_actual_estudios, name = "n_total") %>% 
      mutate(pct = n_total / sum(n_total) * 100) 
  })

# Study levels among those currently studying
df_recode %>%
  filter(Continua_estudiando == 1) %>%
  count(Nivel_actual_estudios)

# ============================================================================
# 4. LIKERT SCALE ANALYSIS
# ============================================================================

likert_numeric <- select(df_recode, ends_with("_num"))

# Cronbach's Alpha
alpha_result <- psych::alpha(likert_numeric, check.keys = TRUE)
alpha_result$total$raw_alpha

psych::describe(likert_numeric)

# Item statistics
paste0(likert_cols, "_num") %>%
  map_df(function(col_num) {
    tibble(
      item = str_remove(col_num, "_num"),
      mean = mean(df_recode[[col_num]], na.rm = TRUE),
      sd = sd(df_recode[[col_num]], na.rm = TRUE),
      p_positive = sum(df_recode[[col_num]] >= 3, na.rm = TRUE) /
        sum(!is.na(df_recode[[col_num]]), na.rm = TRUE) *
        100
    )
  })

# By State
df_recode %>%
  group_by(Estado) %>%
  summarise(
    L1_mean = mean(L1_motivacion_estudiar_num, na.rm = TRUE),
    L5_mean = mean(L5_obstaculos_superados_num, na.rm = TRUE),
    Composite = mean(Likert_total, na.rm = TRUE)
  )

# By Gender
df_recode %>%
  group_by(Sexo_label) %>%
  summarise(
    L1_mean = mean(L1_motivacion_estudiar_num, na.rm = TRUE),
    L5_mean = mean(L5_obstaculos_superados_num, na.rm = TRUE),
    Composite = mean(Likert_total, na.rm = TRUE)
  )

# ============================================================================
# 5. QUALITATIVE CONTENT ANALYSIS
# ============================================================================

text_df <- map(names(text_categories), function(cat_group) {
  category <- text_categories[[cat_group]]
  text_col_name <- text_cols[[cat_group]]
  col_names <- paste0(names(category), "_", cat_group)
  map_df(category, function(cat_name) {
    code_responses(df_recode[[text_col_name]], cat_name)
  }) %>%
    set_names(col_names)
}) %>%
  reduce(bind_cols)

df_recode <- bind_cols(text_df, df_recode)


# Skills categories

# Summarize qualitative results by group
df_recode %>%
  filter(!is.na(Mas_valioso)) %>%
  group_by(Estado) %>%
  summarise(
    n = n(),
    pct_amistades = mean(`val_Amistades/Relaciones`) * 100,
    pct_mentitos = mean(`val_Mentitos/Mentoría`) * 100
  )

df_recode %>%
  filter(!is.na(Habilidades)) %>%
  group_by(Sexo_label) %>%
  summarise(
    n = n(),
    pct_liderazgo = mean(skill_Liderazgo) * 100,
    pct_comunicacion = mean(skill_Comunicación) * 100
  )

# ============================================================================
# END OF ANALYSIS
# ============================================================================
