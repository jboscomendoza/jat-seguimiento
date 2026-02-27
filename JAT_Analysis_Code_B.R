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
library(arrow)

# Define coding function
code_responses <- function(text, keywords) {
  pattern <- paste(keywords, collapse = "|")
  pattern_match <- as.numeric(grepl(pattern, tolower(text), perl = TRUE))
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
  "contacto_01" = 47,
  "contacto_02" = 48,
  "contacto_03" = 49,
  "contacto_04" = 50,
  "contacto_05" = 51,
  "contacto_06" = 52,
  "contacto_07" = 53,
  "componente_ayuda_01" = 54,
  "componente_ayuda_02" = 55,
  "componente_ayuda_03" = 56,
  "componente_ayuda_04" = 57,
  "componente_ayuda_05" = 58,
  "componente_ayuda_06" = 59,
  "componente_ayuda_07" = 60,
  "componente_ayuda_08" = 61,
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

# Group columns
group_cols <- list(
  "General" = "General",
  "Estado" = "Estado",
  "Género" = "Sexo_label",
  "Estado y género" = c("Estado", "Sexo_label")
)

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
  ) %>%
  # Column for general grouping
  mutate(General = "General")

# Dataframe for qualitative analysis
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

# Joint dataframe
df_jat <- bind_cols(df_recode, text_df)

# Write dataframe
arrow::write_parquet(df_jat, "out/df_jat.parquet")

# ============================================================================
# 2. DEMOGRAPHICS
# ============================================================================

# Total N
cat("Total respondents:", nrow(df_jat), "\n")

# Gender distribution
prop.table(table(df_jat$Sexo_label)) *
  100

# State distribution
table(df_jat$Estado)

# Age statistics
summary(df_jat$Edad)
sd(df_jat$Edad, na.rm = TRUE)

# Cross-tabulation: State x Gender
table(df_jat$Estado, df_jat$Sexo_label)

# Age by State and Gender
df_jat %>%
  group_by(Estado, Sexo_label) %>%
  summarise(
    mean_age = mean(Edad, na.rm = TRUE),
    sd_age = sd(Edad, na.rm = TRUE),
    n = n()
  )

# Education level
table(df_jat$Nivel_estudios)

# Occupational status by Gender
table(df_jat$Estatus_ocupacional, df_jat$Sexo_label)

# ============================================================================
# 3. EDUCATIONAL TRAJECTORIES
# ============================================================================

# Overall continuation rate
sum(df_jat$Continua_estudiando) / nrow(df_jat) * 100

# By State and Gender
map(group_cols, function(group_col) {
  df_jat %>%
    group_by(across(group_col)) %>%
    summarise(
      n_studying = sum(Continua_estudiando),
      n_total = n(),
      pct = mean(Continua_estudiando) * 100
    )
  df_jat %>%
    group_by(across(group_col)) %>%
    filter(Continua_estudiando == 1) %>%
    count(Nivel_actual_estudios, name = "n_total") %>%
    mutate(pct = n_total / sum(n_total) * 100)
})

# Study levels among those currently studying
df_jat %>%
  filter(Continua_estudiando == 1) %>%
  count(Nivel_actual_estudios)

# ============================================================================
# 4. LIKERT SCALE ANALYSIS
# ============================================================================

likert_numeric <- select(df_jat, ends_with("_num"))

# Cronbach's Alpha
alpha_result <- psych::alpha(likert_numeric, check.keys = TRUE)
alpha_result$total$raw_alpha

# Item statistics
df_likert <- paste0(likert_cols, "_num") %>%
  map_df(function(col_num) {
    tibble(
      item = str_remove(col_num, "_num"),
      mean = mean(df_jat[[col_num]], na.rm = TRUE),
      sd = sd(df_jat[[col_num]], na.rm = TRUE),
      p_positive = sum(df_jat[[col_num]] >= 3, na.rm = TRUE) /
        sum(!is.na(df_jat[[col_num]]), na.rm = TRUE) *
        100
    )
  })

write_parquet(df_likert, "out/df_likert.parquet")

# By State and Gender
map(group_cols, function(group_col) {
  df_jat %>%
    group_by(across(group_col)) %>%
    summarise(
      L1_mean = mean(L1_motivacion_estudiar_num, na.rm = TRUE),
      L5_mean = mean(L5_obstaculos_superados_num, na.rm = TRUE),
      Composite = mean(Likert_total, na.rm = TRUE)
    )
})

# ============================================================================
# 5. QUALITATIVE CONTENT ANALYSIS
# ============================================================================

# Summarize qualitative results by group
ls_qualitative <-
  map(names(text_cols), function(t_col) {
    map(group_cols, function(g_col) {
      suffix <- paste0("_", t_col)
      df_jat %>%
        filter(!is.na(t_col)) %>%
        group_by(across(g_col)) %>%
        summarise(
          N = n(),
          across(ends_with(suffix), ~ mean(.x, na.rm = TRUE) * 100)
        ) %>%
        ungroup()
    })
  }) %>%
  set_names(names(text_cols))

write_rds(ls_qualitative, "out/ls_qualitative.rds")


# ========
# 6. Program components
# ========

responses <- nrow(df_jat)

df_components <- 
  df_jat %>%
  select(c("respondent_id"), starts_with("componente_ayuda")) %>%
  pivot_longer(
    cols = starts_with("componente_ayuda"),
    names_to = "name",
    values_to = "componente"
  ) %>%
  na.omit() %>%
  count(componente, sort = TRUE) %>%
  mutate(prop = n / responses * 100) %>% 
  rename("Componente" = 1, "Menciones" = 2, "Porcentaje de menciones" = 3)

write_parquet(df_components, "out/df_components.parquet")

# ========
# 7. Maintained contact
# ========

df_contact <- 
  df_jat %>%
  select(c("respondent_id"), starts_with("contacto")) %>%
  mutate(contacto_07 = ifelse(is.na(contacto_07), contacto_07, "Con otros")) %>% 
  pivot_longer(
    cols = starts_with("contacto"),
    names_to = "name",
    values_to = "contacto"
  ) %>%
  na.omit() %>%
  count(contacto, sort = TRUE) %>%
  mutate(prop = n / responses * 100) %>% 
  rename("Mantiene contacto" = 1, "Menciones" = 2, "Porcentaje de menciones" = 3)


write_parquet(df_contact, "out/df_contact.parquet")

# ============================================================================
# END OF ANALYSIS
# ============================================================================
