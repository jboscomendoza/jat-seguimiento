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
  "sexo" = 8,
  "edad" = 12,
  "estado" = 13,
  "generacion_01" = 14,
  "generacion_02" = 17,
  "generacion_03" = 19,
  "generacion_04" = 21,
  "nivel_estudios" = 23,
  "estatus_ocupacional" = 24,
  "quiere_seguir_estudiando" = 29,
  "nivel_estudiando_solo" = 31,
  "institucion_estudiando_solo" = 32,
  "nivel_estudiando_trabajo" = 33,
  "institucion_estudiando_trabajo" = 34,
  "l1_motivacion_estudiar" = 39,
  "l2_seguridad_decisiones" = 40,
  "l3_habilidades_retos" = 41,
  "l4_red_apoyo" = 42,
  "l5_obstaculos_superados" = 43,
  "l6_vision_proyecto_vida" = 44,
  "l7_integracion_espacios" = 45,
  "l8_responsabilidades_comunidad" = 46,
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
  "participacion_red" = 62,
  "expectativas_red" = 63,
  "mas_valioso" = 64,
  "habilidades" = 65,
  "voluntariado" = 66
)

# Likert variables
likert_map <- c(
  "Totalmente en desacuerdo" = 1,
  "En desacuerdo" = 2,
  "De acuerdo" = 3,
  "Totalmente de acuerdo" = 4
)

likert_cols <- c(
  "l1_motivacion_estudiar",
  "l2_seguridad_decisiones",
  "l3_habilidades_retos",
  "l4_red_apoyo",
  "l5_obstaculos_superados",
  "l6_vision_proyecto_vida",
  "l7_integracion_espacios",
  "l8_responsabilidades_comunidad"
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
    "habilidades sociales" = c(
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

omitted_cols <- c(
  "collector_id",
  "date_created",
  "date_modified",
  "ip_address",
  "email_address",
  "first_name",
  "last_name",
  "custom_1",
  "¿Aceptas participar en esta encuesta de seguimiento?"
)

text_cols = list("val" = "mas_valioso", "skill" = "habilidades")

# Group columns
group_cols <- list(
  "General" = "general",
  "Estado" = "estado",
  "Género" = "sexo",
  "Estado y género" = c("estado", "sexo")
)

# Load data
df_raw <- read_excel("final_seguimiento_JAT_Jan_2026_-_anon.xlsx")

df_recode <- df_raw %>%
  # Rename key columns (column indices from Excel)
  rename(all_of(key_renames)) %>%
  # Create derived variables
  mutate(
    sexo = case_when(
      sexo == "M" ~ "Mujer",
      sexo == "H" ~ "Hombre",
      TRUE ~ NA_character_
    ),
    continua_estudiando = as.integer(
      estatus_ocupacional %in% c("Estudiando", "Estudiando y trabajando")
    ),
    nivel_actual_estudios = coalesce(
      nivel_estudiando_solo,
      nivel_estudiando_trabajo
    ),
    institucion_actual = coalesce(
      institucion_estudiando_solo,
      institucion_estudiando_trabajo
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
    likert_total = rowMeans(select(., ends_with("_num")), na.rm = TRUE),
    satisfaction_index = rowMeans(
      select(., ends_with("_num"), -starts_with("l5")),
      na.rm = TRUE
    )
  ) %>%
  # Generation
  mutate(generacion = coalesce(generacion_01, generacion_02, generacion_03, generacion_04)) %>% 
  select(-matches("^generacion.*?_")) %>% 
  mutate(periodo = str_extract(generacion, "\\(.*") %>% 
  str_remove_all("\\D") %>% str_replace("(^\\d{4})", "\\1-")) %>% 
  mutate(periodo = ifelse(periodo == "", "Otro", periodo)) %>% 
  # Column for general grouping
  mutate(general = "General") %>% 
  select(
    -all_of(omitted_cols),
    -starts_with("...")
  )

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
prop.table(table(df_jat$sexo)) *
  100

# State distribution
table(df_jat$estado)

# Age statistics
summary(df_jat$edad)
sd(df_jat$edad, na.rm = TRUE)

# Cross-tabulation: State x Gender
table(df_jat$estado, df_jat$sexo)

# Age by State and Gender
df_jat %>%
  group_by(estado, sexo) %>%
  summarise(
    mean_age = mean(edad, na.rm = TRUE),
    sd_age = sd(edad, na.rm = TRUE),
    n = n()
  )

# Education level
table(df_jat$nivel_estudios)

# Occupational status by Gender
table(df_jat$estatus_ocupacional, df_jat$sexo)

# ============================================================================
# 3. EDUCATIONAL TRAJECTORIES
# ============================================================================

# Overall continuation rate
sum(df_jat$continua_estudiando) / nrow(df_jat) * 100

# By State and Gender
map(group_cols, function(group_col) {
  df_jat %>%
    group_by(across(group_col)) %>%
    summarise(
      n_studying = sum(continua_estudiando),
      n_total = n(),
      pct = mean(continua_estudiando) * 100
    )
  df_jat %>%
    group_by(across(all_of(group_col))) %>%
    filter(continua_estudiando == 1) %>%
    count(nivel_actual_estudios, name = "n_total") %>%
    mutate(pct = n_total / sum(n_total) * 100)
})

# Study levels among those currently studying
df_jat %>%
  filter(continua_estudiando == 1) %>%
  count(nivel_actual_estudios)

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

 df_jat %>% 
  select(respondent_id, matches("^l\\d.*?(_num)")) %>% 
  pivot_longer(cols = starts_with("l")) %>% 
  na.omit() %>% 
  mutate(positivo = value >= 3) %>% 
  group_by(name) %>% 
  count(positivo) %>% 
  mutate(prop = n / sum(n) * 100) %>% 
  filter(positivo) %>% 
  select(-positivo) 

write_parquet(df_likert, "out/df_likert.parquet")

# By State and Gender
map(group_cols, function(group_col) {
  df_jat %>%
    group_by(across(all_of(group_col))) %>%
    summarise(
      l1_mean = mean(l1_motivacion_estudiar_num, na.rm = TRUE),
      l5_mean = mean(l5_obstaculos_superados_num, na.rm = TRUE),
      Composite = mean(likert_total, na.rm = TRUE)
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
        group_by(across(all_of(g_col))) %>%
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
  rename(
    "Mantiene contacto" = 1,
    "Menciones" = 2,
    "Porcentaje de menciones" = 3
  )


write_parquet(df_contact, "out/df_contact.parquet")

# ============================================================================
# END OF ANALYSIS
# ============================================================================