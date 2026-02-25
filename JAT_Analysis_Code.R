###############################################################################
# R CODE FOR JAT FOLLOW-UP SURVEY ANALYSIS
# Jóvenes Atiempo 2025 - Ex-Mentors Follow-up Survey
# Vía Educación
###############################################################################

# Note: This code is equivalent to the Python analysis conducted.
# To run in R, you need: readxl, dplyr, tidyr, psych, stringr packages

# ============================================================================
# 1. SETUP AND DATA LOADING
# ============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(stringr)

# Load data
df <- read_excel("final_seguimiento_JAT_Jan_2026_-_anon.xlsx")

# Rename key columns (column indices from Excel)
colnames(df)[8] <- "Sexo"
colnames(df)[12] <- "Edad"
colnames(df)[13] <- "Estado"
colnames(df)[23] <- "Nivel_estudios"
colnames(df)[24] <- "Estatus_ocupacional"
colnames(df)[29] <- "Quiere_seguir_estudiando"
colnames(df)[31] <- "Nivel_estudiando_solo"
colnames(df)[32] <- "Institucion_estudiando_solo"
colnames(df)[33] <- "Nivel_estudiando_trabajo"
colnames(df)[34] <- "Institucion_estudiando_trabajo"

# Likert columns (indices 39-46)
colnames(df)[39] <- "L1_motivacion_estudiar"
colnames(df)[40] <- "L2_seguridad_decisiones"
colnames(df)[41] <- "L3_habilidades_retos"
colnames(df)[42] <- "L4_red_apoyo"
colnames(df)[43] <- "L5_obstaculos_superados"
colnames(df)[44] <- "L6_vision_proyecto_vida"
colnames(df)[45] <- "L7_integracion_espacios"
colnames(df)[46] <- "L8_responsabilidades_comunidad"

# Open-ended questions
colnames(df)[63] <- "Expectativas_Red"
colnames(df)[64] <- "Mas_valioso"
colnames(df)[65] <- "Habilidades"
colnames(df)[66] <- "Voluntariado"

# Create derived variables
df <- df %>%
  mutate(
    Sexo_label = case_when(
      Sexo == "M" ~ "Mujer",
      Sexo == "H" ~ "Hombre",
      TRUE ~ NA_character_
    ),
    Continua_estudiando = as.integer(Estatus_ocupacional %in% c("Estudiando", "Estudiando y trabajando")),
    Nivel_actual_estudios = coalesce(Nivel_estudiando_solo, Nivel_estudiando_trabajo),
    Institucion_actual = coalesce(Institucion_estudiando_solo, Institucion_estudiando_trabajo)
  )

# ============================================================================
# 2. DEMOGRAPHICS
# ============================================================================

# Total N
cat("Total respondents:", nrow(df), "\n")

# Gender distribution
table(df$Sexo_label)
prop.table(table(df$Sexo_label)) * 100

# State distribution
table(df$Estado)

# Age statistics
summary(df$Edad)
sd(df$Edad, na.rm = TRUE)

# Cross-tabulation: State x Gender
table(df$Estado, df$Sexo_label)

# Age by State and Gender
df %>%
  group_by(Estado, Sexo_label) %>%
  summarise(
    mean_age = mean(Edad, na.rm = TRUE),
    sd_age = sd(Edad, na.rm = TRUE),
    n = n()
  )

# Education level
table(df$Nivel_estudios)

# Occupational status by Gender
table(df$Estatus_ocupacional, df$Sexo_label)

# ============================================================================
# 3. EDUCATIONAL TRAJECTORIES
# ============================================================================

# Overall continuation rate
sum(df$Continua_estudiando) / nrow(df) * 100

# By State
df %>%
  group_by(Estado) %>%
  summarise(
    n_studying = sum(Continua_estudiando),
    n_total = n(),
    pct = mean(Continua_estudiando) * 100
  )

# By Gender
df %>%
  group_by(Sexo_label) %>%
  summarise(
    n_studying = sum(Continua_estudiando),
    n_total = n(),
    pct = mean(Continua_estudiando) * 100
  )

# By State x Gender
df %>%
  group_by(Estado, Sexo_label) %>%
  summarise(
    n_studying = sum(Continua_estudiando),
    n_total = n(),
    pct = mean(Continua_estudiando) * 100
  )

# Study levels among those currently studying
df %>%
  filter(Continua_estudiando == 1) %>%
  count(Nivel_actual_estudios)

# ============================================================================
# 4. LIKERT SCALE ANALYSIS
# ============================================================================

# Convert Likert to numeric
likert_map <- c(
  "Totalmente en desacuerdo" = 1,
  "En desacuerdo" = 2,
  "De acuerdo" = 3,
  "Totalmente de acuerdo" = 4
)

likert_cols <- c("L1_motivacion_estudiar", "L2_seguridad_decisiones", 
                 "L3_habilidades_retos", "L4_red_apoyo", 
                 "L5_obstaculos_superados", "L6_vision_proyecto_vida",
                 "L7_integracion_espacios", "L8_responsabilidades_comunidad")

for (col in likert_cols) {
  df[[paste0(col, "_num")]] <- likert_map[df[[col]]]
}

# Cronbach's Alpha
likert_numeric <- df %>%
  select(ends_with("_num")) %>%
  select(starts_with("L"))

alpha_result <- psych::alpha(likert_numeric, check.keys = TRUE)
print(alpha_result)

# Composite indices
df$Likert_total <- rowMeans(likert_numeric, na.rm = TRUE)

satisfaction_cols <- paste0(likert_cols[!grepl("L5", likert_cols)], "_num")
df$Satisfaction_index <- rowMeans(df[satisfaction_cols], na.rm = TRUE)

# Item statistics
for (col in likert_cols) {
  col_num <- paste0(col, "_num")
  cat("\n", col, ":\n")
  cat("  Mean:", mean(df[[col_num]], na.rm = TRUE), "\n")
  cat("  SD:", sd(df[[col_num]], na.rm = TRUE), "\n")
  positive <- sum(df[[col]] %in% c("De acuerdo", "Totalmente de acuerdo"), na.rm = TRUE)
  total <- sum(!is.na(df[[col]]))
  cat("  % Positive:", positive / total * 100, "\n")
}

# By State
df %>%
  group_by(Estado) %>%
  summarise(
    L1_mean = mean(L1_motivacion_estudiar_num, na.rm = TRUE),
    L5_mean = mean(L5_obstaculos_superados_num, na.rm = TRUE),
    Composite = mean(Likert_total, na.rm = TRUE)
  )

# By Gender
df %>%
  group_by(Sexo_label) %>%
  summarise(
    L1_mean = mean(L1_motivacion_estudiar_num, na.rm = TRUE),
    L5_mean = mean(L5_obstaculos_superados_num, na.rm = TRUE),
    Composite = mean(Likert_total, na.rm = TRUE)
  )

# ============================================================================
# 5. QUALITATIVE CONTENT ANALYSIS
# ============================================================================

# Define coding function
code_responses <- function(text, keywords) {
  pattern <- paste(keywords, collapse = "|")
  as.integer(grepl(pattern, tolower(text), perl = TRUE))
}

# "Lo más valioso" categories
valioso_categories <- list(
  "Amistades/Relaciones" = c("amig", "amistad", "compañer", "personas", "convivencia", "convivir"),
  "Mentitos/Mentoría" = c("mentito", "mentoria", "mentoría"),
  "Habilidades sociales" = c("habilidad", "social", "comunicación", "expresar"),
  "Liderazgo" = c("líder", "liderazgo", "responsabilidad"),
  "Aprendizajes" = c("aprender", "enseñanza", "aprendizaje"),
  "Experiencia general" = c("experiencia", "tiempo", "momentos", "todo")
)

for (cat_name in names(valioso_categories)) {
  df[[paste0("val_", cat_name)]] <- sapply(
    df$Mas_valioso,
    function(x) code_responses(x, valioso_categories[[cat_name]])
  )
}

# Skills categories
skill_categories <- list(
  "Liderazgo" = c("liderazgo", "líder", "liderar"),
  "Comunicación" = c("comunicación", "hablar", "expresar"),
  "Trabajo en equipo" = c("equipo", "trabajo en equipo"),
  "Responsabilidad" = c("responsabilidad", "responsable"),
  "Empatía" = c("empatía", "empatia", "comprensión")
)

for (cat_name in names(skill_categories)) {
  df[[paste0("skill_", cat_name)]] <- sapply(
    df$Habilidades,
    function(x) code_responses(x, skill_categories[[cat_name]])
  )
}

# Summarize qualitative results by group
df %>%
  filter(!is.na(Mas_valioso)) %>%
  group_by(Estado) %>%
  summarise(
    n = n(),
    pct_amistades = mean(`val_Amistades/Relaciones`) * 100,
    pct_mentitos = mean(`val_Mentitos/Mentoría`) * 100
  )

df %>%
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
