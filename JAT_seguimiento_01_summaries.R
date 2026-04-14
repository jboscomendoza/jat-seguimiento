library(tidyverse)
library(readxl)
library(psych)
library(arrow)

# Setup ----
if (!dir.exists("output/seguimiento")) {
  dir.create("output/seguimiento", recursive = TRUE)
} else {
  message("output/seguimiento already exists.")
}

# Variables ----
grouped_cols <- list(
  "General" = "general",
  "Estado" = "estado",
  "Género" = "sexo",
  "Estado y género" = c("estado", "sexo")
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

text_cols = list("val" = "mas_valioso", "skill" = "habilidades")

# Read ----
df_jat <- arrow::read_parquet("data/parquet/seguimiento-26.parquet")

# Analysis ----
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
map(grouped_cols, function(group_col) {
  df_jat %>%
    group_by(across(all_of(group_col))) %>%
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

write_parquet(df_likert, "output/seguimiento/df_likert.parquet")

# By State and Gender
map(grouped_cols, function(group_col) {
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
    map(grouped_cols, function(g_col) {
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

write_rds(ls_qualitative, "output/seguimiento/ls_qualitative.rds")


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

write_parquet(df_components, "output/seguimiento/df_components.parquet")

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

write_parquet(df_contact, "output/seguimiento/df_contact.parquet")
