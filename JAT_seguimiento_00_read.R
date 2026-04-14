library(tidyverse)
library(readxl)
library(arrow)

if (!dir.exists("data/parquet")) {
  dir.create("data/parquet", recursive = TRUE)
} else {
  message("data/parquet already exists.")
}

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


# Load data
df_raw <- readxl::read_excel("final_seguimiento_JAT_Jan_2026_-_anon.xlsx")

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
  mutate(
    generacion = coalesce(
      generacion_01,
      generacion_02,
      generacion_03,
      generacion_04
    )
  ) %>%
  select(-matches("^generacion.*?_")) %>%
  mutate(
    periodo = str_extract(generacion, "\\(.*") %>%
      str_remove_all("\\D") %>%
      str_replace("(^\\d{4})", "\\1-")
  ) %>%
  mutate(periodo = ifelse(periodo == "", "Otro", periodo)) %>%
  mutate(periodo = str_replace_all(periodo, "20(\\d{2})", "\\1")) %>%
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
arrow::write_parquet(df_jat, "data/parquet/seguimiento-26.parquet")
