library(tidyverse)
library(readxl)
library(broom)
library(arrow)

if (!dir.exists("out_glm")) {
  dir.create("out_glm")
} else {
  message("out_glm already exists.")
}

lb_mentitos <- "abandono/LB mentitos 23-24.xlsx"
lf_mentitos <- "abandono/LF mentitos 23-24.xlsx"

cols_names <- c(
  "id",
  "nombre",
  "sexo",
  "fecha_nac",
  "estado",
  "municipio",
  "trabajo",
  "nivel_educ",
  "socioeco",
  "modelo_seguir"
)
cols_patron <- "^(liderazgo|empatia|decision|equipo)"
na_pattern <- c("Prefiero no contestar", "No contestó")

# Read ---
lb_raw <- read_excel(lb_mentitos) %>%
  select(all_of(cols_names), matches(cols_patron))

lf_m <- read_excel(lf_mentitos) %>%
  select(id, nombre, municipio) %>%
  mutate(id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))))

id_concluyo <- unique(lf_m$id)

# Processing ----

lb_m <- lb_raw %>%
  mutate(
    edad = lubridate::interval(
      lubridate::dmy(fecha_nac),
      lubridate::dmy("6/6/2023")
    ) %>%
      as.numeric("year") %>%
      round(),
    edad_cat = cut(edad, breaks = c(0, 11, 12, 13, 14, 15, 16, 99)),
  ) %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = ifelse(id %in% id_concluyo, "Concluyó", "No concluyó"),
    # edad = ifelse(edad == 99, NA, edad),
    # edad_cat = cut(edad, breaks = c(0, 11, 12, 13, 14, 15, 16, 99)),
    socioeco = ifelse(socioeco == 0, NA, socioeco),
    modelo_seguir = ifelse(str_detect(modelo_seguir, "Sí"), "Sí", "No"),
    municipio = str_to_lower(municipio),
    municipio = case_when(
      str_detect(municipio, "victoria") ~ "Cd. Victoria",
      str_detect(municipio, "santa") ~ "Santa Catarina",
      str_detect(municipio, "ecatepec|presa") ~ "Ecatepec",
      str_detect(municipio, "garc(i|í)a") ~ "García",
      str_detect(municipio, "fama") ~ "Fama",
      str_detect(municipio, "chihuahua") ~ "Chihuahua",
      .default = NA_character_
    ),
    across(
      where(is.character),
      ~ ifelse(.x %in% na_pattern, NA_character_, .x)
    ),
  ) %>%
  mutate() %>%
  select(-c(nombre)) %>%
  distinct(id, .keep_all = TRUE)

# Scores ---
lb_m_scores <- lb_m %>%
  pivot_longer(
    cols = matches(cols_patron),
    names_to = "dim_item",
    values_to = "response"
  ) %>%
  mutate(dim_item = str_replace(dim_item, "(\\d)", "_\\1")) %>%
  separate_wider_delim(
    cols = "dim_item",
    delim = "_",
    names = c("dim", "item")
  ) %>%
  mutate(response = as.numeric(response)) %>%
  group_by(id, dim) %>%
  mutate(score = sum(response, na.rm = TRUE)) %>%
  select(-c("item", "response")) %>%
  distinct() %>%
  ungroup()

lb_m_scores_wide <-
  lb_m_scores %>%
  mutate(
    status = ifelse(status == "Concluyó", 1, 0),
    nivel_educ = case_when(
      nivel_educ == "Secundaria" ~ "0_Sec",
      nivel_educ == "Bachillerato o Preparatoria" ~ "1_Bac",
      nivel_educ == "Licenciatura o Universidad" ~ "2_Lic",
      nivel_educ == "Maestría" ~ "3_Mae",
      nivel_educ == "Doctorado" ~ "4_Doc",
      .default = NA_character_
    ),
    estado = case_when(
      estado == "Nuevo León" ~ "0_NL",
      estado == "Chihuahua" ~ "1_CH",
      estado == "Tamaulipas" ~ "2_TM",
      estado == "Estado de México" ~ "3_MX",
      .default = NA_character_
    )
  ) %>%
  mutate(
    across(where(is.character), factor)
  ) %>%
  pivot_wider(names_from = "dim", values_from = "score") %>%
  mutate(across(
    all_of(c("liderazgo", "empatia", "decision", "equipo")),
    scale
  )) %>%
  select(-c("id", "municipio"))

# glm ----
model_formula <-
  paste(
    "sexo",
    "I(edad-12)",
    "estado",
    "trabajo",
    "nivel_educ",
    "socioeco",
    "modelo_seguir",
    "liderazgo",
    "empatia",
    "decision",
    "equipo",
    sep = " + "
  ) %>%
  paste0("status ~ ", .)

glm_model <- glm(
  formula = model_formula,
  data = lb_m_scores_wide,
  family = binomial(link = 'logit')
)

glm_model_nl <- 
  lb_m_scores_wide %>% 
  filter(estado == "0_NL") %>% 
  glm(
  formula = formula(str_remove(model_formula, "\\+ estado ")),
  data = .,
  family = binomial(link = 'logit')
)

broom::tidy(glm_model) %>%
  mutate(
    sig = ifelse(p.value <= 0.05, "*", ""),
    odds = exp(estimate),
  )
summary(glm_model)
anova(glm_model)

# Exports
write_parquet(lb_m_scores, "out_glm/lb mentitos 23-24.parquet")
write_parquet(lb_m_scores_wide, "out_glm/lb mentitos wide 23-24.parquet")
write_rds(glm_model, "out_glm/glm 23-24.rds")
write_rds(glm_model_nl, "out_glm/glm nl 23-24.rds")