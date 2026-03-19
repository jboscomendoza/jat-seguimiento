# Setup ----
# R >= 4.5
# arrow >= 23.0.1.1
# broom >= 1.0.12
# lme4 >= 2.0-1
# readxl >= 1.4.5
# tidyverse >= 2.0.0
library(arrow)
library(broom)
library(lme4)
library(readxl)
library(tidyverse)

if (!dir.exists("out_glm")) {
  dir.create("out_glm")
} else {
  message("out_glm already exists.")
}

# Variables ----
lb_mentitos <- "abandono/LB mentitos 24-25.xlsx"
lb_360_mentitos <- "abandono/LB 360 mentitos 24-25.xlsx"
lb_mentores <- "abandono/LB mentores 24-25.xlsx"
lf_mentitos <- "abandono/LF mentitos 24-25.xlsx"
lf_mentitos_ecatnl <- "abandono/LF mentitos 24-25 ecat-nl.xlsx"

cols_names <- c(
  "id",
  "nombre",
  "sexo",
  "edad",
  "estado",
  "municipio",
  "trabajo",
  "nivel_educ",
  "socioeco",
  "modelo_seguir"
)
cols_patron <- "^(liderazgo|empatia|decision|equipo)"
na_pattern <- c("Prefiero no contestar", "No contestó")

cols_360 <- c(
  "id" = 11,
  "id_mentor" = 7,
  "nombre" = 12,
  "estado" = 5
)

skills <- c("liderazgo", "empatia", "decision", "equipo")

# Read ----
lb_raw <- read_excel(lb_mentitos) %>%
  select(all_of(cols_names), matches(cols_patron))

lb_360_raw <-
  read_excel(lb_360_mentitos) %>%
  select(all_of(cols_360), matches(cols_patron))

lbm_raw <- read_excel(lb_mentores) %>%
  select(
    c("id_mentor" = "id", "nombre", "sexo_mentor" = "sexo"),
    matches(cols_patron)
  )

lf_m <- read_excel(lf_mentitos) %>%
  select(id, nombre, municipio) %>%
  mutate(id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))))

lf_m_ecatnl <- read_excel(lf_mentitos_ecatnl) %>%
  select(id, nombre, municipio) %>%
  mutate(id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))))

id_concluyo <- unique(c(lf_m$id, lf_m_ecatnl$id))

# Processing ----
lb_m <- lb_raw %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = ifelse(id %in% id_concluyo, "Concluyó", "No concluyó"),
    edad = ifelse(edad == 99, NA, edad),
    edad_cat = cut(edad, breaks = c(0, 11, 12, 13, 14, 15, 16, 99)),
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

lb_360_m <-
  lb_360_raw %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = ifelse(id %in% id_concluyo, "Concluyó", "No concluyó"),
    across(
      where(is.character),
      ~ ifelse(.x %in% na_pattern, NA_character_, .x)
    ),
  ) %>%
  select(-c(nombre)) %>%
  distinct(id, .keep_all = TRUE)

lbm <-
  lbm_raw %>%
  mutate(
    #id_mentor = paste0(id_mentor, str_to_lower(str_sub(nombre, 1, 3))),
    sexo_mentor = ifelse(
      sexo_mentor == "Otro (especifique)",
      "Otro",
      sexo_mentor
    ),
    across(
      where(is.character),
      ~ ifelse(.x %in% na_pattern, NA_character_, .x)
    )
  ) %>%
  select(-c(nombre)) %>%
  distinct(id_mentor, .keep_all = TRUE)

# Scores ----
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

lb_360_m_scores <-
  lb_360_m %>%
  pivot_longer(
    cols = matches(cols_patron),
    names_to = "dim_item",
    values_to = "response"
  ) %>%
  mutate(dim_item = str_replace(dim_item, "(\\d)", "_\\1")) %>%
  separate_wider_delim(
    cols = "dim_item",
    delim = "_",
    names = c("dim_360", "item_360"),
    too_few = "align_start"
  ) %>%
  filter(!is.na(item_360)) %>%
  mutate(response = as.numeric(response)) %>%
  group_by(id, dim_360) %>%
  mutate(
    dim_360 = paste0(dim_360, "_360"),
    score_360 = sum(response, na.rm = TRUE)
  ) %>%
  select(-c("item_360", "response")) %>%
  distinct() %>%
  ungroup()

lbm_scores <-
  lbm %>%
  pivot_longer(
    cols = matches(cols_patron),
    names_to = "dim_item",
    values_to = "response"
  ) %>%
  mutate(dim_item = str_replace(dim_item, "(\\d)", "_\\1")) %>%
  separate_wider_delim(
    cols = "dim_item",
    delim = "_",
    names = c("dim_mentor", "item_mentor"),
    too_few = "align_start"
  ) %>%
  filter(!is.na(item_mentor)) %>%
  mutate(response = as.numeric(response)) %>%
  group_by(id_mentor, dim_mentor) %>%
  mutate(
    dim_mentor = paste0(dim_mentor, "_mentor"),
    score_mentor = sum(response, na.rm = TRUE)
  ) %>%
  select(-c("item_mentor", "response")) %>%
  distinct() %>%
  ungroup()


lb_m_scores_united <-
  lb_m_scores %>%
  inner_join(
    select(lb_360_m_scores, -c("estado", "status")),
    by = "id"
  ) %>%
  inner_join(
    lbm_scores,
    by = "id_mentor"
  )

# Scores wide ----
lb_360_m_scores_wide <-
  lb_360_m_scores %>%
  pivot_wider(names_from = "dim_360", values_from = "score_360") %>%
  select(-c("estado", "status")) %>%
  mutate(across(where(is.numeric), scale)) %>%
  distinct()

lbm_scores_wide <- 
  lbm_scores %>%
  pivot_wider(names_from = "dim_mentor", values_from = "score_mentor") %>%
  mutate(across(where(is.numeric), scale)) %>%
  distinct()

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
    all_of(c(skills, "socioeco")),
    scale
  )) %>%
  select(-c("municipio")) %>%
  distinct()

lb_wide <- inner_join(
  lb_m_scores_wide,
  lb_360_m_scores_wide,
  by = "id"
) %>%
  inner_join(
    lbm_scores_wide,
    by = "id_mentor"
  ) %>% 
  select(-c("id", "id_mentor"))

# lm formulas ----
model_formula <-
  paste(
    c(
      "sexo",
      "I(edad-12)",
      "estado",
      "trabajo",
      "nivel_educ",
      "socioeco",
      "modelo_seguir",
      skills
    ),
    collapse = " + "
  ) %>%
  paste0("status ~ ", .)


model_formula_360 <-
  paste(
    c(
      "sexo",
      "I(edad-12)",
      "estado",
      "trabajo",
      "nivel_educ",
      "socioeco",
      "modelo_seguir",
      skills,
      paste0(skills, "_360")
    ),
    collapse = " + "
  ) %>%
  paste0("status ~ ", .)

model_formula_mentor <-
  paste(
    c(
      "sexo",
      "sexo_mentor",
      "I(edad-12)",
      "estado",
      "trabajo",
      "nivel_educ",
      "socioeco",
      "modelo_seguir",
      skills,
      paste0(skills, "_mentor")
    ),
    collapse = " + "
  ) %>%
  paste0("status ~ ", .)

model_formula_global <-
  paste(
    c(
      "sexo",
      "sexo_mentor",
      "I(edad-12)",
      "estado",
      "trabajo",
      "nivel_educ",
      "socioeco",
      "modelo_seguir",
      skills,
      paste0(skills, "_360"),
      paste0(skills, "_mentor")
    ),
    collapse = " + "
  ) %>%
  paste0("status ~ ", .)

# glm fit ----
glm_model <- glm(
  formula = model_formula,
  data = lb_wide,
  family = binomial(link = 'logit')
)

glm_model_360 <- glm(
  formula = model_formula_360,
  data = lb_wide,
  family = binomial(link = 'logit')
)

glm_model_nl <-
  lb_wide %>%
  filter(estado == "0_NL") %>%
  glm(
    formula = str_remove(model_formula, "\\+ estado "),
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_nl_360 <-
  lb_wide %>%
  filter(estado == "0_NL") %>%
  glm(
    formula = str_remove(model_formula_360, "\\+ estado "),
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_mentor <-
  lb_wide %>%
  glm(
    formula = model_formula_mentor,
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_global <-
  lb_wide %>%
  glm(
    formula = model_formula_global,
    data = .,
    family = binomial(link = 'logit')
  )

# lme fit ----
hlm_model <- lme4::glmer(
  formula = str_replace(model_formula, "estado", "(1 + socioeco|estado)"),
  data = lb_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

hlm_model_360 <- lme4::glmer(
  formula = str_replace(model_formula_360, "estado", "(1 + socioeco|estado)"),
  data = lb_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

hlm_model_mentor <- lme4::glmer(
  formula = str_replace(model_formula_mentor, "estado", "(1 + socioeco|estado)"),
  data = lb_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

hlm_model_global <- lme4::glmer(
  formula = str_replace(model_formula_global, "estado", "(1 + socioeco|estado)"),
  data = lb_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)



# Summary ----
summary(glm_model)
summary(glm_model_360)

summary(glm_model_nl)
summary(glm_model_nl_360)

summary(glm_model_global)

summary(hlm_model)
summary(hlm_model_360)
summary(hlm_model_mentor)
summary(hlm_model_global)

anova(glm_model, glm_model_nl, glm_model_nl_360)
anova(glm_model, glm_model_mentor, glm_model_global)
anova(hlm_model, hlm_model_global, hlm_model_360)

# Exports ----
write_parquet(lb_m_scores_united, "out_glm/lb mentitos 24-25.parquet")
write_parquet(lb_wide, "out_glm/lb mentitos wide 24-25.parquet")

write_rds(glm_model, "out_glm/glm 24-25.rds")
write_rds(glm_model_nl, "out_glm/glm nl 24-25.rds")
