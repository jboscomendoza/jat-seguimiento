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
lb_mentitos_path <- "abandono/LB mentitos 24-25.xlsx"
lb_mentitos_360_path <- "abandono/LB 360 mentitos 24-25.xlsx"
lb_mentores_path <- "abandono/LB mentores 24-25.xlsx"
lf_mentitos_path <- "abandono/LF mentitos 24-25.xlsx"
lf_mentitos_ecatnl_path <- "abandono/LF mentitos 24-25 ecat-nl.xlsx"

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
lb_mentitos_raw <- read_excel(lb_mentitos_path)
lb_mentitos_360_raw <- read_excel(lb_mentitos_360_path)
lb_mentores_raw <- read_excel(lb_mentores_path)

lf_mentitos <- read_excel(lf_mentitos_path) %>%
  select(id, nombre, municipio)
lf_mentitos_ecatnl <- read_excel(lf_mentitos_ecatnl_path) %>%
  select(id, nombre, municipio)

id_concluyo <- unique(c(lf_mentitos$id, lf_mentitos_ecatnl$id))

# Processing ----
lb_mentitos <- lb_mentitos_raw %>%
  select(all_of(cols_names), matches(cols_patron)) %>%
  mutate(
    #id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
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

lb_mentitos_360 <-
  lb_mentitos_360_raw %>%
  select(all_of(cols_360), matches(cols_patron)) %>%
  mutate(
    id_mentor = str_remove(id_mentor, " .*") %>% str_replace(., "5CU", "5CH"),
    status = ifelse(id %in% id_concluyo, "Concluyó", "No concluyó"),
    across(
      where(is.character),
      ~ ifelse(.x %in% na_pattern, NA_character_, .x)
    ),
  ) %>%
  select(-c(nombre)) %>%
  distinct(id, .keep_all = TRUE)

lb_mentores <-
  lb_mentores_raw %>%
  select(
    c("id_mentor" = "id", "nombre", "sexo_mentor" = "sexo"),
    matches(cols_patron)
  ) %>%
  mutate(
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
lb_mentitos_scores <- lb_mentitos %>%
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

lb_mentitos_360_scores <-
  lb_mentitos_360 %>%
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

lb_mentores_scores <-
  lb_mentores %>%
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


lb_scores <-
  lb_mentitos_scores %>%
  inner_join(
    select(lb_mentitos_360_scores, -c("estado", "status")),
    by = "id"
  ) %>%
  inner_join(
    lb_mentores_scores,
    by = "id_mentor"
  )

# Scores wide ----
lb_mentitos_scores_wide <-
  lb_mentitos_scores %>%
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

lb_mentitos_360_scores_wide <-
  lb_mentitos_360_scores %>%
  pivot_wider(names_from = "dim_360", values_from = "score_360") %>%
  select(-c("estado", "status")) %>%
  mutate(across(where(is.numeric), scale)) %>%
  distinct()

lb_mentores_scores_wide <-
  lb_mentores_scores %>%
  pivot_wider(names_from = "dim_mentor", values_from = "score_mentor") %>%
  mutate(across(where(is.numeric), scale)) %>%
  distinct()

lb_scores_wide <- inner_join(
  lb_mentitos_scores_wide,
  lb_mentitos_360_scores_wide,
  by = "id"
) %>%
  inner_join(
    lb_mentores_scores_wide,
    by = "id_mentor"
  ) %>%
  select(-c("id", "id_mentor"))

# Export ----
write_parquet(lb_scores, "out_glm/lb mentitos 24-25.parquet")
write_parquet(lb_scores_wide, "out_glm/lb mentitos wide 24-25.parquet")