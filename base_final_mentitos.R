library(tidyverse)
library(readxl)

lb_mentitos <- "abandono/LB mentitos 24-25.xlsx"
lf_mentitos <- "abandono/LF mentitos 24-25.xlsx"

patron_cols <- "^(liderazgo|empatia|decision|equipo)"

# Read ---
lb_m <- read_excel(lb_mentitos) %>%
  select(id, nombre, sexo, edad, municipio, matches(patron_cols)) %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = "Concluyó"
  ) %>%
  select(-c(nombre)) %>%
  mutate(
    municipio = str_to_lower(municipio),
    municipio = case_when(
      str_detect(municipio, "victoria") ~ "Cd. Victoria",
      str_detect(municipio, "santa") ~ "Santa Catarina",
      str_detect(municipio, "ecatepec|presa") ~ "Ecatepec",
      str_detect(municipio, "garc(i|í)a") ~ "García",
      str_detect(municipio, "fama") ~ "Fama",
      str_detect(municipio, "chihuahua") ~ "Chihuahua",
      .default = NA
    )
  ) %>%
  distinct(id, .keep_all = TRUE)

lf_m <- read_excel(lf_mentitos) %>%
  select(id, nombre, fecha_nac, municipio, matches(patron_cols)) %>%
  rename("edad" = "fecha_nac") %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = "Concluyó",
  ) %>%
  select(-c(nombre))

# Processing ---
id_no_concluyo <- dplyr::setdiff(lb_m$id, lf_m$id)

lb_m <- lb_m %>%
  mutate(status = ifelse(id %in% id_no_concluyo, "No concluyó", status))

lb_m_scores <-
  lb_m %>%
  pivot_longer(
    cols = matches(patron_cols),
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

# Plots and summaries ---
lb_m_scores %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("sexo")

lb_m_scores %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("municipio")


lb_m_scores %>%
  group_by(dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))