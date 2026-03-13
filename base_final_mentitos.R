library(tidyverse)
library(readxl)
library(broom)

lb_mentitos <- "abandono/LB mentitos 24-25.xlsx"
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
  "socioeco"
)
cols_patron <- "^(liderazgo|empatia|decision|equipo)"
na_pattern <- c("Prefiero no contestar", "No contestó")

# Read ---
raw_read <- read_excel(lb_mentitos) %>%
  select(all_of(cols_names), matches(cols_patron))

lf_m <- read_excel(lf_mentitos) %>%
  select(id, nombre, fecha_nac, municipio, matches(cols_patron)) %>%
  rename("edad" = "fecha_nac") %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = "Concluyó",
  ) %>%
  select(-c(nombre))

lf_m_ecatnl <- read_excel(lf_mentitos_ecatnl) %>%
  select(id, nombre, fecha_nac, municipio, matches(cols_patron)) %>%
  rename("edad" = "fecha_nac") %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = "Concluyó",
  ) %>%
  select(-c(nombre))

id_no_concluyo <- unique(c(lf_m$id, lf_m_ecatnl$id))

lb_m <-
  read_excel(lb_mentitos) %>%
  select(all_of(cols_names), matches(cols_patron)) %>%
  mutate(
    id = paste0(id, str_to_lower(str_sub(nombre, 1, 3))),
    status = ifelse(id %in% id_no_concluyo, "No concluyó", "Concluyó"),
    edad = ifelse(edad == 99, NA, edad),
    socioeco = ifelse(socioeco == 0, NA, socioeco),
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
  #mutate(edad = cut(edad, breaks = c(0, 11, 12, 13, 14, 15, 16, 99))) %>%
  select(-c(nombre)) %>%
  distinct(id, .keep_all = TRUE)

# Processing ---
lb_m <- lb_m %>%
  mutate(status = ifelse(id %in% id_no_concluyo, "No concluyó", status))

lb_m_scores <-
  lb_m %>%
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
  mutate(across(all_of(c("liderazgo", "empatia", "decision", "equipo")), scale)) %>% 
  select(-c("id", "municipio"))


# Plots ---
lb_m_scores %>%
  filter(!is.na(municipio)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_m_scores %>%
  filter(!is.na(municipio)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("sexo") +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_m_scores %>%
  filter(!is.na(municipio)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("municipio") +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_m_scores %>%
  filter(!is.na(edad)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("edad") +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_m_scores %>%
  filter(!is.na(edad)) %>%
  ggplot() +
  aes(status, socioeco, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  theme_bw() +
  theme(text = element_text(size = 16))


lb_m_scores %>%
  ggplot() +
  aes(socioeco, score, color = dim) +
  geom_point(alpha = .3, size = 1) +
  geom_smooth(method = "lm") +
  facet_grid(dim ~ estado)

# Summaries ----
lb_m_scores %>%
  group_by(dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))

lb_m_scores %>%
  group_by(municipio, dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))

lb_m_scores %>%
  group_by(edad, dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))

# glm ----
model_formula <- 
  paste(
  "sexo",
  "I(edad-14)",
  "estado",
  "trabajo",
  "nivel_educ",
  "socioeco",
  "liderazgo",
  "empatia",
  "decision",
  "equipo",
  sep = " + "
) %>% 
  paste0("status ~ ", .) %>% 
  formula()

glm_model <- glm(formula = model_formula, data = lb_m_scores_wide, family = binomial(link = 'logit'))


broom::tidy(glm_model) %>%
  mutate(
    odds = exp(estimate),
    prob = (exp(estimate) - 1) * 100
  )
summary(glm_model)
anova(glm_model)
