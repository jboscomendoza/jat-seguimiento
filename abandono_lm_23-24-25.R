# Setup ----
# R >= 4.5
# arrow >= 23.0.1.1
# lme4 >= 2.0-1
# readxl >= 1.4.5
# tidyverse >= 2.0.0
library(arrow)
library(lme4)
library(readxl)
library(tidyverse)

if (!dir.exists("data/parquet")) {
  dir.create("data/parquet", recursive = TRUE)
} else {
  message("data/parquet already exists.")
}

if (!dir.exists("output/abandono_lm")) {
  dir.create("output/abandono_lm", recursive = TRUE)
} else {
  message("data/abandono_lm already exists.")
}

# Variables ---
lb_mentitos_23_path = "data/parquet/lb_mentitos_23-24.parquet"
lb_mentitos_24_path = "data/parquet/lb_mentitos_24-25.parquet"
lf_mentitos_23_path = "data/parquet/lf_mentitos_23-24.parquet"
lf_mentitos_24_path = "data/parquet/lf_mentitos_24-25.parquet"

skills <- c("liderazgo", "empatia", "decision", "equipo")

cols_names <- c(
  "id",
  "nombre",
  "edad",
  "sexo",
  "estado",
  "municipio",
  "trabajo",
  "nivel_educ",
  "socioeco",
  "modelo_seguir",
  "periodo",
  "status"
)

# lm formula ----
model_formula <- paste(
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

# Reading and processing----
lb_mentitos_23 = read_parquet("data/parquet/lb_mentitos_23-24.parquet")
lb_mentitos_24 = read_parquet("data/parquet/lb_mentitos_24-25.parquet")
lf_mentitos_23 = read_parquet("data/parquet/lf_mentitos_23-24.parquet")
lf_mentitos_24 = read_parquet("data/parquet/lf_mentitos_24-25.parquet")

lb_mentitos_23 <- lb_mentitos_23 %>%
  mutate(
    status = ifelse(lb_mentitos_23[["id"]] %in% lf_mentitos_23[["id"]], 1, 0),
    across(starts_with(skills) | "socioeco", scale),
    periodo = "23-24"
  )

lb_mentitos_24 <- lb_mentitos_24 %>%
  mutate(
    status = ifelse(lb_mentitos_24[["id"]] %in% lf_mentitos_24[["id"]], 1, 0),
    across(starts_with(skills) | "socioeco", scale),
    periodo = "24-25"
  )

lb_mentitos_both <- bind_rows(
  select(lb_mentitos_23, any_of(c(cols_names, skills))),
  select(lb_mentitos_24, any_of(c(cols_names, skills)))
)

# Model fit ----
mentitos_23_glm <- glm(
  formula = model_formula,
  data = lb_mentitos_23,
  family = binomial(link = 'logit')
)

mentitos_23_hlm <- lme4::glmer(
  formula = str_replace(model_formula, "estado", "(1|estado)"),
  data = lb_mentitos_23,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

mentitos_24_glm <- glm(
  formula = model_formula,
  data = lb_mentitos_24,
  family = binomial(link = 'logit')
)

mentitos_24_hlm <- lme4::glmer(
  formula = str_replace(model_formula, "estado", "(1|estado)"),
  data = lb_mentitos_24,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

mentitos_both_glm <- glm(
  formula = paste0(model_formula, "+ periodo"),
  data = model_formula,
  family = binomial(link = 'logit')
)

mentitos_both_hlm <- lme4::glmer(
  formula = paste0(model_formula, " + (1|periodo)") %>%
    str_replace("estado", "(1|estado)"),
  data = lb_mentitos_both,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

# Summaries ----
summary(mentitos_23_glm)
summary(mentitos_24_glm)
summary(mentitos_both_glm)
summary(mentitos_23_hlm)
summary(mentitos_24_hlm)
summary(mentitos_both_hlm)

# Export ----
lm_fits <- list(
  "mentitos_23_glm" = mentitos_23_glm,
  "mentitos_24_glm" = mentitos_24_glm,
  "mentitos_both_glm" = mentitos_both_glm,
  "mentitos_23_hlm" = mentitos_23_hlm,
  "mentitos_24_hlm" = mentitos_24_hlm,
  "mentitos_both_hlm" = mentitos_both_hlm
)

write_rds(lm_fits, "output/abandono_lm/lm_fits.rds")
