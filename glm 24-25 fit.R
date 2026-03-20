# Setup ----
# R >= 4.5
# arrow >= 23.0.1.1
# broom >= 1.0.12
# lme4 >= 2.0-1
# readxl >= 1.4.5
# tidyverse >= 2.0.0
library(arrow)
library(broom)
library(tidyverse)
library(lme4)

# Data read ----
lb_scores_wide <- read_parquet("out_glm/lb mentitos wide 24-25.parquet")

# lm formulas ----
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


model_formula_360 <- paste(
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

model_formula_mentor <- paste(
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

model_formula_global <- paste(
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
  data = lb_scores_wide,
  family = binomial(link = 'logit')
)

glm_model_360 <- glm(
  formula = model_formula_360,
  data = lb_scores_wide,
  family = binomial(link = 'logit')
)

glm_model_nl <-
  lb_scores_wide %>%
  filter(estado == "0_NL") %>%
  glm(
    formula = str_remove(model_formula, "\\+ estado "),
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_nl_360 <-
  lb_scores_wide %>%
  filter(estado == "0_NL") %>%
  glm(
    formula = str_remove(model_formula_360, "\\+ estado "),
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_nl_global <-
  lb_scores_wide %>%
  filter(estado == "0_NL") %>%
  glm(
    formula = str_remove(model_formula_global, "\\+ estado "),
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_mentor <-
  lb_scores_wide %>%
  glm(
    formula = model_formula_mentor,
    data = .,
    family = binomial(link = 'logit')
  )

glm_model_global <-
  lb_scores_wide %>%
  glm(
    formula = model_formula_global,
    data = .,
    family = binomial(link = 'logit')
  )

# lme fit ----
hlm_model <- lme4::glmer(
  formula = str_replace(model_formula, "estado", "(1|estado)"),
  data = lb_scores_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

hlm_model_360 <- lme4::glmer(
  formula = str_replace(model_formula_360, "estado", "(1|estado)"),
  data = lb_scores_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

hlm_model_mentor <- lme4::glmer(
  formula = str_replace(model_formula_mentor, "estado", "(1|estado)"),
  data = lb_scores_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

hlm_model_global <- lme4::glmer(
  formula = str_replace(model_formula_global, "estado", "(1|estado)"),
  data = lb_scores_wide,
  family = binomial(link = 'logit'),
  control = lme4::glmerControl(optimizer = "bobyqa")
)

# Summary ----
summary(glm_model)
summary(glm_model_360)
summary(glm_model_mentor)
summary(glm_model_global)

summary(glm_model_nl)
summary(glm_model_nl_360)

summary(hlm_model)
summary(hlm_model_360)
summary(hlm_model_mentor)
summary(hlm_model_global)

anova(glm_model, glm_model_360, glm_model_global)
anova(glm_model_nl, glm_model_nl_360)
anova(glm_model, glm_model_mentor, glm_model_global)
anova(hlm_model_360, hlm_model_mentor, hlm_model_global)


# Exports ----
write_rds(glm_model, "out_glm/glm 24-25.rds")
write_rds(glm_model_nl, "out_glm/glm nl 24-25.rds")
