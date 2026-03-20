# Setup ----
# R >= 4.5
# arrow >= 23.0.1.1
# tidyverse >= 2.0.0
library(arrow)
library(tidyverse)

lb_scores <- read_parquet("out_glm/lb mentitos 24-25.parquet")

# Plots ----
lb_scores %>%
  filter(!is.na(municipio)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_scores %>%
  filter(!is.na(municipio)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("sexo") +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_scores %>%
  filter(!is.na(municipio)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("municipio") +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_scores %>%
  filter(!is.na(edad)) %>%
  ggplot() +
  aes(dim, score, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  facet_wrap("edad") +
  theme_bw() +
  theme(text = element_text(size = 16))

lb_scores %>%
  filter(!is.na(edad)) %>%
  ggplot() +
  aes(status, socioeco, fill = status, color = status) +
  geom_boxplot(alpha = .5) +
  theme_bw() +
  theme(text = element_text(size = 16))


lb_scores %>%
  ggplot() +
  aes(socioeco, score, color = dim) +
  geom_point(alpha = .3, size = 1) +
  geom_smooth(method = "lm") +
  facet_grid(dim ~ estado)

# Summaries ----
lb_scores %>%
  group_by(dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))

lb_scores %>%
  group_by(municipio, dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))

lb_scores %>%
  group_by(edad, dim, status) %>%
  summarize(across(
    score,
    list("mean" = mean, "sd" = sd),
    .names = "{.col}_{.fn}"
  ))