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


# Differences in scores -----
n_size <-
  lb_scores %>%
  filter(score != 0) %>%
  count(id) %>%
  nrow()

lb_scores %>%
  filter(score != 0) %>%
  group_by(dim) %>%
  mutate(
    score = scale(score),
    dim = case_when(
      dim == "decision" ~ "Toma de decisión",
      dim == "empatia" ~ "Empatía",
      dim == "equipo" ~ "Trabajo en equipo",
      dim == "liderazgo" ~ "Liderazgo"
    )
  ) %>%
  group_by(status, dim) %>%
  summarise(
    media = mean(score, na.rm = TRUE),
    ee = sd(score, na.rm = TRUE) / sqrt(n_size)
  ) %>%
  ggplot() +
  aes(dim, media, color = status) +
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(
    aes(ymin = media - ee * 1.96, ymax = media + ee * 1.96),
    position = position_dodge(width = 1),
    width = .3
  ) +
  scale_color_manual(name = "Estatus", values = c("#06d6a0", "#f78c6b")) +
  scale_y_continuous(limits = c(-0.3, 0.3)) +
  labs(x = "Habilidad", y = "Puntaje promedio\n(estandarizado)") +
  facet_wrap("dim", scales = "free_x") +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  filename = "output/plots_graduacion/diferencia_scores.png",
  units = "cm",
  width = 12,
  height = 12,
  scale = 1.3,
  dpi = 150
)
