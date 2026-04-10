library(tidyverse)
library(readxl)
library(janitor)

# Setup ----
if (!dir.exists("output/plots_graduacion/")) {
  dir.create("output/plots_graduacion/", recursive = TRUE)
} else {
  message("output/plots_graduacion/ already exists.")
}

# Variables and funs ----
color_pair <- c("#06d6a0", "#f78c6b")
path_operativo <- "data/operativo/02. Alcance operativo.xlsx"

plot_ment <- function(df_ment, grupo_ment) {
  df_ment %>%
    filter(grupo == grupo_ment) %>%
    ggplot() +
    aes(ciclo, conteo, fill = status) +
    geom_col(position = "stack") +
    geom_text(aes(label = conteo), position = position_stack(vjust = .5)) +
    scale_fill_manual(name = "Estatus", values = color_pair) +
    labs(x = "Ciclo", y = "Conteo") +
    facet_grid(rows = vars(estado), axes = "all_x") +
    theme_bw() +
    theme(
      legend.position = "top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# Data reading and cleaning ----
jat_operativo <- read_excel(path_operativo, sheet = "JAT") %>%
  select(-starts_with("."), -c("Definición")) %>%
  janitor::clean_names() %>%
  tidyr::fill(everything(), .direction = "down")

jat_clean <-
  jat_operativo %>%
  select(-starts_with("total_")) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "ciclo",
    values_to = "conteo"
  ) %>%
  mutate(
    ciclo = str_remove(ciclo, "^x") %>%
      str_replace_all("\\d{2}(\\d{2})", "\\1") %>%
      str_replace("_", "-"),
  )

# DF mentores-mentitos -----
jat_ment <-
  jat_clean %>%
  select(-c("beneficiarios")) %>%
  filter(stringr::str_detect(indicador, "Mentores|Mentitos")) %>%
  tidyr::separate_wider_delim(
    cols = "indicador",
    delim = " ",
    names = c("grupo", "status"),
    too_many = "merge"
  ) %>%
  mutate(
    status = stringr::str_to_sentence(status),
    conteo = ifelse(conteo == 0, NA, conteo)
  )

# Plots mentores-mentitos ----
plot_ment(jat_ment, "Mentores")
ggsave(
  filename = "output/plots_graduacion/mentores_operativo.png",
  units = "cm",
  width = 11,
  height = 11,
  scale = 1.5,
)

plot_ment(jat_ment, "Mentitos")
ggsave(
  filename = "output/plots_graduacion/mentitos_operativo.png",
  units = "cm",
  width = 11,
  height = 11,
  scale = 1.5,
)
