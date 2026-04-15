library(tidyverse)
library(readxl)
library(janitor)
library(gt)

# Setup ----
if (!dir.exists("output/plots_graduacion/")) {
  dir.create("output/plots_graduacion/", recursive = TRUE)
} else {
  message("output/plots_graduacion/ already exists.")
}

if (!dir.exists("output/docs_graduacion/")) {
  dir.create("output/docs_graduacion/", recursive = TRUE)
} else {
  message("output/docs_graduacion/ already exists.")
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
    labs(title = grupo_ment, x = "Ciclo", y = "Conteo") +
    facet_grid(rows = vars(estado_lab), axes = "all_x") +
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
  ) %>%
  mutate(
    estado_lab = case_when(
      estado == "Chihuahua" ~ "Chihuahua\n(1 sede)",
      estado == "Estado de México" ~ "Estado de México\n(1 sede)",
      estado == "Nuevo León" ~ "Nuevo León\n(3 sedes)",
      estado == "Tamaulipas" ~ "Tamaulipas\n(1 sede)"
    ),
    estado_lab = factor(
      estado_lab,
      ordered = TRUE,
      levels = c(
        "Nuevo León\n(3 sedes)",
        "Chihuahua\n(1 sede)",
        "Estado de México\n(1 sede)",
        "Tamaulipas\n(1 sede)"
      )
    )
  )


jat_ment %>%
  select(-c("estado")) %>%
  group_by(grupo, ciclo) %>%
  mutate(
    n = sum(conteo, na.rm = TRUE),
    porcentaje = conteo / sum(conteo, na.rm = TRUE)
  ) %>%
  filter(!is.na(conteo)) %>%
  ggplot() +
  aes(ciclo, porcentaje, fill = status) %>%
    geom_col(position = "stack") +
  facet_wrap("grupo")

jat_ment %>%
  group_by(estado, grupo, ciclo) %>%
  mutate(
    n = sum(conteo, na.rm = TRUE),
    porcentaje = conteo / sum(conteo, na.rm = TRUE)
  ) %>%
  filter(!is.na(conteo) & status == "Graduados") %>%
  select(-c("status", "conteo", "n")) %>%
  arrange(grupo, ciclo, estado) %>%
  pivot_wider(names_from = "ciclo", values_from = "porcentaje") %>%
  gt(omit_na_group = TRUE, groupname_col = "grupo", rowname_col = "estado") %>%
  gt::tab_header("Porcentaje de participantes graduados") %>%
  gt::tab_spanner(
    label = "Ciclo",
    columns = matches("^\\d")
  ) %>%
  gt::fmt_percent(decimals = 1) %>%
  gt::sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) %>%
  gt::summary_rows(
    fns = list(
      list(label = "Promedio", fn = "mean")
    ),
    fmt = ~ fmt_percent(., decimals = 1)
  ) %>%
  gt::gtsave("output/docs_graduacion/graduacion_participantes.docx")

# Plots mentores-mentitos ----
plot_ment(jat_ment, "Mentores")
ggsave(
  filename = "output/plots_graduacion/mentores_operativo.png",
  units = "cm",
  width = 12,
  height = 12,
  scale = 1.3,
  dpi = 150
)

plot_ment(jat_ment, "Mentitos")
ggsave(
  filename = "output/plots_graduacion/mentitos_operativo.png",
  units = "cm",
  width = 12,
  height = 12,
  scale = 1.3,
  dpi = 150
)

# Beneficiarios ----
jat_clean %>%
  filter(beneficiarios == "Beneficiarios indirectos") %>%
  group_by(indicador, ciclo, estado) %>%
  mutate(conteo = sum(conteo)) %>%
  mutate(
    indicador = ifelse(
      str_detect(indicador, "Beneficiarios"),
      "Beneficiarios intencionados",
      indicador
    )
  ) %>%
  filter(conteo != 0) %>%
  select(-beneficiarios) %>%
  ggplot() +
  aes(ciclo, conteo, fill = indicador) +
  geom_col() +
  geom_text(aes(label = conteo), position = position_stack(vjust = .5)) +
  facet_grid(rows = vars(estado), scales = "free_y") +
  theme_bw()
