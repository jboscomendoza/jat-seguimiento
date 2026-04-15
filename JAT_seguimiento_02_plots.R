library(tidyverse)

# Read ----
df_jat <- arrow::read_parquet("data/parquet/seguimiento-26.parquet")

# Plots ----
df_jat %>%
  mutate(periodo = str_replace_all(periodo, "20(\\d{2})", "\\1")) %>%
  filter(periodo != "Otro" & estatus_ocupacional != "Otro (especifique)") %>%
  count(periodo, estatus_ocupacional) %>%
  group_by(periodo) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ggplot() +
  aes(periodo, porcentaje, fill = estatus_ocupacional) +
  geom_col() +
  geom_text(aes(label = porcentaje), position = position_stack(vjust = .5)) +
  labs(x = "Ciclo", y = "Porcentaje") +
  scale_fill_manual(
    name = "Estatus ocupacional",
    values = c("#98f5e1", "#90dbf4", "#cfbaf0")
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  "output/seguimiento/mentores_estatus_ocupacional.png",
  width = 8,
  height = 6
)

df_jat %>%
  mutate(periodo = str_replace_all(periodo, "20(\\d{2})", "\\1")) %>%
  filter(periodo != "Otro" & estatus_ocupacional != "Otro (especifique)") %>%
  count(periodo, sexo, estatus_ocupacional) %>%
  group_by(periodo, sexo) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ggplot() +
  aes(periodo, porcentaje, fill = estatus_ocupacional) +
  geom_col() +
  geom_text(aes(label = porcentaje), position = position_stack(vjust = .5)) +
  labs(x = "Ciclo", y = "Porcentaje") +
  scale_fill_manual(
    name = "Estatus ocupacional",
    values = c("#98f5e1", "#90dbf4", "#cfbaf0")
  ) +
  facet_grid(rows = "sexo") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  "output/seguimiento/mentores_estatus_ocupacional_sexo.png",
  width = 8,
  height = 6
)

# Estudios actuales ----
df_jat %>%
  mutate(periodo = str_replace_all(periodo, "20(\\d{2})", "\\1")) %>%
  filter(periodo != "Otro" & continua_estudiando) %>%
  mutate(
    estudios_actuales = coalesce(
      nivel_estudiando_solo,
      nivel_estudiando_trabajo
    )
  ) %>%
  filter(!is.na(estudios_actuales)) %>%
  mutate(
    estudios_actuales = case_when(
      str_detect(estudios_actuales, "Media") ~ "1. EMS",
      str_detect(estudios_actuales, "técnica") ~ "2. Carrera técnica",
      str_detect(
        estudios_actuales,
        "Licenciatura"
      ) ~ "3. Licenciatura / Ingeniería",
      str_detect(estudios_actuales, "Posgrado") ~ "4. Posgrado",
    )
  ) %>%
  count(periodo, estatus_ocupacional, estudios_actuales) %>%
  group_by(periodo, estatus_ocupacional) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  ggplot() +
  aes(periodo, porcentaje, fill = estudios_actuales) +
  geom_col() +
  geom_text(aes(label = porcentaje), position = position_stack(vjust = .5)) +
  scale_fill_manual(
    name = "Estudios actuales",
    values = c("#f2bac9", "#f2e2ba", "#b0f2b4", "#bad7f2")
  ) +
  labs(x = "Ciclo", y = "Porcentaje") +
  facet_grid(rows = vars(estatus_ocupacional)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  "output/seguimiento/mentores_estatus_educativo.png",
  width = 8,
  height = 6
)

df_jat %>%
  filter(periodo != "Otro" & continua_estudiando) %>%
  mutate(
    estudios_actuales = coalesce(
      nivel_estudiando_solo,
      nivel_estudiando_trabajo
    )
  ) %>%
  filter(!is.na(estudios_actuales)) %>%
  mutate(
    estudios_actuales = case_when(
      str_detect(estudios_actuales, "Media") ~ "1. EMS",
      str_detect(estudios_actuales, "técnica") ~ "2. Carrera técnica",
      str_detect(
        estudios_actuales,
        "Licenciatura"
      ) ~ "3. Licenciatura / Ingeniería",
      str_detect(estudios_actuales, "Posgrado") ~ "4. Posgrado",
    )
  ) %>%
  count(periodo, sexo, estatus_ocupacional, estudios_actuales) %>%
  group_by(periodo, sexo, estatus_ocupacional) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  ggplot() +
  aes(periodo, porcentaje, fill = estudios_actuales) +
  geom_col() +
  geom_text(aes(label = porcentaje), position = position_stack(vjust = .5)) +
  scale_fill_manual(
    name = "Estudios actuales",
    values = c("#f2bac9", "#f2e2ba", "#b0f2b4", "#bad7f2")
  ) +
  labs(x = "Ciclo", y = "Porcentaje") +
  facet_grid(rows = vars(estatus_ocupacional), cols = vars(sexo)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  "output/seguimiento/mentores_estatus_educativo_sexo.png",
  width = 8,
  height = 6
)


df_jat %>%
  filter(periodo != "Otro") %>%
  select(periodo, matches("l.*?_num")) %>%
  group_by(periodo) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(
    cols = starts_with("l"),
    names_to = "pregunta",
    values_to = "promedio"
  ) %>%
  mutate(
    pregunta = str_remove_all(pregunta, pattern = "l\\d_|_num") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  ggplot() +
  aes(periodo, promedio, color = pregunta) +
  geom_hline(yintercept = 3, alpha = .3) +
  geom_point() +
  geom_line(aes(group = pregunta)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Ciclo", y = "Puntaje promedio") +
  facet_wrap(pregunta~.) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")
ggsave(
  filename = "output/plots_seguimiento/mentores_escalas.png",
  units = "cm",
  width = 16,
  height = 13,
  scale = 1.3,
  dpi = 150
)
