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
    values = c("#98f5e1", "#90dbf4", "#cfbaf0", "pink")
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
    values = c("#98f5e1", "#90dbf4", "#cfbaf0", "pink")
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
