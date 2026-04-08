library(arrow)
library(tidyverse)

if (!dir.exists("output/plots_graduacion")) {
  dir.create("output/plots_graduacion", recursive = TRUE)
} else {
  message("output/plots_graduacion already exists")
}

color_pair <- c("#f78c6b", "#06d6a0")

lb_periodos <- c("21-22", "22-23", "23-24", "24-25", "25-26")
lf_periodos <- c("21-22", "22-23", "23-24", "24-25")

lb_mentitos <- list.files(
  path = "data/parquet",
  pattern = "^lb_mentitos",
  full.names = TRUE
) %>%
  map2(lb_periodos, function(ruta, periodo) {
    read_parquet(ruta) %>%
      mutate(periodo = periodo)
  }) %>%
  set_names(lb_periodos)

lf_mentitos <- list.files(
  path = "data/parquet",
  pattern = "^lf_mentitos",
  full.names = TRUE
) %>%
  map2(lf_periodos, function(ruta, periodo) {
    read_parquet(ruta) %>%
      mutate(periodo = periodo)
  }) %>%
  set_names(lf_periodos)


lb_df <- map_df(lb_mentitos, function(x) {
  select(
    x,
    any_of(c(
      "id",
      "periodo",
      "estado",
      "estado" = "selecciona_el_estado_donde_vives",
      "sexo",
      "edad"
    ))
  ) %>%
    mutate(
      id = as.character(id),
      estado = stringr::str_to_title(estado),
      tipo = "No se graduó"
    )
})

lf_df <- map_df(lf_mentitos, function(x) {
  select(
    x,
    any_of(c(
      "id",
      "periodo",
      "estado",
      "estado" = "selecciona_el_estado_donde_vives",
      "sexo",
      "edad"
    ))
  ) %>%
    mutate(
      id = as.character(id),
      estado = stringr::str_to_title(estado),
      tipo = "Se graduó"
    )
})

united <- bind_rows(lb_df, lf_df) %>%
  filter(periodo != "25-26") %>%
  mutate(edad = cut(edad, breaks = c(0, 12, 14, 16, 99)))

# Plots ----
united %>%
  count(tipo, periodo) %>%
  group_by(periodo) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  filter(tipo == "Se graduó") %>%
  ggplot() +
  aes(periodo, porcentaje) +
  geom_point(color = color_pair[[2]]) +
  geom_line(color = color_pair[[2]], aes(group = 1)) +
  geom_text(aes(label = porcentaje), position = position_nudge(y = 5)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Ciclo", y = "Porcentaje de\nmentitos graduados") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  filename = "output/plots_graduacion/mentitos_general.png",
  width = 8,
  height = 5
)

united %>%
  mutate(estado = ifelse(estado == "Estado De México", "Ecatepec", estado)) %>%
  count(tipo, estado, periodo) %>%
  group_by(estado, periodo) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  filter(tipo == "Se graduó") %>%
  ggplot() +
  aes(periodo, porcentaje) +
  geom_point(color = color_pair[[2]]) +
  geom_line(color = color_pair[[2]], aes(group = estado)) +
  geom_text(aes(label = porcentaje), position = position_nudge(y = 5)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Ciclo", y = "Porcentaje de\nmentitos graduados") +
  facet_wrap("estado") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  filename = "output/plots_graduacion/mentitos_estado.png",
  width = 8,
  height = 5
)


united %>%
  filter(!is.na(sexo)) %>%
  mutate(
    sexo = ifelse(sexo == "1", "Mujer", ifelse(sexo == 0, "Hombre", sexo))
  ) %>%
  count(tipo, periodo, sexo) %>%
  group_by(periodo, sexo) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  filter(tipo == "Se graduó") %>%
  ggplot() +
  aes(periodo, porcentaje, color = sexo) +
  geom_point() +
  geom_line(aes(group = sexo)) +
  geom_text(aes(label = porcentaje), position = position_nudge(y = 5)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_manual(name = "Sexo", values = color_pair) +
  labs(x = "Ciclo", y = "Porcentaje de\nmentitos graduados") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(
  filename = "output/plots_graduacion/mentitos_sexo.png",
  width = 8,
  height = 5
)