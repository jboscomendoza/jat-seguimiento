library(arrow)
library(tidyverse)

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
      "estado" = "selecciona_el_estado_donde_vives"
    ))
  ) %>%
    mutate(
      id = as.character(id),
      estado = stringr::str_to_title(estado),
      tipo = "LB: inició"
    )
})

lf_df <- map_df(lf_mentitos, function(x) {
  select(
    x,
    any_of(c(
      "id",
      "periodo",
      "estado",
      "estado" = "selecciona_el_estado_donde_vives"
    ))
  ) %>%
    mutate(
      id = as.character(id),
      estado = stringr::str_to_title(estado),
      tipo = "LF: se graduó"
    )
})

unida <- bind_rows(lb_df, lf_df)

unida

unida %>% 
  count(tipo, periodo) %>% 
  ggplot() +
  aes(periodo, n, color = tipo) +
  geom_point() +
  geom_line(aes(group = tipo)) +
  geom_text(aes(label = n), nudge_y = 25) +
  scale_color_manual(name = "Tipo", values = c("#E15554", "#3BB273")) +
  labs(x = "Ciclo", y = "Número de mentitos") +
  theme_minimal()

unida %>% 
  count(tipo, estado, periodo) %>% 
  mutate(estado = ifelse(estado == "Estado De México", "Ecatepec", estado)) %>% 
  ggplot() +
  aes(periodo, n, color = tipo) +
  geom_point() +
  geom_line(aes(group = tipo)) +
  geom_text(aes(label = n), nudge_y = 25) +
  scale_color_manual(name = "Tipo", values = c("#E15554", "#3BB273")) +
  labs(x = "Ciclo", y = "Número de mentitos") +
  facet_wrap("estado") +
  theme_bw()