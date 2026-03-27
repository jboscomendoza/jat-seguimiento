# Setup ----
# R >= 4.5
# arrow >= 23.0.1.1
# janitor >= 2.2.1
# mirt >= 1.45.1
# readxl >= 1.4.5
# tidyverse >= 2.0.0
# Setup ----
library(arrow)
library(janitor)
library(mirt)
library(readxl)
library(tidyverse)

if (!dir.exists("data/parquet")) {
  dir.create("data/parquet", recursive = TRUE)
} else {
  message("data/parquet already exists")
}

if (!dir.exists("output/psychometrics")) {
  dir.create("output/psychometrics", recursive = TRUE)
} else {
  message("output/psychometrics already exists")
}

# funs and variables ----
reverse_order <- function(x) {
  reverse = ifelse(!is.na(x), ((x - 4) * -1) + 1, NA)
  return(reverse)
}

scale_names <- c(
  "agencia",
  "equipo",
  "liderazgo",
  "empatia",
  "decision"
)

scale_colnames_mentores <- c(
  paste0("agencia_", 1:10),
  paste0("equipo_", 1:9),
  paste0("liderazgo_", 1:7),
  paste0("empatia_", 1:5),
  paste0("decision_", 1:9)
)

scale_colnames_mentitos <- c(
  paste0("liderazgo_", 1:5),
  paste0("decision_", 1:6),
  paste0("empatia_", 1:7),
  paste0("equipo_", 1:6)
)

scale_inverse_mentores <- c(
  "equipo_1",
  "equipo_9",
  "empatia_2",
  "empatia_3",
  "empatia_4",
  "decision_6",
  "decision_7",
  "decision_8",
  "decision_9"
)

col_indices_agencia_mentores <- 27:36
col_indices_scales_mentores <- 44:73
col_indices_scales_mentitos <- 30:53

cols_remove <- c(
  "respondent_id",
  "collector_id",
  "date_created",
  "date_modified",
  "ip_address",
  "email_address",
  "municipio_donde_vives",
  "colonia_donde_vives_solo_aplica_a_nl",
  "celular_10_digitos",
  "correo_electronico",
  "curp_si_no_lo_conoces_lo_puedes_obtener_en_https_www_gob_mx_curp",
  "custom_1"
)

cols_person_name <-
  c(
    "nombre",
    "nombre_s",
    "apellido_paterno",
    "apellido_materno"
  )

# reading and cleaning ----
path_lb_mentores_25 <- "data/surveymonkey_2025/Línea Base Mentores JAT 25-26.xlsx"
path_lb_mentitos_25_online <- "data/surveymonkey_2025/Online Línea Base adolescentes secundaria JAT 2025-2026.xlsx"
path_lb_mentitos_25_vaciado <- "data/surveymonkey_2025/Vaciado Línea Base adolescentes secundaria JAT 2025-2026.xlsx"

header_top_mentores <- names(read_excel(path_lb_mentores_25, n_max = 1))
header_bot_mentores <- names(read_excel(
  path_lb_mentores_25,
  n_max = 1,
  skip = 1
))

header_top_mentitos <- names(read_excel(path_lb_mentitos_25_online, n_max = 1))
header_bot_mentitos <- names(read_excel(
  path_lb_mentitos_25_online,
  n_max = 1,
  skip = 1
))

header_names_mentores <- tibble(
  header_top_mentores,
  header_bot_mentores
) %>%
  mutate(
    header = ifelse(
      stringr::str_detect(header_bot_mentores, "\\.{3}"),
      ifelse(
        stringr::str_detect(header_bot_mentores, "Otro"),
        "Otro",
        header_top_mentores
      ),
      header_bot_mentores
    )
  ) %>%
  pull(header)

header_names_mentitos <- tibble(
  header_top_mentitos,
  header_bot_mentitos
) %>%
  mutate(
    header = ifelse(
      stringr::str_detect(header_bot_mentitos, "\\.{3}"),
      ifelse(
        stringr::str_detect(header_bot_mentitos, "Otro"),
        "Otro",
        header_top_mentitos
      ),
      header_bot_mentitos
    )
  ) %>%
  pull(header)

lb_mentores_25 <- read_excel(
  path_lb_mentores_25,
  skip = 2,
  col_names = header_names_mentores
) %>%
  janitor::clean_names() %>%
  filter(!is.na(nombre))

lb_mentitos_25_online <- read_excel(
  path_lb_mentitos_25_online,
  skip = 2,
  col_names = header_names_mentitos
) %>%
  janitor::clean_names()

lb_mentitos_25_vaciado <- read_excel(
  path_lb_mentitos_25_vaciado,
  skip = 2,
  col_names = header_names_mentitos
) %>%
  janitor::clean_names()

lb_mentitos_25 <-
  bind_rows(lb_mentitos_25_online, lb_mentitos_25_vaciado) %>%
  rename("id" = "id_numero") %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  filter(date_created == max(date_created)) %>%
  ungroup()

# Scales ----
lb_mentores_25_scales <-
  lb_mentores_25 %>%
  filter(aceptas_participar_en_esta_encuesta == "Sí" & !is.na(nombre)) %>%
  select(
    all_of(col_indices_agencia_mentores),
    all_of(col_indices_scales_mentores)
  ) %>%
  mutate(across(
    everything(),
    ~ case_when(
      . %in% c("Muy parecido a mí", "Siempre/Casi siempre") ~ 4,
      . %in% c("Parecido a mí", "Muchas veces") ~ 3,
      . %in% c("Poco parecido a mí", "Pocas veces") ~ 2,
      . %in% c("Nada parecido a mí", "Nunca/Casi nunca") ~ 1,
      .default = NA
    )
  )) %>%
  set_names(scale_colnames_mentores) %>%
  mutate(across(all_of(scale_inverse_mentores), reverse_order))

lb_mentitos_25_scales <-
  lb_mentitos_25 %>%
  select(all_of(col_indices_scales_mentitos)) %>%
  mutate(across(
    everything(),
    ~ case_when(
      . %in% c("Muy parecido a mí", "Siempre/Casi siempre") ~ 4,
      . %in% c("Parecido a mí", "Muchas veces") ~ 3,
      . %in% c("Poco parecido a mí", "Pocas veces") ~ 2,
      . %in% c("Nada parecido a mí", "Nunca/Casi nunca") ~ 1,
      .default = NA
    )
  )) %>%
  set_names(scale_colnames_mentitos)

lb_mentores_25_export <- bind_cols(lb_mentores_25, lb_mentores_25_scales) %>%
  select(-any_of(cols_remove))
lb_mentitos_25_export <- bind_cols(lb_mentitos_25, lb_mentitos_25_scales) %>%
  select(-any_of(cols_remove), -any_of(cols_person_name))

# analysis ----
item_stats_mentores_25 <-
  map(scale_names, function(x) {
    col_pattern <- paste0("^", x)
    item_cols <- select(lb_mentores_25_scales, matches(col_pattern))
    item_stats <- mirt::itemstats(item_cols)
    item_stats$scores <- rowSums(item_cols, na.rm = TRUE)
    item_stats
  }) %>%
  set_names(scale_names)

item_irt_mentores_25 <-
  map(scale_names, function(x) {
    col_pattern <- paste0("^", x)
    item_cols <- select(lb_mentores_25_scales, matches(col_pattern))
    irt <- list()
    irt$modelo <- mirt::mirt(item_cols, itemtype = "gpcmIRT")
    irt$scores <- mirt::fscores(irt$modelo)
    irt
  }) %>%
  set_names(scale_names)

# Plots -----
plot_scores <- map_df(item_irt_mentores_25, "scores") %>%
  mutate(id = row_number()) %>%
  pivot_longer(-c("id"), names_to = "scale", values_to = "score") %>%
  ggplot() +
  aes(score) +
  geom_vline(xintercept = 0, alpha = .25) +
  geom_density(aes(color = scale)) +
  scale_x_continuous(limits = c(-4, 4)) +
  facet_grid(rows = "scale") +
  theme_bw() +
  theme(legend.position = "none")

irt_coefs_mentores_25 <-
  map_df(item_irt_mentores_25, function(x) {
    coef(x$modelo, simplify = TRUE)$items %>%
      as.data.frame() %>%
      rownames_to_column("item")
  })

plot_coefs <- irt_coefs_mentores_25 %>%
  filter(str_detect(item, "agencia")) %>%
  pivot_longer(
    cols = starts_with("b"),
    names_to = "threshold",
    values_to = "logits"
  ) %>%
  ggplot() +
  aes(item, logits, color = threshold) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_point() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(-4, 4)) +
  coord_flip()

scale_cors <-
  map_df(item_stats_mentores_25, "scores") %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column("scale") %>%
  as_tibble()

# export ----
write_parquet(lb_mentores_25_export, "data/parquet/lb_mentores_25-26.parquet")
write_parquet(lb_mentitos_25_export, "data/parquet/lb_mentitos_25-26.parquet")

write_rds(item_stats_mentores_25, "output/psychometrics/lb_mentores_25-26_item_stats.rds")
write_rds(item_irt_mentores_25, "output/psychometrics/lb_mentores_25-26_item_irt.rds")
write_rds(irt_coefs_mentores_25, "output/psychometrics/lb_mentores_25-26_irt_coefs.parquet")