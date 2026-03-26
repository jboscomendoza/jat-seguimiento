
# Setup ----
library(arrow)
library(janitor)
library(mirt)
library(readxl)
library(tidyverse)

if (!dir.exists("data")) {
  dir.create("data")
} else {
  message("data already exists")
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

cols_remove <- c(
  "respondent_id",
  "collector_id",
  "date_created",
  "date_modified",
  "ip_address",
  "email_address",
  "ip_address",
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
path_lb_mentores_25 <- "data/Línea Base Mentores JAT 25-26.xlsx"

header_top_mentores <- names(read_excel(path_lb_mentores_25, n_max = 1))
header_bot_mentores <- names(read_excel(
  path_lb_mentores_25,
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

lb_mentores_25 <- read_excel(
  path_lb_mentores_25,
  skip = 2,
  col_names = header_names_mentores
) %>%
  janitor::clean_names() %>%
  filter(!is.na(nombre))

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

# analysis ----
item_stats <-
  map(scale_names, function(x) {
    col_pattern <- paste0("^", x)
    item_cols <- select(lb_mentores_25_scales, matches(col_pattern))
    item_stats <- mirt::itemstats(item_cols)
    item_stats$scores <- rowSums(item_cols, na.rm = TRUE)
    item_stats
  }) %>%
  set_names(scale_names)

item_irt <-
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
plot_scores <- map_df(item_irt, "scores") %>%
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

irt_coefs <-
  map_df(item_irt, function(x) {
    coef(x$modelo, simplify = TRUE)$items %>%
      as.data.frame() %>%
      rownames_to_column("item")
  })

plot_coefs <- irt_coefs %>%
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
  map_df(item_stats, "scores") %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column("scale") %>%
  as_tibble()

# export ----
lb_mentores_25_export <- bind_cols(lb_mentores_25, lb_mentores_25_scales) %>%
  select(-any_of(cols_remove))

write_parquet(lb_mentores_25_export, "data/lb_mentores_25.parquet")
write_rds(item_stats, "data/item_stats.rds")
write_rds(item_irt, "data/item_irt.rds")
