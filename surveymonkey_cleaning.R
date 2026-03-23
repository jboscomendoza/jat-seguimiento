library(janitor)
library(mirt)
library(readxl)
library(tidyverse)

scale_names <- c(
  "agencia",
  "liderazgo",
  "equipo",
  "empatia",
  "decision"
)

scale_colnames <- c(
  paste0("agencia_", 1:10),
  paste0("liderazgo_", 1:9),
  paste0("equipo_", 1:7),
  paste0("empatia_", 1:5),
  paste0("decision_", 1:9)
)

path_lb_mentores_25 <- "surveymonkey/Línea Base Mentores JAT 25-26.xlsx"

header_top <- names(read_excel(path_lb_mentores_25, n_max = 1))
header_bot <- names(read_excel(path_lb_mentores_25, n_max = 1, skip = 1))

header_names <- tibble(
  header_top,
  header_bot
) %>%
  mutate(
    header = ifelse(
      stringr::str_detect(header_bot, "\\.{3}"),
      ifelse(stringr::str_detect(header_bot, "Otro"), "Otro", header_top),
      header_bot
    )
  ) %>%
  pull(header)

lb_mentores_25 <- read_excel(
  path_lb_mentores_25,
  skip = 2,
  col_names = header_names
) %>%
  janitor::clean_names()

lb_scales <-
  lb_mentores_25 %>%
  filter(aceptas_participar_en_esta_encuesta == "Sí") %>%
  select(27:36, 44:73) %>%
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
  set_names(scale_colnames)

item_stats <-
  map(scale_names, function(x) {
    col_pattern <- paste0("^", x)
    item_cols <- select(lb_scales, matches(col_pattern))
    item_stats <- mirt::itemstats(item_cols)
    item_stats$scores <- rowSums(item_cols, na.rm = TRUE) %>%
      scale() %>%
      as.vector()
    item_stats
  }) %>%
  set_names(scale_names)

item_irt <-
  map(scale_names, function(x) {
    col_pattern <- paste0("^", x)
    item_cols <- select(lb_scales, matches(col_pattern))
    item_stats <- mirt::mirt(item_cols, itemtype = "gpcm")
  }) %>%
  set_names(scale_names)

map(item_irt, ~ coef(.x, IRTPars = TRUE, simplify = TRUE)$items)

scale_cors <-
  map_df(item_stats, "scores") %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column("scale") %>%
  as_tibble()

mirt::coef()
