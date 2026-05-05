library(tidyverse)
library(janitor)


# Functions ----
recode_answer <- function(column) {
  recoded_answer <-
    column %>%
    stringr::str_squish() %>%
    dplyr::recode_values(
      c("Nada parecido a mí", "Nunca/Casi nunca") ~ 1,
      c("Poco parecido a mí", "Pocas veces") ~ 2,
      c("Parecido a mí", "Muchas veces") ~ 3,
      c("Muy parecido a mí", "Siempre/Casi siempre") ~ 4,
    )
  return(recoded_answer)
}

reverse_scale <- function(scale_values, max_value = 4) {
  reverse_values <- (max_value + 1) - scale_values
  return(reverse_values)
}

lb <- readr::read_csv(file = "data/excel/LB Mentores 25-26 260505.csv") %>%
  janitor::clean_names()

lf <- readr::read_csv(file = "data/excel/LF Mentores 25-26 260505.csv") %>%
  janitor::clean_names()


lb_cats <-
  lb %>%
  select(
    "respondent_id",
    "edad" = "edad_anos_cumplidos",
    "sexo",
    "estado" = "estado_donde_vives",
    "sede" = "selecciona_tu_sede",
  ) %>%
  mutate(
    nacional = "Nacional",
    sede = ifelse(is.na(sede), estado, sede),
    sexo = stringr::str_replace(sexo, "Otro.*", "Otro"),
  )

lf_cats <-
  lf %>%
  select(
    "respondent_id",
    "edad" = "edad_anos_cumplidos",
    "sexo",
    "estado" = "estado_donde_vives",
    "sede" = "selecciona_tu_sede",
  ) %>%
  mutate(
    nacional = "Nacional",
    sede = ifelse(is.na(sede), estado, sede),
    sexo = stringr::str_replace(sexo, "Otro.*", "Otro"),
  )

items_to_reverse <- c(
  "Teamwork_07",
  "Empathy_02",
  "Empathy_03",
  "Empathy_04",
  "Decision_06",
  "Decision_07",
  "Decision_08",
  "Decision_09"
)

group_list <- list(
  "nacional" = "nacional",
  "sexo" = "sexo",
  "edad" = "edad",
  "estado" = "estado",
  "sede" = "sede"
)

lb_scale_list <- list(
  "Agency" = list(
    name = "Agency",
    item_index = 30:39,
    col_names = paste0(
      "Agency_",
      stringr::str_pad(1:length(30:39), width = 2, pad = "0")
    )
  ),
  "Teamwork" = list(
    name = "Teamwork",
    item_index = 47:55,
    col_names = paste0(
      "Teamwork_",
      stringr::str_pad(1:length(47:55), width = 2, pad = "0")
    )
  ),
  "Leadership" = list(
    name = "Leadership",
    item_index = 56:62,
    col_names = paste0(
      "Leadership_",
      stringr::str_pad(1:length(56:62), width = 2, pad = "0")
    )
  ),
  "Empathy" = list(
    name = "Empathy",
    item_index = 63:67,
    col_names = paste0(
      "Empathy_",
      stringr::str_pad(1:length(63:67), width = 2, pad = "0")
    )
  ),
  "Decision" = list(
    name = "Decision",
    item_index = 68:76,
    col_names = paste0(
      "Decision_",
      stringr::str_pad(1:length(68:76), width = 2, pad = "0")
    )
  )
)

lf_scale_list <- list(
  "Agency" = list(
    name = "Agency",
    item_index = 31:40,
    col_names = paste0(
      "Agency_",
      stringr::str_pad(1:length(31:40), width = 2, pad = "0")
    )
  ),
  "Teamwork" = list(
    name = "Teamwork",
    item_index = 67:75,
    col_names = paste0(
      "Teamwork_",
      stringr::str_pad(1:length(67:75), width = 2, pad = "0")
    )
  ),
  "Leadership" = list(
    name = "Leadership",
    item_index = 76:82,
    col_names = paste0(
      "Leadership_",
      stringr::str_pad(1:length(76:82), width = 2, pad = "0")
    )
  ),
  "Empathy" = list(
    name = "Empathy",
    item_index = 83:87,
    col_names = paste0(
      "Empathy_",
      stringr::str_pad(1:length(83:87), width = 2, pad = "0")
    )
  ),
  "Decision" = list(
    name = "Decision",
    item_index = 88:96,
    col_names = paste0(
      "Decision_",
      stringr::str_pad(1:length(88:96), width = 2, pad = "0")
    )
  )
)

lb_stats <-
  map(lb_scale_list, function(list_x) {
    lb %>%
      select(list_x[["item_index"]]) %>%
      na.omit() %>%
      set_names(list_x[["col_names"]]) %>%
      map_df(recode_answer) %>%
      mutate(across(any_of(items_to_reverse), ~ reverse_scale(.x))) %>%
      mirt::itemstats()
  })

lf_stats <-
  map(lf_scale_list, function(list_x) {
    lf %>%
      select(list_x[["item_index"]]) %>%
      na.omit() %>%
      set_names(list_x[["col_names"]]) %>%
      map_df(recode_answer) %>%
      mutate(across(any_of(items_to_reverse), ~ reverse_scale(.x))) %>%
      mirt::itemstats()
  })

lb_scores <-
  map(lb_scale_list, function(list_x) {
    df_x <- lb %>%
      select("respondent_id", list_x[["item_index"]]) %>%
      na.omit()

    ids_x <- select(df_x, "respondent_id")

    means_x <- df_x %>%
      select(-c("respondent_id")) %>%
      set_names(list_x[["col_names"]]) %>%
      map_df(recode_answer) %>%
      mutate(across(any_of(items_to_reverse), ~ reverse_scale(.x))) %>%
      rowMeans()

    ids_x %>%
      mutate(score = means_x) %>%
      inner_join(lb_cats, by = "respondent_id")
  })

lf_scores <-
  map(lf_scale_list, function(list_x) {
    df_x <- lf %>%
      select("respondent_id", list_x[["item_index"]]) %>%
      na.omit()

    ids_x <- select(df_x, "respondent_id")

    means_x <- df_x %>%
      select(-c("respondent_id")) %>%
      set_names(list_x[["col_names"]]) %>%
      map_df(recode_answer) %>%
      mutate(across(any_of(items_to_reverse), ~ reverse_scale(.x))) %>%
      rowMeans()

    ids_x %>%
      mutate(score = means_x) %>%
      inner_join(lf_cats, by = "respondent_id")
  })

scale_names <- c(
  "Agency" = "Agency",
  "Teamwork" = "Teamwork",
  "Leadership" = "Leadership",
  "Empathy" = "Empathy",
  "Decision" = "Decision"
)

type_names <- c("lb" = "lb", "lf" = "lf")

scores_list <-
  list(
    "lb" = lb_scores,
    "lf" = lf_scores
  )

map(group_list, function(group_x) {
  map_df(scale_names, function(scale_x) {
    map(type_names, function(type_x) {
      scores_list[[type_x]][[scale_x]] %>%
        summarise(
          mean = mean(score, na.rm = TRUE),
          sd = sd(score, na.rm = TRUE),
          n = n(),
          se = sd / sqrt(n),
          .by = c(group_x)
        ) %>%
        mutate(
          scale = scale_x,
          type = type_x
        ) %>%
        arrange(.data[[group_x]])
    }) %>% 
      reduce(inner_join, by = c("scale", group_x), suffix = c("_lb", "_lf")) %>%
      mutate(diferencia = mean_lf - mean_lb) %>%
      select(
        scale,
        all_of(group_x),
        starts_with("mean"),
        diferencia,
        starts_with("sd"),
        starts_with("se"),
        starts_with("n"),
        -starts_with("type")
       )
  })
})
