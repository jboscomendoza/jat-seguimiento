library(tidyverse)
library(janitor)

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

scale_names <- c(
  "Agency" = "Agency",
  "Teamwork" = "Teamwork",
  "Leadership" = "Leadership",
  "Empathy" = "Empathy",
  "Decision" = "Decision"
)

type_names <- c("lb" = "lb", "lf" = "lf")


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

mentores_ids <- readr::read_csv(
  file = "output/seguimiento/mentores-25-25-ids.csv"
)


lb <- readr::read_csv(file = "data/excel/LB Mentores 25-26 260505.csv") %>%
  janitor::clean_names() %>%
  filter(respondent_id %in% na.omit(mentores_ids$respondent_id_lb))

lf <- readr::read_csv(file = "data/excel/LF Mentores 25-26 260505.csv") %>%
  janitor::clean_names() %>%
  filter(respondent_id %in% na.omit(mentores_ids$respondent_id_lf))

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

items_clave <- map(lb_scale_list, function(list_x) {
  lb %>%
    slice(1) %>%
    select(respondent_id, list_x[["item_index"]]) %>%
    pivot_longer(
      -respondent_id,
      names_to = "question",
      values_to = "answer"
    ) %>%
    select(-c(respondent_id, answer)) %>%
    distinct() %>%
    mutate(
      item = paste0(
        list_x[["name"]],
        "_",
        stringr::str_pad(row_number(), width = 2, pad = "0")
      ),
      question = stringr::str_remove(
        question,
        ".*(enunciado(s)?|representa)_"
      ) %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_sentence() %>%
        stringr::str_squish()
    ) %>%
    select("Item" = "item", "Question" = "question")
})

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

scores_list <-
  list(
    "lb" = lb_scores,
    "lf" = lf_scores
  )

group_summary <-
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
        reduce(
          inner_join,
          by = c("scale", group_x),
          suffix = c("_lb", "_lf")
        ) %>%
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
        ) %>%
        mutate(
          linf_lb = mean_lb - 1.96 * se_lb,
          lsup_lb = mean_lb + 1.96 * se_lb,
          linf_lf = mean_lf - 1.96 * se_lf,
          lsup_lf = mean_lf + 1.96 * se_lf,
        ) %>%
        mutate(
          test_a = dplyr::between(linf_lb, linf_lf, lsup_lf),
          test_b = dplyr::between(lsup_lb, linf_lf, lsup_lf),
          test_c = dplyr::between(lsup_lf, linf_lb, lsup_lb),
          test_d = dplyr::between(lsup_lf, linf_lb, lsup_lb)
        ) %>%
        mutate(
          sig = ifelse(test_a + test_b + test_c + test_d == 0, "*", "")
        ) %>%
        select(-starts_with("test_"))
    })
  })


stats_list <-
  list(
    "lb" = lb_stats,
    "lf" = lf_stats
  )

group_stats <-
  map(scale_names, function(scale_x) {
    map(type_names, function(type_x) {
      question_stats <- stats_list[[type_x]][[scale_x]]

      alpha_value = question_stats$overall$alpha

      question_itemstats <- dplyr::bind_cols(
        question_stats$itemstats,
        question_stats$proportions
      ) %>%
        mutate(
          alpha = alpha_value,
          across(where(is.numeric), ~ round(.x, 2)),
        ) %>%
        rownames_to_column("item") %>%
        as_tibble() %>%
        select(all_of(c(
          "item",
          "N",
          "mean",
          "sd",
          "alpha"
        ))) %>%
        mutate(se = sd / sqrt(N))
    }) %>%
      reduce(
        inner_join,
        by = "item",
        suffix = c("_lb", "_lf")
      ) %>%
      mutate(diferencia = mean_lf - mean_lb) %>%
      select(
        item,
        starts_with("mean"),
        diferencia,
        starts_with("sd"),
        starts_with("se"),
        starts_with("N"),
        -starts_with("type")
      ) %>%
      mutate(
        linf_lb = mean_lb - 1.96 * se_lb,
        lsup_lb = mean_lb + 1.96 * se_lb,
        linf_lf = mean_lf - 1.96 * se_lf,
        lsup_lf = mean_lf + 1.96 * se_lf,
      ) %>%
      mutate(
        test_a = dplyr::between(linf_lb, linf_lf, lsup_lf),
        test_b = dplyr::between(lsup_lb, linf_lf, lsup_lf),
        test_c = dplyr::between(lsup_lf, linf_lb, lsup_lb),
        test_d = dplyr::between(lsup_lf, linf_lb, lsup_lb)
      ) %>%
      mutate(
        sig = ifelse(test_a + test_b + test_c + test_d == 0, "*", "")
      ) %>%
      select(-starts_with("test_"))
  })

group_summary <- map(group_summary, function(df_x) {
  names(df_x) <- stringr::str_to_sentence(names(df_x))
  df_x %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
})

group_stats <- map(group_stats, function(df_x) {
  names(df_x) <- stringr::str_to_sentence(names(df_x))
  df_x %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
})

names(items_clave) <- stringr::str_to_sentence(names(items_clave))

# Ganancia individual ----
gains <- list()

gains$df <- map_df(scale_names, function(scale_x) {
  df_lb <- lb_scores[[scale_x]] %>%
    select("respondent_id_lb" = respondent_id, "score_lb" = score)
  df_lf <- lf_scores[[scale_x]] %>%
    select("respondent_id_lf" = respondent_id, "score_lf" = score)
  df_ids <- select(mentores_ids, starts_with("respondent"))
  inner_join(
    df_lb,
    df_ids,
    by = "respondent_id_lb"
  ) %>%
    inner_join(
      df_lf,
      by = "respondent_id_lf"
    ) %>%
    mutate(
      scale = scale_x,
      diferencia = score_lf - score_lb,
      status = ifelse(diferencia > 0, "Con ganancia", "Sin ganancia")
    )
})

gains$by_scale <-
  gains$df %>%
  group_by(scale) %>%
  count(status) %>%
  mutate(percent = round(n / sum(n) * 100, 2)) %>%
  filter(status == "Con ganancia")

gains$at_least_one <-
  gains$df %>%
  group_by(respondent_id_lb) %>%
  count(status) %>%
  pivot_wider(names_from = "status", values_from = "n") %>%
  ungroup() %>%
  rename("scales_with_gains" = "Con ganancia") %>%
  count(scales_with_gains) %>%
  mutate(
    percent = round(n / sum(n) * 100, 2),
    scales_with_gains = ifelse(is.na(scales_with_gains), 0, scales_with_gains)
  )

gains$cor_pre <-
  gains$df %>%
  select(scale, ends_with("_lb")) %>%
  pivot_wider(names_from = "scale", values_from = score_lb) %>%
  select(-c("respondent_id_lb")) %>%
  cor(use = "pairwise") %>%
  round(2) %>%
  as.data.frame() %>%
  rownames_to_column("scale") %>%
  as_tibble()

gains$cor_post <-
  gains$df %>%
  select(scale, ends_with("_lf")) %>%
  pivot_wider(names_from = "scale", values_from = score_lf) %>%
  select(-c("respondent_id_lf")) %>%
  cor(use = "pairwise") %>%
  round(2) %>%
  as.data.frame() %>%
  rownames_to_column("scale") %>%
  as_tibble()

# Exports ----
readr::write_rds(group_stats, "output/mentores 25-26/group_stats.rds")
readr::write_rds(group_summary, "output/mentores 25-26/group_summary.rds")
readr::write_rds(items_clave, "output/mentores 25-26/items_clave.rds")
readr::write_rds(gains, "output/mentores 25-26/gains.rds")
