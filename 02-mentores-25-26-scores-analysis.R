library(mirt)
library(readxl)
library(tidyverse)

# Setup ----
output_path <- "output/mentores 25-26"

if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
} else {
    message(paste0(output_path, " already exists."))
}

# Vars ----
path_lb <- "data/excel/LB mentores 25-26.xlsx"
path_lf <- "data/excel/LF mentores 25-26.xlsx"

lb <- readxl::read_excel(path_lb) %>%
    janitor::clean_names() %>%
    filter(stringr::str_length(q4_correo_electronico) > 1)

lf <- readxl::read_excel(path_lf) %>%
    janitor::clean_names()

lb_cats <-
    lb %>%
    select(
        "respondent_id",
        "edad" = "q5_edad_anos_cumplidos",
        "sexo" = "q6_sexo",
        "estado" = "q10_estado_donde_vives",
        "sede" = "q11_selecciona_tu_sede",
    ) %>%
    mutate("nacional" = "Nacional")

lf_cats <-
    lf %>%
    select(
        "respondent_id",
        "edad" = "q6_edad_anos_cumplidos",
        "sexo" = "q7_sexo",
        "estado" = "q11_estado_donde_vives",
        "sede" = "q12_selecciona_tu_sede",
    ) %>%
    mutate("nacional" = "Nacional")

group_list <- list(
    "nacional" = "nacional",
    "edad" = "edad",
    "estado" = "estado",
    "sede" = "sede"
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

reemplazos_nombres <- tibble(
        from = c(
            "Aidee Joselin Quiajda Dominguez",
            "Allisson Fernanda Garcia Castro",
            "Analia Galicia Serrano",
            "Angel Morin Villarreal",
            "Christian Abdiel Jaramillo Medellin.",
            "Devany Lizath Zuniga Arriaga",
            "Yosgard Garcia Hernandez",
            "Vanesa Chavez Juarez",
            "Gerardo Martinez Vallejo",
            "Lidia Fernandez Marquez",
            "Luz Anayetzy Orgaz Diaz Diaz",
            "Citlali Antonio Hernandez",
            "Dayra Natividad Leos",
            "Karen Viri Hernandez",
            "Maria De Jesus Martinez Martinez",
            "Ricardo Martinez Martinez",
            "Santiago Escamilla Araujo",
            "Sarahi Robledo Iracheta",
            "Darynka Martinez Gonzalez",
            "Dulce Julian Mendez",
            "Ricardo Martinez Canamar"
        ),
        to = c(
            "Aidee Joselin Quijada Dominguez",
            "Allison Fernanda Garcia Castro",
            "Analia Rubi Galicia Serrano",
            "Angel Enrique Morin Villarreal",
            "Christian Abdiel Jaramillo Medellin",
            "Devany Lizeth Zuniga Arriaga",
            "Yosgard Said Garcia Hernandez",
            "Vanesa Gabriela Chavez Juarez",
            "Gerardo Israel Martinez Vallejo",
            "Lidia Viridiana Fernandez Marquez",
            "Luz Anayetzy Orgaz Diaz",
            "Citlali Magdale Antonio Hernandez",
            "Dayra Astrid Natividad Leos",
            "Karen Viridiana Sandoval Hernandez",
            "Maria De Jesus Martinez Escamilla",
            "Ricardo Martinez Canamar",
            "Alexander Santiago Escamilla Araujo",
            "Maria Sarahi Robledo Iracheta",
            "Isis Darynka Martinez Gonzalez",
            "Dulce Silvana Julian Mendez",
            "Oliver Ricardo Martinez Canamar"
        )
    )


# Functions ----
recode_answer <- function(wide_question) {
    recoded_answer <- wide_question %>%
        mutate(
            answer = dplyr::recode_values(
                answer,
                c("Nada parecido a mí", "Nunca/Casi nunca") ~ 1,
                c("Poco parecido a mí", "Pocas veces") ~ 2,
                c("Parecido a mí", "Muchas veces") ~ 3,
                c("Muy parecido a mí", "Siempre/Casi siempre") ~ 4,
            )
        )
    return(recoded_answer)
}

reverse_scale <- function(scale_values, max_value = 4) {
    reverse_values <- (max_value + 1) - scale_values
    return(reverse_values)
}

widen_question <- function(
    df_linea,
    question_prefix,
    question_label,
    number_of_questions
) {
    n_questions <- 1:number_of_questions
    wide_question <-
        select(df_linea, c("respondent_id"), starts_with(question_prefix)) |>
        separate_wider_delim(
            starts_with(question_prefix),
            delim = "|",
            names = paste0(question_prefix, "_", n_questions),
            too_few = "align_start"
        ) %>%
        pivot_longer(
            cols = starts_with(question_prefix),
            names_to = "question_n",
            values_to = "question_text"
        ) %>%
        separate_wider_delim(
            "question_n",
            delim = "_",
            names = c("scale", "number"),
            too_few = "align_start"
        ) %>%
        separate_wider_delim(
            "question_text",
            delim = ":",
            names = c("question", "answer"),
            too_few = "align_start"
        ) %>%
        mutate(
            across(everything(), ~ stringr::str_squish(.x)),
            scale = question_label
        ) %>%
        recode_answer()
    return(wide_question)
}

get_itemstats <- function(wide_question) {
    question_stats <- wide_question %>%
        select(-c("question")) %>%
        mutate(number = stringr::str_pad(number, width = 2, pad = "0")) %>%
        tidyr::unite("item", c("scale", "number")) %>%
        pivot_wider(names_from = "item", values_from = "answer") %>%
        select(-c("respondent_id")) %>%
        mutate(across(any_of(items_to_reverse), ~ reverse_scale(.x))) %>%
        mirt::itemstats()

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
        rename(
            "total_r" = "total.r",
            "r_if_rm" = "total.r_if_rm",
            "p_1" = "1",
            "p_2" = "2",
            "p_3" = "3",
            "p_4" = "4"
        ) %>%
        select(all_of(c(
            "item",
            "N",
            "mean",
            "sd",
            "alpha",
            "alpha_if_rm",
            "total_r",
            "r_if_rm",
            "p_1",
            "p_2",
            "p_3",
            "p_4"
        )))
    return(question_itemstats)
}

scale_summary <- function(scale_df, data_cats, group) {
    label = unique(scale_df[["scale"]])
    df_cats <- inner_join(data_cats, scale_df, by = "respondent_id")
    number_of_items <- length(unique(df_cats$number))

    df_summary <- df_cats %>%
        summarise(
            mean = mean(answer),
            sd = sd(answer),
            n = n() / number_of_items,
            .by = c(group)
        ) %>%
        arrange(.data[[group]]) %>%
        mutate(
            across(where(is.numeric), ~ round(.x, 2)),
            scale = label
        ) %>%
        select(all_of("scale"), everything())
    names(df_summary) <- stringr::str_to_title(names(df_summary))
    return(df_summary)
}

# Analysis ----
lb_ag_1 <- widen_question(lb, "q13", "Agency", 5)
lb_ag_2 <- widen_question(lb, "q14", "Agency", 5)
lb_team <- widen_question(lb, "q21", "Teamwork", 9)
lb_lead <- widen_question(lb, "q22", "Leadership", 7)
lb_empt <- widen_question(lb, "q23", "Empathy", 5)
#widen_question("q24", "Decision", 9)

lf_ag_1 <- widen_question(lf, "q14", "Agency", 5)
lf_ag_2 <- widen_question(lf, "q15", "Agency", 5)
lf_team <- widen_question(lf, "q29", "Teamwork", 9)
lf_lead <- widen_question(lf, "q30", "Leadership", 7)
lf_empt <- widen_question(lf, "q31", "Empathy", 5)
#widen_question("q32", "Decision", 9)

lb_agnc <-
    lb_ag_2 %>%
    mutate(number = as.character(as.numeric(number) + 5)) %>%
    bind_rows(lb_ag_1) %>%
    arrange(respondent_id, number)
lb_agnc_itemstats <- get_itemstats(lb_agnc)
lb_team_itemstats <- get_itemstats(lb_team)
lb_lead_itemstats <- get_itemstats(lb_lead)
lb_empt_itemstats <- get_itemstats(lb_empt)

lf_agnc <-
    lf_ag_2 %>%
    mutate(number = as.character(as.numeric(number) + 5)) %>%
    bind_rows(lf_ag_1) %>%
    arrange(respondent_id, number)
lf_agnc_itemstats <- get_itemstats(lf_agnc)
lf_team_itemstats <- get_itemstats(lf_team)
lf_lead_itemstats <- get_itemstats(lf_lead)
lf_empt_itemstats <- get_itemstats(lf_empt)

# Grouped
lb_itemstats_list <- list(
    "agnc" = lb_agnc_itemstats,
    "team" = lb_team_itemstats,
    "lead" = lb_lead_itemstats,
    "empt" = lb_empt_itemstats
)

lb_scale_list <- list(
    "agnc" = lb_agnc,
    "team" = lb_team,
    "lead" = lb_lead,
    "empt" = lb_empt
)

lf_itemstats_list <- list(
    "agnc" = lf_agnc_itemstats,
    "team" = lf_team_itemstats,
    "lead" = lf_lead_itemstats,
    "empt" = lf_empt_itemstats
)

lf_scale_list <- list(
    "agnc" = lf_agnc,
    "team" = lf_team,
    "lead" = lf_lead,
    "empt" = lf_empt
)

lb_scale_summaries <-
    map(lb_scale_list, function(scale_x) {
        map(group_list, function(group_x) {
            scale_summary(scale_x, lb_cats, group_x)
        })
    })

lf_scale_summaries <-
    map(lf_scale_list, function(scale_x) {
        map(group_list, function(group_x) {
            scale_summary(scale_x, lf_cats, group_x)
        })
    })

# Export ----
write_rds(scale_list, paste0(output_path, "/lb-scales-mentores-25-26.rds"))
write_rds(
    itemstats_list,
    paste0(output_path, "/lb-psychometrics-mentores-25-26.rds")
)
write_rds(
    scale_summaries,
    paste0(output_path, "/lb-statistics-mentores-25-26.rds")
)


write_rds(scale_list, paste0(output_path, "/lf-scales-mentores-25-26.rds"))
write_rds(
    itemstats_list,
    paste0(output_path, "/lf-psychometrics-mentores-25-26.rds")
)
write_rds(
    scale_summaries,
    paste0(output_path, "/lf-statistics-mentores-25-26.rds")
)
