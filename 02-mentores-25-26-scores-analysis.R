library(mirt)
library(readxl)
library(tidyverse)

path_lb <- "data/excel/LB mentores 25-26.xlsx"
path_lf <- "data/excel/LF mentores 25-26.xlsx"

lb <- readxl::read_excel(path_lf) %>%
    janitor::clean_names()

lf <- readxl::read_excel(path_lf) %>%
    janitor::clean_names()

lf_cats <- 
    lf %>%
    select(
        "respondent_id",
        "edad" = "q6_edad_anos_cumplidos",
        "sexo" = "q7_sexo",
        "estado" = "q11_estado_donde_vives",
        "sede" = "q12_selecciona_tu_sede",
    )


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

widen_question <- function(
    question_prefix,
    question_label,
    number_of_questions
) {
    n_questions <- 1:number_of_questions
    wide_question <-
        select(lf, c("respondent_id"), starts_with(question_prefix)) |>
        separate_wider_delim(
            starts_with(question_prefix),
            delim = "|",
            names = paste0(question_prefix, "_", n_questions)
        ) %>%
        pivot_longer(
            cols = starts_with(question_prefix),
            names_to = "question_n",
            values_to = "question_text"
        ) %>%
        separate_wider_delim(
            "question_n",
            delim = "_",
            names = c("scale", "number")
        ) %>%
        separate_wider_delim(
            "question_text",
            delim = ":",
            names = c("question", "answer")
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
        mirt::itemstats()

    alpha_value = question_stats$overall$alpha

    question_itemstats <- dplyr::bind_cols(
        question_stats$itemstats,
        question_stats$proportions
    ) %>%
        mutate(alpha = alpha_value) %>%
        rownames_to_column("item") %>%
        as_tibble() %>%
        rename(
            "prop_1" = "1",
            "prop_2" = "2",
            "prop_3" = "3",
            "prop_4" = "4"
        )
    return(question_itemstats)
}

agnc_1 <- widen_question("q14", "Agency", 5)
agnc_2 <- widen_question("q15", "Agency", 5)
team <- widen_question("q29", "Teamwork", 9)
lead <- widen_question("q30", "Leadership", 7)
empt <- widen_question("q31", "Empathy", 5)
#widen_question("q32", "Decision", 9)

agnc <-
    agnc_2 %>%
    mutate(number = as.character(as.numeric(number) + 5)) %>%
    bind_rows(agnc_1) %>%
    arrange(respondent_id, number)

agnc_itemstats <- get_itemstats(agnc)
team_itemstats <- get_itemstats(team)
lead_itemstats <- get_itemstats(lead)
empt_itemstats <- get_itemstats(empt)

