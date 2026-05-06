library(janitor)
library(openxlsx)
library(stringi)
library(tidyverse)

lb_path = "data/excel/LB Mentores 25-26 260505.csv"
lf_path = "data/excel/LF Mentores 25-26 260505.csv"

cols_id <- c(
    "nombre" = "nombre_completo_nombre",
    "apellido_1" = "nombre_completo_apellido_1",
    "apellido_2" = "nombre_completo_apellido_2",
    "celular" = "celular_diez_numeros",
    "email" = "correo_electronico",
    "curp" = "curp_18_digitos_alfanumerico_aqui_puedes_consultarlo_en_caso_de_no_tenerlo_a_la_mano_https_www_gob_mx_curp",
    "edad" = "edad_anos_cumplidos",
    "sexo",
    "estado" = "estado_donde_vives",
    "sede" = "selecciona_tu_sede",
    "respondent_id",
    "last_update_date"
)

reemplazos_nombres <-
    tibble(
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

text_cleaning <- function(m_df) {
    m_df_cleaned <- m_df %>%
        mutate(across(
            where(is.character),
            ~ stringr::str_to_title(.x) %>%
                stringr::str_squish()
        )) %>%
        mutate(
            across(any_of(c("id", "email")), ~ stringr::str_to_lower(.x)),
            celular = as.character(celular)
        ) %>%
        filter(
            !is.na(apellido_2) &
                stringr::str_length(nombre) > 3 &
                !nombre %in% c("Prueba")
        ) %>%
        tidyr::unite(
            nombre_completo,
            nombre,
            apellido_1,
            apellido_2,
            sep = " "
        ) %>%
        mutate(
            nombre_completo = stringi::stri_trans_general(
                nombre_completo,
                "latin-ascii"
            )
        ) %>%
        mutate(
            nombre_completo = dplyr::replace_values(
                nombre_completo,
                from = reemplazos_nombres$from,
                to = reemplazos_nombres$to
            )
        ) %>%
        distinct()
    return(m_df_cleaned)
}

lb_mentores <- read_csv(lb_path) %>%
    janitor::clean_names() %>%
    select(all_of(cols_id)) %>%
    text_cleaning() %>%
    mutate(
        inicia = TRUE,
        respondent_id = as.character(respondent_id)
    ) %>%
    rename("respondent_id_lb" = "respondent_id") %>%
    group_by(nombre_completo) %>%
    arrange(nombre_completo) %>%
    filter(last_update_date == max(last_update_date)) %>%
    ungroup()

lf_mentores <- read_csv(lf_path) %>%
    janitor::clean_names() %>%
    select("id", all_of(cols_id)) %>%
    text_cleaning() %>%
    mutate(
        concluye = TRUE,
        respondent_id = as.character(respondent_id)
    ) %>%
    rename("respondent_id_lf" = "respondent_id") %>%
    group_by(nombre_completo) %>%
    arrange(nombre_completo) %>%
    filter(last_update_date == max(last_update_date)) %>%
    ungroup()

lb_respondent <- select(lb_mentores, c("nombre_completo", "respondent_id_lb"))
lf_respondent <- select(lf_mentores, c("nombre_completo", "respondent_id_lf"))
lb_mentores <- select(lb_mentores, -c("respondent_id_lb", "last_update_date"))
lf_mentores <- select(lf_mentores, -c("respondent_id_lf", "last_update_date"))

mentores_ids <-
    full_join(
        select(lb_mentores, -any_of(c("celular", "email", "curp"))),
        select(lf_mentores, -any_of(c("celular", "email", "curp"))),
        by = "nombre_completo"
    ) %>%
    mutate(
        edad = coalesce(edad.x, edad.y),
        sexo = coalesce(sexo.x, sexo.y),
        estado = coalesce(estado.x, estado.y),
        sede = coalesce(sede.x, sede.y)
    ) %>%
    select(-matches("\\.(x|y)$")) %>%
    mutate(
        estado = stringr::str_replace(estado, "De", "de"),
        across(all_of(c("inicia", "concluye")), ~ ifelse(is.na(.x), FALSE, .x))
    ) %>%
    mutate(
        status = case_when(
            inicia & concluye ~ "Graduado",
            inicia & !concluye ~ "No graduado",
            !inicia & concluye ~ "Graduado, no inició"
        )
    ) %>%
    distinct()

mentores_ids <-
    reduce(
        list(mentores_ids, lb_respondent, lf_respondent),
        ~ left_join(.x, .y, by = "nombre_completo")
    ) %>% 
  distinct()

write_csv(mentores_ids, "output/seguimiento/mentores-25-25-ids.csv")
