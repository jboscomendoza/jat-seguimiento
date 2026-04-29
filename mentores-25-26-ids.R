library(tidyverse)
library(janitor)
library(stringi)

lb_path = "data/excel/LB-Mentores-JAT-25-26.csv"
lf_path = "data/excel/LF-Mentores-JAT-25-26.csv"

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
    "sede" = "selecciona_tu_sede"
)

text_cleaning <- function(m_df) {
    m_df_cleaned <- m_df %>%
        mutate(across(
            where(is.character),
            ~ stringr::str_to_title(.x) %>%
                stringr::str_squish()
        )) %>%
        mutate(
            across(any_of(c("email")), ~ stringr::str_to_lower(.x)),
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
        distinct()
    return(m_df_cleaned)
}

lb_mentores <- read_csv(lb_path) %>%
    janitor::clean_names() %>%
    select(all_of(cols_id)) %>%
    text_cleaning()

lf_mentores <- read_csv(lf_path) %>%
    janitor::clean_names() %>%
    select("id", all_of(cols_id)) %>%
    text_cleaning()