# Setup ----
# R >= 4.5
# arrow >= 23.0.1.1
# readxl >= 1.4.5
# tidyverse >= 2.0.0
library(arrow)
library(readxl)
library(tidyverse)

if (!dir.exists("data/parquet")) {
  dir.create("data/parquet", recursive = TRUE)
} else {
  message("data/parquet already exists.")
}

# Variables ---
cols_scales <- c(
  "awareness" = "awareness",
  "autogestion" = "autogestion",
  "autoeficacia" = "autoeficacia",
  "comunicacion" = "comunicacion",
  "connection" = "connection",
  "creatividad" = "creatividad",
  "decisiones" = "decisiones",
  "insight" = "insight",
  "liderazgo" = "liderazgo",
  "purpose" = "purpose"
)

na_pattern <- c("Prefiero no contestar", "No contestó")

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
  "custom_1",
  "nombre",
  "nombre_s",
  "apellido_paterno",
  "apellido_materno",
  "apellido1",
  "apellido2",
  "celular",
  "correo",
  "curp",
  "first_name",
  "last_name"
)

cols_to_text <- c(
  "sexo",
  "empleo",
  "trabajo",
  "materias",
  "celular",
  "eval_expectativas"
)

cols_to_numeric <- c(
  "amai",
  "promedio"
)

estados <- c(
  "nuevo león",
  "chihuahua",
  "ecatepec"
)

data_paths <- list(
  "lb_mentitos_21-22_ch" = "data/excel/LB mentitos 21-22 ch.xlsx",
  "lb_mentitos_21-22_nl" = "data/excel/LB mentitos 21-22 nl.xlsx",
  "lb_mentitos_22-23" = "data/excel/LB mentitos 22-23.xlsx",
  "lf_mentitos_21-22_ch" = "data/excel/LF mentitos 21-22 ch.xlsx",
  "lf_mentitos_21-22_nl" = "data/excel/LF mentitos 21-22 nl.xlsx",
  "lf_mentitos_22-23" = "data/excel/LF mentitos 22-23.xlsx",
  "lf_mentitos_22-23_vaciado" = "data/excel/LF mentitos 22-23 vaciado.xlsx"
)

data_raw <-
  map(data_paths, function(x_path) {
    if (stringr::str_detect(x_path, "22-23")) {
      map_df(estados, function(est) {
        path_df <- read_excel(x_path, sheet = est) %>%
          mutate(estado = est) %>%
          mutate(
            across(any_of(cols_to_text), as.character),
            across(starts_with(cols_to_numeric), as.numeric)
          )
      })
    } else if (stringr::str_detect(x_path, "nl")) {
      read_excel(x_path) %>%
        mutate(estado = "Nuevo León") %>%
        mutate(
          across(any_of(cols_to_text), as.character),
          across(starts_with(cols_to_numeric), as.numeric)
        )
    } else if (stringr::str_detect(x_path, "ch")) {
      read_excel(x_path) %>%
        mutate(estado = "Chihuahua") %>%
        mutate(
          across(any_of(cols_to_text), as.character),
          across(starts_with(cols_to_numeric), as.numeric)
        )
    }
  })


data_raw[["lb_mentitos_21-22"]] <- bind_rows(
  data_raw[["lb_mentitos_21-22_nl"]],
  data_raw[["lb_mentitos_21-22_ch"]]
)

data_raw[["lf_mentitos_21-22"]] <- bind_rows(
  data_raw[["lf_mentitos_21-22_nl"]],
  data_raw[["lf_mentitos_21-22_ch"]]
)

data_raw[["lf_mentitos_22-23"]] <- bind_rows(
  data_raw[["lf_mentitos_22-23"]],
  data_raw[["lf_mentitos_22-23_vaciado"]]
)

data_raw <- purrr::discard_at(
  data_raw,
  c(
    "lb_mentitos_21-22_nl",
    "lb_mentitos_21-22_ch",
    "lf_mentitos_21-22_nl",
    "lf_mentitos_21-22_ch",
    "lf_mentitos_22-23_vaciado"
  )
)

# Read and processing ----
data_process <-
  data_raw %>%
  map(function(x) {
    if (!"edad" %in% names(x)) {
      if (is.numeric(x[["fecha_nac"]])) {
        x[["edad"]] <- x[["fecha_nac"]]
      } else {
        x <- x %>%
          mutate(
            edad = lubridate::interval(
              lubridate::dmy(fecha_nac),
              lubridate::dmy("6-6-2023")
            ),
            edad = round(edad / lubridate::years(1))
          )
      }
    }
    x
  }) %>%
  map(function(x) {
    if ("socioeco" %in% names(x)) {
      x <- x %>%
        mutate(socioeco = ifelse(socioeco == 0, NA, socioeco))
    }
    x
  }) %>%
  map(function(x) {
    x %>%
      mutate(
        edad = ifelse(dplyr::between(edad, 10, 30), edad, NA)
      ) %>%
      mutate(
        across(
          where(is.character),
          ~ ifelse(.x %in% na_pattern, NA_character_, .x)
        ),
      ) %>%
      mutate(
        across(starts_with(cols_scales), as.numeric)
      ) %>%
      select(-any_of(cols_remove)) %>%
      distinct(id, .keep_all = TRUE)
  })

# Scales ----
data_scales <- map(data_process, function(x_df) {
  map_df(cols_scales, function(x_col) {
    x_df %>%
      select(starts_with(x_col)) %>%
      rowSums(na.rm = TRUE)
  })
})

# Export ----
data_export <- map2(data_process, data_scales, bind_cols)

map(names(data_export), function(x_name) {
  path_export = paste0("data/parquet/", x_name, ".parquet")
  write_parquet(data_export[[x_name]], path_export)
})
