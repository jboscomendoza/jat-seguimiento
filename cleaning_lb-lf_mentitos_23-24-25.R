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
cols_scales <-
  c(
    "liderazgo" = "liderazgo",
    "empatia" = "empatia",
    "decision" = "decision",
    "equipo" = "equipo"
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
  "apellido_materno"
)

data_paths <- list(
  "lb_mentitos_24-25" = "data/excel/LB mentitos 24-25.xlsx",
  "lb_mentitos_23-24" = "data/excel/LB mentitos 23-24.xlsx",
  "lf_mentitos_23-24" = "data/excel/LF mentitos 23-24.xlsx",
  "lf_mentitos_24-25" = "data/excel/LF mentitos 24-25.xlsx",
  "lf_mentitos_24-25_ecatnl" = "data/excel/LF mentitos 24-25 ecat-nl.xlsx"
)

# Read and processing ----
data_raw <-
  data_paths %>%
  map(read_excel) %>%
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
        edad = ifelse(dplyr::between(edad, 10, 30), edad, NA),
        municipio = stringr::str_to_sentence(municipio),
        modelo_seguir = ifelse(
          stringr::str_detect(modelo_seguir, "Sí"),
          "Sí",
          "No"
        )
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

data_raw[["lf_mentitos_24-25"]] <- bind_rows(
  data_raw[["lf_mentitos_24-25"]],
  data_raw[["lf_mentitos_24-25_ecatnl"]]
)

data_raw <- purrr::discard_at(data_raw, "lf_mentitos_24-25_ecatnl")

# Scales ----
data_scales <- map(data_raw, function(x_df) {
  map_df(cols_scales, function(x_col) {
    x_df %>% 
      select(starts_with(x_col)) %>% 
      rowSums(na.rm = TRUE)
  })
})

# Export ----
data_export <- map2(data_raw, data_scales, bind_cols)

map(names(data_export), function(x_name) {
  path_export = paste0("data/parquet/", x_name, ".parquet")
  write_parquet(data_export[[x_name]], path_export)
})
