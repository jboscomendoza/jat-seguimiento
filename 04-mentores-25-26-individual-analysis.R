library(tidyverse)

output_path <- "output/mentores 25-26"

read_rds(paste0(output_path, "/lb-scales-mentores-25-26.rds"))
read_rds(paste0(output_path, "/lb-psychometrics-mentores-25-26.rds"))
lb_summary <- read_rds(paste0(output_path, "/lb-statistics-mentores-25-26.rds"))

read_rds(paste0(output_path, "/lf-scales-mentores-25-26.rds"))
read_rds(paste0(output_path, "/lf-psychometrics-mentores-25-26.rds"))
lf_summary <- read_rds(paste0(output_path, "/lf-statistics-mentores-25-26.rds"))

grupos <- c(
    "nacional" = "nacional",
    "sexo" = "sexo",
    "edad" = "edad",
    "estado" = "estado",
    "sede" = "sede"
)

comp_summary <- 
  map(grupos, function(grupo_x) {
    inner_join(
        map_df(lb_summary, grupo_x),
        map_df(lf_summary, grupo_x),
        by = c("Scale", stringr::str_to_title(grupo_x)),
        suffix = c("_lb", "_lf")
    ) %>%
        select(-starts_with("Type")) %>%
        mutate(Diferencia = Mean_lf - Mean_lb) %>%
        select(
            all_of(c("Scale", stringr::str_to_title(grupo_x))),
            starts_with("Mean"),
            all_of("Diferencia"),
            starts_with("Sd"),
            starts_with("Se"),
            starts_with("N")
        )
})