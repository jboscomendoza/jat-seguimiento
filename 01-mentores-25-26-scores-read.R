library(googledrive)
library(janitor)
library(readxl)
library(tidyverse)

url_id_lb <- "1AB4ik3ccD0mHRSsroIcXied6oEvLck0Nv44eicIjsg8"
url_id_lf <- "1QWOZi7U_kg3ELUIvoQI35iso7ZcGKZGj6FPF0cSZ-38"

path_lb <- "data/excel/LB mentores 25-26.xlsx"
path_lf <- "data/excel/LF mentores 25-26.xlsx"

googledrive::drive_download(
    file = googledrive::as_id(url_id_lb),
    path = path_lb,
    overwrite = TRUE
)

googledrive::drive_download(
    file = googledrive::as_id(url_id_lf),
    path = path_lf,
    overwrite = TRUE
)
