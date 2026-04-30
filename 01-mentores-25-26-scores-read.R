library(googledrive)
library(janitor)
library(readxl)
library(tidyverse)

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

url_id_lb <- "1CzxR6Gn8MOsFnbQ871tIHoP1pn0v8JMqXH7zhBZXL1k"
url_id_lf <- "1QWOZi7U_kg3ELUIvoQI35iso7ZcGKZGj6FPF0cSZ-38"

path_lb <- "data/excel/LB mentores 25-26.xlsx"
path_lf <- "data/excel/LF mentores 25-26.xlsx"

googledrive::drive_download(
    file = googledrive::as_id(url_id_lb),
    path = path_lb
)

googledrive::drive_download(
    file = googledrive::as_id(url_id_lf),
    path = path_lf
)
