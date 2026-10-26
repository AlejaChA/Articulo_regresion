###Instalacion de librerias###
install.packages("tidyverse")
install.packages("summarytools")
install.packages("skimr")
install.packages("readxl")

###Lectura de librerias###
library(tidyverse)
library(summarytools)
library(skimr)
library(readxl)

###Lectura de la base de datos###
url_datos <- "https://raw.githubusercontent.com/AlejaChA/Articulo_regresion/main/datos.xlsx"

# Descargar el archivo temporalmente en segundo plano
temp_file <- tempfile(fileext = ".xlsx")
download.file(url_datos, destfile = temp_file, mode = "wb")

# Leer la base de datos
datos <- read_excel(temp_file)
