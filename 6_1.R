library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

#Ver cual es mi working directory.
getwd()
#Ver donde se guardan los datos de dslabs.
path <- system.file("extdata", package = "dslabs")
#Ver los archivos en esa carpeta.
list.files(path)
#Ver la direccion completa para un archivo en esa carpeta.
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
#Copiar el archivo de donde estaba al working directory.
file.copy(fullpath, getwd())
#Checar si ese archivo ya existe en el wd.
file.exists(filename)

#Podemos importar un documento con read_csv(), read_tsv(), read_delim() y otros.
#Para saber con que funcion importamos un documento, podemos ver sus primeras filas.
read_lines("murders.csv", n_max = 3)
#En este caso podemos ver que son datos separados por comas (CSV) y que la primera fila
#no son datos, sino un encabezado. Por lo tanto usaremos read_csv().
dat <- read_csv(filename)
#Tambien la podemos leer con su direccion completa.
dat <- read_csv(fullpath)
#Nos aparece una lista de que tipo de dato hay en cada columna.
head(dat)
#Estas funciones tambien existen fuera del tidyverse como read.csv(), pero son mejores
#en el tidyverse.

#Tambien es posible cargar datos directamente de internet si tenemos el link.
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
#La podemos descargar para tener una copia en la computadora.
download.file(url, "murders.csv")
#Si queremos descargar la base de datos pero que desaparezca despues, puede ser una base
#temporal con un nombre temporal.
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

file.copy(file.path(path, "murders.csv"), getwd())
setwd("data")
file.copy(file.path(path, filename), getwd())
