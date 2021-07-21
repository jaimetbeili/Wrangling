#TEMA 1
library(tidyverse)
library(dslabs)
data(gapminder)

#Los datos aqui presentados estan bien acomodados y organizados.
tidy_data <- gapminder %>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#Funcionan perfecto para hacer esta grafica.
tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#Esos mismos datos se pueden encontrar con pesimo formato.
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

#La variable año ni siquiera es una variable, esta en los headers.
#Ademas, cada fila tiene mas de una sola observacion.
select(wide_data, country, `1960`:`1967`)
#Afortunadamente la podemos limpiar.

#El comando gather convierte columnas en filas.
#Primero le decimos como queremos que se llame la variable en las nuevas filas.
#Luego le decimos como queremos que se llama la informacion que estaba bajo las columnas originales.
#Finalmente le decimos que columnas convertir en filas.
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

#En el ultimo paso, tambien podemos decirle que columnas NO convertir.
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
head(new_tidy_data)

#El unico problema es que gather asume que todo es texto.
class(tidy_data$year)
class(new_tidy_data$year)
#Tenemos que convertirlos en numeros para poderlos usar.

#Gather tiene un argumento para eso. convert = TRUE.
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
head(new_tidy_data)
class(new_tidy_data$year)

#Ya funciona para graficar
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#Si por alguna razon queremos expandir una base de datos, tenemos la funcion spread.
#Primero le decimos que variable se va a convertir en el header de las columnas.
#Luego le decimos que variables va a usar para rellenar esas nuevas columnas.
new_wide_data <- new_tidy_data %>%
  spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

#Este ejemplo es bastante sencillo. Hay mas complicados y realistas.
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
#Dos variables: fertilidad y esperanza de vida.
raw_data <- read_csv(filename)
select(raw_data, 1:5)

#Lo primero es convertir las columnas en filas.
#El problema es que hay mas de una variable aqui, y estan mezcladas con los años.
#Llamemos la nueva columna key y sus valores value.
dat <- raw_data %>%
  gather(key, value, -country)
head(dat)

#Ahora el problema es separar las dos variables de los años. Usamos separate.
#Primero le decimos que columna queremos que separe en varias.
#Luego le decimos que nombres queremos que tengan las nuevas columnas.
#Finalmente le decimos que caracter separa lo que queremos separar.
dat %>% separate(key, c("year", "variable_name"), "_")

#La variable life_expectancy tambien tiene un _ por lo que estaba siendo dividida dos veces.
#Esto generaba un error porque dejaba valores flotando. Podemos resolver eso:
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), "_")
#fill = "right" es lo que hace que aparezcan los NA pero ya no como un error.
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), "_",
                 fill = "right")

#Sin embargo, hay una forma mas sencilla de resolver este problema.
#Solo le decimos a la funcion como actuar si encuentra un _ extra.
dat %>% separate(key, c("year", "variable_name"), "_", extra = "merge")

#Lo siguiente es crear una columna para cada variable. Ya vimos que se puede hacer con spread.
dat %>% separate(key, c("year", "variable_name"), "_", extra = "merge") %>%
  spread(variable_name, value)

#Lo mismo se puede hacer de una forma mucho menos eficiente:
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)
