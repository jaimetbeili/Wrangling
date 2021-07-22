#CONFORME VAS CAMBIANDO LAS CARACTERISTICAS DE UNA BASE DE DATOS, REPETIR UNA FUNCION
#PUEDE CAMBIAR TUS RESULTADOS. POR EJEMPLO, SI QUERIAS ELIMINAR LA PRIMERA FILA DE UNA
#TABLA Y REPETISTE ESE PASO, ELIMINASTE LAS PRIMERAS DOS. SI VAS A ESTAR REPITIENDO,
#TIENES QUE HACERLO DESDE EL PRINCIPIO. REDEFINIR TUS OBJETOS O CREAR  NUEVOS EN LUGAR
#DE MODIFICAR LOS EXISTENTES.
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

#CONFORME VAS CAMBIANDO LAS CARACTERISTICAS DE UNA BASE DE DATOS, REPETIR UNA FUNCION
#PUEDE CAMBIAR TUS RESULTADOS. POR EJEMPLO, SI QUERIAS ELIMINAR LA PRIMERA FILA DE UNA
#TABLA Y REPETISTE ESE PASO, ELIMINASTE LAS PRIMERAS DOS. SI VAS A ESTAR REPITIENDO,
#TIENES QUE HACERLO DESDE EL PRINCIPIO. REDEFINIR TUS OBJETOS O CREAR  NUEVOS EN LUGAR
#DE MODIFICAR LOS EXISTENTES.
#Tema 2
#Vamos a ver como combinar tablas.
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(dslabs)
ds_theme_set()

#Tenemos aqui datos de asesinatos por estado y datos electorales por estado.
data(murders)
head(murders)
data(polls_us_election_2016)
head(results_us_election_2016)

#Podriamos simplemente pegar las dos tablas, pero el orden de los estados no es igual
identical(results_us_election_2016$state, murders$state)

#La funcion join junta las tablas pero haciendo coincidir la variable que le digamos.
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

#Ya la podemos graficar.
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

#En la practica, las dos tablas no siempre van a coincidir en los datos que tienen.
#En este caso ambas tenian los 50 estados, pero podemos crear un ejemplo donde no sea asi.
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(22, 44, 14, 1, 27, 45)) %>% select(state, electoral_votes)
tab2
#tab1 y tab2 tienen diferentes estados.

#left_join mantiene los estados de la primera tabla y pone NA cuando la segunda tabla no tenga datos.
left_join(tab1, tab2)
#right_join mantiene los estados de la segunda tabla y pone NA cuando la primera tabla no tenga datos.
right_join(tab1, tab2)
#inner_join mantiene los estados para los que ambas tablas tienen informacion.
inner_join(tab1, tab2)
#full_join mantiene todos los estados en ambas tablas, sin importar si tienen datos o no.
full_join(tab1, tab2)

#Las siguientes dos funciones no unen las dos tablas tal cual, mas bien modifican la existente.
#semi_join mantiene los estados en tab1 para los que hay datos en tab2, sin añadir los datos de tab2.
semi_join(tab1, tab2)
#anti_join mantiene los estados en tab1 para los que no hay datos en tab2. (contrario de semi_join).
anti_join(tab1, tab2)
#Otra forma de escribir las mismas funciones es con %>%.

tab1 %>% inner_join(tab2)
tab2 %>% anti_join(tab1)

#La funcion binding tambien junta tablas, pero no trata de hacer que coincidan datos.
#Es necesario que lo que vamos a combinar sea del mismo tamano, de otra forma nos da error.
#bind_cols nos hace una tablita con las columnas que le digamos.
bind_cols(a = 1:3, b = 4:6)
#Podemos hacer lo mismo con cbind(), pero nos crea un data frame, no una tablita impresa.

#Podemos romper una data frame y volverlo a reunir con bind_cols.
tab1 <- tab[, 1:3]
head(tab1)
tab2 <- tab[, 4:6]
head(tab2)
tab3 <- tab[, 7:9]
head(tab3)
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

#Lo mismo podemos hacer con bind_rows() pero para filas.
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
tab3 <- tab[5:6,]
bind_rows(tab1, tab2, tab3)

#Hay operadores que se pueden usar en vectores, pero tambien en data frames con dplyr cargado.
#Intersect en vectores (devuelve lo que esta en ambos).
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
#Intersect en data frame
tab1 <- tab[1:5,]
tab1
tab2 <- tab[3:7,]
tab2
intersect(tab1, tab2)

#Union en vectores (une sin repetir lo que esta en ambos).
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
#Union en data frame
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

#Diferencias en vectores (devuelve lo que esta en el primero pero no en el segundo).
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
setdiff(c("a","b","c"), c("b","c","d"))
setdiff(c("b","c","d"), c("a","b","c"))
#Diferencias en data frame
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)
setdiff(tab2, tab1)

#Equal en vectores (nos avisa si son iguales, sin importar el orden).
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

#CONFORME VAS CAMBIANDO LAS CARACTERISTICAS DE UNA BASE DE DATOS, REPETIR UNA FUNCION
#PUEDE CAMBIAR TUS RESULTADOS. POR EJEMPLO, SI QUERIAS ELIMINAR LA PRIMERA FILA DE UNA
#TABLA Y REPETISTE ESE PASO, ELIMINASTE LAS PRIMERAS DOS. SI VAS A ESTAR REPITIENDO,
#TIENES QUE HACERLO DESDE EL PRINCIPIO. REDEFINIR TUS OBJETOS O CREAR  NUEVOS EN LUGAR
#DE MODIFICAR LOS EXISTENTES.
#Tema 3
#Algunas veces la informacion que necesitamos no viene en spreadsheets. Tenemos que minarla de la web.
#Esto se conoce como webscraping y se empieza asi.
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
#h es un objeto xml, general markup language, que puede contener cualquier tipo de informacion.
class(h)
h

#En HTML el codigo se encuentra en grupitos marcados por <> que se llaman nodos.
#La funcion html_nodes() extrae todos los nodos del tipo que se le indique. 
#html_node() extrae el primer nodo del tipo que se le indique.
#Extraemos todas las tablas del url de wikipedia.
tab <- h %>% html_nodes("table")
#Solo queremos la segunda tabla
tab <- tab[[2]]
#La convertimos en datos utilizables, un data frame.
tab <- tab %>% html_table()
head(tab)
class(tab)

#Ahora cambiamos los nombres de las columnas porque los que trae wikipedia son largos.
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

#CONFORME VAS CAMBIANDO LAS CARACTERISTICAS DE UNA BASE DE DATOS, REPETIR UNA FUNCION
#PUEDE CAMBIAR TUS RESULTADOS. POR EJEMPLO, SI QUERIAS ELIMINAR LA PRIMERA FILA DE UNA
#TABLA Y REPETISTE ESE PASO, ELIMINASTE LAS PRIMERAS DOS. SI VAS A ESTAR REPITIENDO,
#TIENES QUE HACERLO DESDE EL PRINCIPIO. REDEFINIR TUS OBJETOS O CREAR  NUEVOS EN LUGAR
#DE MODIFICAR LOS EXISTENTES.