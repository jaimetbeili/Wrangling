#Tema 1
#Vamos a volver a usar los datos de wikipedia, asi que los ponemos en una tabla.
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))
head(murders_raw)

#Hasta ahora todo bien, pero algunos de los datos son texto en lugar de ser numeros.
class(murders_raw$population)
class(murders_raw$total)
#Esto sucede porque algunas paginas usan la coma como separador de miles.

#PAUSA
#Recordemos algunas cosas. Para definir un string en r se usan "" o '', pero nunca ``
s <- "Hello!"
s <- 'Hello!'
s <- `Hello!`

#Pero que pasa si quiero definir un string que contenga las comillas en si mismas.
#Si quiero un string que diga 10" y escribo esto: s <- "10""
#Me lo marca como si hubiera algo mas que agregar, por las comillas que quedaron abiertas.
#Lo correcto seria usar las comillas individuales:
s <- '10"'
#Podemos ver como queda el string usando la funcion cat()
cat(s)

#Si queremos un string que incluya ', lo definimos con "
s <-"5'"
cat(s)

#Ahora, si queremos un string que contenga ambos, tenemos que usar un \ asi:
s <- '5\'10"'
#El \ nos permite escaparnos de la comilla con la que abrimos.
#Asi  podemos agregar otra comilla sin cerrar el string.
cat(s)

#Otra forma de hacerlo, pero escapando a las comillas dobles.
s <- "5'10\""
cat(s)

#AHORA SI, RETOMANDO DONDE ESTABAMOS PERO REDEFINIENDO LA TABLA
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table()
murders_raw <- tab %>% setNames(c("state", "population", "total", "murders",
                                  "gun_murders", "gun_ownership", "total_rate",
                                  "murder_rate", "gun_murder_rate"))
head(murders_raw)


#Estos son los primeros tres strings del vector pobalacion en la tabla de asesinatos.
murders_raw$population[1:3]
#Convertirlos directamente en numeros no funciona por sus comas.
as.numeric(murders_raw$population[1:3])

#Tenemos que quitar las comas. R tiene funciones para hacerlo, pero hay un paquete
#que se llama stringr que funciona mejor y es mas facil de usar.
#De saque podemos checar que variables tienen comas.
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
#Esta es la forma de checar solo una variable:
any(str_detect(murders_raw$population, ","))

#Usamos str_replace_all para quitarlas, pero sigue siendo texto. Hay que pasarlo a numero.
test_1 <- str_replace_all(murders_raw$population, ",", "")
class(test_1)
test_1 <- as.numeric(test_1)
test_1

#Quitar comas y convertir a numeros se puede hacer directamente con esta funcion:
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2) #Aparece que no por un error en los datos, pero si funciona.

#Modificamos con parse_number las columnas 2 y 3.
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
head(murders_new)

#Tema 2
#El origen de la base de datos Heights es una pagina de internet.
library(dslabs)
data(reported_heights)
#Cuando se sacaron los datos de los alumnos, algunos no contestaron en inches, por lo que
#se guardo la variable como character.
class(reported_heights$height)

#Si lo conviertes a numeros algunos no se convierten y tienes muchos NAs.
x <- as.numeric(reported_heights$height)
#Muchos si son en inches, pero tienes 81 que son NAs.
head(x)
sum(is.na(x))

#Podemos tratar de encontrar patterns en los errores para ver que hacer.
#Primero nos quedamos solo con los NAs.
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)
#Por ejemplo, en este caso vemos que hay gente que puso un numero, luego un '
#y luego otro numero con " o algo asi. Esos lo pusieron en pies en lugar de 
#pulgadas. Tambien hay quienes pusieron cm.

#Para crear codigo que resuelva estos problemas encontrados en patterns, nos
#quedaremos solo con NAs o valores demasiado extremos para ser ciertos.
#Primero vamos a calcular los limites en los que esta el 99.999% de la gente.
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

#Luego vamos a tomar los valores que sean NAs o esten fuera de esos limites.
#Hacemos eso porque puede haber gente que si registrara su estatura en puro
#numero, pero que lo dejara en pies o en centimetros.
#Programamos esta funcion para que haga eso.
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

#Vemos que el total de errores son 292.
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

#El primer pattern que encontramos es con ' y ".
#Sacamos diez ejemplos:
# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

#El segundo pattern es de gente que puso un numero con punto o coma.
#Sacamos diez ejemplos:
# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

#El tercero pattern es de gente que puso centimetros.
#Sacamos diez ejemplos:
# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat
#Trataremos de resolver cada uno.

#PRIMER TIPO DE PROBLEMAS: ' y "

#Con str_detect podemos encontrar patterns. Por ejemplo, busquemos una coma.
pattern <- ","
str_detect(murders_raw$total, pattern) 

#str_detect nos dice donde hay. str_subset nos dice cuales son.
str_subset(reported_heights$height, "cm")

#Pongamos un vector yes que usa las palabras cm e inches y un vector no que solo
#usa numeros. Metemos a ambos vectores en s.
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
#Or (|)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")
str_detect(s, "inches")
str_subset(s, "cm|inches")

#\d significa any digit from 0 to 9. Se usa con dos diagonales (\\d).
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

#Podemos usar la funcion str_view para encontrar la primera vez que se presenta
#el pattern en cada elemento de un vector.
str_view(s, pattern)
#Y str_view_all para ver todas las veces que esto sucede.
str_view_all(s, pattern)

#Si queremos ver todos los 5 y 6, podemos utilizar [].
str_view(s, "[56]") #Tambien funciona con un espacio entre el 5 y el 6.

#Tambien lo podemos usar para un rango. Por ejemplo, del 4 al 7.
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")
#Funciona tambien con letras "[a-z]" son todas las minusculas de la a a la z
#"[A-Z]" todas las mayusculas. "[a-zA-Z]" son todas las letras.
#El tema es que todo aqui son caracteres. Si le ponemos "[1-20]" me va a buscar
#todo del 1 al dos y ademas el digito 0.

#Se pueden usar anclas para decirle a R que buscamos algo que empiece o termine
#en lugares especificos. ^ es para hablar de inicios y $ de finales.
#Por ejemplo ^\\d$ es empieza, sigue un digito y cierra. Solo detecta los strings
#con un digito.
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
#El uno no te lo marca porque tiene un espacio antes.

#Si lo queremos para uno o dos digitos, usariamos ^\\d{1,2}$
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

#Si lo queremos especificamente para dos digitos usariamos ^\\d{2}$
pattern <- "^\\d{2}$"
str_view(c(yes, no), pattern)

#Entonces para buscar pies y pulgadas usamos esto, donde decimos que empieza con
#un numero entre el 4 y el 7, luego un ' despues uno o dos digitos y luego un ".
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)


#Solo 14 de los problemas encontrados estan en este formato de pies y pulgadas
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

#No todos cuadran con nuestro pattern.
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
#Algunos usaron la palabra inches, otros usaron dos veces el ', en lugar de ".
str_subset(problems, "inches")
str_subset(problems, "''")

#Vamos a homogeneizar para que todos los pies tengan un ' y todas las pulgadas
#no tengan ningun simbolo.
#Entonces, nuestro pattern ya no tendra un " de pulgadas al final y quedaria asi:
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()
#Ahora en lugar de tener solo 14 problemas con este pattern tenemos 48.

#R si reconoce los espacios, hay que quitarlos. El espacio se reperesenta como s\
identical("Hi", "Hi ")
#Vamos a ver si alguno de nuestros errores tiene espacios entre pies y pulgadas.
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

#Pero no queremos tener que usar un pattern para espacios y otro para no espacios.
#En Regex, * significa 0 o mas repeticiones de un caracter.
#Este codigo va a buscar todos los 1 entre una A y una B:
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

yes <- c("AB","A1B", "A11B", "A111B", "A1111B", "A21B")
#En Regex, ? significa 0 o 1 repeticion de un caracter.
#En Regex, + significa 1 o mas repeticiones de un caracter.
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B", "A21B"),
           none_or_more = str_detect(yes, "A1*B"),
           none_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

#Los tres simbolos nos van a ser de utilidad, pero por lo pronto podemos anadir nada mas *
#en nuestro pattern para que detecte espacios. Tanto antes como despues del '.
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>%
  sum
#Ya llegamos a 53. No podemos quitar todos los espacios porque una persona que puso
#6 3, acabaria midiendo 63.

#SEGUNDO TIPO DE PROBLEMAS: x.y; x,y; x y

#No podemos simplemente quitar todos los puntos y pasarlos a ' porque una persona
#que puso 70.5 mediria 70'5. Para eso podemos usar grupos.
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7]),(\\d*)$"
#Dividir un pattern en grupos no afecta en como se detecta el pattern:
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
#Ahora, con la funcion str_match se crea una tabla. En la primera columna esta el valor
#identificado por el pattern. En las columnas subsecuentes, esta dividido en grupos.
str_match(s, pattern_with_groups)
#La funcion str_extract solo nos daria la primera columna de str_match.
str_extract(s, pattern_with_groups)

#En Regex \\1 es el valor del primer grupo, \\2 es el valor del segundo grupo...
#Podemos reemplazar la , por ' en todos los valores que concuerden con el patern
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

#Ahora usando grupos podemos crear un pattern que resuelva el problema dos.
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(problems, pattern_with_groups) %>% head
#Y podemos reemplazarlos
str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") %>% head
#El unico problema es que una persona reporto 25 pulgadas (12in = 1ft)
#pero eso lo dejamos para mas adelante.

#Vamos a poner una funcion que resuelva todos los problemas que hemos visto
#hasta ahorita (tomando en cuenta que algunos siguen en cm)
#Primero creamos esta funcion para identificar los problemas que no son ni pulgadas ni cm.
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
#Metemos nuestros datos en esa funcion. Tenemos 200 problemas.
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

#Ahora vamos a hacer los cambios que ya conocemos:
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") #quitar , . y espacios

#Con estos cambios ya resolvimos el 61.5% de nuestros problemas. Ya se encuentran
#dentro de este pattern:
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

#Los restantes se ven asi:
converted[!index]
#Los problemas restantes incluyen alumnos que pusieron solo los pies que miden,
#o quienes usaron letras, entre otros.


pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

#Tema 3
#Ya aprendimos a poner los datos en pies y pulgadas, pero queremos convertirlo a pulgadas
#Usemos un caso simple para ver como se hace.
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
#Los separamos en pies y pulgadas.
tab %>% separate(x, c("feet", "inches"), sep = "'")
#Podemos hacer exactamente lo mismo con extract.
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
#Usamos extract porque asi involucramos grupos, lo que nos da mayor flexibilidad.
#Por ejemplo, si tenemos estos datos y queremos solo los numeros separate
#no funciona, extract si.
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#En la seccion pasada vimos que quedaban algunos temas por resolver. Aqui veremos
#como se resuelven 7 probelmas menores diferentes.

#1. Personas que pusieron solo x
#Solucion: Agregar un '0 al final
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")
#2. Personas que pusieron solo x'
#Solucion: Agregar un 0 al final
str_replace(s, "^([56])'?$", "\\1'0") #Solo usamos 5 y 6 porque es demasiado raro medir
#justo 4 o justo 7 pies.
#3. Personas que pusieron inches con decimales
#Solucion: Cuando haggamos la correccion (linea 350) cambiamos el pattern.
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
#4. Personas que pusieron metros con comas.
#Solucion: Convertir a puntos y borrar espacios
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
#5. Personas que pusieron espacios al final.
#Solucion: Trimearlos. Hay formula que lo hace automatico.
str_trim("5 ' 9 ")
#6. Personas que pusieron palabras y usaron mayusculas
#Solucion: Convertir todo a minusculas
s <- c("Five Feet Eight Inches")
str_to_lower(s)
#7. Personas que pusieron palabras en lugar de numeros.
#Solucion: Funcion que convierte palabras a numeros.
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

#AHORA PONEMOS ESTOS 7 CAMBIOS MAS EL PATTERN QUE YA TENIAMOS EN UNA SOLA FORMULA
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

#Y RESOLVEMOS NUESTROS PROBLEMAS
converted <- problems %>% words_to_numbers %>% convert_format
#Ya con esto nos quedan muy pocos problemas.
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#El codigo completo quedaria asi:
#Fijamos el pattern
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
#Ponemos los limites
smallest <- 50
tallest <- 84
#Creamos un nuevo objeto new_heights
new_heights <- reported_heights %>% 
  mutate(original = height,  #Usamos las funciones que acabamos de crear
         height = words_to_numbers(height) %>% convert_format()) %>% 
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%  #extraemos los numeros
  mutate_at(c("height", "feet", "inches"), as.numeric) %>% #guardamos como numeric
  mutate(guess = 12*feet + inches) %>% #formula para convertir pies a pulgadas
  mutate(height = case_when( #Que hacer en cada caso
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

#Estas son todas las correciones que hicimos
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

#Los mas chaparros
new_heights %>% arrange(height) %>% head(n=7)
#Otro tema muy comun en wrangling es string splitting. Empezamos con un ejemplo
#Si leemos un CSV con la funcion que viene en R, tendriamos el texto de cada columna
#acumulado en un solo string.
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()
#Queremos separar los valres con comas. La funcion str_split lo hace
x <- str_split(lines, ",") 
x %>% head()
#Podemos separar la primera fila que era nombres de columnas
col_names <- x[[1]]
x <- x[-1]

#Hay una funcion en la libreria purrr que nos ayuda a convertir esto en un data frame
library(purrr)
#Esto nos da la primera columna.
map(x, function(y) y[1]) %>% head()
#Tambien se puede escribir asi
map(x,1) %>% head()
#Para todas las columnas (con sus tipos de valor)
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)
dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 
dat %>% head

#Pero nos podemos evitar todo esto. Si le ponemos simplify a str_split
#Con ese argumento nos devuleve ya una matriz en lugar de una lista.
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

#EJEMPLO DE SACAR DATOS DE UN PDF
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]
#Igual los datos ya estan en dslabs
data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)
#No esta muy explicado, pero tampoco tiene nada nuevo.

#Recoding es cambiar el nombre de categoricals
#Para esto se puede usar el case_when que ya se introdujo antes o la funcion recode
#Usemos un ejemplo.
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
#Tenemos esta grafica con nombres de paises muy largos.
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 
#Recode nos ayuda a cambiar el nombre de paises a mas cortos
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()







