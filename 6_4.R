#FECHAS Y HORAS
#Es comun encontrarnos con datos guardados como fechas, por lo que R tiene una categoria
#para este tipo de informacion (Date). Las fechas se pueden convertir en numeros.
#El 1 de enero de 1970 es el dia uno en lenguajes de computacion.
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

#ggplot sabe reconocer fechas
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

#tidyverse tiene funciones especiales para tratar fechas, se llaman lubridate
library(lubridate)
#Para ver como funciona tomemos 10 fechas al azar
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

#Podemos extraer el dia mes y ano de cada fecha:
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

dias <- day(dates)
dias
meses <- month(dates[3])
meses
#Tambien los nombres de los meses.
month(dates, label = TRUE)

#Podemos convertir cualquier modelo de fecha a YYYY-MM-DD con la funcion ymd:
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

#Pero la fecha puede escribirse de formas enganosas, por ejemplo 09-01-02, donde no sabes
#cual es el mes/dia/ano. Checas tus datos para ver cual es cual y luego puedes usar la
#funcion correspondiente:
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

#Otra utilidad de lubridate es que la podemos usar para tiempo. R tiene la funcion
Sys.time()

#Pero lubridate tiene now, que es mas avanzada porque te permite usar timezones
now()    # current time in your time zone
now("GMT")    # current time in GMT
OlsonNames()
now("America/Mexico_City")

now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

#Tambien puede traducir strings a horarios
x <- c("12:34:56")
hms(x)

#Finalmente, podemos convertir horarios y fechas
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

#TEXTO
#Algunas veces la informacion viene en forma de texto, no en numeros.
#Ejemplo: Analisis de lenguaje. Aqui vemos algunas herramientas para texto.
#Usaremos estas librerias y un ejemplo con Donald Trump:
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

#During the 2016 US presidential election, then-candidate Donald Trump used his
#Twitter account as a way to communicate with potential voters. On August 6, 2016 Todd
#Vaziri tweeted about Trump that "Every non-hyperbolic tweet is from iPhone (his staff).
#Every hyperbolic tweet is from Android (from him)." Data scientist David Robinson
#conducted an analysis to determine if data supported this assertion.
#Here we go through David's analysis to learn some of the basics of text mining.

#Los datos que usaremos traian este mensaje:
#In general, we can extract data directly from Twitter using the rtweet package.
#However, in this case, a group has already compiled data for us and made
#it available at https://www.thetrumparchive.com/ 
url <- 'https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at,
                                      orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 
#Los datos estan ya en dslabs.
library(dslabs)
data("trump_tweets")
head(trump_tweets)
#Y las variables que trae son estas:
names(trump_tweets)

#Los tweets estan representados por la variable text.
trump_tweets %>% select(text) %>% head
#La variable source nos dice de que dispositivo salio.
trump_tweets %>% count(source) %>% arrange(desc(n))

#Creamos una tabla que cuente el numero de tweets por fuente. Solo tomamos los que
#empiecen con "Twitter for" y quitamos esa frase.
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

#Solo queremos los de la campana y que no sean retweets. Ademas, solo de iPhone y Android
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

#Esta grafica parece indicar que hay dos personas o entes diferentes usando las cuentas
#de iPhone y de Android. Es el porcentaje de tweets por fuente y por hora (en EST).
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "Source")

#Ahora si, podemos empezar con el texto
library(tidytext)

#Lo principal que va a hacer tidytext es encontrar tokens, que son unidades de texo que
#vamos a analizar. Pueden ser palabras, frases o caracteres.
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,",
                               "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

#Ahora un ejemplo con un Tweet
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#El problema aqui es que R no considera como tokens el # o @, y en Twitter importan.
#Podemos definir un pattern que los traiga.
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Tambien queremos quitar los links a imagenes.
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Y ya podemos hacer de cada palabra una observacion.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#Podemos ver la palabra mas comun:
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

#Las primeras palabras que aparecen son stop words. No son importantes (the, to, and, a
# y otras como esas). Pero tidytext tiene un catalofo de esas, solo hay que quitarlas
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

#Este top 10 de palabras de Trump si son mas significativas
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#Todavia hay algunos cambios que hacer. Quitar numeros (tipo anos) y quitar puntos como
#la comilla ' al principio de un quote.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

#Ahora si, queremos ver si las palabras de los tweets vienen de iphone o de android.
#Las asociamos a un indice "or" que aumenta si la palabra es mas de android y
#disminuye si es mas de iphone.
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#Para evitar que sean todas las palabras, usamos las que suman al menos 100 menciones.
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

#Lo siguiente seria hacer analisis de sentimientos.
sentiments
#Los sentimientos en Bing dividen en positivos y negativos.
get_sentiments("bing")
#En afinn se asigna un valor entre -5 y 5 dependiendo del sentimiento
get_sentiments("afinn")

#loughran y nrc contienen muchos tipos de sentimientos
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

#nrc es el mas completo, asi que ese usaremos
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

#Podemos combinar palabras con sentimientos con inner_join. Este comando solo nos dejara
#las palabras que si se puedan asociar a algun sentimiento.
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

#Analizar cada tweet seria complicado porque puede tener palabras positivas y negativas.
#Lo que hacemos es ver cuantas palabaras sentimentales vienen de cada source.
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts
#Se nota que habian en total mas palabras en Android que en iPhone...
tweet_words %>% group_by(source) %>% summarize(n = n())
#Pero podemos resolver eso usando el or que hicimos antes
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

#Para confirmar esto podemos anadir un intervalo de confianza
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))
log_or

#Que podemos analizar graficamente.
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

#Podemos ver que palabras generan estos sentimientos
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

#Y verlo graficamente
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


table(hour(movielens$timestamp))
newdates <- as_datetime(movielens$timestamp)
head(newdates)
table(hour(newdates))

#El paquete gutenbergr importa libros de Gutenberg!!!!!
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gut <- gutenberg_metadata

gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

gutenberg_works(title == "Pride and Prejudice")$gutenberg_id


book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
nrow(words)

words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()

words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n)) 


afinn <- get_sentiments("afinn")
words %>% inner_join(afinn, by = "word") %>% 
  select(word, value) %>%
  filter(value > 0)

