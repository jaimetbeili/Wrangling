library(tidyverse)
library(pdftools)
library(pdftools)
library(stringr)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))
txt <- pdf_text(fn)
View(txt)

x <- str_split(txt[9], "\n")
class(x)
length(x)

s <- x[[1]]
class(s)
length(s)

s <- str_trim(s)
s[1]

header_index <- str_which(s, "2015")[1]
header_index

tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]
month
remove(tmp)

tail_index <- str_which(s, "Total")[1]
tail_index

n <- str_count(s, "\\d+")
sum(n == 1)

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")
mean(tab$"2016")
mean(tab$"2017"[1:19])
mean(tab$"2017"[20:30])

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()





