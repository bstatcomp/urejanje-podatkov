## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE, # do not interrupt in case of errors
  warnings = FALSE,
  message = FALSE
)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(openxlsx)
Sys.setlocale(category = "LC_ALL", locale = "Slovenian_Slovenia.1250")
TabelaA <- tibble(openxlsx::read.xlsx("./data-raw/Primer.xlsx", 
                                     rows = 3:15, cols = 1:8, 
                                     detectDates = TRUE))
TabelaB <- tibble(openxlsx::read.xlsx("./data-raw/Primer.xlsx", 
                                     rows = 21:34, cols = 1:8,
                                     detectDates = TRUE))
TabelaA <- TabelaA %>% mutate(
  ID_BZ             = as.integer(ID_BZ),
  ID_ZO             = as.integer(ID_ZO),
  Vrsta.storitve    = as.integer(Vrsta.storitve),
  Naziv.storitve    = factor(Naziv.storitve, 
                             levels = c("NBO", "SPP", "REH", "BOL")),
  Datum.začetka_a   = as.Date(Datum.začetka_a),
  Datum.zaključka_a = as.Date(Datum.zaključka_a),
  Leto.zaključka    = as.integer(Leto.zaključka),
  Trajanje_a        = as.double(Trajanje_a))

TabelaB <- TabelaB %>% mutate(
  ID.bolnišničnega.zdravljenja = as.integer(ID.bolnišničnega.zdravljenja),
  ID_ZO                        = as.integer(ID_ZO),
  Vrsta.storitve               = as.integer(Vrsta.storitve),
  Naziv.storitve               = factor(Naziv.storitve,
                                        levels = c("NBO", "SPP", "REH", "BOL")),
  Datum.začetka_b              = as.Date(Datum.začetka_b),
  Datum.zaključka_b            = as.Date(Datum.zaključka_b),
  Leto.zaključka.BZ            = as.integer(Leto.zaključka.BZ),
  Trajanja_b                   = as.double(Trajanja_b))

TabelaA
TabelaB



## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
RazsirjenA <- inner_join(TabelaA, TabelaB, by = "ID_ZO", 
                         suffix = c("", "_b"))
RazsirjenA <- RazsirjenA %>% 
    filter(Vrsta.storitve_b == 7) %>%
    filter(Datum.zaključka_a < Datum.začetka_b,
           Datum.začetka_b < Datum.zaključka_a + days(30))
RazsirjenA


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Dodajmo sprejeme.
TabelaA <- TabelaA %>% 
  mutate("Sprejem_DA/NE" = factor(ID_BZ %in% RazsirjenA$ID_BZ,
                                  levels = c(TRUE, FALSE),
                                  labels = c("Da", "Ne")))
#Dodajmo trajanja_b.
left_join(TabelaA, RazsirjenA %>% 
            select(ID_ZO, Trajanja_b), by = "ID_ZO", suffix = c("", "")) %>%
  mutate(Trajanja_b = replace_na(Trajanja_b, 0)) %>%
  select(ID_BZ, ID_ZO, "Sprejem_DA/NE", Trajanja_b)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
df <- read_csv("./data-raw/age-cases.csv")
df


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
df_tidy <- df %>% 
  pivot_longer(cols = starts_with("age"), values_to = "cumulative")
df_tidy


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
df_tidy <- df %>% 
  pivot_longer(cols = starts_with("age"), values_to = "cumulative") %>%
  separate(name, into = c("delete1", "sex", "age", "delete2"), "\\.")
df_tidy


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
colnames(df)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
df_tidy <- df %>%
  pivot_longer(cols = starts_with("age"), values_to = "cumulative") %>%
  filter(str_detect(name, "[:alpha:]*\\.[:alpha:]*\\.[0-9-]*\\.[:alpha:]*")) %>%
  separate("name", into = c("delete1", "sex", "age", "delete2"), sep = "\\.") %>%
  select(date, sex, age, cumulative)
df_tidy


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
df_tidy <- df %>%
  pivot_longer(cols = starts_with("age"), values_to = "cumulative") %>%
  filter(str_detect(name, "[:alpha:]*\\.[:alpha:]*\\.[0-9-]*\\.[:alpha:]*")) %>%
  separate("name", into = c("delete1", "sex", "age", "delete2"), sep = "\\.") %>%
  select(date, sex, age, cumulative) %>%
  mutate(cumulative = replace_na(cumulative, 0))
df_tidy


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
df_tmp <- filter(df_tidy, sex == "female", age == "25-34")
df_tmp$cumulative


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
x_cum <- c(1, 5, 6, 7)
x_dly <- x_cum - c(0, x_cum[1:3])
x_cum
x_dly


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
x_cum <- c(1, 5, 6, 7)
x_dly <- c(x_cum[1], diff(x_cum))
x_cum
x_dly


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
df_tidy <- df %>%
  pivot_longer(cols = starts_with("age"), values_to = "cumulative") %>%
  filter(str_detect(name, "[:alpha:]*\\.[:alpha:]*\\.[0-9-]*\\.[:alpha:]*")) %>%
  separate("name", into = c("delete1", "sex", "age", "delete2"), sep = "\\.") %>%
  select(date, sex, age, cumulative) %>%
  mutate(cumulative = replace_na(cumulative, 0)) %>%
  group_by(sex, age) %>%
  mutate(daily = c(cumulative[1], diff(cumulative)))
df_tidy
df_tidy %>%
  filter(sex == "female", age == "25-34")


## ---- message = FALSE, warning = FALSE---------------------------------------------------------------------------------------------------------------------------
dir.create("./data-clean/") # Če še ni mape data-clean jo ustvarimo.
write_csv2(df_tidy, "./data-clean/age-cases-tidy.csv")


## ---- fig.width = 7, fig.height = 5------------------------------------------------------------------------------------------------------------------------------
ggplot()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
pozari <- read_csv2("./data-raw/forest-fires.csv")
pozari


## ---- fig.width = 7, fig.height = 5------------------------------------------------------------------------------------------------------------------------------
pozari_per_m <- pozari %>% 
  mutate(month = factor(month, labels = c("jan", "feb", "mar", "apr", 
                                                   "maj", "jun", "jul", "avg", 
                                                   "sep", "okt", "nov", "dec"), 
                                 ordered = T)) %>% count(month)
ggplot(pozari_per_m, aes(x = month, y = n)) +
  geom_point()



## ---- fig.width = 7, fig.height = 5------------------------------------------------------------------------------------------------------------------------------
ggplot(pozari_per_m, aes(x = as.integer(month), y = n)) +
  geom_point() +
  geom_line(aes(colour = "red"))



## ---- , fig.width = 7, fig.height = 5----------------------------------------------------------------------------------------------------------------------------
ggplot(pozari, aes(x = month)) + geom_bar()


## ---- fig.width = 7, fig.height = 5------------------------------------------------------------------------------------------------------------------------------
pozari_xy <- pozari %>%
      group_by(X, Y) %>% 
  mutate(area_sum = sum(area))
ggplot(pozari_xy, aes(x = X, y = Y, fill = area_sum)) + 
  geom_tile()


## ---- fig.width = 7, fig.height = 5------------------------------------------------------------------------------------------------------------------------------
pozari_xy <- pozari %>%
      group_by(X, Y) %>% 
  mutate(area_sum = sum(area))
ggplot(pozari_xy, aes(x = X, y = Y, fill = area_sum)) + 
  geom_tile() +
  labs(x     = "X koordinata", y = "Y koordinata", 
       title = "Intenziteta požarov glede na koordinate.",
       fill  = "Površina")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
head(windowsFonts())


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(extrafont)
#font_import() # Naloži vse pisave, ki jih dobi.
loadfonts(device = "win", quiet = TRUE)
cat(str_c("Stevilo najdenih pisav: ", length(windowsFonts())))


## ---- fig.width = 7, fig.height = 5------------------------------------------------------------------------------------------------------------------------------
pozari_xy <- pozari %>%
      group_by(X, Y) %>% 
  mutate(area_sum = sum(area))
ggplot(pozari_xy, aes(x = X, y = Y, fill = area_sum)) + 
  geom_tile() +
  labs(x     = "X koordinata", y = "Y koordinata", 
       title = "Intenziteta požarov glede na koordinate.",
       fill  = "Površina") +
  theme(text=element_text(size=9, family="Ravie", face = "italic"))


## ----fig.width = 7, fig.height = 5, message = FALSE, warning = FALSE---------------------------------------------------------------------------------------------
graf <- ggplot(pozari_xy, aes(x = X, y = Y, fill = area_sum)) + 
  geom_tile()

dir.create("./data-plots/")
ggsave("./data-plots/graf.jpg", plot = graf, width = 4, height = 3, dpi = 300)
ggsave("./data-plots/graf.png", width = 4, height = 3, dpi = 300) 
ggsave("./data-plots/graf.pdf", width = 4, height = 3, dpi = 300)
getwd()


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
getwd()
png("./data-plots/grafR.png")
plot(pozari$temp, type = "l", col = "blue")
lines(pozari$wind, type = "l", col = "red")
dev.off() # Povrnemo izrise nazaj v R.


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
pdf("./data-plots/grafR.pdf")
plot(pozari$temp, type = "l", col = "blue")
lines(pozari$wind, type = "l", col = "red")
plot(pozari$wind, type = "l", col = "red")
dev.off() # Povrnemo izrise nazaj v R.


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(data.table)
library(dtplyr)


# Generirani podatki, da lahko poljubno nastavimo število primerov. ------------
n         <- 100000000
x         <- rnorm(n)
price     <- rnorm(n, 5000, 100)
all_types <- apply(expand.grid(LETTERS, LETTERS, 0:9), 1, paste, collapse=".")
types     <- sample(all_types, n, replace = T)

tib_dplyr  <- tibble(x = x, price = price, type = types)
dt_dtplyr  <- lazy_dt(data.table::data.table(x = x, price = price, 
                                             type = types))
dt_dtable  <- data.table::data.table(x = x, price = price, type = types)


# dplyr ------------------------------------------------------------------------
t1 <- Sys.time()
tib_dplyr %>% 
  group_by(type) %>% 
  summarise(mean_price = mean(price))
t2 <- Sys.time()
t2 - t1


# data.table -------------------------------------------------------------------
t1 <- Sys.time()
dt_dtable[, sum(price), keyby=type]
t2 <- Sys.time()
t2 - t1


# dtplyr -----------------------------------------------------------------------
t1 <- Sys.time()
dt_dtplyr %>% 
  group_by(type) %>% 
  summarise(mean_price = mean(price)) %>%
  as_tibble()
t2 <- Sys.time()
t2 - t1



## ---- eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
## # dplyr ------------------------------------------------------------------------
## t1 <- Sys.time()
## tib_dplyr %>%
##   filter(x < 2) %>%
##   group_by(type) %>%
##   mutate(price_stand = (price - mean(price)) / sd(price)) %>%
##   summarize(mean_price_stand = mean(price_stand))
## t2 <- Sys.time()
## t2 - t1
## 
## 
## # data.table -------------------------------------------------------------------
## t1 <- Sys.time()
## dt_dtable %>%
##   .[x < 2] %>%
##   .[ , price_stand := (price - mean(price)) / sd(price), by = type] %>%
##   .[ , .(mean_price_stand = mean(price_stand)), keyby = type]
## t2 <- Sys.time()
## t2 - t1
## 
## 
## # dtplyr -----------------------------------------------------------------------
## t1 <- Sys.time()
## dt_dtplyr %>%
##   filter(x < 2) %>%
##   group_by(type) %>%
##   mutate(price_stand = (price - mean(price)) / sd(price)) %>%
##   summarize(mean_price_stand = mean(price_stand)) %>%
##   as_tibble() # To vrstico moramo zapisati, v koliko želimo da se ukaz izvede!
## t2 <- Sys.time()
## t2 - t1


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dbplyr)
library(DBI)
con <- dbConnect(RSQLite::SQLite(), ":memory:") # Ustvarimo začasno bazo (RAM).
dbWriteTable(con, "age-cases-clean", df_tidy) # Shranimo podatke v bazo.
df_tidy_from_db <- dbReadTable(con, "age-cases-clean") # Preberemo podatke v R.
head(df_tidy_from_db)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
 # S tbl() dostopamo do razpredelnice na bazi.
razpredelnica_na_bazi <- tbl(con, "age-cases-clean")
razpredelnica_na_bazi


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
povzemi <- razpredelnica_na_bazi %>%
  group_by(sex, age) %>%
  summarise(sum(daily))
povzemi %>% show_query() # Prikaže prevedeno kodo v SQL.


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
povzetek <- povzemi %>% collect()
povzetek
dbDisconnect(con) # Zapremo povezavo do baze.


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())
set.seed(1) # Zagotovimo ponovljivost.
n <- 2000000 # Število primerov v podatkih.

# Generiramo podatke.
x1 <- rnorm(n)
y1 <- rnorm(n, 4 + 2 * x1, 0.2)
x2 <- rnorm(n)
y2 <- rnorm(n, 3 - 1.5 * x2, 0.2)

df1 <- data.frame(x = x1, y = y1)
df2 <- data.frame(x = x2, y = y2)
df_list <- list(df1, df2)


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
t1 <- Sys.time() # Za izračun potrebnega časa.
my_lms <- list() # V ta seznam bomo shranili rezultate.
for (i in 1:length(df_list)) {
  my_lms[[i]] <- lm(y ~ x, data = df_list[[i]])
}
t2 <- Sys.time()
my_lms
# t2 - t1


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
library(doParallel)
detectCores() # Preverimo, koliko procesorjev imamo na voljo.
nc <- 2 # Število procesorjev.
mc <- makeCluster(nc) # Ustvarimo cluster 2 procesorjev.

# S spodnjim klicem bomo ustvarili log datoteko, kamor se bodo zapisovale
# informacije iz vsakega procesorja.
clusterEvalQ(mc, sink(paste0("./log", Sys.getpid(), ".txt")))

registerDoParallel(mc) # Registriramo cluster.

my_lms <- list()
t1 <- Sys.time() # Za izračun potrebnega časa.
foreach(i = 1:length(df_list)) %dopar% {
  print(paste0("Računam model: ", i))
  my_lms[[i]] <- lm(y ~ x, data = df_list[[i]])
}
stopCluster(mc) # Ustavimo cluster.
t2 <- Sys.time() # Za izračun potrebnega časa.
my_lms
# t2 - t1

