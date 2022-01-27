setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Povezave na bazo----------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(DBI)
library(RMariaDB)
#RMySQL, RPostgreSQL, RSQLite, ROracle, odbc...

#con <- dbConnect(RSQLite::SQLite(), ":memory:") # V pomnilniku ustvarimo začasno bazo.
con <- dbConnect(RMariaDB::MariaDB(), 
                 user = "urejanje", 
                 password = "podatkov", 
                 dbname = "delavnica", 
                 host = "localhost")
#naložimo že znane podatke z diska v R
accounts <- read.csv2("./data-raw/financial/account.csv")
dbWriteTable(con, "accounts", accounts, overwrite = T) #DBI verzija
#copy_to(con, accounts, "accounts2") #dplyr verzija, ustvari le začasno tabelo

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
#poženimo poizvedbo iz dodanih podatkov
res <- dbGetQuery(con, "SELECT trans_id, type, balance  
                 FROM transactions_small 
                 WHERE balance > 100000")
head(tibble(res))

#preberimo že obstoječe podatke na bazi (naloženi pred predavanji)
trans <- dbReadTable(con, "transactions_small") # Preberemo podatke iz baze v R.
trans <- tibble(trans)
head(trans)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
razpredelnica_na_bazi <- tbl(con, "transactions_small") # S tbl() dostopamo do razpredelnice na bazi.
razpredelnica_na_bazi


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
povzemi <- razpredelnica_na_bazi %>%
  group_by(type, operation) %>%
  summarise(stevilo = n())
povzemi %>% show_query() # Prikaže prevedeno kodo v SQL.


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
povzetek <- povzemi %>% collect()
povzetek
dbDisconnect(con) # Zapremo povezavo do baze.

#povzetek še imamo
povzetek
#kazalca poizvedbe ni več
povzemi
#razpredelnica ni več dostopna
razpredelnica_na_bazi
#podatke o transakcijah še vedno imamo v R-ju
head(trans)

#odbc trenutno omogoča tudi intergracijo z RStudio
library("odbc")
sort(unique(odbcListDrivers()[[1]]))
povezava <- dbConnect(odbc(), 
                      driver = "MySQL ODBC 8.0 ANSI Driver",
                      user = "urejanje", 
                      password = "podatkov", 
                      dbname = "delavnica", 
                      host = "localhost")
#če imate nastavljen DSN se lahko preprosteje povežete z
#povezava <- dbConnect(odbc(), dsn = "MojDSN")
dbDisconnect(povezava)



## vzorčenje -----------------------------------------------------------------------------------------------------------------------------------------------------------------
sample(1:10, 5)
sample(1:10, 5, replace = T)
sample(1:10, 5, prob = c(20, 20, 15, 15, 10, 10, 5, 5, 1, 1))

## naključno vzorčenje -----------------------------------------------------------------------------------------------------------------------------------------------------------------
v <- sample(1:nrow(trans), 1000)
trans1000 <- trans[v,]
trans19 <- trans[-v,]
table(trans$type)/nrow(trans)
table(trans1000$type)/nrow(trans1000)

#vizuelno lahko primerjamo dobljene distribucije zveznih spremenljivk
hist(trans$amount)
hist(trans1000$amount)
ggplot(trans, aes(x = amount, fill = "trans")) +    
  geom_histogram(aes(y=..count../sum(..count..)), alpha = 0.3, bins = 20) +
  geom_histogram(data = trans1000, aes(y=..count../sum(..count..), fill = "trans1000"), 
                 alpha = 0.3, bins = 20)

## stratified -----------------------------------------------------------------------------------------------------------------------------------------------------------------

dist <- table(trans$type)/nrow(trans)*1000
dist <- round(dist)
dist
sum(dist)

calculate_subsamples_n <- function(data, colName, n){
  dist <- table(trans$type)/nrow(trans)*n
  dist <- round(dist)
  #dodatno preverjanje, ker zaradi zaokroževanja
  #lahko pride do manjših odstopanj v n-ju
  #največji vrednosti prištejemo oz. odštejemo dodatne primere
  max_value <- which.max(dist)
  odstopanje <- n - sum(dist)
  if(odstopanje > 0){
    dist[max_value] <- dist[max_value] - odstopanje
  }else if(odstopanje < 0){
    dist[max_value] <- dist[max_value] + odstopanje
  }
  dist
}

calculate_subsamples_n(trans, "type", 1000)

select_n_rows <- function(data, n){
  data[sample(1:nrow(data), n),]
}

t1 <- trans %>% filter(type == "CHOICE") %>% select_n_rows(14)
t2 <- trans %>% filter(type == "EXPENDITURE") %>% select_n_rows(600)
t3 <- trans %>% filter(type == "INCOME") %>% select_n_rows(386)

t <- union(t1, union(t2, t3))

table(trans$type)/nrow(trans)
table(t$type)/nrow(t)

## stratified po več spremenljivkah-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#install.packages("splitstackshape")
library(splitstackshape)

trans5p <- tibble(stratified(trans, group = "type", size = 0.05))
table(trans$type)/nrow(trans)
table(trans5p$type)/nrow(trans5p)

table(trans$operation)/nrow(trans)
table(trans5p$operation)/nrow(trans5p)

trans5p <- tibble(stratified(trans, group = c("type", "operation"), size = 0.05))
table(trans$type)/nrow(trans)
table(trans5p$type)/nrow(trans5p)

table(trans$operation)/nrow(trans)
table(trans5p$operation)/nrow(trans5p)


## EDA-------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(curl)
library(lubridate)
set.seed(1234)

df <- read_csv(curl("https://raw.githubusercontent.com/sledilnik/data/master/csv/stats.csv"))
df

summary(df)
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------

make_NA <- function(x, pctg = 0.05){
  "x = vector, pctg = percentage of data that will become NA"
  n = length(x)
  indices <- sample(1:n, n%*%pctg, replace = T)
  x[indices] <- NA
  return(x)
}
df <- df %>% mutate(across(.cols = everything(), make_NA))

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------

print(tail(df), width = Inf)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------

testi     <- select(df, day, date, starts_with("tests.")) 
okuzbe    <- select(df, day, date, starts_with("cases."))
regije    <- select(df, day, date, starts_with("region.")) 
stanje    <- select(df, day, date, starts_with("state.")) 
starost   <- select(df, day, date, starts_with("age."))
smrti     <- select(df, day, date, starts_with("deceased."))
cepljeni  <- select(df, day, date, starts_with("vaccination."))
## stanja bolnisnic-----------------------------------------------------------------------------------------------------------------------------------------------------------------

print(stanje, width = Inf, n = 20)
colnames(stanje)

## kumulative-----------------------------------------------------------------------------------------------------------------------------------------------------------------

x <- c(1, 2, 1, 2, 3, 4, 5, 8, 9, 10, 7, 3, 1, 1, 4, 5, 7, 4, 3, 1)
kum_x <- cumsum(x)

y <- seq(1, length(kum_x))
podatki <- tibble(x=x,y=y,kum_x=kum_x)

ggplot(podatki, aes(y, x)) + geom_line()
ggplot(podatki, aes(y, kum_x)) + geom_line()

## back to stanje (NA) -------------------------------------------------------------------------------------------------------------------------------------------------------------
stanje 
stanje <- filter(stanje, day >= 7)

stanje <- mutate(stanje, 
                 date = ifelse(is.na(date), lag(date) + days(1), ymd(date)),
                 date = as_date(date),
                 day = ifelse(is.na(day), lag(day) + 1, day))

library(zoo)
stanje <- mutate(stanje, 
                 across(starts_with("state."), na.spline),
                 across(starts_with("state."), as.integer))
stanje

apply(stanje, 2, function(x){sum(is.na(x))})


## odstrani kumulativo-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#zakaj - motivacija

stanje <- stanje %>%
  mutate(across(ends_with(".todate"), ~ .x -lag(.x))) %>%
  mutate(across(ends_with(".todate"), ~ na.fill0(.x, fill = 0)))

stanje <- stanje %>% 
  pivot_longer(cols = c(-day, -date), 
               names_to = "stanje_pacienta", 
               values_to = "st_oseb", 
               names_prefix = "state.") %>%
  mutate(stanje_pacienta = str_replace(stanje_pacienta, ".todate", "_novi"))
stanje

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------

prikazemo <- c("icu", 
               "in_hospital", 
               "critical", 
               "in_hospital_novi", 
               "out_of_hospital_novi",
               "deceased_novi")

stanje %>% mutate(val1 = ifelse(date > make_date(2020, 11, 1) & date < make_date(2021, 1, 25), 2021, 0),
                  val2 = ifelse(date > make_date(2021, 11, 1) & date < make_date(2022, 2, 25), 2022, 0), 
                  val = val1 + val2) %>% #case_when?
  filter(stanje_pacienta %in% prikazemo, val > 0 ) %>%
  ggplot(., aes(x = date, y = st_oseb, color = stanje_pacienta)) + geom_line() + 
  facet_wrap(.~ val, scales = "free_x" )

#stanje %>% mutate(val = case_when(date > make_date(2020, 11, 1) & date < make_date(2021, 1, 25) ~ 2021,
#                                  date > make_date(2021, 11, 1) & date < make_date(2022, 2, 25) ~ 2022,
#                                  TRUE ~ 0)) %>% #case_when?
#                    filter(stanje_pacienta %in% prikazemo, val > 0 ) %>%
#                    ggplot(., aes(x = date, y = st_oseb, color = stanje_pacienta)) + geom_line() + 
#                    facet_wrap(.~ val, scales = "free_x" )


## smrti-----------------------------------------------------------------------------------------------------------------------------------------------------------------

tail(smrti)

smrti <- mutate(smrti, 
                date = ifelse(is.na(date), lag(date) + days(1), ymd(date)),
                date = as_date(date),
                day = ifelse(is.na(day), lag(day) + 1, day))

smrti[1, 3:37] <- 0
smrti <- mutate(smrti,across(starts_with("deceased."), na.locf ))

smrti %>% 
  filter(day == 691) %>%
  select(day, date, (contains("female") | contains("male")) & matches("\\d")) %>%
  pivot_longer(cols = contains("deceased."), 
               names_to = c("spol", "starost"),
               names_sep = "\\.",
               names_prefix = "deceased.", 
               values_to = "smrti") %>% #warning, ker zavržemo todate
  mutate( spol = factor(spol), 
          starost = factor(starost, levels = unique(starost), ordered = T)) %>%
  ggplot(., aes(x = starost, y = smrti, fill = spol )) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Covid smrti po spolu in starosti")