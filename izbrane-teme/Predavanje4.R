setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Povezave na bazo----------------------------------------------------------------------------------------------------------------------------------------------------
library(dbplyr)
library(DBI)
library(RMariaDB)
#con <- dbConnect(RSQLite::SQLite(), ":memory:") # V pomnilniku ustvarimo začasno bazo.
con <- dbConnect(RMariaDB::MariaDB(), 
                 user = "urejanje", 
                 password = "podatkov", 
                 dbname = "delavnica", 
                 host = "localhost")
#naložimo že znane podatke z diska v R
accounts <- read.csv2("./data-raw/financial/account.csv")

dbWriteTable(con, "transactions_small", trans) # Shranimo podatke v bazo.
df_tidy_from_db <- dbReadTable(con, "age_cases_clean") # Preberemo podatke iz baze v R.
head(df_tidy_from_db)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
razpredelnica_na_bazi <- tbl(con, "age_cases_clean") # S tbl() dostopamo do razpredelnice na bazi.
razpredelnica_na_bazi


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
povzemi <- razpredelnica_na_bazi %>%
  group_by(sex, age) %>%
  summarise(sum(daily))
povzemi %>% show_query() # Prikaže prevedeno kodo v SQL.


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
povzetek <- povzemi %>% collect()
povzetek
dbDisconnect(con) # Zapremo povezavo do baze.

#povzetek še imamo
povzetek
#razpredelnica ni več dostopna
razpredelnica_na_bazi


library(curl)
library(tidyverse)

#poglejmo razliko pri branju
#read.csv doda več pik kar je potem težje za separate
covid <- read.csv(curl("https://raw.githubusercontent.com/sledilnik/data/master/csv/stats.csv"))
covid <- read_csv(curl("https://raw.githubusercontent.com/sledilnik/data/master/csv/stats.csv"))

print(head(covid), width = Inf)
print(tail(covid), width = Inf)

#pogljemo datume od, kdaj do kdaj imamo podatke od 24.2.2020 do danes.
covid$date



dim(covid)
covid %>% map(~ sum(is.na(.x))/length(.x)*100)

#stratified in baze
#https://db.rstudio.com/getting-started/overview

install.packages("DBI")
install.packages("odbc")

library("DBI")
library("odbc")

#lahko pogledamo katere gonilnike imamo nameščene
#drugače jih lahko pogooglamo recimo z "MySQL odbc connector" za "MySQL ODBC 8.0 ANSI Driver"                            
#[5] "MySQL ODBC 8.0 Unicode Driver"

sort(unique(odbcListDrivers()[[1]]))

povezava <- dbConnect(odbc(), "FRI server")
close(povezava)

#ROracle

#con <- dbConnect(odbc(),
#                 Driver = "SQL Server",
#                 Server = "localhost\\SQLEXPRESS",
#                 Database = "datawarehouse",
#                 Trusted_Connection = "True")


library(ggplot2)
s = c(rep("A",50),rep("B",35),rep("C",15))
d = as.data.frame(table(s))
p = ggplot(d,aes(x=s,y=Freq,fill=s)) + geom_bar(stat="identity")+
  geom_text(aes(label=Freq),vjust=1.6) +
  theme(legend.position = "none")

p

sample(d$s,replace = TRUE,prob = d$Freq,10)


library(DBI)
library(dplyr) #vsebuje copy_to
library(ggplot2)

# SQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")
copy_to(con, nycflights13::flights, "FLIGHTS")

# ODBC databases (requires a live database connection)
#con <- dbConnect(odbc::odbc(), "SQL Server")
#con <- dbConnect(odbc::odbc(), "Snowflake")

# Query, collect results, and visualize
tbl(con, "FLIGHTS") %>%
  filter(distance > 75) %>%
  group_by(origin, hour) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  collect() %>%
  ggplot(aes(hour, delay, color = origin)) + geom_line()

tbl_vars(tbl(con, "FLIGHTS"))


setwd("F:/projekti/urejanjeJan22/urejanje-podatkov")
trans <- tibble(read.csv2("./data-raw/financial/transaction-smaller.csv"))

head(trans)

#razloži sample funkcijo
sample(1:10, 5)
sample(1:10, 5, replace = T)

#naključno sampliranje 1000 vzorcev brez ponavljanja,
#če si lahko privoščimo dovolj velik vzorec je to dovolj dobro
trans1000 <- trans[sample(1:nrow(trans), 1000),]

realDist <- table(trans$type)/nrow(trans)
uniformSampling <- table(trans1000$type)/nrow(trans1000)

tibble(Nacin = c("real", "uniform"), 
       Choice = c(realDist[1], uniformSampling[1]),
       Expenditure = c(realDist[2], uniformSampling[2]),
       Income = c(realDist[3], uniformSampling[3]))

dist <- table(trans$type)/nrow(trans)*1000
dist <- round(dist)
dist
sum(dist)

calculate_sub_n <- function(data, colName, n){
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

calculate_sub_n(trans, "type", 1234)
#check
sum(calculate_sub_n(trans, "type", 1234))




select_n_rows <- function(data, n){
  data[sample(1:nrow(data), n),]
}


#ohranimo 'popolno distribucijo' ene spremenljivke - uporabno za learn/test set
izbrani <- sample(1:length(table(trans$type)), 1000, replace = T, prob = table(trans$type))
sum(izbrani == 1)
sum(izbrani == 2)
sum(izbrani == 3)
trans1000 <- trans[izbrani,]

t1 <- trans %>% filter(type == "CHOICE") %>% select_n_rows(14)
t2 <- trans %>% filter(type == "EXPENDITURE") %>% select_n_rows(600)
t3 <- trans %>% filter(type == "INCOME") %>% select_n_rows(386)

t <- union(t1, union(t2, t3))
table(t$type)/1000

table(trans1000$type)/nrow(trans1000)
table(trans$type)/nrow(trans)

#lahko pa izbiramo enega po enega
#napišimo kar funkcijo
select_n_keep_dist <- function(data, n, cols, rep = TRUE){
  cur_selection <- vector()
  dataModified <- data %>% mutate(row_num = row_number())
  for(col in cols){
    dataModified[[col]] <- replace_na(dataModified[[col]], "unknown")
  }
  for(i in 1:n){
    subdata <- dataModified
    for(col in cols){
      counts <- table(subdata[[col]]) #preštej vrednosti
      #izberi eno vrednost glede na distribucijo
      selected <- sample(names(counts), 1, prob = counts)
      #filtriraj
      subdata <- subdata %>% filter(.data[[col]] == selected)
      #kaj naredim, če je subsample - aja ne more bit
    }
    #izberemo eno vrstico iz subsampla
    cur_selection <- c(cur_selection, unlist(subdata[sample(1:nrow(subdata), 1), "row_num"]))
  }
  data[cur_selection,]
}

debug(select_n_keep_dist)
select_n_keep_dist(trans, 2, cols = c("type", "operation"))

#če želimo ohraniti distribucijo po večih atributih


#10-fold cross-validation



#sampling z ohranjanjem vseh distribucij
#https://towardsdatascience.com/stratified-sampling-and-how-to-perform-it-in-r-8b753efde1ef
#dela lepo za diskretne atribute, za zvezne je ful težje - diskretizacija

install.packages("splitstackshape")
library(splitstackshape)
?stratified

set.seed(1)
DF <- data.frame(
  ID = 1:100,
  A = sample(c("AA", "BB", "CC", "DD", "EE"), 100, replace = TRUE),
  B = rnorm(100), C = abs(round(rnorm(100), digits=1)),
  D = sample(c("CA", "NY", "TX"), 100, replace = TRUE),
  E = sample(c("M", "F"), 100, replace = TRUE))
