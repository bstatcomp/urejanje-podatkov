library(lubridate)
x  <- ymd("2020-10-25", tz = "Europe/Ljubljana")
df <- tibble(datum = x, ura_trgovanja = 1:25)
tail(df)
df_obdobja  <- df %>% mutate(datetime = datum + hours(ura_trgovanja))
df_trajanja <- df %>% mutate(datetime = datum + dhours(ura_trgovanja))
df_obdobja # Ni v redu.
df_trajanja # Je v redu.
tail(df_obdobja)
tail(df_trajanja)

