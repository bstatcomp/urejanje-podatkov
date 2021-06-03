setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(openxlsx)
library(tidyverse)
library(lubridate)

# openxlsx
math_scores <- openxlsx::read.xlsx("./data-raw/student-performance.xlsx",
                                   sheet = "Math scores")
port_scores <- openxlsx::read.xlsx("./data-raw/student-performance.xlsx",
                                   sheet = "Portuguese scores")

# tidyr
math_longer <- pivot_longer(math_scores, cols = c("G1", "G2", "G3"))

# forcats
math_longer$name <- factor(math_longer$name)
math_longer$name <- fct_recode(
  math_longer$name,
  "score 1" = "G1",
  "score 2" = "G2",
  "score 3" = "G3"
)
math_longer$name <- fct_relevel(math_longer$name, "score 3", "score 2")

# ggplot
ggplot(math_longer, aes(x = value)) +
  geom_bar() +
  facet_wrap(~ name)
ggsave("./moji-rezultati/primer-graf.png", width = 4, height = 3)

# dplyr
math_smaller <- math_scores %>%
  slice(1:5) %>%
  select(school, sex, famsize, G1)

# stringr
math_smaller$famsize <- str_replace(math_smaller$famsize,
                                    "3",
                                    "-3")

# lubridate
datetimes <- seq(ymd_hms("2020-06-12 23:04:30"),
                 ymd_hms("2020-06-16 23:04:30"),
                 by = "day")
datetimes <- datetimes + days(1) + hours(1)
math_smaller$date <- date(datetimes)

# readr
write_csv2(math_smaller, "./moji-rezultati/primer-csv.csv")
