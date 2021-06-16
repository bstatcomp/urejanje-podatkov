setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
df <- read_csv("./raw-data/age-cases.csv")
df

df_longer <- df %>%
  pivot_longer(cols = starts_with("age"), values_to = "cumulative") %>%
  filter(str_detect(name, "[:alpha:]*\\.[:alpha:]*\\.[0-9-]*\\.[:alpha:]*")) %>%
  separate("name", c("tmp1", "sex", "age", "tmp2"), sep = "\\.") %>%
  select(date, sex, age, cumulative) %>%
  group_by(sex, age) %>%
  mutate(cumulative = replace_na(cumulative, 0),
         daily      = c(cumulative[1], diff(cumulative)))

library(doParallel)
