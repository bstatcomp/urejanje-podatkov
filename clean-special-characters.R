setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Functions. -------------------------------------------------------------------
replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)  
  df
}


# Main. ------------------------------------------------------------------------
lf <- list.files("./data-raw")
lf_slo <- str_subset(lf, "^SLO")
lf_slo

for (f in lf_slo) {
  df <- read_csv2(paste0("./data-raw/", f), 
                  locale = readr::locale(encoding = "cp1250"))
  colnames(df) <- str_replace(colnames(df), "È", "C")
  colnames(df) <- str_replace(colnames(df), "è", "c")
  colnames(df) <- str_replace(colnames(df), "Š", "S")
  colnames(df) <- str_replace(colnames(df), "š", "s")
  colnames(df) <- str_replace(colnames(df), "Ž", "Z")
  colnames(df) <- str_replace(colnames(df), "ž", "z")
  df <- replace_all(df, "È", "C")
  df <- replace_all(df, "è", "c")
  df <- replace_all(df, "Š", "S")
  df <- replace_all(df, "š", "s")
  df <- replace_all(df, "Ž", "Z")
  df <- replace_all(df, "ž", "z")
  write_csv2(df, paste0("./data-no-special-characters/", f))
  # 
  # 
  # df           <- df %>%
  #   mutate(OBCINE = str_replace(OBCINE, "È", "C"),
  #          OBCINE = str_replace(OBCINE, "è", "c"),
  #          OBCINE = str_replace(OBCINE, "Š", "S"),
  #          OBCINE = str_replace(OBCINE, "š", "s"),
  #          OBCINE = str_replace(OBCINE, "Ž", "Z"),
  #          OBCINE = str_replace(OBCINE, "ž", "z"))
  # write_csv2(df, paste0("./data-no-special-characters/", f))
}

