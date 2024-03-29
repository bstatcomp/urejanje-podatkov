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
  colnames(df) <- str_replace(colnames(df), "�", "C")
  colnames(df) <- str_replace(colnames(df), "�", "c")
  colnames(df) <- str_replace(colnames(df), "�", "S")
  colnames(df) <- str_replace(colnames(df), "�", "s")
  colnames(df) <- str_replace(colnames(df), "�", "Z")
  colnames(df) <- str_replace(colnames(df), "�", "z")
  df <- replace_all(df, "�", "C")
  df <- replace_all(df, "�", "c")
  df <- replace_all(df, "�", "S")
  df <- replace_all(df, "�", "s")
  df <- replace_all(df, "�", "Z")
  df <- replace_all(df, "�", "z")
  write_csv2(df, paste0("./data-no-special-characters/", f))
}

