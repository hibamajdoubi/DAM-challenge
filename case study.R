
library(tidyverse)
library(caret)       
library(mice)       


df <- read_csv("Master Actuariat/case study/srcsc-2025-dam-data-for-students.csv")

str(df)

# ---- 1. Gestion des variables catégorielles ----

categorical_vars <- names(df)[sapply(df, is.character)]
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# ---- 2. Gestion des valeurs manquantes ----

impute_missing <- function(data) {
  for (col in names(data)) {
    if (any(is.na(data[[col]]))) {
      if (is.numeric(data[[col]])) {
        # Imputer par la médiane (plus robuste que la moyenne)
        data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
      } else if (is.factor(data[[col]])) {
        
        mode_value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
        data[[col]][is.na(data[[col]])] <- mode_value
      }
    }
  }
  return(data)
}

df <- impute_missing(df)

numeric_vars <- names(df)[sapply(df, is.numeric)]
df[numeric_vars] <- as.data.frame(scale(df[numeric_vars], center = TRUE, scale = TRUE))


sum(is.na(df))  
write.csv(df, "data_cleaned.csv", row.names = FALSE)
head(df)

