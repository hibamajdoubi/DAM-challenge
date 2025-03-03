
install.packages("mice")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("lubridate")
install.packages("VIM")
install.packages("doParallel")
library(caret)
library(mice)
library(dplyr)
library(ggplot2)
library(corrplot)
library(lubridate)
library(VIM)
library(doParallel)
library(parallel)


df <- srcsc_2025_dam_data_for_students

#--------------------------------------------------
#convertir les colonnes de dates en `Date` puis en `double`**
date_cols <- c("Assessment Date", "Last Inspection Date")  

for (col in date_cols) {
  if (col %in% names(df)) {
    df[[col]] <- as.numeric(as.Date(df[[col]], format = "%d/%m/%Y"))  
  }
}


for (col in names(df)) {
  if (is.character(df[[col]])) {
    df[[col]] <- as.factor(df[[col]])
  }
}

#--------------------------------------------------

# Filtrer les colonnes avec +80% de valeurs manquantes**
#seuil_na <- 0.80  
#col_na_pct <- colMeans(is.na(df))
#cols_to_drop <- names(col_na_pct[col_na_pct > seuil_na])
#df_filtered <- df %>% select(-all_of(cols_to_drop))
#--------------------------------------------------


#imputer dabord les variables num

df_filtered_numeric <- df %>%
  select(where(is.numeric))


#-----------------------------------------------
num_cores <- detectCores() - 1

# Fonction d'imputation avec Random Forest

impute_data <- function(df) {
  imputed_df <- mice(df, method = "rf", m = 1, maxit = 5, n.core = num_cores, n.imp.core = 1) 
  df_complete <- complete(imputed_df)
  return(df_complete)
}

df_filtered_imputed <- impute_data(df_filtered_numeric)

#--------------------------------------------------

#reconversion en date

date_cols <- c("Assessment Date", "Last Inspection Date")


for (col in date_cols) {
  if (col %in% names(df_filtered_imputed)) {
    df_filtered_imputed[[col]] <- as.Date(df_filtered_imputed[[col]], origin = "1970-01-01") 
  }
}

#--------------------------------------------------
# ⚡imputation kNN avec parallélisation  pour  cat
 
df_categorical <- df %>% select(where(is.factor))

cl <- makeCluster(num_cores)
registerDoParallel(cl)


df_categorical_imputed <- kNN(df_categorical, k = 5, numFun = median)


stopCluster(cl)

df_categorical_imputed <- df_categorical_imputed %>%
  select(!ends_with("_imp"))

df_categorical_imputed

# combiner valeur num et categorielles
data <- cbind(df_categorical_imputed, df_filtered_imputed)
write.csv(data, "datasansNA.csv", row.names = FALSE)

#--------------------------------------------------
