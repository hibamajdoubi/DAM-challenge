
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
library(tidyr)     # Pour le pivot_longer et la transformation des données



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
# imputation kNN avec parallélisation  pour  variables cateagorielles
 
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
sum(is.na(datasansNA))

#--------------------------------------------------
#  bin de variables numeriques 

for (col in names(datasansNA)) {
  if (is.character(datasansNA[[col]])) {
    df[[col]] <- as.factor(datasansNA[[col]])
  }
}
datasansNA$`Height (m)` <- cut(
  datasansNA$`Height (m)`, 
  breaks = quantile(datasansNA$`Height (m)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
  include.lowest = TRUE
)

datasansNA$`Length (km)` <- cut(
  datasansNA$`Length (km)`, 
  breaks = quantile(datasansNA$`Length (km)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
  include.lowest = TRUE
)

#datasansNA$`Volume (m3)` <- cut(
#  datasansNA$`Volume (m3)`, 
# breaks = quantile(datasansNA$`Volume (m3)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
#  include.lowest = TRUE
#)


datasansNA$`Year Completed` <- cut(
  datasansNA$`Year Completed`, 
  breaks = quantile(datasansNA$`Year Completed`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
  include.lowest = TRUE
)



#datasansNA$`Surface (km2)` <- cut(
#  datasansNA$`Surface (km2)`, 
#  breaks = quantile(datasansNA$`Surface (km2)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
#  include.lowest = TRUE
#)


#datasansNA$`Drainage (km2)` <- cut(
#  datasansNA$`Drainage (km2)`, 
#  breaks = quantile(datasansNA$`Drainage (km2)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
#  include.lowest = TRUE
#)


#datasansNA$`Inspection Frequency` <- cut(
#  datasansNA$`Inspection Frequency`, 
#  breaks = quantile(datasansNA$`Inspection Frequency`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
#  include.lowest = TRUE
#)


#datasansNA$`Distance to Nearest City (km)` <- cut(
#  datasansNA$`Distance to Nearest City (km)`, 
#  breaks = quantile(datasansNA$`Distance to Nearest City (km)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
#  include.lowest = TRUE
#)




datasansNA$`Loss given failure - prop (Qm)` <- cut(
  datasansNA$`Loss given failure - prop (Qm)`, 
  breaks = quantile(datasansNA$`Loss given failure - prop (Qm)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
  include.lowest = TRUE
)

datasansNA$`Loss given failure - liab (Qm)` <- cut(
  datasansNA$`Loss given failure - liab (Qm)`, 
  breaks = quantile(datasansNA$`Loss given failure - liab (Qm)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
  include.lowest = TRUE
)

datasansNA$`Loss given failure - BI (Qm)` <- cut(
  datasansNA$`Loss given failure - BI (Qm)`, 
  breaks = quantile(datasansNA$`Loss given failure - BI (Qm)`, probs = seq(0, 1, by = 1/10), na.rm = TRUE), 
  include.lowest = TRUE
)

#--------------------------------------------------
# Split the data into training and testing sets
set.seed(42) # For reproducibility
in_training <- createDataPartition(datasansNA$`Probability of Failure`, times = 1, p = 0.8, list = FALSE)
training_set <- datasansNA[in_training, ]
testing_set <- datasansNA[-in_training, ]


#--------------------------------------------------
str(datasansNA)
# Analyse exploratoire des variables numériques
num_vars <- datasansNA %>% select(  `Volume (m3)`, `Surface (km2)`, `Drainage (km2)`, `Inspection Frequency`, `Distance to Nearest City (km)`, `Probability of Failure`)

# Histogrammes des variables numériques
num_vars_long <- num_vars %>% pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(num_vars_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogrammes des variables numériques", x = "Valeurs", y = "Fréquence")

# Corrélation entre les variables numériques
cor_matrix <- cor(num_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8, tl.col = "black")

# Boxplots pour identifier les valeurs extrêmes

ggplot(num_vars_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplots des variables numériques", x = "Variable", y = "Valeur")

# Relation entre la "Probability of Failure" et d'autres variables

ggplot(datasansNA, aes(x = `Height (m)`, y = `Probability of Failure`)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Relation entre la hauteur et la probabilité de défaillance",
       x = "Hauteur (m)", y = "Probabilité de défaillance")

# Distribution de la variable cible

ggplot(datasansNA, aes(x = `Probability of Failure`)) +
  geom_density(fill = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution de la probabilité de défaillance",
       x = "Probabilité de défaillance", y = "Densité")

# Analyse des variables catégorielles
cat_vars <- datasansNA %>% select(`Height (m)`,`Region`,`Length (km)`, `Year Completed`, `Regulated Dam`, `Primary Purpose`, `Primary Type`, `Spillway`, `Hazard`, `Assessment`,`Loss given failure - prop (Qm)`, `Loss given failure - liab (Qm)`, `Loss given failure - BI (Qm)`)
# Barplots des variables catégorielles
cat_vars_long <- cat_vars %>% pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category")

ggplot(cat_vars_long, aes(x = Category, fill = Variable)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution des variables catégorielles", x = "Catégorie", y = "Fréquence")
