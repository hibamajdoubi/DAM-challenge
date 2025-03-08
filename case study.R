
<<<<<<< HEAD
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
=======
library(tidyverse)
library(caret)       
library(mice)       
library(dplyr)
library(MASS)
library(MuMIn)
>>>>>>> 94e354f (nouvelle mdif)


df <- srcsc_2025_dam_data_for_students

#--------------------------------------------------
#convertir les colonnes de dates en `Date` puis en `double`**
date_cols <- c("Assessment Date", "Last Inspection Date")  

for (col in date_cols) {
  if (col %in% names(df)) {
    df[[col]] <- as.numeric(as.Date(df[[col]], format = "%d/%m/%Y"))  
  }
}

<<<<<<< HEAD

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
=======
df <- impute_missing(df)
names(df) <- gsub(" ", "_", names(df))
numeric_vars <- setdiff(names(df)[sapply(df, is.numeric)], "Probability_of_Failure")

df[numeric_vars] <- as.data.frame(scale(df[numeric_vars], center = TRUE, scale = TRUE))
>>>>>>> 94e354f (nouvelle mdif)


#imputer dabord les variables num

<<<<<<< HEAD
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
=======









df$logit_Probability_of_Failure <- log(df$Probability_of_Failure / (1 - df$Probability_of_Failure))

#Vérifier les variables inutiles / redondantes

#df <- subset(df, select = -c(Last_Inspection_Date, Assessment_Date))  # Supprimer les dates

#cor_matrix <- cor(df[sapply(df, is.numeric)], use = "pairwise.complete.obs")
#high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)  # Seuil de 0.9
#df <- df[, - high_corr]
#sapply(df[sapply(df, is.factor)], nlevels)
#df$Region <- factor(ifelse(table(df$Region)[df$Region] < 5, "Other", df$Region))



df$Last_Inspection_Date <- as.Date(df$Last_Inspection_Date, format="%d/%m/%Y")

df$Assessment_Date <- as.Date(df$Assessment_Date, format="%d/%m/%Y")
# Diviser la variable en deux colonnes : année et lettre
df$Year_Modif <- substr(df$Years_Modified, 1, 4)  # Extraire les 4 premiers caractères (année)

df$Year_Modif <- as.Date(df$Year_Modif, format="%Y")
df$letter <- substr(df$Years_Modified, 5, 5)  # Extraire le dernier caractère (lettre)




#1) Frequence:
#using the whole dataset. 
#without years-modified
model_glm <- glm(Probability_of_Failure ~ Region+ Regulated_Dam + Primary_Purpose + Primary_Type+`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
             data = df, 
             family = binomial(link = "logit"))

summary(model_glm)
model1<- step(model_glm, direction = "both", k = log(nrow(df)))

#with years-modified
model_glm1 <- glm(Probability_of_Failure ~ Region+ Regulated_Dam + Primary_Purpose + Primary_Type+`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+Year_Modif+letter+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
                 data = df, 
                 family = binomial(link = "logit"))

summary(model_glm1)
model1<- step(model_glm1, direction = "both", k = log(nrow(df)))

#using earthen dams dataset.



#using glm binomial logit
earthen_dams <- df %>% 
  filter(Primary_Type == "Earth")

earth_glm <- glm(Probability_of_Failure ~ Region+ Regulated_Dam + Primary_Purpose +`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+Year_Modif+letter+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
                  data = earthen_dams, 
                  family = binomial(link = "logit"))

summary(earth_glm)
model1<- step(earth_glm, direction = "both", k = log(nrow(earthen_dams)))  ##warnings!!
#using glm quasibinomial logit
earth_glm3 <- glm(Probability_of_Failure ~ Region+ Regulated_Dam + Primary_Purpose +`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+Year_Modif+letter+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
                 data = earthen_dams, 
                 family = quasibinomial(link = "logit"))

summary(earth_glm3)
drop1(earth_glm3, test = "F") 
drop1(earth_glm3, test = "Chisq") 
model3<- step(earth_glm3, direction = "both", k = log(nrow(earthen_dams)))  ## plus de variables explicatives

#using glm logit + lm. -->we can use it if we prefer ordinary least squares regression on a transformed scale.
earthen_dams$logit_prob <- log(earthen_dams$Probability_of_Failure  / (1 - earthen_dams$Probability_of_Failure))

earth_glm1 <-lm(logit_prob ~ Region+ Regulated_Dam + Primary_Purpose +`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+Year_Modif+letter+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
                 data = earthen_dams)

summary(earth_glm1)

model1<- step(earth_glm1, direction = "both", k = log(nrow(earthen_dams)))  ## plus de variables explicatives

#using Beta Regression (For Continuous Probabilities).


library(betareg)

earth_glm2 <- betareg(Probability_of_Failure ~ Region+ Regulated_Dam + Primary_Purpose +`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+Year_Modif+letter+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
                 data = earthen_dams,family=binomial(link="logit"))

summary(earth_glm2)

model2<- stepAIC(earth_glm2, direction = "both")  ## plus de variables explicatives



earth_glm2 <- betareg(Probability_of_Failure ~ Region+ Regulated_Dam  +Year_Modif+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment, 
                      data = earthen_dams,family=binomial(link="logit"))

summary(earth_glm2)






set.seed(123)  # Reproductibilité
df_sample <- df[sample(nrow(df), size = 5000), ]  # Garde 5000 lignes aléatoires

model_glm1 <- glm(Probability_of_Failure ~ Region+ Regulated_Dam + Primary_Purpose + Primary_Type+`Height_(m)`+`Length_(km)`+`Volume_(m3)`+Year_Completed+Years_Modified+`Surface_(km2)`+`Drainage_(km2)`+Spillway+Last_Inspection_Date +Inspection_Frequency+`Distance_to_Nearest_City_(km)`+Hazard+Assessment+Assessment_Date , 
                 data = df_sample, 
                 family = binomial(link = "logit"))

summary(model_glm1)
>>>>>>> 94e354f (nouvelle mdif)
