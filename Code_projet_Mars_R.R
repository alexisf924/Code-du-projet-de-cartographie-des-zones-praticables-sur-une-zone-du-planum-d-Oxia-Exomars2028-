library(dplyr)
library(readxl)
library(sf)
library(terra)
dtm<- rast( "C:/Users/alexi/Downloads/20260227T02500196121/cartOrder/cartorder/dteec_003195_1985_002694_1985_l01.img")
#Calcul de la pente
slope_real <- terrain(dtm, v ="slope", unit="degrees")
plot(slope_real , main = "pente(°)")
#Calcul de la rugosité
rugosite_tri <- terrain(dtm, v="TRI")
plot(rugosite_tri , main = "Rugosité(TRI)")
pente_accessible <- slope_real <= 21.5

#Verification des valeurs manquantes 
summary(slope_real)

#Echantillonage pour régression 
set.seed(42)
#Indice accessible et non accesible 

idx_accessible <- which(values(pente_accessible)==1)
idx_non_accessible <- which(values(pente_accessible)==0)



#Echantillonage taille maximale disponible 
n_accessible <- min(250000, length(idx_accessible))
n_non_accessible <- min(250000, length(idx_non_accessible))

#Echantillonage équilibré 
sample_idx <- c(
  sample(idx_accessible, n_accessible  , replace = FALSE),
  sample(idx_non_accessible, n_non_accessible, replace = FALSE)
)
data_sample <- data.frame(
  slope = values(slope_real)[sample_idx],
  roughness = values(rugosite_tri)[sample_idx],
  accessible = as.integer(values(pente_accessible)[sample_idx])
)
data_sample_clean <- na.omit(data_sample)

summary(data_sample_clean)
#Vérification de l'équilibre 
table(data_sample_clean$accessible)

#Standardisation des variables 
data_sample_clean <- data_sample_clean %>%
mutate(
  slope_s = as.numeric(scale(slope)),
  roughness_s = as.numeric(scale(roughness))
)

model <- glm(
  accessible ~ slope_s + roughness_s,
  data = data_sample_clean,
  family = binomial(link = "logit")
)
#Ajuster avec brglm2::brglmFit 
install.packages("brglm2")
library(brglm2)
model_br <- glm(
  accessible~ slope_s + roughness_s,
  data = data_sample_clean, 
  family = binomial(link = "logit"),
  method = "brglmFit",
  type = "AS_mean"
)

#Réguslarisation  pour réduire les 0/1 
data_sample_clean <- data_sample_clean %>%
  mutate(
    slope_s =  slope_s + rnorm(n(),0,0.01),
    roughness_s = roughness_s + rnorm(n(),0,0.01)
  )

model_br <- glm(
  accessible~ slope_s + roughness_s,
  data = data_sample_clean, 
  family = binomial(link = "logit"),
  method = "brglmFit",
  type = "AS_mean"
)
mean_slope <- mean(data_sample_clean$slope)
sd_slope <- sd(data_sample_clean$slope)
mean_rough <- mean(data_sample_clean$roughness)
sd_rough <- sd(data_sample_clean$roughness)
#Fonction de prédiction pour chaque pixel 
predict_pixel <- function(x){
  slope_val <- as.numeric(x[1])
  rough_val <-as.numeric(x[2])
  
  if(is.na(slope_val) | is.na(rough_val))return(NA)
  #Standardisation avec les mêmes moyennes et écarts-types que dans l'échantillon 
  slope_s <- (slope_val - mean_slope) / sd_slope
  rough_s <- (rough_val - mean_rough) / sd_rough

  if(is.na(slope_val) | is.na(rough_val))return(NA)
  #probabilité prédicte 
  p <- predict(model_br , newdata = data.frame(slope_s = slope_s,roughness_s = rough_s), type = "response")
  return(p)
}
#Appliquer la fonction par blocs avec terra:app()
rast_stack <- c(slope_real,rugosite_tri)
names(rast_stack) <- c("slope", "roughness")

prob_raster <- terra::predict(
  rast_stack,
  model_br,
  fun =function(model,data){
    data$slope_s <- (data$slope - mean_slope)/sd_slope 
    data$roughness_s <- (data$roughness - mean_rough)/ sd_rough
    predict(model,newdata = data, type = "response")
  }
)
plot(prob_raster)


#transformer en carte de praticabilité 
praticability <- prob_raster > 0.5
plot(praticability , main = "Carte de praticabilité")
class(dtm)
plot(dtm)
dtm

plot(model_br)




