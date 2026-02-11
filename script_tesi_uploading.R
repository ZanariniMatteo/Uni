####################################################
#   Data Science Techniques for Real Estate Data:    
#   Dimension Reduction for Regression and 
#   Classification Model Selection
####################################################

# Loading libraries:
library(readxl)
library(readr)
library(dplyr)
library(tibble)
library(mice)
library(ggplot2)
library(patchwork)
library(tidygeocoder)
library(geosphere)
library(lubridate)
library(reshape2)
library(car)
library(DescTools)
library(glmnet)
library(leaps)
library(plotmo)
library(pls)
library(ROCR)
library(e1071)
library(caret)
library(kknn)
library(bestglm)
library(MASS)
library(randomForest)
library(tree)

# ---- Loading dataset ----
# import reduced dataset after Excel analysis
data <- read_excel("dataset_ridotto.xlsx")
data <- as.data.frame(data)
dim(data)
head(data)
# 2202 rows, 44 vars
sum(duplicated(data$ID_Immobile))

# ---- Cleaning ----
str(data)
# leveling
factor_vars <- c("Stato_Immobile", "Macro_Tipologia", "Giardino", 
                 "Climatizzazione", "Riscaldamento", "Solare_Termico",
                 "Fotovoltaico")
data[factor_vars] <- lapply(data[factor_vars], factor)

table(data$Condizioni_Interne)
data$Condizioni_Interne <- factor(data$Condizioni_Interne,
                                  levels=c("Da Ristrutturare",
                                           "Buono / Abitabile",
                                           "Ottimo / Ristrutturato",
                                           "Nuovo / In Costruzione"))
table(data$Condizioni_Esterne)
data$Condizioni_Esterne <- factor(data$Condizioni_Esterne,
                                  levels=c("Da Ristrutturare",
                                           "Buono / Abitabile",
                                           "Ottimo / Ristrutturato",
                                           "Nuovo / In Costruzione"))
table(data$Classe_APE)
data$Classe_APE <- factor(data$Classe_APE, 
                          levels=c("G", "F", "E", "D", "C", "B", "A"))

## NAs and MIN-MAX values
sum(is.na(data))
colSums(is.na(data))

# NAs in Classe_APE for half of the dataset --> delete
# NAs in DataIncarico, CondizioniInterne, CondizioniEsterne 
#        and geographical description to solve
# NAs in DataAccettazione, PrezzoAccettazione are because 
# the house is not sold
# NAs in perc_Ribasso because some values in Prezzo_diVendita 
# are 0 (problem!)

summary(data)
# AnnoCostruzione and vars Prezzo_[..]_Vendita have 0 
# as min value: no sense

# vars PrezzoVendita put in k
data$Prezzo_Accettazione <- data$Prezzo_Accettazione/1000
data$Prezzo_di_Vendita <- data$Prezzo_di_Vendita/1000
data$Prezzo_Minimo_di_Vendita <- data$Prezzo_Minimo_di_Vendita/1000
data$Primo_prezzo_di_vendita <- data$Primo_prezzo_di_vendita/1000

colSums(is.na(data[data$Prezzo_di_Vendita==0, c("Data_Incarico",
                                                "Condizioni_Esterne",
                                                "Condizioni_Interne")]))
# 71/158, 79/136, 79/136 NAs

## Deletion
data$Classe_APE <- NULL

data <- data[!data$Prezzo_di_Vendita==0, ]
dim(data) # 2117 rows  (only 4% of rows deleted)

colSums(is.na(data))

data <- data[!is.na(data$Data_Incarico), ]
dim(data) # 2030 rows  (only 4% of rows deleted)

colSums(is.na(data))
summary(data$Anno_Costruzione)

# still 2 problmes: 0 as min in AnnoCostruzione, 
#                   NAs in CondizioniInterne_Esterne

## Imputation using mice function and cart method
# https://www.appsilon.com/post/imputation-in-r

# Imputation on data$AnnoCostruzione = 0
plot_original <- ggplot(data, aes(x = Anno_Costruzione)) +
  geom_histogram(fill = "#ad1538", color = "#000000",
                 position = "identity") +
ggtitle("Original distribution") +
theme_classic()

data[data$Anno_Costruzione==0, "Anno_Costruzione"] <- NA
numerical_data <- data[, sapply(data, is.numeric)]
numerical_data$ID_Immobile <- NULL
numerical_data$Prezzo_Accettazione <- NULL
# complete dataset
data$Anno_Costruzione <- complete(mice(numerical_data,
                                       method = "cart"))$Anno_Costruzione

summary(data$Anno_Costruzione)
sum(is.na(data$Anno_Costruzione))

plot_imputed <- ggplot(data, aes(x = Anno_Costruzione)) +
  geom_histogram(fill = "#15ad4f", color = "#000000",
                 position = "identity") +
ggtitle("Imputed distribution") +
theme_classic()

plot_original+plot_imputed

## Imputation on CondizioniInterne
plot_original <- ggplot(data, aes(x = Condizioni_Interne)) +
  geom_bar(fill = "#ad1538", color = "#000000") +
  geom_text(stat = 'count', aes(label = after_stat(count))) +
  ggtitle("Condizioni Interne frequencies") +
  theme_minimal()

numerical_data$Condizioni_Interne <- data$Condizioni_Interne
data$Condizioni_Interne <- complete(mice(numerical_data, 
                                         method = "cart"))$Condizioni_Interne

plot_imputed <- ggplot(data, aes(x = Condizioni_Interne)) +
  geom_bar(fill = "#15ad4f", color = "#000000") +
  geom_text(stat = 'count', aes(label = after_stat(count))) +
  ggtitle("Condizioni Interne frequencies") +
  theme_minimal()
plot_original+plot_imputed

## Imputation on CondizioniEsterne
plot_original <- ggplot(data, aes(x = Condizioni_Esterne)) +
  geom_bar(fill = "#ad1538", color = "#000000") +
  geom_text(stat = 'count', aes(label = after_stat(count))) +
  ggtitle("Condizioni Esterne frequencies") +
  theme_minimal()

numerical_data$Condizioni_Esterne <- data$Condizioni_Esterne
data$Condizioni_Esterne <- complete(mice(numerical_data, 
                                         method = "cart"))$Condizioni_Esterne

plot_imputed <- ggplot(data, aes(x = Condizioni_Esterne)) +
  geom_bar(fill = "#15ad4f", color = "#000000") +
  geom_text(stat = 'count', aes(label = after_stat(count))) +
  ggtitle("Condizioni Esterne frequencies") +
  theme_minimal()
plot_original+plot_imputed


colSums(is.na(data))


# ---- Exploration analysis ----
numerical_data <- data[, sapply(data, is.numeric)]
names(numerical_data)
numerical_data$ID_Immobile <- NULL
numerical_data$CAP <- NULL
numerical_data$Latitudine <- NULL
numerical_data$Longitudine <- NULL

dicotom_var <- c("Ascensore","Taverna","Mansarda","Soffitta",
                 "Ingresso_indipendente","Porta_Blindata")
for (name in dicotom_var){
  numerical_data[[name]] <- NULL
}

## Numerical vars 
# Boxplot  --> presence of outlier values
layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3), 
       heights = c(1,1),
       widths = c(1,1,1))
for (var in colnames(numerical_data[1:12])) {
  boxplot(data[[var]], ylab=var)
}
layout(matrix(c(1,2,3,4,5,6,7,7),nrow=2,ncol=4), 
       heights = c(1,1),
       widths = c(1,1,1,2))
for (var in colnames(numerical_data[13:19])) {
  boxplot(data[[var]], ylab=var)
}

# Distribution  --> skewed data
layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3), 
       heights = c(1,1),
       widths = c(1,1,1))
for (var in colnames(numerical_data[1:12])) {
  plot(density(data[[var]]), main=paste("Density of", var), xlab=var)
  lines(
    density(data[[var]])$x, 
    dnorm(density(data[[var]])$x, mean(data[[var]]), sd(data[[var]])), 
    col=2
  )
}
layout(matrix(c(1,2,3,4,5,6,7,7),nrow=2,ncol=4), 
       heights = c(1,1),
       widths = c(1,1,1,2))
complete_data <- na.omit(data)
for (var in colnames(numerical_data[13:19])) {
  plot(density(complete_data[[var]]), main=paste("Density of", var), xlab=var)
  lines(
    density(complete_data[[var]])$x, 
    dnorm(density(complete_data[[var]])$x,
          mean(complete_data[[var]]),
          sd(complete_data[[var]])), 
    col=2
  )
}


# HeatMap  ---> high correlated variables (problem of multicollinearity)
corr_matrix <- cor(numerical_data, use = "complete.obs")
corr_data <- melt(corr_matrix)

heatmap_plt <- ggplot(corr_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(title = "Correlation of numerical variables", x = "", y = "")
labs <- as.character(round(corr_data[, 3], 2))
heatmap_plt+geom_text(aes(label=labs), size=4)


## Categorical vars
categorical_var <- c(dicotom_var,names(data[sapply(data, is.factor)]))
setdiff( setdiff(names(data), names(numerical_data)) , categorical_var)
categorical_data <- data[,categorical_var]
# Barplot
layout(matrix(c(1,2,3,4), nrow=2, ncol=2), 
       heights = c(1,1),
       widths = c(1,1))
for (var in colnames(categorical_data)) {
  freq_table <- table(data[[var]])
  percentages <- prop.table(freq_table)*100
  
  bar_graph <- barplot(freq_table, main=paste("Distribution of", var))
  text(x=bar_graph, y=freq_table, 
       labels=paste0(round(percentages,1),"%"),
       col="black", cex=1, font=2)
}
# StatoImmobile (sold/not_sold) is balanced: 55%-45%


## Geographical vars (Comune,Localita,CodiceIstat,CAP,Longitudine,Latitudine)
unique_muni <- data %>% distinct(Comune)
# geocoding: coordinates from OpenStreetMap
coords_muni <- unique_muni %>%
  geocode(address = Comune, method = "osm", lat = lat, long = lon)
bologna_coord <- coords_muni[coords_muni$Comune=="Bologna", 2:3]
lat_bologna <- bologna_coord$lat
lon_bologna <- bologna_coord$lon
# distance in km of every municipality from Bologna
coords_muni$distance_km <- distHaversine(
  cbind(coords_muni$lon, coords_muni$lat),
  c(lon_bologna, lat_bologna)
) / 1000
# new concentric zones
bins <- c(0, 6, 12, 30, Inf)
labels <- c("Zona 1 (Centro/Periferia Immediata)",
            "Zona 2 (Prima Cintura)",
            "Zona 3 (Seconda Cintura)",
            "Zona 4 (Provincia Remota)")
coords_muni$Zona_Concentrica <- cut(
  coords_muni$distance_km,
  breaks = bins,
  labels = labels,
  right = FALSE,
  include.lowest = TRUE
)
coords_muni %>% arrange(distance_km)
# merge
coords_muni <- as.data.frame(coords_muni)
data <- data %>%
  left_join(coords_muni %>% dplyr::select(Comune, Zona_Concentrica),
            by = "Comune")
geografical_vars <- c("Comune","Localita","Codice_Istat_del_Comune",
                      "CAP","Longitudine","Latitudine")
for (name in geografical_vars){
  data[[name]] <- NULL
}
data$Zona_Concentrica <- factor(data$Zona_Concentrica,
                                levels = c("Zona 1 (Centro/Periferia Immediata)",
                                           "Zona 2 (Prima Cintura)",
                                           "Zona 3 (Seconda Cintura)",
                                           "Zona 4 (Provincia Remota)"))
# visualization
freq_table <- table(data$Zona_Concentrica)
percentages <- prop.table(freq_table)*100
bar_graph <- barplot(freq_table, main=paste("Distribution of Zona_Concentrica"))
text(x=bar_graph, y=freq_table-(0.05*max(freq_table)), 
     labels=paste0(round(percentages,1),"%"),
     col="black", cex=1, font=2)

## Date vars: data DataIncarico vs DataAccettazione
range(data$Data_Incarico)
round(table(year(data$Data_Incarico))/dim(data)[1]*100, 2)
hist(data$Data_Incarico, breaks = "years", freq = TRUE,
     main = "Date distribution (by year)")

complete_data <- na.omit(data)
range(complete_data$Data_Accettazione)
round(table(year(complete_data$Data_Accettazione))/dim(complete_data)[1]*100, 2)
hist(complete_data$Data_Accettazione, breaks = "years", freq = TRUE,
     main = "Date distribution (by year)")

data <- data %>%
  mutate(Diff_giorni = as.numeric(difftime(Data_Accettazione, Data_Incarico,
                                           units = "days")))
# visualization
summary(data$Diff_giorni)
boxplot(data$Diff_giorni, ylab="Diff_giorni")


### Save work space for the modelling part
save.image("Env_postExplor.RData")





# ---- Regression ----
load("Env_postExplor.RData")

data_reg <- data %>% column_to_rownames(var="ID_Immobile")
colSums(is.na(data))
# NAs problem: delete Data_Accettazione, Prezzo_Accettazione, Diff_giorni
data_reg$Data_Accettazione <- NULL
data_reg$Prezzo_Accettazione <- NULL
data_reg$Diff_giorni <- NULL

data_reg$Anno_Incarico <- as.numeric(format(data_reg$Data_Incarico, "%Y"))
data_reg$Data_Incarico <- NULL

sum(is.na(data_reg))


y <- data_reg$Primo_prezzo_di_vendita
# we have to delete Prezzo_di_Vendita, Prezzo_minimo, perc_Ribasso, Stato_Immobile
# --> information that the company will know after the prediction
data_reg$Prezzo_di_Vendita <- NULL
data_reg$perc_Ribasso <- NULL
data_reg$Prezzo_Minimo_di_Vendita <- NULL
data_reg$Stato_Immobile <- NULL

dim(data_reg) # 2030 rows, 31 cols

X <- data_reg %>% dplyr::select(-Primo_prezzo_di_vendita)
numerical_vars <- setdiff(names(X[, sapply(X, is.numeric)]),dicotom_var)

# Winsorization:
X[numerical_vars] <- lapply(X[numerical_vars], 
                            function(x) Winsorize(x, probs = c(0.05, 0.95)))

# check for high correlated vars
corr_matrix <- cor(X[,numerical_vars], use = "complete.obs")
diag(corr_matrix) <- 0
which(abs(corr_matrix) > 0.8, arr.ind = TRUE)

X$Totale_locali <- NULL
numerical_vars <- numerical_vars[numerical_vars!="Totale_locali"]
X$Piano <- NULL
numerical_vars <- numerical_vars[numerical_vars!="Piano"]

# Scaling:
X[,numerical_vars] <- scale(X[,numerical_vars])


## TRAIN vs TEST
data_reg <- cbind(X, data_reg[, "Primo_prezzo_di_vendita", drop = FALSE])
# split
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data_reg), replace=TRUE, prob=c(0.7,0.3))
data_train <- data_reg[sample,]
data_test <- data_reg[!sample,]
nrow(data_train)/nrow(data_reg)

# log transformation of the dependent var
data_train$Primo_prezzo_di_vendita <- log(data_train$Primo_prezzo_di_vendita)



### Different approaches for the dimension reduction:
evaluation_matrix <- matrix(rep(NA, 3*6), ncol=6)
colnames(evaluation_matrix ) <- c("Ridge", "Lasso", "Forward", "Backward",
                                  "PCR", "PLS")
rownames(evaluation_matrix ) <- c("MSE", "MAE", "MAPE")

# -------------------- RIDGE:
x_train <- model.matrix(Primo_prezzo_di_vendita ~ ., data_train)[, -1]

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x_train, data_train$Primo_prezzo_di_vendita, alpha = 0, 
                    lambda = grid, thresh = 1e-12)
plot(ridge.mod)
dim(coef(ridge.mod))
# cv to choose best lambda
set.seed(1)
cv.out <- cv.glmnet(x_train, data_train$Primo_prezzo_di_vendita, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
points(x=log(bestlam), y=cv.out$cvm[cv.out$lambda == bestlam],
       col = "black",pch = 19,cex = 1.5)
# coeff
ridge.coef <- predict(ridge.mod, type = "coefficients",
                      s = bestlam)
ridge.coef
plot_glmnet(ridge.mod,label=TRUE,s=cv.out$lambda.min)
# Prediction:
x_test <- model.matrix(Primo_prezzo_di_vendita ~ ., data_test)[, -1]
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = x_test)
# Evaluation metrics:
evaluation_matrix[1,1] <- mean((exp(ridge.pred)-data_test$Primo_prezzo_di_vendita)^2) 
# MSE: 7766
evaluation_matrix[2,1] <- mean(abs((exp(ridge.pred)-data_test$Primo_prezzo_di_vendita))) 
# MAE: 53
evaluation_matrix[3,1] <- mean(abs((exp(ridge.pred)-data_test$Primo_prezzo_di_vendita)
                                   /data_test$Primo_prezzo_di_vendita))*100
# MAPE: 21


# ------------------- LASSO:
x_train <- model.matrix(Primo_prezzo_di_vendita ~ ., data_train)[, -1]

grid <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x_train, data_train$Primo_prezzo_di_vendita, alpha = 1,
                    lambda = grid)
plot(lasso.mod)
dim(coef(lasso.mod))
# cv to choose lambda
set.seed(1)
cv.out <- cv.glmnet(x_train, data_train$Primo_prezzo_di_vendita, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
points(x=log(bestlam), y=cv.out$cvm[cv.out$lambda == bestlam],
       col = "black",pch = 19,cex = 1.5)
# coeff
lasso.coef <- predict(lasso.mod, type = "coefficients",
                      s = bestlam)
lasso.coef
plot_glmnet(lasso.mod,label=TRUE,s=cv.out$lambda.min)
# Prediction:
x_test <- model.matrix(Primo_prezzo_di_vendita ~ ., data_test)[, -1]
lasso.pred <- predict(lasso.mod, s = bestlam,newx = x_test)
# Evaluation metrics:
evaluation_matrix[1,2] <- mean((exp(lasso.pred)-data_test$Primo_prezzo_di_vendita)^2) 
# MSE: 8042
evaluation_matrix[2,2] <- mean(abs((exp(lasso.pred)-data_test$Primo_prezzo_di_vendita))) 
# MAE: 54
evaluation_matrix[3,2] <- mean(abs((exp(lasso.pred)-data_test$Primo_prezzo_di_vendita)
                                   /data_test$Primo_prezzo_di_vendita))*100
# MAPE: 22


# ------------------- FORWARD and BACKWARD:
# no best subset because it takes so long --> too many variables

# FWD: choice of number of regressors using BIC
regfit.fwd <- regsubsets(Primo_prezzo_di_vendita ~ .,
                         data_train, method = "forward", nvmax = dim(X)[2])
reg.summary <- summary(regfit.fwd)
minBIC.fwd <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(minBIC.fwd, reg.summary$bic[minBIC.fwd], col = "red", cex = 2,
       pch = 20)
# BWD: choice of number of regressors using BIC
regfit.bwd <- regsubsets(Primo_prezzo_di_vendita ~ .,
                         data_train, method = "backward", nvmax = dim(X)[2])
reg.summary <- summary(regfit.bwd)
minBIC.bwd <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(minBIC.bwd, reg.summary$bic[minBIC.bwd], col = "red", cex = 2,
       pch = 20)
# coefs
coef(regfit.fwd, minBIC.fwd)
coef(regfit.bwd, minBIC.bwd)

# Prediction:
# predict function
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
# FWD
fwd.pred <- predict.regsubsets(
  object = regfit.fwd, 
  newdata = data_test, 
  id = minBIC.fwd
)
# BWD
bwd.pred <- predict.regsubsets(
  object = regfit.bwd, 
  newdata = data_test, 
  id = minBIC.bwd
)

# Evaluation metrics:
evaluation_matrix[1,3] <- mean((exp(fwd.pred)-data_test$Primo_prezzo_di_vendita)^2)
# MSE: 7710
evaluation_matrix[2,3] <- mean(abs((exp(fwd.pred)-data_test$Primo_prezzo_di_vendita))) 
# MAE: 53
evaluation_matrix[3,3] <- mean(abs((exp(fwd.pred)-data_test$Primo_prezzo_di_vendita)
                                   /data_test$Primo_prezzo_di_vendita))*100
# MAPE: 21

evaluation_matrix[1,4] <- mean((exp(bwd.pred)-data_test$Primo_prezzo_di_vendita)^2)
# MSE: 7421
evaluation_matrix[2,4] <- mean(abs((exp(bwd.pred)-data_test$Primo_prezzo_di_vendita))) 
# MAE: 52
evaluation_matrix[3,4] <- mean(abs((exp(bwd.pred)-data_test$Primo_prezzo_di_vendita)
                                   /data_test$Primo_prezzo_di_vendita))*100
# MAPE: 21


# ------------------- PCA regression:
x_train <- model.matrix(Primo_prezzo_di_vendita ~ ., data_train)[, -1]
x_test <- model.matrix(Primo_prezzo_di_vendita ~ ., data_test)[, -1]

## Principal Components Regression:
set.seed(1)
pcr.fit <- pcr(Primo_prezzo_di_vendita ~ ., data=data_train, validation = "CV")
summary(pcr.fit)
# selection of the number of components
validationplot(pcr.fit, val.type = "MSEP")
points(x=18,y=MSEP(pcr.fit)$val[1,1,19],col="red",cex=1,pch=20)
# refitting
pcr.fit <- pcr(Primo_prezzo_di_vendita ~ ., data=data_train, ncomp = 18)
summary(pcr.fit)
# Prediction:
pcr.pred <- predict(pcr.fit, x_test, ncomp = 18)
# Evaluation metrics:
evaluation_matrix[1,5] <- mean((exp(pcr.pred)-data_test$Primo_prezzo_di_vendita)^2)
# MSE: 9340
evaluation_matrix[2,5] <- mean(abs((exp(pcr.pred)-data_test$Primo_prezzo_di_vendita))) 
# MAE: 60
evaluation_matrix[3,5] <- mean(abs((exp(pcr.pred)-data_test$Primo_prezzo_di_vendita)
                                   /data_test$Primo_prezzo_di_vendita ))*100
# MAPE: 24%


## Partial Least Squares:
set.seed(1)
pls.fit <- plsr(Primo_prezzo_di_vendita ~ ., data=data_train, validation = "CV")
summary(pls.fit)
# selection of the number of components
validationplot(pls.fit, val.type = "MSEP")
points(x=10,y=MSEP(pls.fit)$val[1,1,11],col="red",cex=1,pch=20)
# refitting
pls.fit <- plsr(Primo_prezzo_di_vendita ~ ., data=data_train, ncomp = 10)
summary(pls.fit)
# Prediction:
pls.pred <- predict(pls.fit, x_test, ncomp = 10)
# Evaluation metrics:
evaluation_matrix[1,6] <- mean((exp(pls.pred)-data_test$Primo_prezzo_di_vendita)^2)
# MSE: 7519
evaluation_matrix[2,6] <- mean(abs((exp(pls.pred)-data_test$Primo_prezzo_di_vendita))) 
# MAE: 53
evaluation_matrix[3,6] <- mean(abs((exp(pls.pred)-data_test$Primo_prezzo_di_vendita)
                                   /data_test$Primo_prezzo_di_vendita ))*100
# MAPE: 21%


### MODELS SELECTION:
round(evaluation_matrix, 2)




# ---- Classification ----
load("Env_postExplor.RData")

data_clas <- data %>% column_to_rownames(var="ID_Immobile")
colSums(is.na(data))
# NAs problem: delete Data_Accettazione, Prezzo_Accettazione, Diff_giorni
data_clas$Data_Accettazione <- NULL
data_clas$Prezzo_Accettazione <- NULL
data_clas$Diff_giorni <- NULL

data_clas$Anno_Incarico <- as.numeric(format(data_clas$Data_Incarico, "%Y"))
data_clas$Data_Incarico <- NULL

dim(data_clas) # 2030 rows, 35 cols
sum(is.na(data_clas))

y <- data_clas$Stato_Immobile
X <- data_clas %>% dplyr::select(-Stato_Immobile)
numerical_vars <- setdiff(names(X[, sapply(X, is.numeric)]),dicotom_var)

# Winsorization:
X[numerical_vars] <- lapply(X[numerical_vars], 
                            function(x) Winsorize(x, probs = c(0.05, 0.95)))

# check for high correlated vars
corr_matrix <- cor(X[,numerical_vars], use = "complete.obs")
diag(corr_matrix) <- 0
which(abs(corr_matrix) > 0.8, arr.ind = TRUE)

X$Totale_locali <- NULL
numerical_vars <- numerical_vars[numerical_vars!="Totale_locali"]
X$Piano <- NULL
numerical_vars <- numerical_vars[numerical_vars!="Piano"]
X$Prezzo_di_Vendita <- NULL
numerical_vars <- numerical_vars[numerical_vars!="Prezzo_di_Vendita"]
X$Prezzo_Minimo_di_Vendita <- NULL
numerical_vars <- numerical_vars[numerical_vars!="Prezzo_Minimo_di_Vendita"]

# Scaling:
X[,numerical_vars] <- scale(X[,numerical_vars])


## TRAIN vs TEST
data_clas <- cbind(X, data_clas[, "Stato_Immobile", drop = FALSE])
# split
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data_clas), replace=TRUE, prob=c(0.7,0.3))
data_train <- data_clas[sample,]
data_test <- data_clas[!sample,]
nrow(data_train)/nrow(data_clas)

# check distribution of Stato_Immobile
table(data_train$Stato_Immobile)/sum(table(data_train$Stato_Immobile))
table(data_test$Stato_Immobile)/sum(table(data_test$Stato_Immobile))
# equally distributed also in the train and test subset

### Different models for classification:
evaluation_matrix <- matrix(rep(NA, 3*6), ncol=6)
colnames(evaluation_matrix ) <- c("Logistic","Naive Bayes","Knn","LDA",
                                  "Random Forest","Decision Tree")
rownames(evaluation_matrix ) <- c("Misclassification Error","F1","adj AUC")



# -------------------- LOGISTIC:
# function for the calculation of the threshold
calc_threshold <- function(model, data_val){
  # calculate misclassification error for each threshold from 0.005 to 1
  prediction <- predict(model,newdata=data_val,type="response")
  step <- 0.005
  threshMeasures <- matrix(rep(NA, 2*(0.5/step+1)), ncol=2)
  i <- 1
  for(threshold in seq(0,0.5,step)){
    tab <- table(pred=prediction>threshold, Stato_Immobile=data_val$Stato_Immobile)
    miss.error <- 1-sum(diag(tab))/sum(tab)  # 1 - correct classif
    
    threshMeasures[i,1] <- threshold
    threshMeasures[i,2] <- miss.error  
    i<-i+1
    rm(tab)
  }
  results <- as.data.frame(matrix(c(threshMeasures[!is.na(threshMeasures)]), ncol=2))
  colnames(results) <- c("Threshold","Misclass_error")
  # find the threshold that minimize misclassfification error
  min_error <- min(results$Misclass_error)
  thresh <- min(results$Threshold[results$Misclass_error == min_error])
  
  plot_thresh <- ggplot(data = results, aes(x = Threshold, y = Misclass_error)) +
    geom_line(linewidth = 1) +
    geom_point(aes(x=thresh,y=min_error),colour="red",size=2) +
    labs(x="Threshold",y="Misclassification Error") +
    ylim(0.10,0.70)
  
  return(thresh)
}
# end 

# function for the calculation of the mc fadden index
mc.fadden <- function(model){
  1-(model$deviance/model$null.deviance)
}
# end


## Logistic models:
log.mod <- glm(Stato_Immobile ~ .,family="binomial",
                data=data_train)
summary(log.mod)
mc.fadden(log.mod)# 0.21
# comparison with the null model (without predictors)
anova(log.mod, test='Chisq') 
# model reduced: less regressors
log.mod.red.1 <- glm(Stato_Immobile ~ n_Unita_abitative+N_BalconiTerrazzi+
                       MQ_Garage+n_Posti_Auto+Fotovoltaico+Primo_prezzo_di_vendita+
                       Zona_Concentrica,
                     family="binomial",data=data_train)
summary(log.mod.red.1)
mc.fadden(log.mod.red.1) # 0.15
anova(log.mod.red.1, log.mod, test="Chisq")

log.mod.red.2 <- glm(Stato_Immobile ~ Macro_Tipologia+Rapporto_PianoAltezza+
                       n_Camere_da_Letto+n_Bagni+MQ_Commerciali+N_BalconiTerrazzi+
                       Ascensore+Porta_Blindata+Climatizzazione+Riscaldamento+
                       Fotovoltaico+Anno_Costruzione+Condizioni_Interne+
                       Primo_prezzo_di_vendita+Zona_Concentrica+Anno_Incarico,
                     family="binomial",data=data_train)
summary(log.mod.red.2)
mc.fadden(log.mod.red.2) # 0.19
anova(log.mod.red.2, log.mod, test="Chisq")

log.mod.red.3 <- glm(Stato_Immobile ~ Macro_Tipologia+Rapporto_PianoAltezza+
                       n_Camere_da_Letto+n_Bagni+MQ_Commerciali+N_BalconiTerrazzi+
                       Ascensore+Porta_Blindata+Climatizzazione+Riscaldamento+
                       Fotovoltaico+Anno_Costruzione+Condizioni_Interne+
                       Primo_prezzo_di_vendita+Zona_Concentrica+Anno_Incarico+
                       n_Unita_abitative+MQ_Garage+n_Posti_Auto,
                     family="binomial",data=data_train)
summary(log.mod.red.3)
mc.fadden(log.mod.red.3) # 0.20
anova(log.mod.red.3, log.mod, test="Chisq")

log.mod.final <- glm(Stato_Immobile ~ poly(MQ_Commerciali,2)+N_BalconiTerrazzi+
                       Fotovoltaico+Condizioni_Interne+Primo_prezzo_di_vendita+
                       Zona_Concentrica+poly(Anno_Incarico,2)+MQ_Garage+n_Posti_Auto,
                     family="binomial",data=data_train)
summary(log.mod.final)
mc.fadden(log.mod.final) # 0.22
anova(log.mod.final, log.mod, test="Chisq")
anova(log.mod.final, test="Chisq")
# Prediction:
pred_log <- predict(log.mod.final,newdata=data_test,type="response")
# 10 cv to choose the threshold
set.seed(1)
K <- 10
folds <- sample(rep(1:K, length.out = nrow(data_train)))
threshold_list <- rep(NA, K)
for(i in 1:K){
  data_train_fold <- data_train[folds!=i,]
  data_val_fold <- data_train[folds==i,]
  # retrain the model on the fold's training set
  log.mod.final_fold <- glm(Stato_Immobile ~ poly(MQ_Commerciali,2)+N_BalconiTerrazzi+
                              Fotovoltaico+Condizioni_Interne+Primo_prezzo_di_vendita+
                              Zona_Concentrica+poly(Anno_Incarico,2)+
                              MQ_Garage+n_Posti_Auto,
                            family="binomial",data=data_train_fold)
  # calculate the optimal threshold using the fold's valuation set
  threshold_list[i] <- calc_threshold(log.mod.final_fold, data_val_fold)
}
threshold <- mean(threshold_list)
y_pred_log <- as.integer(pred_log > threshold)
# Evaluation metrics:
conf.matrix <- table(data_test$Stato_Immobile, y_pred_log)
evaluation_matrix[1,1] <- 1-(sum(diag(conf.matrix)/sum(conf.matrix))) # Misclass err

recall <- conf.matrix[2,2]/sum(conf.matrix[2,]) # sensitivity
precision <- conf.matrix[2,2]/sum(conf.matrix[,2])
evaluation_matrix[2,1] <- 2*precision*recall/(precision+recall)

pred <- prediction(pred_log, labels = data_test$Stato_Immobile)
# ROC curve and AUC
roc.perf <- performance(pred,"tpr","fpr")
par(mfrow=c(1,1))
plot(roc.perf, colorize = TRUE,
     main = "ROC Curve",
     ylab = "True Positive Rate",
     xlab = "False Positive Rate")
abline(a=0, b=1, lty=2, col="black")

auc <- performance(pred,"auc")
auc@y.values
evaluation_matrix[3, 1] <- (auc@y.values[[1]]-0.5)/0.5  # 0 - 1 indicator (adj AUC)

# Lift curve
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift curve", colorize = T)



# -------------------- NAIVE BAYES:
# using of e1071::naiveBayes
nb.mod <- naiveBayes(Stato_Immobile ~ ., data = data_train)
# Prediction: 
y_nb_pred <- predict(nb.mod, newdata = data_test, type = "class")
conf.matrix <- table(data_test$Stato_Immobile, y_nb_pred)
# Evaluation metrics:
evaluation_matrix[1,2] <- 1-(sum(diag(conf.matrix)/sum(conf.matrix))) 

recall <- conf.matrix[2,2]/sum(conf.matrix[2,])
precision <- conf.matrix[2,2]/sum(conf.matrix[,2])
evaluation_matrix[2,2] <- 2*precision*recall/(precision+recall)


pred <- prediction(predict(nb.mod,newdata=data_test,type="raw")[,2],
                   labels=data_test$Stato_Immobile)
# ROC curve and AUC
roc.perf <- performance(pred,"tpr","fpr")
par(mfrow=c(1,1))
plot(roc.perf, colorize = TRUE,
     main = "ROC Curve",
     ylab = "True Positive Rate",
     xlab = "False Positive Rate")
abline(a=0, b=1, lty=2, col="black")

auc <- performance(pred,"auc")
auc@y.values
evaluation_matrix[3, 2] <- (auc@y.values[[1]]-0.5)/0.5  

# Lift curve
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift curve", colorize = T)



# -------------------- KNN:
# using of kknn::kknn

# 10 cv folds for select the best number of neighbors
set.seed(1)
K <- 10
folds <- sample(1:K, nrow(data_train), replace=TRUE)
k_values <- seq(1, 50)
miss.error_mat <- matrix(NA, nrow=K, ncol=length(k_values))

for(i in 1:K){
  data_train_fold <- data_train[folds!=i,]
  data_val_fold <- data_train[folds==i,]
  
  for (j in 1:length(k_values)){
   knn.fit <- kknn(formula = Stato_Immobile ~ ., 
                    train = data_train_fold, 
                    test = data_val_fold, 
                    k = k_values[j], 
                    kernel = "rectangular") 
    knn.pred <- fitted(knn.fit)
    tab <- table(pred=knn.pred, Stato_Immobile=data_val_fold$Stato_Immobile)
    miss.error_mat[i,j] <- 1 - sum(diag(tab)) / sum(tab)
  }
}
mean_miss_error <- colMeans(miss.error_mat)
best_k <- k_values[which.min(mean_miss_error)]
# model with best k
knn.mod <- kknn(formula = Stato_Immobile ~ ., 
                 train = data_train, 
                 test = data_test, 
                 k = best_k, 
                 kernel = "rectangular")
# Prediction:
y_knn_pred <- fitted(knn.mod)
# Evaluation metrics:
conf.matrix <- table(data_test$Stato_Immobile, y_knn_pred)
evaluation_matrix[1,3] <- 1-(sum(diag(conf.matrix)/sum(conf.matrix))) 

recall <- conf.matrix[2,2]/sum(conf.matrix[2,]) 
precision <- conf.matrix[2,2]/sum(conf.matrix[,2])
evaluation_matrix[2,3] <- 2*precision*recall/(precision+recall)

pred <- prediction(knn.mod$prob[,2],labels=data_test$Stato_Immobile)
# ROC curve and AUC
roc.perf <- performance(pred,"tpr","fpr")
par(mfrow=c(1,1))
plot(roc.perf, colorize = TRUE,
     main = "ROC Curve",
     ylab = "True Positive Rate",
     xlab = "False Positive Rate")
abline(a=0, b=1, lty=2, col="black")

auc <- performance(pred,"auc")
auc@y.values
evaluation_matrix[3,3] <- (auc@y.values[[1]]-0.5)/0.5

# lift curve
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift curve", colorize = T)



# -------------------- LDA
lda.mod <- lda(Stato_Immobile ~ .,data=data_train)
lda.mod
# Prediction:
y_lda_pred  <- predict(lda.mod, newdata=data_test)$class
# Evaluation metrics:
conf.matrix <- table(data_test$Stato_Immobile, y_lda_pred)
evaluation_matrix[1,4] <- 1-(sum(diag(conf.matrix)/sum(conf.matrix))) 

recall <- conf.matrix[2,2]/sum(conf.matrix[2,]) 
precision <- conf.matrix[2,2]/sum(conf.matrix[,2])
evaluation_matrix[2,4] <- 2*precision*recall/(precision+recall)

pred <- prediction(predict(lda.mod, newdata=data_test)$posterior[,2], 
                   labels=data_test$Stato_Immobile)
# ROC curve and AUC
roc.perf <- performance(pred,"tpr","fpr")
par(mfrow=c(1,1))
plot(roc.perf, colorize = TRUE,
     main = "ROC Curve",
     ylab = "True Positive Rate",
     xlab = "False Positive Rate")
abline(a=0, b=1, lty=2, col="black")

auc <- performance(pred,"auc")
auc@y.values
evaluation_matrix[3,4] <- (auc@y.values[[1]]-0.5)/0.5

# lift curve
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift curve", colorize = T)



# -------------------- RANDOM FOREST
set.seed(1)
rf.mod <- randomForest(x=subset(data_train, select = -Stato_Immobile),
                       y=data_train$Stato_Immobile,
                       ntree=1000,     
                       nodesize=5,    
                       importance=T)
plot(rf.mod)  # curve becomes flat around 300 trees
rf.mod <- randomForest(x=subset(data_train, select = -Stato_Immobile),
                       y=data_train$Stato_Immobile,
                       ntree=300,     
                       nodesize=5,    
                       importance=T)
plot(rf.mod)
# randomForest variable importance 
varImp <- importance(rf.mod)
varImpPlot(rf.mod, type=1, main="Importance")
# sort the variables by their importance
selVars <- rownames(varImp)[varImp[,3] > 5] # only for ones whit imp > 5
rf.mod.imp <- randomForest(x=data_train[, selVars],
                           y=data_train$Stato_Immobile,
                           ntree=200,     
                           nodesize=5,    
                           importance=T)
# Prediction:
y_rf_pred <- predict(rf.mod.imp, newdata=data_test, type='class')
# Evaluation metrics:
conf.matrix <- table(data_test$Stato_Immobile, y_rf_pred)
evaluation_matrix[1,5] <- 1-(sum(diag(conf.matrix)/sum(conf.matrix))) 

recall <- conf.matrix[2,2]/sum(conf.matrix[2,]) 
precision <- conf.matrix[2,2]/sum(conf.matrix[,2])
evaluation_matrix[2,5] <- 2*precision*recall/(precision+recall)

pred <- prediction(predict(rf.mod.imp, newdata=data_test, type='prob')[,2],
                   labels=data_test$Stato_Immobile)
# ROC curve and AUC
roc.perf <- performance(pred,"tpr","fpr")
par(mfrow=c(1,1))
plot(roc.perf, colorize = TRUE,
     main = "ROC Curve",
     ylab = "True Positive Rate",
     xlab = "False Positive Rate")
abline(a=0, b=1, lty=2, col="black")

auc <- performance(pred,"auc")
auc@y.values
evaluation_matrix[3,5] <- (auc@y.values[[1]]-0.5)/0.5

# lift curve
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift curve", colorize = T)



# -------------------- DECISION TREE
# validation set
set.seed(1)
obs.valid <- sample(x=1:dim(data_train)[1], 
                    size=0.25*dim(data_train)[1])
valid_set <- data_train[obs.valid,]
train_set <- data_train[-obs.valid,]
# tree
tree.max <- tree(Stato_Immobile ~ ., 
                 data = train_set, 
                 control = tree.control(nobs=nrow(train_set),
                 minsize = 2, mindev = 0.0001))
tree.control(nobs=nrow(train_set), 
             minsize = 2, mindev = 0.0001)

plot(tree.max)
text(tree.max, cex=.5)
# pruning
tree.pruned <- prune.tree(tree.max, newdata=valid_set)
plot(tree.pruned)
# best size
prun.ott <- tree.pruned$size[which.min(tree.pruned$dev)]
plot(tree.pruned)
points(x=prun.ott,
       y=tree.pruned$dev[tree.pruned$size==prun.ott],
       pch=19,lwd=2,col="red")

tree.pruned.ott <- prune.tree(tree.max, best=prun.ott)
plot(tree.pruned.ott)
text(tree.pruned.ott, cex=.8)
# Prediction:
y_tree_pred <- predict(tree.pruned.ott, newdata=data_test, type="class")
# Evaluation metrics:
conf.matrix <- table(data_test$Stato_Immobile, y_tree_pred)
evaluation_matrix[1,6] <- 1-(sum(diag(conf.matrix)/sum(conf.matrix))) 

recall <- conf.matrix[2,2]/sum(conf.matrix[2,])
precision <- conf.matrix[2,2]/sum(conf.matrix[,2])
evaluation_matrix[2,6] <- 2*precision*recall/(precision+recall)

pred <- prediction(predict(tree.pruned.ott, data_test, type="vector")[,2],
                   labels=data_test$Stato_Immobile)
# ROC curve and AUC
roc.perf <- performance(pred,"tpr","fpr")
par(mfrow=c(1,1))
plot(roc.perf, colorize = TRUE,
     main = "ROC Curve",
     ylab = "True Positive Rate",
     xlab = "False Positive Rate")
abline(a=0, b=1, lty=2, col="black")

auc <- performance(pred,"auc")
auc@y.values
evaluation_matrix[3,6] <- (auc@y.values[[1]]-0.5)/0.5

# lift curve
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift curve", colorize = T)




### MODELS SELECTION:
round(evaluation_matrix, 2)
# look also ROC and Lift curve
