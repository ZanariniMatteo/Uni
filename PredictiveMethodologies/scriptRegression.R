library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(GGally)
library(leaps)
library(car)
library(sandwich)
library(DescTools)
library(lmtest)
library(tseries)

data <- read_excel("datasetRegression.xlsx")

# ---- Cleaning ----
dim(data)  # 5000 rows, 24 cols
head(data)
str(data)

sum(duplicated(data$custid))

## NAs
sum(is.na(data))
colSums(is.na(data))

data[is.na(data$townsize), ]
table(data$region, data$townsize)

# only 2 obs on 5000 --> deletion
2/5000*100  # 0.04% 

data <- data[!is.na(data$townsize), ]


## Leveling and renaming
data$region <- factor(data$region)
data$townsize <- factor(data$townsize,levels=c(5,4,3,2,1), 
                        labels=c("<2.5k","2.5-10k","10-50k","50-250k",">250k"))
data$gender <- factor(data$gender,labels=c("Male","Female"))
data$edcat <- factor(data$edcat,
                     labels=c("NoHigh","High","SomeCollege","College","Post"))
data$jobcat <- factor(data$jobcat, 
                        labels=c("Managerial","Sales","Service","Agricultural","PrecisionProd","Fabrication"))
data$union <- factor(data$union)
data$retire <- factor(data$retire,labels=c("No","Yes"))
data <- data %>%
  rename(income_k = income)
data <- data %>%
  rename(creddebt_k = creddebt)
data$default <- factor(data$default,labels=c("No","Yes"))
data$marital <- factor(data$marital,labels=c("No","Yes"))
data$card <- factor(data$card, 
                      labels=c("AE","Visa","MasterC","Discover","Other"))
data$cardbenefit <- factor(data$cardbenefit, 
                      labels=c("None","CashBack","AirMiles","Other"))
data$cardtype <- factor(data$cardtype, 
                           labels=c("None","Gold","Platinum","Other"))
data$cardfee <- factor(data$cardfee,labels=c("No","Yes"))
data$active <- factor(data$active,labels=c("No","Yes"))

str(data)
# df and index
data <- as.data.frame(data)
row.names(data) <- data$custid
data$custid <- NULL


# ---- Exploration analysis ----
numerical_data <- data[, !sapply(data, is.factor)]
categorical_data <- data[, sapply(data, is.factor)]

summary(numerical_data) # min - max are reasonable
# check age reasonability 
sum(data$age < data$ed)
sum(data$age < data$employ)
sum(data$age < data$cardtenure)

boxplot(data$cardspent, main = "Card spent boxplot") # target var

mystats <- function(x){
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, 
           skew=skew, kurtosis=kurt))
}
mystats(data$cardspent)  # skew and kurtosis too far from norm values
qqnorm(data$cardspent)
qqline(data$cardspent, col = "red", lwd = 2)
# not normally distributed, too many outliers on the right tail


## Categorical vars barplot
layout(matrix(c(1,2,3,4,5,6,7,8), nrow=2, ncol=4), 
       heights = c(1,1),
       widths = c(1,1,1,1,1,1,1,1))
for (var in colnames(categorical_data[1:8])) {
  freq_table <- table(data[[var]])
  percentages <- prop.table(freq_table)*100
  
  bar_graph <- barplot(freq_table, main=paste("Distribution of", var))
  text(x=bar_graph, y=freq_table-(0.05*max(freq_table)), 
       labels=paste0(round(percentages,1),"%"),
       col="black", cex=1, font=2)
}
layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3), 
       heights = c(1,1),
       widths = c(1,1,1,1,1,1))
for (var in colnames(categorical_data[9:14])) {
  freq_table <- table(data[[var]])
  percentages <- prop.table(freq_table)*100
  
  bar_graph <- barplot(freq_table, main=paste("Distribution of", var))
  text(x=bar_graph, y=freq_table-(0.05*max(freq_table)), 
       labels=paste0(round(percentages,1),"%"),
       col="black", cex=1, font=2)
}


## Numerical vars distribution
layout(matrix(c(1,2,3,4,5,6,7,8,9,9),nrow=2,ncol=5), 
       heights = c(1,1),
       widths = c(1,1,1,1,2))
for (var in colnames(numerical_data)) {
  plot(density(data[[var]]), main=paste("Density of", var), xlab=var)
  lines(
    density(data[[var]])$x, 
    dnorm(density(data[[var]])$x, mean(data[[var]]), sd(data[[var]])), 
    col=2
  )
}

## Numerical vars boxplots
layout(matrix(c(1,2,3,4,5,6,7,8,9,9),nrow=2,ncol=5), 
       heights = c(1,1),
       widths = c(1,1,1,1,2))
for (var in colnames(numerical_data)) {
  boxplot(data[[var]], ylab=var)
}



## Relation
layout(matrix(c(1,2,3,4,5,6,7,8), nrow=2, ncol=4), 
       heights = c(1,1),
       widths = c(1,1,1,1,1,1,1,1))
for (var in colnames(categorical_data[1:8])) {
  boxplot(cardspent ~ data[[var]], 
          data = data,
          main = paste("Distribution of Exam Score for ", var),
          xlab = var,
          ylab = "Card spent",
          col = "lightblue")
}
layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3), 
       heights = c(1,1),
       widths = c(1,1,1,1,1,1))
for (var in colnames(categorical_data[9:14])) {
  boxplot(cardspent ~ data[[var]], 
          data = data,
          main = paste("Distribution of", var),
          xlab = var,
          ylab = "Card spent",
          col = "lightblue")
}
dev.off()


corr_matrix <- cor(numerical_data, use = "complete.obs")
corr_data <- melt(corr_matrix)

# heatmap
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
heatmap_plt +  geom_text(aes(label=labs), size=4)




# scatter and corr grams
lowerplots <- function(data, mapping) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color="darkgrey") +
    geom_smooth(method = "lm", color = "steelblue", se=FALSE) +
    geom_smooth(method="loess", color="red", se=FALSE, linetype="dashed")
}
diagplots <- function(data, mapping) {
  ggplot(data = data, mapping = mapping) +
    geom_histogram(fill="lightblue", color="black")
}
upperplots <- function(data, mapping) {
  ggally_cor(data=data, mapping=mapping, 
             display_grid=FALSE, size=3.5, color="black")
}
mytheme <-  theme(strip.background = element_blank(),
                  panel.grid       = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(color="grey20", fill=NA))
ggpairs(data, 
        columns=colnames(numerical_data),
        title = "Scatterplot Matrix with Linear and Loess Fits",
        lower = list(continuous = lowerplots),
        diag =  list(continuous = diagplots),
        upper = list(continuous = upperplots)) +
  mytheme






# ---- PreProcessing ----
# winsorization
data[names(numerical_data)] <- lapply(numerical_data, 
                                      function(x) Winsorize(x, probs = c(0.05, 0.95)))

# log
data$creddebt_k <- log(data$creddebt_k)
data <- data %>% rename(creddebt_k_log = creddebt_k)

data$income_k <- log(data$income_k)
data <- data %>% rename(income_k_log = income_k)

data$cardspent <- log(data$cardspent)
data <- data %>% rename(cardspent_log = cardspent)


# ---- Modelling ----
# train-test  0.7-0.3
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]

n_test <- nrow(test)

# baseline model 
# all the variables and normality assumption on residual
mod_baseline <- lm(cardspent_log ~ ., data=train)
summary(mod_baseline)# adjusted_R2: 0.5775

(mse_test_baseline <- mean( (test$cardspent_log-predict(mod_baseline, test))^2 ))
#  0.1430191
(bic_test <- BIC(mod_baseline))
# 3332.554


plot(density(mod_baseline$residuals)) 
plot(mod_baseline, which=1)
plot(mod_baseline, which=2)
plot(mod_baseline, which=3)



# subset, back, forw

# forward
mod_forw <- regsubsets(cardspent_log ~ ., data=train, method="forward", nvmax=37)
mod_summary <- summary(mod_forw)

plot(mod_summary$adjr2, type="l")
which.max(mod_summary$adjr2)
mod_summary$adjr2[which.max(mod_summary$adjr2)]
points(12, mod_summary$adjr2[which.max(mod_summary$adjr2)], pch=20, col="red") 
coef(mod_forw, which.max(mod_summary$adjr2))

# backward
mod_back <- regsubsets(cardspent_log ~ ., data=train, method="backward", nvmax=37)
mod_summary <- summary(mod_back)

plot(mod_summary$adjr2, type="l")
which.max(mod_summary$adjr2)
mod_summary$adjr2[which.max(mod_summary$adjr2)]
points(12, mod_summary$adjr2[which.max(mod_summary$adjr2)], pch=20, col="red") 
coef(mod_back, which.max(mod_summary$adjr2))

# best subset
mod_bestsub <- regsubsets(cardspent_log ~ ., data=train, nvmax=37)
mod_summary <- summary(mod_bestsub)

plot(mod_summary$adjr2, type="l")
which.max(mod_summary$adjr2)
mod_summary$adjr2[which.max(mod_summary$adjr2)]
points(12, mod_summary$adjr2[which.max(mod_summary$adjr2)], pch=20, col="red") 
coef(mod_bestsub, which.max(mod_summary$adjr2))


# lm with bestsubset method
mod_subset <- lm(cardspent_log ~ region+townsize+gender+edcat+jobcat+union+retire+
                   income_k_log+default+card+cardbenefit+cardfee+carditems, data=train)
summary(mod_subset) # non-relevant variables

mod_reduced <- lm(cardspent_log ~ townsize+gender+retire+income_k_log+card+carditems, data=train)
summary(mod_reduced)

# anova and vif
anova(mod_subset, mod_baseline)
anova(mod_reduced, mod_subset)

vif(mod_reduced)

summary(mod_reduced)
# intercept is positive
# female and retired people spend less, more carditems more cardspent_log
# American Express is in the intercept, all the other cards spend less
# the adjusted R^2 is 0.578, model explains more than the 50% of the variability of cardspent_log



# ---- Diagnostic ----
# check the assumptions of the model performing statistical tests and plots
# to check linearity (residuals vs fitted)
plot(predict(mod_reduced), residuals(mod_reduced))
abline(h=0, col="red", lwd=2)

# normality plot
plot(mod_reduced, which=2)
# Jarque-Bera test
jarque.bera.test(residuals(mod_reduced)) # reject H0

# constant variance
plot(mod_reduced, which=3)
# Breusch-Pagan TEST
bptest(mod_reduced) # reject H0

# leverage points
p <- length(coef(mod_reduced))
n <- nrow(train)
plot(hatvalues(mod_reduced))
abline(h=2*(p+1)/n, col="red", lw=2)
length(which(hatvalues(mod_reduced) > (2*(p+1)/n) )) / n *100
# 4% of leverage points

# outliers
plot(predict(mod_reduced), rstudent(mod_reduced))
abline(h=c(-3,3), col="blue")
(length(which(rstudent(mod_reduced)>3)) + length(which(rstudent(mod_reduced)<(-3)))) / n *100
# 0.2% outliers

# poly
mod_poly <- lm(cardspent_log ~ townsize+gender+retire+income_k_log+card+poly(carditems,4), 
               data=train)
summary(mod_poly)
plot(mod_poly, which=1)


# interaction
mod_int <- lm(cardspent_log ~ townsize+gender+retire+income_k_log+card*poly(carditems,4), 
              data=train)
summary(mod_int)
plot(mod_int, which=1)


#---------------------#### Robust error and coeff
jarque.bera.test(residuals(mod_int))
bptest(mod_int)
## Prediction model
# calculate robust standard errors for model coefficients
coef_mod_int <- coeftest(mod_int, vcov = vcovHC(mod_int, type = "HC0"))
confint(mod_int, vcov. = vcovHC(mod_int, type = "HC0"))

pred_mod_int <- predict(mod_int, newdata = test, interval = "confidence", 
                     vcov = vcovHC(mod_int, type = "HC0"))

# best model: compute the MSE (test) according to robust standard errors
# mod_int
summary(mod_int) # adj R^2 of 0.5973


pred_train <- predict(mod_int, newdata = train, interval = "confidence", 
                        vcov = vcovHC(mod_int, type = "HC0"))
(mse_int_train <- mean((train$cardspent_log - pred_train[, "fit"])^2)) # MSE_train 0.131898
(mse_int <- mean((test$cardspent_log - pred_mod_int[, "fit"])^2)) # MSE_test 0.1383176
(bic_int <- BIC(mod_int)) # BIC 3087.998

# mse_test_baseline: 0.1430191
# bic_test: 3332.554
# adjR: 0.5775

