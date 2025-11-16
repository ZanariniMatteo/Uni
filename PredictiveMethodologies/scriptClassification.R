# ---- 1 LIBRARIES ----

library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
library(pscl)
library(pROC)
library(ModelMetrics)



# ---- 2 DATASET ----

data <- read_excel("datasetClassification.xlsx")
View(data)
summary(data)
colSums(is.na(data))
str(data)


# ---- 2.1 FEATURE ENGENEERING ----

#The variables Balance and EstimatedSalary were converted from character to numeric type, 
#while Geography and Gender were transformed into categorical variables.

data$Balance <- as.numeric(data$Balance)
data$EstimatedSalary <- as.numeric(data$EstimatedSalary)
data$Geography <- as.factor(data$Geography)
data$Gender <- as.factor(data$Gender)
str(data)

#We now have: 2 categorical (factor) variables: Geography with 3 levels: France, Germany, and Spain
#Gender with 2 levels: Male and Female ; 1 text variable: Surname ; 11 numerical variables


# ---- 2.2 DEPENDENT VARIABLE ---- 

#The distribution of the target variable (Exited) was visualized to understand the class balance

ggplot(data = data, aes(x = factor(Exited))) +
  geom_bar(fill = "lightblue", color = "black") +
  xlab("Exited") +
  ylab("Frequency") +
  ggtitle("Frequency of Customer Churn") +
  theme_minimal() +
  theme(panel.grid = element_blank())

#The frequency and percentage of each class of the target variable were calculated to assess class distribution.
exited_freq <- table(data$Exited)
exited_perc <- exited_freq / sum(exited_freq) * 100
exited_perc
#Exited is the 20.37%
#No Exited is the 79.63%

str(data)
datanum <- data [,-c(1,2,3,5,6)]
str(datanum)



# ---- 3 EXPLORATORY ANALYSIS ---- 

#we recode our dependent variable (Exited) as a binary (1=yes, 0=no)
y<-data$Exited


# ---- 3.1 NUMERICAL VARIABLE ---- 

# ---- 3.1.1 BOXPLOT FOR EACH VARIABLE ---- 


#CREDIT SCORE VS EXITED
boxplot(data$CreditScore ~ data$Exited, outline=F, main="Churn vs CreditScore", xlab="Exited", ylab="CreditScore")
#As shown in the boxplot, there is no significant difference in credit scores between customers who exited (Exited = 1) and those who stayed (Exited = 0)
#This suggests that credit score might not be a strong predictor of churn in this dataset.

#AGE VS EXITED
boxplot(data$Age ~ data$Exited, outline=F, main="Churn vs Age", xlab="Exited", ylab="Age")
#The boxplot reveals a clear difference in age between customers who exited (Exited = 1) and those who stayed (Exited = 0).
#Customers who churned tend to be significantly older, with a higher median age and a wider interquartile range.
#In contrast, customers who stayed have a lower median age and a more concentrated distribution.
#This suggests that age may be an important factor influencing customer churn.

#TENURE VS EXITED
boxplot(data$Tenure ~ data$Exited, outline=F, main="Churn vs Tenure", xlab="Exited", ylab="Tenure")
#The boxplot shows that the distribution of Tenure is quite similar between customers who exited (Exited = 1) and those who remained (Exited = 0).
#Both groups have comparable medians and ranges, with slightly more variability observed among customers who churned.
#However, the lack of a strong difference suggests that Tenure may not be a decisive factor in predicting customer churn.

#BALANCE VS EXITED
boxplot(data$Balance ~ data$Exited, outline=F, main="Churn vs Balance", xlab="Exited", ylab="Balance")
#The boxplot comparing Balance across churned and retained customers shows similar distributions.
#Both groups have comparable median values and interquartile ranges, although customers who exited (Exited = 1) exhibit a slightly higher spread and upper range.

#NUMOFPRODUCTS VS EXITED
boxplot(data$NumOfProducts ~ data$Exited, outline=F, main="Churn vs NumOfProducts", xlab="Exited", ylab="NumOfProducts")
#The boxplot shows a notable difference in the median number of products between the two customer groups.
#Customers who did not churn (Exited = 0) have a median of 2 products, while those who churned (Exited = 1) have a median of just 1.
#This suggests that customers with fewer products are more likely to leave the bank.
#Despite similar interquartile ranges and overall distributions, the lower central tendency among churners may indicate a weaker engagement with the bank's services.

#HASCRCARD VS EXITED
boxplot(data$HasCrCard ~ data$Exited, outline=F, main="Churn vs HasCrCard", xlab="Exited", ylab="HasCrCard")

ggplot(data, aes(x = factor(HasCrCard), fill = factor(Exited))) +
  geom_bar(position = "fill") +
  labs(title = "Proporzione di Churn per HasCrCard", x = "HasCrCard", y = "Proporzione", fill = "Exited") +
  scale_y_continuous(labels = scales::percent)
#The proportion of customers who exited (Exited = 1) is nearly identical regardless of whether they have a credit card (HasCrCard = 1) or not (HasCrCard = 0).
#This indicates that having a credit card does not appear to be a strong predictor of churn.

#ISACTIVEMEMBER VS EXITED
boxplot(data$IsActiveMember ~ data$Exited, outline=F, main="Churn vs IsActiveMember", xlab="Exited", ylab="IsActiveMember")

ggplot(data, aes(x = factor(IsActiveMember), fill = factor(Exited))) +
  geom_bar(position = "fill") +
  labs(title = "Proporzione di Churn per IsActiveMember", x = "IsActiveMember", y = "Proporzione", fill = "Exited") +
  scale_y_continuous(labels = scales::percent)
#The proportion of customers who exited (Exited = 1) is slightly higher among those who are not active members (IsActiveMember = 0) compared to those who are active members (IsActiveMember = 1).
#This suggests that being an active member may be associated with a lower probability of churn.

#ESTIMATEDSALARY VS EXITED
boxplot(data$EstimatedSalary ~ data$Exited, outline=F, main="Churn vs EstimatedSalary", xlab="Exited", ylab="EstimatedSalary")
#The boxplot shows that the distribution of EstimatedSalary is almost identical for both customers who exited and those who remained. 
#The medians, interquartile ranges, and overall spread appear very similar, suggesting that EstimatedSalary does not play a significant role in predicting churn.


# ---- 3.2 CATEGORICAL VARIABLE ---- 

str(data)


# ---- 3.2.1 COUNT PLOT AND HEATMAP FOR EACH VARIABLE ---- 


#EXITED VS GEOGRAPHY

ggplot(data = data) +
  geom_count(mapping = aes(x = Exited, y = Geography))
data %>%
  count(Exited, Geography)
data %>%
  count(Exited, Geography) %>%
  ggplot(mapping = aes(x = Exited, y = Geography)) +
  geom_tile(mapping = aes(fill = n))
#From the heatmap, we observe that the majority of customers are from France, regardless of whether they exited or not. 
#However, Germany seems to have a relatively higher proportion of customers who exited compared to Spain and France. 
#This suggests that customer churn could be more associated with customers located in Germany.

#EXITED VS GENDER

ggplot(data = data) +
  geom_count(mapping = aes(x = Exited, y = Gender))
data %>%
  count(Exited, Gender)
data %>%
  count(Exited, Gender) %>%
  ggplot(mapping = aes(x = Exited, y = Gender)) +
  geom_tile(mapping = aes(fill = n))
#From the heatmap, it can be seen that the number of male customers is higher than that of female customers, both among those who exited and those who stayed.
#However, the difference between males and females among exited customers does not appear very pronounced from the visual alone.




# ---- 4 REGRESSION ---- 
set.seed(1)  
datareg <- data[,-c(1,2,3)]
sample <- sample(c(TRUE, FALSE), nrow(datareg), replace = TRUE, prob = c(0.7, 0.3))

train <- datareg[sample, ]
test <- datareg[!sample, ]

str(train)  


# ---- 4.1 PROBIT ----

#PROBIT REGRESSION 0 (complete)

# Complete probit regression with all variable

fit_probit <- glm(data = train, 
                  Exited ~ ., 
                  family = binomial(link = 'probit'))

summary(fit_probit)
#Significant variable (3 star): GeographyGermany, GenderMale, Age, Balance, IsActiveMember 
#Marginally significant variable (1 star): CreditScore, NumOfProducts, Tenure
#Insignificant variable (pvalue > 0.1): GeographySpain, Tenure, HasCrCard, EstimatedSalary

# Comparison with the null model (without predictors)
anova(fit_probit, test = "Chisq")
# The lower the(>Chi), the more useful the variable is (same as above)


#PROBIT 1
#We only use significant variable (3 star)
fit_probit1 <- glm(data = train,
                   Exited ~ Geography + Gender + Age + Balance + IsActiveMember,
                   family = binomial(link = 'probit'))

summary(fit_probit1)
#Customers in Germany have higher probability of churning compared to those in France (reference category)
#Male customers have a lower probability of churning compared to female customers
#The older the customer, the higher the probability of churn
#A higher account balance is associated with a greater probability of churn
#Being an active member reduces the probability of churn
#Spain is not statistically significant; it does not seem to affect the probability of churn compared to France

confint(fit_probit1)
#The confidence intervals for each coefficient help evaluate the stability and reliability of the estimates
#For example: Age - the entire confidence interval is positive, which suggests a consistently positive and certain effect on the probability of churn
#Spain - the confidence interval includes zero, which suggests that the effect is not statistically significant

#No exact equivalent of R^2 so we use the McFadden R^2 index
pR2(fit_probit1)
#McFadden 0.1515


#BASELINE ACCURACY 

prop.table(table(train$Exited)) #0.7962 no exited, 0,2038 exited
pred.probit1 <- predict(fit_probit1, newdata = test, type = "response")
#Put the treshold on 0.5 (if prediction is above --> 1; otherwise --> 0)
d_probit1 <- 1 * (pred.probit1 > 0.5)


#CONFUSION MATRIX

cm_probit1 <- table(test$Exited, d_probit1)
cm_probit1
#2343 TN: customers who did not churn and the model correctly predicted 0
#116 TP: customers who churned and the model correctly predicted 1
#502 FN: customers who churned but the model incorrectly predicted 0
#75 FP: customers who did not churn but the model incorrectly predicted 1

#Accuracy
acc_probit1 <- (cm_probit1[1,1] + cm_probit1[2,2])/nrow(test)
acc_probit1 #accuracy of 0.8099

#Precision
precision_probit1 <- (cm_probit1[2,2])/colSums(cm_probit1)[2]
precision_probit1 #precision of 0.6073

#ROC Curve and AUC
testRoc <- roc(test$Exited ~ pred.probit1, plot=T, print.auc=T) #AUC 0.766
testRoc$specificities
testRoc$sensitivities

#Plot the ROC Curve
Roc_obj <- roc(test$Exited, pred.probit1)
plot(Roc_obj, main = "ROC Curve fot the Probit Regression Model")
abline(0,1,lty = 2, col = "gray")



#Probit2 
#Add CreditScore and NumOfProducts (1 star)
fit_probit2 <- glm(data = train,
                   Exited ~ Geography + Gender + Age + Balance + IsActiveMember + CreditScore + NumOfProducts,
                   family = binomial(link = 'probit'))

summary(fit_probit2)
confint(fit_probit2)
pR2(fit_probit2) #McFadden 0.1531

#BASELINE ACCURACY 

prop.table(table(train$Exited))
pred.probit2 <- predict(fit_probit2, newdata = test, type = "response")
#Put the treshold on 0.5 (if prediction is above --> 1; otherwise --> 0)
d_probit2 <- 1 * (pred.probit2 > 0.5)


#CONFUSION MATRIX

cm_probit2 <- table(test$Exited, d_probit2)
cm_probit2
#2335 TN: customers who did not churn and the model correctly predicted 0
#124 TP: customers who churned and the model correctly predicted 1
#494 FN: customers who churned but the model incorrectly predicted 0
#83 FP: customers who did not churn but the model incorrectly predicted 1

#Accuracy
acc_probit2 <- (cm_probit2[1,1] + cm_probit2[2,2])/nrow(test)
acc_probit2 #Accuracy of 0.8099

#Precision
precision_probit2 <- (cm_probit2[2,2])/colSums(cm_probit2)[2]
precision_probit2 #Precision of 0.5990

#ROC Curve and AUC
testRoc2 <- roc(test$Exited ~ pred.probit2, plot=T, print.auc=T) #AUC of 0.766 

#Comparison between fit_probit1 and fit_probit2:
#fit_probit2 is slightly more performant in terms of McFadden
#However, precision is higher in fit_probit0
#Accuracy and AUC are identical between the two models.


#Probit 3
#Add age^2

fit_probit3 <- glm(data = train,
                   Exited ~ Geography + Gender + Age + Balance + IsActiveMember + CreditScore + NumOfProducts + I(Age^2),
                   family = binomial(link = 'probit'))

summary(fit_probit3)
pR2(fit_probit3) #McFadden 0.1753

#BASELINE ACCURACY 

prop.table(table(train$Exited))
pred.probit3 <- predict(fit_probit3, newdata = test, type = "response")
#Put the treshold on 0.5 (if prediction is above --> 1; otherwise --> 0)
d_probit3 <- 1 * (pred.probit3 > 0.5)


#CONFUSION MATRIX

cm_probit3 <- table(test$Exited, d_probit3)
cm_probit3
#2340 TN: customers who did not churn and the model correctly predicted 0
#149 TP: customers who churned and the model correctly predicted 1
#469 FN: customers who churned but the model incorrectly predicted 0
#78 FP: customers who did not churn but the model incorrectly predicted 1

#Accuracy
acc_probit3 <- (cm_probit3[1,1] + cm_probit3[2,2])/nrow(test)
acc_probit3 #Accuracy of 0.8198

#Precision
precision_probit3 <- (cm_probit3[2,2])/colSums(cm_probit3)[2]
precision_probit3 #Precision of 0.6564

#ROC Curve and AUC
testRoc3 <- roc(test$Exited ~ pred.probit3, plot=T, print.auc=T) #AUC of 0.782



#Probit 4
#Add I(NumOfProducts^2)

fit_probit4 <- glm(data = train,
                   Exited ~ Geography + Gender + Age + Balance + IsActiveMember + CreditScore + NumOfProducts + I(Age^2) + I(NumOfProducts^2),
                   family = binomial(link = 'probit'))




summary(fit_probit4)
pR2(fit_probit4) #McFadden 0.2886

#BASELINE ACCURACY 
#proportion of 0 (No churn) in the train set

prop.table(table(train$Exited))
pred.probit4 <- predict(fit_probit4, newdata = test, type = "response")
#threshold of 0.5: if >0.5 we predict 1 (exited), otherwise 0
d_probit4 <- 1 * (pred.probit4 > 0.5)


#CONFUSION MATRIX
#Compare predictions with actual values
cm_probit4 <- table(test$Exited, d_probit4)
cm_probit4
#2323 TN: customers who did not churn and the model correctly predicted 0
#247 TP: customers who churned and the model correctly predicted 1
#371 FN: customers who churned but the model incorrectly predicted 0
#95 FP: customers who did not churn but the model incorrectly predicted 1

#Accuracy
acc_probit4 <- (cm_probit4[1,1] + cm_probit4[2,2])/nrow(test)
acc_probit4 #0.8465

#Precision
precision_probit4 <- (cm_probit4[2,2])/colSums(cm_probit4)[2]
precision_probit4 # 0.7222

#ROC Curve and AUC
testRoc4 <- roc(test$Exited ~ pred.probit4, plot=T, print.auc=T) #AUC 0.835


# ---- 4.2 LOGIT REGRESSION OK---- 

#complete model

logit <- glm(Exited ~., family = binomial(link = 'logit'), data = train)
summary(logit)
anova(logit, test = "Chisq")

#model 2 logit regression with only relevant variables (3 stelline)

logit2 <- glm(Exited ~ Geography + Gender + Age + Balance + IsActiveMember, family = binomial(link = 'logit'), data = train)
summary(logit2)
pR2(logit2) #McFadden 0.1524

#model 3 add CreditScore and NumOfProduct like in probit model (1/2/3 stelline)

logit3 <- glm(Exited ~ CreditScore + Geography + Gender + Age + Balance + NumOfProducts + IsActiveMember, family = binomial(link = 'logit'), data = train)
summary(logit3)
pR2(logit3) #McFadden 0.1539

#baseline accuracy
pred_logit3<- predict(logit3, newdata=test, type="response")
pred_logit3  
d_logit3 <- 1*(pred_logit3 >0.5)

#confusion matrix
cm_logit3<- table(test$Exited, d_logit3)
cm_logit3

#Accuracy

acc_logit3<- (cm_logit3[1,1] + cm_logit3[2,2])/nrow(test)
acc_logit3 #accuracy of 0.8069 

#Precision

precision.logit3 <- (cm_logit3[2,2])/colSums(cm_logit3)[2]
precision.logit3 #precision of 0.5696

#ROC Curve and AUC

testRoc5.log3 <- roc(test$Exited, pred_logit3, plot=T, print.auc=T) #AUC 0.764

#model 4, i start adding the variables i included in the probit (age^2)

logit4 <- glm(Exited ~ Geography + Gender + Age + Balance + IsActiveMember + CreditScore + NumOfProducts + I(Age^2),
              family = binomial(link = 'logit'),
              data = train)
summary(logit4)
pR2(logit4) #0.1834

#baseline accuracy
pred_logit4<- predict(logit4, newdata=test, type="response")
pred_logit4  
d_logit4 <- 1*(pred_logit4 >0.5)

#confusion matrix
cm_logit4<- table(test$Exited, d_logit4)
cm_logit4

#Accuracy

acc_logit4<- (cm_logit4[1,1] + cm_logit4[2,2])/nrow(test)
acc_logit4 #accuracy of 0.8238 

#Precision

precision.logit4 <- (cm_logit4[2,2])/colSums(cm_logit4)[2]
precision.logit4 #precision of 0.6578

#ROC Curve and AUC

testRoc5.log4 <- roc(test$Exited, pred_logit4, plot=T, print.auc=T) #AUC 0.783


#model 5 (i add I(NumOfProducts^2))

logit5 <- glm(Exited ~ Geography + Gender + Age + Balance + IsActiveMember + CreditScore + NumOfProducts + I(Age^2) + I(NumOfProducts^2),
              family = binomial(link = 'logit'),
              data = train)
summary(logit5)
pR2(logit5) # 0.2966

#baseline accuracy
pred_logit5<- predict(logit5, newdata=test, type="response")
pred_logit5  
d_logit5 <- 1*(pred_logit5 >0.5)

#confusion matrix
cm_logit5<- table(test$Exited, d_logit5)
cm_logit5

#Accuracy

acc_logit5<- (cm_logit5[1,1] + cm_logit5[2,2])/nrow(test)
acc_logit5 #accuracy of 0.8481 

#Precision

precision.logit5 <- (cm_logit5[2,2])/colSums(cm_logit5)[2]
precision.logit5 #precision of 0.7163

#ROC Curve and AUC

testRoc5.log5 <- roc(test$Exited, pred_logit5, plot=T, print.auc=T) #AUC 0.835



# ---- 5 BRIER SCORE ---- 


brier_score_probit2 <- brier(fit_probit2, pred.probit2) #0.1354
brier_score_probit4 <- brier(fit_probit4, pred.probit4) #0.1090
brier_score_logit3 <- brier(logit3, pred_logit3) #0.1352
brier_score_logit5 <- brier(logit5, pred_logit5) #0.1077
which.min(c(brier_score_probit2, brier_score_probit4, brier_score_logit3, brier_score_logit5))
#brier_score_logit5
