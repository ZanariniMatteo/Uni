#############################################################
# change working directory
setwd("~/.MATTEO/ReportDataScience")
# install these libraries and then load them:
library(ggplot2)
library(magrittr)
library(dplyr)
library(corrplot)
library(reshape2)
library(car)
library(lars)
library(rpart)
library(class)
library(randomForest)
library(ROCR)
############################################################


# loading data
data <- read.csv("watson_healthcare_modified.csv")
head(data, 3)

# cleaning
str(data)
summary(data)
dim(data) # 1676 rows, 35 col

for (var in colnames(data)) {
  cat(paste("\n----------------------------------------------\n"), var)
  print(table(data[[var]]))
}


# eliminating variables with no interest
copy <- data
colnames(data)
data <- data[, -c(1,5,10,13,20,22,27)]

# NAs
colSums(is.na(data))

# factors
data$Attrition <- factor(data$Attrition, levels = c("No", "Yes"))
data$BusinessTravel <- factor(data$BusinessTravel, levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))
data$Department <- factor(data$Department, levels = c("Maternity", "Neurology", "Cardiology"))
data$EducationField <- factor(data$EducationField, levels = c("Other", "Human Resources", "Life Sciences", "Marketing", "Technical Degree", "Medical"))
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))
data$JobRole <- factor(data$JobRole, levels = c("Other", "Admin", "Administrative", "Nurse", "Therapist"))
data$MaritalStatus <- factor(data$MaritalStatus, levels = c("Single", "Divorced", "Married"))
data$OverTime <- factor(data$OverTime, levels = c("No", "Yes"))

str(data)
colSums(is.na(data))

# exploration
# dependent variable
freq_table <- table(data$Attrition)
percentages <- prop.table(freq_table)*100
bar_graph <- barplot(freq_table, main="Distribution of Attrition",xlab = "Attrition",ylab = "Frequency")
text(x=bar_graph, y=freq_table-(0.05*max(freq_table)), 
     labels=paste0(round(percentages,1),"%"),
     col="black", cex=1, font=2)

# no attrition: 88.13%
# attrition: 11.87%

data_categorical <- data[, sapply(data, is.factor)]
data_numerical <- data[, !sapply(data, is.factor)]

# numerical var
summary(data_numerical)

layout(matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=2,ncol=5), 
       heights = c(1,1),
       widths = c(1,1,1,1,1))
for (var in colnames(data_numerical[, 1:10])) {
  boxplot(data[[var]] ~ data$Attrition, outline=F, xlab="Attrition", ylab=var)
}
layout(matrix(c(1,2,3,4,5,6,7,8,9,10),nrow=2,ncol=5), 
       heights = c(1,1),
       widths = c(1,1,1,1,1))
for (var in colnames(data_numerical[, 11:20])) {
  boxplot(data[[var]] ~ data$Attrition, outline=F, xlab="Attrition", ylab=var)
}

# attrition-age: people who drop their job are on average younger
# attrition-distance from home: higher distance from home are more likely to drop their job
# attrition-education: people who left their job have on average less years of education than people who do not
# attrition-environment satisfaction:the ones who drop their jobs are people less satisfied with the environment where they work
# attrition-job level: people with an higher job level are unlikely to drop their work 
# attrition-job satisfaction: same as job level
# attrition-monthly income: monthly income is lower among people who decide to stop working
# attrition-total working years: workers who left their job had on average lower total working years
# attrition-years at company: this means that the majority of people who left their job was not working there for a large amount of time
# attrition-years current role: workers who decide to continue working are the ones with the larger amount of years in that role
# attrition-years since last promotion: quite strange, as people who do not left their work have an higher range - maybe there are other factors
# attrition-years with current manager: no attrition has more years on average with the current manager



# categorical vars
# Attrition-BusinessTravel
ggplot(data = data) +
  geom_count(mapping = aes(x = Attrition, y = BusinessTravel))
data %>% 
  count(Attrition, BusinessTravel)
data %>% 
  count(Attrition, BusinessTravel) %>%  
  ggplot(mapping = aes(x = BusinessTravel, y = Attrition)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs BusinessTravel",
       x = "BusinessTravel",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# it doesn't look interesting 

# Attrition-Department
ggplot(data = data) +
  geom_count(mapping = aes(x = Department, y = Attrition))
data %>% 
  count(Attrition, Department) 
data %>% 
  count(Attrition, Department) %>%  
  ggplot(mapping = aes(x = Department, y = Attrition)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs Department",
       x = "BusinessTravel",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# largest attrition is in maternity

# Attrition-Education Field
ggplot(data = data) +
  geom_count(mapping = aes(x = EducationField, y = Attrition))
data %>% 
  count(Attrition, EducationField)
data %>% 
  count(Attrition, EducationField) %>%  
  ggplot(mapping = aes(x = EducationField, y = Attrition)) +
  geom_tile(mapping = aes(fill = n))  +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs EducationField",
       x = "EducationField",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# it doesn't look interesting 

# Attrition-Gender
ggplot(data = data) +
  geom_count(mapping = aes(x = Gender, y = Attrition))
data %>% 
  count(Attrition, Gender) 
data %>% 
  count(Attrition, Gender) %>%  
  ggplot(mapping = aes(x = Gender, y = Attrition)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs Gender",
       x = "Gender",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# largest attrition is in male

# Attrition-Job Role
ggplot(data = data) +
  geom_count(mapping = aes(x = JobRole, y = Attrition))
data %>% 
  count(Attrition, JobRole)
data %>% 
  count(Attrition, JobRole) %>%  
  ggplot(mapping = aes(x = JobRole, y = Attrition)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs JobRole",
       x = "JobRole",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# it doesn't look interesting 

# Attrition-Marital Status
ggplot(data = data) +
  geom_count(mapping = aes(x = MaritalStatus, y = Attrition))
data %>% 
  count(Attrition, MaritalStatus) 
data %>% 
  count(Attrition, MaritalStatus) %>%  
  ggplot(mapping = aes(x = MaritalStatus, y = Attrition)) +
  geom_tile(mapping = aes(fill = n))  +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs MaritalStatus",
       x = "MaritalStatus",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# divorced people seems to be the least likely to drop their job

# Attrition-OverTime
ggplot(data = data) +
  geom_count(mapping = aes(x = OverTime, y = Attrition)) 
data %>% 
  count(Attrition, OverTime) 
data %>% 
  count(Attrition, OverTime)  %>%  
  ggplot(mapping = aes(x = OverTime, y = Attrition)) +
  geom_tile(mapping = aes(fill = n))  +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap: Attrition vs OverTime",
       x = "OverTime",
       y = "Attrition",
       fill = "Count") +
  theme_minimal()
# overtime working leads to more attrition


# correlation
corr_matrix <- cor(data_numerical, use="complete.obs")
corr_data <- melt(corr_matrix)
ggplot(corr_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(title = "Correlation of numerical variables", x = "", y = "")
# it seems to be multicollinearity problem


data_num_Attrition <- data[, c(colnames(data_numerical), "Attrition")]
data_num_Attrition$Attrition <- as.numeric(data_num_Attrition$Attrition) -1
model <- lm(Attrition ~ ., data = data_num_Attrition)
vif(model)
# JobLevel, MonthlyIncome, TotalWorkingYears, YearsAtCompany high level of vif
model <- lm(Attrition ~ . -JobLevel, data = data_num_Attrition)
vif(model)
model <- lm(Attrition ~ . -JobLevel -TotalWorkingYears, data = data_num_Attrition)
vif(model)
model <- lm(Attrition ~ . -JobLevel -TotalWorkingYears -YearsAtCompany, data = data_num_Attrition)
vif(model) # no more multicollinearity problem


dev.off()

## MODELS
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
dtrain <- data[sample, ]
dtest <- data[!sample, ]

nrow(dtrain)/(nrow(dtest)+nrow(dtrain)) *100

copy_train <- dtrain
copy_test <- dtest

### Logit model:
all <- glm(Attrition ~ . -JobLevel -TotalWorkingYears -YearsAtCompany,
    family=binomial(link='logit'), data=dtrain)
summary(all)


logit <- glm(Attrition ~ Age+BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+MonthlyIncome+
              JobInvolvement+JobRole+JobSatisfaction+MaritalStatus+NumCompaniesWorked+OverTime+Shift+
                WorkLifeBalance+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,
            family=binomial(link='logit'), data=dtrain)
summary(logit)

anova(logit, all, test="Chisq")
# predicting:
dtrain$pred_logit <- predict(logit,newdata=copy_train,type="response")
dtest$pred_logit <- predict(logit,newdata=copy_test,type="response")


### Lasso model:
y <- as.numeric(dtrain$Attrition) - 1
x <- model.matrix(Attrition ~ ., 
                  data=copy_train)
head(x)

lasso <- lars(x=x,y=y, trace=TRUE)
plot(lasso)

# cross-validation using 30 folds
set.seed(123)
cv <- cv.lars(x=x, y=y, K=30)
cv_min <- cv$index[cv$cv==min(cv$cv)] # find the optimal shrinkage parameter
points(cv_min, cv$cv[cv$cv==min(cv$cv)],
       col="red",pch=1,cex=2,lwd=2)

coefs <- coef(lasso, s=cv_min, mode="fraction")
coefs[coefs!=0]
# predicting:
dtrain$pred_lasso <- predict(lasso,x,s=cv_min,mode="fraction")$fit

x <- model.matrix(Attrition ~ ., 
                  data=copy_test)
dtest$pred_lasso <- predict(lasso,x,s=cv_min,mode="fraction")$fit



### Decision tree model:
dt_model <- rpart(Attrition=="Yes" ~ . -JobLevel -TotalWorkingYears -YearsAtCompany,
                  data = copy_train)

plot(dt_model, 
     uniform=TRUE,margin = 0.1,compress = TRUE,main = "Decision Tree")
text(dt_model,use.n=TRUE,all=TRUE,cex=0.8,minlength=10)
# predicting:
dtrain$pred_tree <- predict(dt_model, newdata=copy_train)
dtest$pred_tree <- predict(dt_model, newdata=copy_test)


### Knn model:
table(dtrain$Attrition)
k <- round(1/(140/1023), 0)

numerical_regr_train <- copy_train[, colnames(data_numerical)]
numerical_regr_train <- numerical_regr_train[, !colnames(numerical_regr_train) 
                                             %in% c("JobLevel", "TotalWorkingYears", "YearsAtCompany")]
numerical_regr_test <- copy_test[, colnames(data_numerical)]
numerical_regr_test <- numerical_regr_test[, !colnames(numerical_regr_test) 
                                             %in% c("JobLevel", "TotalWorkingYears", "YearsAtCompany")]
output_model <- copy_train[,"Attrition"]=="Yes"
# predicting:
knn_prediction <- function(dataset) {
  knn_decision <- knn(numerical_regr_train, dataset, output_model,k=k,prob=T,use.all=T) 
  
  ifelse(knn_decision==TRUE, 
         attributes(knn_decision)$prob,
         1-(attributes(knn_decision)$prob))
}
dtrain$pred_knn <- knn_prediction(numerical_regr_train)
dtest$pred_knn <- knn_prediction(numerical_regr_test)


### Random forests:
set.seed(123)
rf_model <- randomForest(x=copy_train[, -which(colnames(copy_train)=="Attrition")],
                         y=as.factor(copy_train$Attrition),
                         ntree=10,     
                         nodesize=5,    
                         importance=T)
# randomForest variable importance 
varImp <- importance(rf_model)
varImpPlot(rf_model, type=1, main="Importance")
# sort the variables by their importance
selVars <- names(sort(varImp[,3]>0.5,decreasing=T)) # only for ones whit imp>0.5
rf_model_imp <- randomForest(x=copy_train[, selVars],
                             y=as.factor(copy_train$Attrition),
                             ntree=10,     
                             nodesize=5,    
                             importance=T)
dtrain$rand_forest_imp <- predict(rf_model_imp,newdata=copy_train,type='prob')[,2]
dtest$rand_forest_imp <- predict(rf_model_imp,newdata=copy_test,type='prob')[,2]



### EVALUATION:
f1_score_matrix <- matrix(rep(NA, 2*5), ncol=5)
colnames(f1_score_matrix ) <- c("logit", "lasso_cv", "decision_tree", "knn", "random_forest")
rownames(f1_score_matrix ) <- c("train", "test")

calc_threshold_train <- function(prediction_col){
  step <- 0.005
  threshMeasures <- matrix(rep(NA, 4*(0.5/step+1)), ncol=4)
  i<-1
  for(threshold in seq(0,0.5,step)){
    tab <- table(pred=dtrain[[prediction_col]]>threshold, Attrition=dtrain$Attrition)
    
    if(dim(tab)[1]==2&dim(tab)[2]==2){ 
      precision <- tab[2,2]/sum(tab[2,])   # TP/(TP+FP)
      recall <- tab[2,2]/sum(tab[,2])   # TP/(TP+FN)
      # Precision: the predicted positives are true positives
      # Recall: how many of the true positives the classifier finds
      
      f1 <- 2*precision*recall/(precision+recall)
      
      threshMeasures[i,1] <- threshold
      threshMeasures[i,2] <- precision
      threshMeasures[i,3] <- recall
      threshMeasures[i,4] <- f1  
      i<-i+1
      rm(tab)
    }
  }
  threshMeasures
  results <- as.data.frame(matrix(c(threshMeasures[!is.na(threshMeasures)]), ncol=4))
  colnames(results) <- c("Threshold","Precision","Recall","F1")
  results
  
  thresh_f1_max <- min(results$Threshold[results$F1==max(results$F1)]) # find the max f1
  f1_max <- results$F1[results$Threshold==thresh_f1_max]
  
  plot(results[,1], results[,4], type="l",
       xlab="Threshold", ylab="F1",
       ylim=c(0,1), lwd=2)
  points(thresh_f1_max, f1_max,
         col="red",pch=1,cex=2,lwd=2)
  
  return(c(f1_max, thresh_f1_max))
}

calc_f1_test <- function(threshold, prediction_col){
  tab <- table(pred=dtest[[prediction_col]]>threshold, Attrition=dtest$Attrition)
  
  if(dim(tab)[1]==2&dim(tab)[2]==2){ 
    precision <- tab[2,2]/sum(tab[2,])   # TP/(TP+FP)
    recall <- tab[2,2]/sum(tab[,2])   # TP/(TP+FN)
    # Precision: the predicted positives are true positives
    # Recall: how many of the true positives the classifier finds
    
    f1 <- 2*precision*recall/(precision+recall)
  }
   
    return(f1)
}


# logit
logit_f1_train <- calc_threshold_train("pred_logit")
logit_f1_test <- calc_f1_test(logit_f1_train[2], "pred_logit") # logit_f1_train[2] returns the train threshold

f1_score_matrix[1,1] <- logit_f1_train[1] # logit_f1_train[1] returns the f1 value
f1_score_matrix[2,1] <- logit_f1_test
f1_score_matrix


# lasso
lasso_f1_train <- calc_threshold_train("pred_lasso")
lasso_f1_test <- calc_f1_test(lasso_f1_train[2], "pred_lasso")

f1_score_matrix[1,2] <- lasso_f1_train[1]
f1_score_matrix[2,2] <- lasso_f1_test
f1_score_matrix


# decision tree model
dt_f1_train <- calc_threshold_train("pred_tree")
dt_f1_test <- calc_f1_test(dt_f1_train[2], "pred_tree")

f1_score_matrix[1,3] <- dt_f1_train[1]
f1_score_matrix[2,3] <- dt_f1_test
f1_score_matrix


# knn model
knn_f1_train <- calc_threshold_train("pred_knn")
knn_f1_test <- calc_f1_test(knn_f1_train[2], "pred_knn") 

f1_score_matrix[1,4] <- knn_f1_train[1]
f1_score_matrix[2,4] <- knn_f1_test
f1_score_matrix


# random forest
rand_forest_imp_f1_train <- calc_threshold_train("rand_forest_imp")
rand_forest_imp_f1_test <- calc_f1_test(rand_forest_imp_f1_train[2], "rand_forest_imp") 

f1_score_matrix[1,5] <- rand_forest_imp_f1_train[1]
f1_score_matrix[2,5] <- rand_forest_imp_f1_test
f1_score_matrix







# ROC/AUC
AUC_matrix <- matrix(rep(NA, 2*5), ncol=5)
colnames(AUC_matrix ) <- c("logit", "lasso_cv", "decision_tree", "knn", "random_forest")
rownames(AUC_matrix ) <- c("train", "test")

calcAUC <- function(predcol,outcol) { 
  perf <- performance(prediction(predcol,outcol=="Yes"),"auc")
  as.numeric(perf@y.values)
}
pred_name <- c("pred_logit", "pred_lasso", "pred_tree", "pred_knn", "rand_forest_imp")

for (i in seq(1,5)){
  AUC_matrix[1,i] <- calcAUC(dtrain[[pred_name[i]]], dtrain$Attrition)
}
for (i in seq(1,5)){
  AUC_matrix[2,i] <- calcAUC(dtest[[pred_name[i]]], dtest$Attrition)
}
AUC_matrix


# ROC curve
roc_logit <- prediction(dtest$pred_logit, dtest$Attrition)
roc_lasso <- prediction(dtest$pred_lasso, dtest$Attrition)

layout(matrix(c(1,2),nrow=2,ncol=1), 
       heights = c(1,1),
       widths = c(1))
plot(performance(roc_logit,"tpr","fpr"), main = "ROC curve for the Logistic regression model")
abline(a=0, b=1, col="lightgray", lty=2, lwd=2)
plot(performance(roc_lasso,"tpr","fpr"), main = "ROC curve for the Lasso regression model")
abline(a=0, b=1, col="lightgray", lty=2, lwd=2)
# True positive rate vs False positive rate



