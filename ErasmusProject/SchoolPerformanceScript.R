#############################################################
# change working directory
setwd("~/.MATTEO/ReportDataAnalysis")
# install these libraries and then load them:
library(ggplot2)
library(GGally)
library(ggplot2)
library(reshape2)
library(leaps)
############################################################


## Dataset loading
data <- read.csv("StudentPerformancefactors.csv")

## Exploration data
dim(data)  # 6607 rows, 20 cols
head(data)
str(data)

sum(is.na(data)) # no NA

for (col in colnames(data)) {
    print(table(data[[col]]))
  }
# space values for DistanceFromHome, ParentalEducationLevel and TeacherQuality

data$Distance_from_Home[data$Distance_from_Home == ""] <- NA
data$Parental_Education_Level[data$Parental_Education_Level == ""] <- NA
data$Teacher_Quality[data$Teacher_Quality == ""] <- NA
# changed empty values in NA

# omit NA
data <- na.omit(data)
dim(data) # 6378 rows, 20 cols
copy <- data

# changing from char to factor
data$Parental_Involvement <- factor(data$Parental_Involvement, levels = c("Low", "Medium", "High"))
data$Access_to_Resources <- factor(data$Access_to_Resources, levels = c("Low", "Medium", "High"))
data$Extracurricular_Activities <- factor(data$Extracurricular_Activities, levels = c("No", "Yes"))
data$Motivation_Level <- factor(data$Motivation_Level, levels = c("Low", "Medium", "High"))
data$Internet_Access <- factor(data$Internet_Access, levels = c("No", "Yes"))
data$Family_Income <- factor(data$Family_Income, levels = c("Low", "Medium", "High"))
data$Teacher_Quality <- factor(data$Teacher_Quality, levels = c("Low", "Medium", "High"))
data$School_Type <- factor(data$School_Type, levels = c("Public", "Private"))
data$Peer_Influence <- factor(data$Peer_Influence, levels = c("Negative", "Neutral", "Positive"))
data$Learning_Disabilities <- factor(data$Learning_Disabilities, levels = c("No", "Yes"))
data$Parental_Education_Level <- factor(data$Parental_Education_Level, levels = c("High School", "College", "Postgraduate"))
data$Distance_from_Home <- factor(data$Distance_from_Home, levels = c("Near", "Moderate", "Far"))
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))

str(data)


## Exploratory Analysis
data_categorical <- data[, sapply(data, is.factor)]
data_numerical <- data[, !sapply(data, is.factor)]

summary(data_numerical)

# ExamScore has max 101, change in 100 as maximum
data$Exam_Score[data$Exam_Score == 101] <- 100
data_numerical <- data[, !sapply(data, is.factor)]

# boxplot exam score
boxplot(data$Exam_Score, main = "Exam Score boxplot")

mystats <- function(x){
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, 
           skew=skew, kurtosis=kurt))
}
mystats(data$Exam_Score)  # skew and kurtosis too far from norm values
qqnorm(data$Exam_Score)
qqline(data$Exam_Score, col = "red", lwd = 2)
# not normally distributed

# numerical vars distribution
layout(matrix(c(1,2,3,4,5,6,7,7),nrow=2,ncol=4), 
       heights = c(1,1),
       widths = c(1,1,1,2))
for (var in colnames(data_numerical)) {
    plot(density(data[[var]]), main=paste("Density of", var), xlab=var)
    lines(
      density(data[[var]])$x, 
      dnorm(density(data[[var]])$x, mean(data[[var]]), sd(data[[var]])), 
      col=2
    )
}

# categorical vars barplot
layout(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3), 
       heights = c(1,1),
       widths = c(1,1,1))
for (var in colnames(data_categorical[, 1:6])) {
  freq_table <- table(data[[var]])
  percentages <- prop.table(freq_table)*100
  
  bar_graph <- barplot(freq_table, main=paste("Distribution of", var))
  text(x=bar_graph, y=freq_table-(0.05*max(freq_table)), 
       labels=paste0(round(percentages,1),"%"),
       col="black", cex=1, font=2)
}

layout(matrix(c(1,2,3,4,5,6,7,7), nrow=2, ncol=4), 
       heights = c(1,1),
       widths = c(1,1,1,1))
for (var in colnames(data_categorical[, 7:13])) {
  freq_table <- table(data[[var]])
  percentages <- prop.table(freq_table)*100
  
  bar_graph <- barplot(freq_table, main=paste("Distribution of", var))
  text(x=bar_graph, y=freq_table-(0.05*max(freq_table)), 
       labels=paste0(round(percentages,1),"%"),
       col="black", cex=1, font=2)
}

## Statistical Analysis
layout(matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3), 
       heights = c(1,1,1),
       widths = c(1,1,1))
for (var in colnames(data_categorical[1:9])) {
  boxplot(Exam_Score ~ data[[var]], 
          data = data,
          main = paste("Distribution of Exam Score for ", var),
          xlab = var,
          ylab = "Exam score",
          col = "lightblue")
}

layout(matrix(c(1,2,3,4), nrow=2, ncol=2), 
       heights = c(1,1),
       widths = c(1,1))
for (var in colnames(data_categorical[10:13])) {
  boxplot(Exam_Score ~ data[[var]], 
          data = data,
          main = paste("Distribution of", var),
          xlab = var,
          ylab = "Exam score",
          col = "lightblue")
}

dev.off()

## Demographic factors
# Gender
ggplot(data, aes(x=Gender, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Gender") +
  theme(plot.title = element_text(hjust = 0.5))
# seems no difference
t.test(Exam_Score ~ Gender, data=data) # not refusing H0 

group1 <- data$Exam_Score[data$Gender == "Male"]
group2 <- data$Exam_Score[data$Gender == "Female"]
# Shapiro-Wilk test for normality
shapiro.test(group1)
shapiro.test(group2)

qqnorm(group1)
qqline(group1, col = "red")
# normality problem -> Mann-Whitney U-test
wilcox.test(Exam_Score ~ Gender, data=data)  # p.value=0.7 >> 0.5, do not reject H0



# urban or extra-urban zone
ggplot(data, aes(x=Distance_from_Home, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Distance from home") +
  theme(plot.title = element_text(hjust = 0.5))
# seems to be a small difference

aov_model <- aov(Exam_Score ~ Distance_from_Home, data = data)
summary(aov_model)  # rejecting H0 -> different means for groups

group1 <- data$Exam_Score[data$Distance_from_Home == "Near"]
group2 <- data$Exam_Score[data$Distance_from_Home == "Moderate"]
group3 <- data$Exam_Score[data$Distance_from_Home == "Far"]
# Shapiro-Wilk test for normality
shapiro.test(group1)
shapiro.test(group2)
shapiro.test(group3)

qqnorm(group1)
qqline(group1, col = "red")
# normality problem -> Kruskal-Wallis test
kruskal.test(Exam_Score ~ Distance_from_Home, data = data)  # p.value small -> rejecting H0



# internet
ggplot(data, aes(x=Internet_Access, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Internet acces") +
  theme(plot.title = element_text(hjust = 0.5))
# seems to be a small difference
wilcox.test(Exam_Score ~ Internet_Access, data=data) # p.value small -> rejecting H0




## Social factor
# parental education level
ggplot(data, aes(x=Parental_Education_Level, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Parental education level") +
  theme(plot.title = element_text(hjust = 0.5))
kruskal.test(Exam_Score ~ Parental_Education_Level, data = data)  # p.value small

# family income
ggplot(data, aes(x=Family_Income, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Family income") +
  theme(plot.title = element_text(hjust = 0.5))
kruskal.test(Exam_Score ~ Family_Income, data = data)  # p.value small

# parental involvement
ggplot(data, aes(x=Parental_Involvement, y=Exam_Score)) +
geom_boxplot() +
  ggtitle("Distribution of the exam score for Parental involvement") +
  theme(plot.title = element_text(hjust = 0.5))
kruskal.test(Exam_Score ~ Parental_Involvement, data = data)  # p.value small






## Educational factor
# learning disabilities and tutoring sessions

# learnign disabilities
ggplot(data, aes(x=Learning_Disabilities, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Learning disabilities") +
  theme(plot.title = element_text(hjust = 0.5))
# seems to be a small difference
wilcox.test(Exam_Score ~ Learning_Disabilities, data=data) # p.value small -> rejecting H0


# access to resource
ggplot(data, aes(x=Access_to_Resources, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Access to resources") +
  theme(plot.title = element_text(hjust = 0.5))
kruskal.test(Exam_Score ~ Access_to_Resources, data = data)  # p.value small

# teacher quality
ggplot(data, aes(x=Teacher_Quality, y=Exam_Score)) +
  geom_boxplot() +
  ggtitle("Distribution of the exam score for Teacher quality") +
  theme(plot.title = element_text(hjust = 0.5))
kruskal.test(Exam_Score ~ Teacher_Quality, data = data)  # p.value small





## Student lifestyle
# corr matrix
corr_matrix <- cor(data_numerical, use = "complete.obs")
corr_data <- melt(corr_matrix)

# heatmap
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
        columns=colnames(data_numerical),
        title = "Scatterplot Matrix with Linear and Loess Fits",
        lower = list(continuous = lowerplots),
        diag =  list(continuous = diagplots),
        upper = list(continuous = upperplots)) +
  mytheme

# cor tests
# problem with normality -> Kendall test
for (var in colnames(data_numerical[, 1:6])){
  cat("\nCorrelation between", var, "and Exam_Score:\n")
  print(cor.test(data_numerical[[var]], data_numerical$Exam_Score, method = "kendall"))
  cat("\n----------------------------------------------------------------------------")
}
# tests confirm that there's correlation between the exam score and all the other 
# numerical vars, except for Sleep hours and Physical activity





## Global performance
# most influential factor for the exam score
fit0 <- lm(Exam_Score ~ Hours_Studied, data=data)
summary(fit0)

plot(data$Hours_Studied, data$Exam_Score, 
     xlab="Hours studied", 
     ylab="Exam score")
abline(fit0)
# seems linear dependence

fit_all <- lm(Exam_Score ~ ., data=data) # all vars
fit_reduced <- lm(Exam_Score ~ Hours_Studied+Attendance+Previous_Scores+Tutoring_Sessions+
             Distance_from_Home+Internet_Access+Parental_Education_Level+Family_Income+
             Parental_Involvement+Learning_Disabilities+Access_to_Resources+Teacher_Quality,
           data=data) # selection through previous analysis
anova(fit_reduced, fit_all)
# not considering some relevant regressors in fit_reduced

AIC(fit_all,fit_reduced) # also AIC suggests fit_all


# backward stepwise selection
fit_back <- lm(Exam_Score ~ ., data=data)
step(fit_back, direction="backward") # eliminate gender, schol type and school hours

fit_back <- lm(Exam_Score ~ . -Sleep_Hours -School_Type -Gender, data=data)
anova(fit_back, fit_all) # not relevant regrs

# all subsets regression
leaps <- regsubsets(Exam_Score ~ . -Sleep_Hours -School_Type -Gender, data=data, nbest=3, nvmax=10)
subsTable <- function(obj, scale){
  x <- summary(leaps)
  m <- cbind(round(x[[scale]],3), x$which[,-1])
  colnames(m)[1] <- scale
  m[order(m[,1]), ]
}
subsTable(leaps, scale="adjr2")

fit_subs <- lm(Exam_Score ~ Hours_Studied+Attendance+Previous_Scores+Tutoring_Sessions+
                            Parental_Involvement+Access_to_Resources+Peer_Influence+Motivation_Level,
                  data=data)
anova(fit_subs, fit_back)
AIC(fit_subs, fit_back)


# selection of the regressors for the final model:
fit_final <- lm(Exam_Score ~ Hours_Studied+Attendance+Previous_Scores+Tutoring_Sessions+Peer_Influence+
                    Distance_from_Home+Internet_Access+Parental_Education_Level+Family_Income+Motivation_Level+
                    Parental_Involvement+Learning_Disabilities+Access_to_Resources+Teacher_Quality,
                  data=data)
step(fit_final, direction="backward")
summary(fit_final)

