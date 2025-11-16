library(ggfortify)
library(forecast)
library(tseries)
library(car)
library(zoo)
library(readxl)
library(rugarch)
library(MASS)
library(fGarch)
library(fracdiff)
library(dplyr)
library(strucchange)
library(readxl)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(lmtest)
library(FinTS)

# Import data
btp.data <- read_excel("datasetTimeSeries.xlsx")
btp <- btp.data[,2]

ts.btp <- ts(btp$`Rendimento lordo BTP quinquennale benchmark`, start = 1989, frequency = 12)
str(ts.btp)


# Plot 
autoplot(ts.btp) +
  ggtitle("Gross return five-year BTP") +
  xlab("Years") + ylab("Gross return") +
  theme_minimal()
# ACF plot
ggAcf(ts.btp) +  ggtitle("ACF Gross return five-year BTP") +  theme_minimal()
# PACF plot
ggPacf(ts.btp) + ggtitle("PACF Gross return five-year BTP") + theme_minimal()
# it seems a not stationary time series, with a decreasing trend



# Classical decomposition
ts.btp %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Gross return five-year BTP")
# seasonality has a very low breadth

# Seasonal plot
ggseasonplot(ts.btp)
ggsubseriesplot(ts.btp)
# seasonal behavior is practically identical every year,
# so seasonality is stable, regular and low amplitude.
# The most important information in the series is the trend


# Test
# Augmented Dickey-Fuller test --> H0: not stationary
adf.test(ts.btp)  # do not reject
# We have to use 1st order differences and check again if time series gets stationary 
ts.btp.diff <- diff(ts.btp)
autoplot(ts.btp.diff) +
  ggtitle("Differentiated ts - BTP Gross return") +
  xlab("Years") + ylab("Gross return (1st order diff)") +
  theme_minimal()
# it seems stationary
# ADF test 
adf.test(ts.btp.diff) # reject --> stationary



# FORECASTING WITH ARIMA
# Splitting data into training and testing sets
dim(btp)[1]*0.7 # split 70-30

idx.train <- 1:304
idx.test <- 305:434

train_set <- ts.btp[idx.train]
train_set <- ts(train_set, start = c(1989, 1), frequency = 12)

test_set <- ts.btp[idx.test]
test_set <- ts(test_set, start = c(2014, 5), frequency = 12)

# starting modes, by looking acf and pacf 
mod_baseline <-  Arima(train_set, order = c(1, 1, 0))
(aic_baseline <- AIC(mod_baseline))   # 204.7566
(bic_baseline <- BIC(mod_baseline))   # 212.1841

p_vals <- 0:10
d_vals <- 1      
q_vals <- 0:10

results <- data.frame(p = integer(),
                      d = integer(),
                      q = integer(),
                      AIC = numeric(),
                      BIC = numeric())

for (p in p_vals) {
  for (d in d_vals) {
    for (q in q_vals) {
      
      try({
        fit <- Arima(train_set, order = c(p, d, q))
        results <- rbind(results, data.frame(p = p,
                                             d = d,
                                             q = q,
                                             AIC = AIC(fit),
                                             BIC = BIC(fit)))
      }, silent = TRUE)
    }
  }
}

(best_aic_model <- results[which.min(results$AIC), ])  # Best model according to AIC
(best_bic_model <- results[which.min(results$BIC), ])  # Best model according to BIC
# baseline model is the best model according to BIC
# theoretical approach was right

forecast_aic <- forecast(Arima(train_set, order = c(6,1,6)), h = length(test_set))
forecast_bic <- forecast(Arima(train_set, order = c(1,1,0)), h = length(test_set))
accuracy(forecast_aic, test_set)
accuracy(forecast_bic, test_set)
# it seems the the ARIMA(6,1,6) perform a little better 
# (looking only at the test values and RMSE and MAE, values near 0 so MPE and MAPE not the best ones)
# both suffers of overfitting


# Fit the selected model with drift on the training set
best_model <- Arima(train_set, order = c(6, 1, 6))
simple_model <- Arima(train_set, order = c(1, 1, 0))

# Residuals and performance diagnostic
# Check residuals 
checkresiduals(best_model)
checkresiduals(simple_model)
# both seem not normal

et <- residuals(best_model)
plot.ts(et, main = "Residuals Time Series", ylab="residual", xlab="Years")
plot(et^2, main = "Squared Residuals of ARIMA Model")
qqnorm(residuals(best_model))
qqline(residuals(best_model))
jarque.bera.test(et) # test for normality --> reject

acf(et, main = "ACF of Residuals")
pacf(et, main= "PACF of Residuals")
# it seems not autocrrelation
# Ljung-Box Test
Box.test(et, lag = 5, type = "Ljung-Box")  # not reject --> no autocorrelation

acf(et^2, main = "ACF of Squared Residuals")
pacf(et^2, main= "PACF of squared residuals" )
# a little bit of heteroskedasticity
# ARCH test
ArchTest(et, lags = 5) # reject --> heteroskedasticity




# Forecast out-of-sample 1-step ahead
h <- 1
n <- length(idx.test) 
y_pred <- rep(NA, n-h+1)

# Generate ARIMA forecasts (Best Model)
for(i in 1:(n-h+1)) {
  idx.update <- c(idx.train, idx.test[1:i])
  fit.update <- Arima(ts.btp[idx.update], model = best_model)
  y_pred[i] <- forecast(fit.update, h=h)$mean[h]
}
y_pred
y_pred <- ts(y_pred, start = c(2014, 6), frequency = 12)

# Plot the actual out-of-sample data and forecasted values
autoplot(ts.btp) +
  autolayer(y_pred, series = "Forecast") +
  autolayer(ts.btp, series = "Test Set") +
  ggtitle("Forecast vs Test Set") +
  scale_colour_manual(values = c("Test Set" = "black", "Forecast" = "red")) +
  xlab("Years") + ylab("Gross returns 5-years BTP") +
  theme_minimal()

autoplot(ts.btp) +
  autolayer(y_pred, series = "Forecast") +
  autolayer(ts.btp, series = "Test Set") +
  ggtitle("Forecast vs Test Set") +
  scale_colour_manual(values = c("Test Set" = "black", "Forecast" = "red")) +
  xlab("Years") + ylab("Gross returns 5-years BTP") +
  theme_minimal() +
  xlim(2020 + 0/12, 2025 + 2/12) + ylim(-1.5,5.5)



# Forecast out-of-sample 6-step ahead
h <- 6
n <- length(idx.test) 
y_pred <- rep(NA, n-h+1)

# Generate ARIMA forecasts (Best Model)
for(i in 1:(n-h+6)) {
  idx.update <- c(idx.train, idx.test[1:i])
  fit.update <- Arima(ts.btp[idx.update], model = best_model)
  y_pred[i] <- forecast(fit.update, h=h)$mean[h]
}
y_pred
y_pred <- ts(y_pred, start = c(2014, 11), frequency = 12)

# Plot the actual out-of-sample data and forecasted values
autoplot(ts.btp) +
  autolayer(y_pred, series = "Forecast") +
  autolayer(ts.btp, series = "Test Set") +
  ggtitle("Forecast vs Test Set") +
  scale_colour_manual(values = c("Test Set" = "black", "Forecast" = "red")) +
  xlab("Years") + ylab("Gross returns 5-years BTP") +
  theme_minimal()

autoplot(ts.btp) +
  autolayer(y_pred, series = "Forecast") +
  autolayer(ts.btp, series = "Test Set") +
  ggtitle("Forecast vs Test Set") +
  scale_colour_manual(values = c("Test Set" = "black", "Forecast" = "red")) +
  xlab("Years") + ylab("Gross returns 5-years BTP") +
  theme_minimal() +
  xlim(2020 + 0/12, 2025 + 10/12) + ylim(-1.5,5.5)


# Forecast out-of-sample 12-step ahead
h <- 12
y_pred <- rep(NA, n-h+1)

for(i in 1:(n-h+12)) {
  idx.update <- c(idx.train, idx.test[1:i])
  fit.update <- Arima(ts.btp[idx.update], model = best_model)
  y_pred[i] <- forecast(fit.update, h=h)$mean[h]
}
y_pred
y_pred <- ts(y_pred, start = c(2015, 5), frequency = 12)

autoplot(ts.btp) +
  autolayer(y_pred, series = "Forecast") +
  autolayer(ts.btp, series = "Test Set") +
  ggtitle("Forecast vs Test Set") +
  scale_colour_manual(values = c("Test Set" = "black", "Forecast" = "red")) +
  xlab("Years") + ylab("Gross returns 5-years BTP") +
  theme_minimal()

autoplot(ts.btp) +
  autolayer(y_pred, series = "Forecast") +
  autolayer(ts.btp, series = "Test Set") +
  ggtitle("Forecast vs Test Set") +
  scale_colour_manual(values = c("Test Set" = "black", "Forecast" = "red")) +
  xlab("Years") + ylab("Gross returns 5-years BTP") +
  theme_minimal() +
  xlim(2020 + 0/12, 2026 + 2/12) + ylim(-1.5,5.5)
