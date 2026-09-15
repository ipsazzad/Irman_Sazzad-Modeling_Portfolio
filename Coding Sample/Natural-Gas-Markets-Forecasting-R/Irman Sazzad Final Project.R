## Loading necessary libraries

library(fpp3)
library(readxl)
library(ggplot2)
library(tidyverse)
library(GGally)
library(latticeExtra)
library(scales)
library(seasonal)
library(forecast)
library(tsibble)
library(lubridate)
library(zoo)
library(knitr)
library(grid)
library(gridExtra)
library(tsibbledata)
library(fable)
library(dplyr)
library(cowplot)
library(ggpubr)
library(feasts)
library(broom)
library(writexl)
library(urca)

## Reading in monthly data

# Setting working directory and creating a DF consolidating 2 series from a specific date with adjustments
setwd("E:/Study/Fall 24/TS/Assignments/Project/")
Price <- merge(read_xlsx("NG.xlsx", sheet = 1), read_xlsx("Oil.xlsx", sheet = 1), by = "Date", all = TRUE) %>%
  rename(NG = NG_Price, CO = Oil_Price) %>% filter(Date >= as.Date("1997-01-07")) %>% mutate(Month = yearmonth(Date)) %>%
  select(Month, NG, CO) %>% as_tsibble(index = Month)

## Using graphs and summary statistics to identify notable patterns

# Time series, pair plots, summary stats
autoplot(Price, vars(NG, CO)) + ggtitle("Time Series of NG and CO Prices") + ylab("Price") + xlab("Date")
ggpairs(Price %>% as_tibble(), columns = c("NG", "CO"), title = "Pairplot of NG and CO Prices", progress = FALSE)
summary(Price[, c("NG", "CO")])

# Checking if log transformation is necessary
Price %>% mutate(log_NG = log(NG), Month = as.Date(Month)) %>%
  { obj1 <- xyplot(NG ~ Month, ., grid = TRUE, type = "l", lwd = 2, ylab = "NG Price ($)")
    obj2 <- xyplot(log_NG ~ Month, ., grid = TRUE, type = "l", lwd = 2, ylab = "Log of NG Price ($)")
    combined_plot <- doubleYScale(obj1, obj2, use.style = TRUE, text = c("NG", "Log NG"), add.ylab2 = TRUE)
    update(combined_plot, main = "Original vs Transformed Data")}

## Decomposition and De-trending

# Classical decomposition
Price %>%
  model(classical_decomposition(NG, type = "additive")) %>%
  components() %>%
  {p1 <- Price %>% gg_subseries(NG) +                                           # seasonal subseries
    ggtitle("Monthly Natural Gas Price Subseries") + ylab("Natural Gas Price ($)") + xlab("Month")
  p2 <- autoplot(.) + xlab("Month") + ggtitle("Classical Decomposition of Monthly NG Prices")
  residuals_data <- mutate(., Residuals = NG - trend)
  p3 <- ACF(residuals_data, Residuals, lag_max = 20) %>%
    autoplot() + ggtitle("ACF of Detrended Monthly NG Prices: Classical Decomposition")
  print(p1)
  print(p2)
  print(p3)}

# X11 decomposition
x11_dcmp_NG <- Price %>%
  model(x11 = feasts:::X11(NG, type = "additive")) %>%
  components()
autoplot(x11_dcmp_NG) +
  xlab("Month") +
  ggtitle("Additive X11 Decomposition of NG Prices")
x11_dcmp_NG %>%
  ACF(irregular, lag_max = 20) %>%
  autoplot() +
  ggtitle("ACF of Irregular Component from X11 Decomposition")

# STL decomposition
stl_dcmp_NG <- Price %>%
  model(STL(NG ~ season(window = "periodic")))
components(stl_dcmp_NG) %>%
  autoplot() +
  ggtitle("STL Decomposition of Monthly NG Prices")
components(stl_dcmp_NG) %>%
  ACF(remainder, lag_max = 20) %>%
  autoplot() +
  ggtitle("ACF of Remainder from STL Decomposition")

# Ljung-Box test for white noise (STL)
components(stl_dcmp_NG) %>%
  features(remainder, ljung_box, lag = 20)

# 3-month moving average de-trending
Price %>%
  mutate(ma03 = rollmean(NG, k = 3, fill = NA, align = "center")) %>% mutate(Residuals = NG - ma03) %>%
  {p1 <- autoplot(., NG) + autolayer(., ma03, color = "red") +
      xlab("Month") + ylab("Natural Gas Price ($)") + ggtitle("NG Prices & 3-Month Moving Average") +
      guides(colour = guide_legend(title = "Series"))
    plot(.$Month, .$NG, type = "l", col = "blue", pch = "o", 
         ylab = "Natural Gas Price ($)", xlab = "Month", lty = 1, main = "Decomposed Monthly NG Price")
    lines(.$Month, .$Residuals, col = "red", lty = 2)
    lines(.$Month, .$ma03, col = "green", lty = 5)
    legend("topright", legend = c("NG Price ($)", "Residuals", "MA3"),
           col = c("blue", "red", "green"), pch = c("o", "*", "+"), lty = c(1, 2, 5), ncol = 1)
    print(p1)
    ACF(., Residuals, lag_max = 20) %>%
      autoplot() +
      ggtitle("ACF of De-Trended NG Prices (3-Month Moving Average)")
    lb_results <- features(., Residuals, ljung_box, lag = 20, dof = 0)
    lb_results}

# Linear regression (log transformed) de-trending
Price %>%
  mutate(log_NG = log(NG), FittedValues_log = lm(log_NG ~ as.numeric(Month), data = .)$fitted.values,
    Residuals_log = lm(log_NG ~ as.numeric(Month), data = .)$residuals) %>%
  {p1 <- ggplot(.) + geom_line(aes(x = Month, y = log_NG), color = "blue") +
      geom_line(aes(x = Month, y = FittedValues_log), color = "red") +
      ggtitle("Logged Monthly NG Prices with Best Fitted Line") + xlab("Month") + ylab("Log(NG)")
    p2 <- ggplot(.) + geom_line(aes(x = Month, y = Residuals_log), color = "red", linetype = "dashed") +
      ggtitle("Logged Monthly NG Residuals of Regression") + xlab("Month") + ylab("Residuals")
    p3 <- ACF(., Residuals_log, lag_max = 20) %>% autoplot() + ggtitle("ACF of Logged Monthly NG Residuals")
    grid.arrange(p1, p2, p3, ncol = 1)
    lb_results <- features(., Residuals_log, ljung_box, lag = 20, dof = 1)
    lb_results}

# First difference detrending
Price %>%
  mutate(Diff_NG = difference(NG)) %>%
  {p1 <- ggplot(.) +
    geom_line(aes(x = Month, y = NG), color = "blue") +
    ggtitle("Original NG Prices") + xlab("Month") + ylab("($)")
  p2 <- ggplot(.) +
    geom_line(aes(x = Month, y = Diff_NG), color = "red") +
    ggtitle("First Differenced NG Prices") + xlab("Month") + ylab("($)")
  p3 <- ACF(., Diff_NG, lag_max = 20) %>%
    autoplot() +
    ggtitle("ACF of First Differenced NG Price")
  grid.arrange(p1, p2, p3, ncol = 1)
  lb_results <- features(., Diff_NG, ljung_box, lag = 20, dof = 1)
  lb_results}

## Seasonality

# Regression on trend and seasonality
fit_ng_trend_season <- Price %>% model(TSLM(NG ~ trend() + season()))
report(fit_ng_trend_season)

# Removing seasonality, analyzing ACF and residuals using regression on dummies
fit_ng_trend_season %>% augment() %>% mutate(NG_resid = .resid) %>%
  {acf_plot <- ACF(., NG_resid, lag_max = 20) %>% autoplot() + ggtitle("ACF of Residuals After Removing Seasonality")
    resid_plot <- ggplot(.) +
      geom_line(aes(x = Month, y = NG_resid), color = "red") +
      ggtitle("Residuals from Trend + Seasonality using Regression") + xlab("Month") + ylab("Residuals")
    lb_test <- features(., NG_resid, ljung_box, lag = 20, dof = 2)
    print(acf_plot)
    print(resid_plot)
    print(lb_test)}

# Removing seasonality, analyzing ACF and residuals using seasonal difference
Price %>% mutate(NG_diff = difference(NG, lag = 12)) %>%
  {acf_plot <- ACF(., NG_diff, lag_max = 20) %>% autoplot() + ggtitle("ACF of Seasonally Differenced NG Prices")
    residuals_df <- augment(fit_ng_trend_season) %>% mutate(NG_resid = .resid)
    resid_plot <- ggplot(residuals_df) + geom_line(aes(x = Month, y = NG_resid), color = "red") +
      ggtitle("Residuals from Trend + Seasonal Model using Seasonal Difference") + xlab("Month") + ylab("Residuals")
    lb_test <- residuals_df %>% features(NG_resid, ljung_box, lag = 10, dof = 2)
    print(acf_plot)
    print(resid_plot)
    print(lb_test)}

# First differenced trend and seasonality
Price %>% mutate( Diff_NG = difference(NG), Seasonal_Diff_NG = difference(Diff_NG, lag = 12)) %>%
  {acf_plot <- ACF(., Seasonal_Diff_NG, lag_max = 20) %>% autoplot() +
    ggtitle("ACF of Seasonally Differenced (De-Trended) NG Prices")
  resid_plot <- ggplot(.) +
    geom_line(aes(x = Month, y = Seasonal_Diff_NG), color = "red") +
    ggtitle("Residuals After De-Trending and De-Seasonalizing") + xlab("Month") + ylab("Residuals")
  lb_test <- features(., Seasonal_Diff_NG, ljung_box, lag = 10, dof = 1)
  print(acf_plot)
  print(resid_plot)
  print(lb_test)}

## Exponential smoothing

# Selecting the Best ETS Model automatically, forecasting and plotting
Price %>% model(ETS(NG)) %>% 
  {report(.)
    components(.) %>% autoplot() + ggtitle("ETS Model Components for NG Prices") + ylab("Natural Gas Price ($)") %>% print()
    forecast(., h = 12) %>% autoplot(Price) + ggtitle("NG Price Forecasts Using Best ETS Model") +
      xlab("Month") + ylab("Natural Gas Price ($)") %>% print()}

# Evaluating model accuracy by splitting data into training and testing sets
Price_train <- Price %>% filter(Month < yearmonth("2023-01"))
Price_test <- Price %>% filter(Month >= yearmonth("2023-01"))
fit_ng_train <- Price_train %>% model(ETS(NG))
ng_forecast_test <- fit_ng_train %>% forecast(h = nrow(Price_test))
accuracy(ng_forecast_test, Price_test)
augment(fit_ng_train) %>% features(.resid, ljung_box, lag = 20, dof = 6)

## ARIMA

# Searching for the best ARIMA model
fit_ng <- Price %>% model(ARIMA(NG ~ pdq(0:5, 0:2, 0:5) + PDQ(0:2, 0:1, 0:2), stepwise = FALSE, approximation = FALSE))
report(fit_ng)
fit_ng %>% gg_tsresiduals() + ggtitle("Residual Diagnostics for Manual ARIMA Model")
augment(fit_ng) %>% features(.resid, ljung_box, lag = 20, dof = 6)
Price_train <- Price %>% filter(Month < yearmonth("2023-01"))
Price_test <- Price %>% filter(Month >= yearmonth("2023-01"))
fit_ng_train <- Price_train %>% model(ARIMA(NG))
ng_forecast_test <- fit_ng_train %>% forecast(h = nrow(Price_test))
ng_forecast_test %>% autoplot(Price) + ggtitle("Test Set Forecast for NG Prices Using Manual ARIMA Model") +
  xlab("Month") + ylab("Natural Gas Price ($)")
accuracy(ng_forecast_test, Price_test)
arima_forecast <- fit_ng %>% forecast(h = 12)
arima_forecast %>% as_tibble()
arima_forecast %>% autoplot(Price) +
  ggtitle("Manual ARIMA Model Forecast for NG Prices") + xlab("Month") + ylab("Natural Gas Price ($)")
fit_ng %>% glance() %>% bind_cols(augment(fit_ng) %>% features(.resid, ljung_box, lag = 20, dof = 6))

## Dynamic regression model

# Fitting a dynamic regression model using CO as a predictor with the best ARIMA model
fit_dynamic <- Price %>% model(ARIMA(NG ~ CO + pdq(5, 1, 0) + PDQ(0, 0, 1, period = 12)))
report(fit_dynamic)
augment(fit_dynamic) %>% features(.resid, ljung_box, lag = 20, dof = 6)
ng_future <- new_data(Price, 12) %>% mutate(CO = c(80, 82, 81, 78, 77, 79, 80, 82, 84, 83, 82, 81)) ## Made future CO values

# Forecasting
fit_dynamic %>% forecast(new_data = ng_future) %>% autoplot(Price) + xlab("Month") + ylab("Natural Gas Price ($)") +
  ggtitle("Dynamic Regression Model Forecasts (ARIMA(5,1,0)(0,0,1)[12]) for NG Prices")