library(ggplot2)
library(forecast)
library(tseries)
library(cowplot)

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

daily_data$Date <- as.Date(daily_data$dteday)
plot1 <- ggplot(daily_data, aes(x = Date, y = cnt)) + geom_line() + scale_x_date("month")

# 1) Remove outliers that could bias the model by skewing statistical summaries
count_ts <- ts(daily_data[, c("cnt")])
daily_data$clean_cnt <- tsclean(count_ts)

plot2 <- ggplot(daily_data, aes(x = Date, y = clean_cnt)) + geom_line() + scale_x_date("month")
plot_grid(plot1, plot2, nrow = 2)

# 2) Data still volatile - apply moving average to smooth this out
daily_data$cnt_ma <- ma(daily_data$clean_cnt, order = 7)
daily_data$cnt_ma30 <- ma(daily_data$clean_cnt, order = 30)

plot3 <- ggplot() +
    geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
    geom_line(data = daily_data, aes(x = Date, y = cnt_ma, colour = "Weekly Moving Average")) +
    geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average")) +
    ylab("Bicycle Count")
plot3

# 3) Decompose data
count_ma <- ts(na.omit(daily_data$cnt_ma), frequency = 30)
decomp <- stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# 4) Test for stationarity
adf.test(count_ma, alternative = "stationary")

par(mfrow = c(1,2))
Acf(count_ma)
Pacf(count_ma)

# 5) Choosing model order
count_d1 <- diff(deseasonal_cnt, differences = 1)
par(mfrow = c(1,1))
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
par(mfrow = c(1,2))
Acf(count_d1)
Pacf(count_d1)

# 6) Fitting ARIMA model
auto.arima(deseasonal_cnt, seasonal = FALSE)

# 7) Evaluate
fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = "(1,1,1) Model Residuals")

fit2 <- arima(deseasonal_cnt, order = c(1,1,7)); fit2
tsdisplay(residuals(fit2), lag.max = 45, main = "(1,1,7) Model Residuals")

# 8) Forecast
fcast <- forecast(fit2, h = 30)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start = 700)
fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:725)]), order = c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout, h = 25)
plot(fcast_no_holdout)
lines(ts(deseasonal_cnt))

fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal = TRUE)
fit_w_seasonality
tsdisplay(residuals(fit_w_seasonality), lag.max = 45, main = "(2,1,2)(1,0,0)[30] Model Residuals")

hold <- window(ts(deseasonal_cnt), start = 710)
fit_no_holdout <- arima(ts(deseasonal_cnt[-c(710:725)]), order = c(2,1,2),
                        seasonal = list(order = c(1,0,0), period = 30))
fcast_no_holdout <- forecast(fit_no_holdout, h = 30)
plot(fcast_no_holdout)
lines(ts(deseasonal_cnt))
