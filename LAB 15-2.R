packages <- c("tseries", "forecast", "TTR", "ggplot2", "dplyr", "tidyr", "lubridate")
installed <- packages %in% installed.packages()

if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load the libraries
library(tseries)
library(forecast)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


setwd("D:/systemDir/Downloads")
soybeans <- read.csv("soybean-prices-historical-chart-data.csv")
       print(head(soybeans,3))     
head(soybeans)       
str(soybeans)
CPI <- read.csv("historical-cpi-u-2025031.csv")
       
       print(head(CPI, 3))

CPI <- CPI %>% pivot_longer(cols=-Year,
                           names_to='Month',
                           values_to='CPI')
head(CPI)

# Convert month names 
CPI <- CPI %>% mutate(Month = match(gsub("\\.", "", Month), month.abb))  # Convert month names to numbers

# define date to be 1
CPI <- CPI %>% mutate(date = as.Date(paste(Year, Month, 1, sep = "-")))
head(CPI)
# Format date as m/1/yyyy
#CPI <- CPI %>% mutate(date = format(date, "%Y/%m/%d"))

# Drop the Year and Month columns and ensure chronological order
CPI <- CPI %>% select(date, CPI) %>% arrange(as.Date(date, format = "%Y-%m-%d"))  
head(CPI)
str(CPI) # See format of each column

# Step3: Filter both soybean data and CPI from 1969-01 to 2025-03 to get two new vectors.
# Then, combine them together and get a new data frame and calculate the real price of soybeans.
# I try my best to change date to desired data format and merge two vectors together.
# However, maybe because my computer or my software settings issue, I can not manage to fix it. 
# To proceed smoothly, I will use the uploaded file. 

soybeans <-read.csv("soybeans.csv")
head(soybeans)
tail(soybeans)
str(soybeans)
soybeans$date <- as.Date(soybeans$date, format = "%m/%d/%Y")
# Step5: plot nominal and real soybean prices over time.
ggplot(soybeans, aes(x = date)) +
  geom_line(aes(y = price, color = "Nominal Price"), linewidth = 1) +  
  geom_line(aes(y = price_real, color = "Real Price"), linewidth = 1) +  
  labs(
    title = "Nominal vs Real Soybean Prices Over Time",
    x = "Date", 
    y = "Price",
    color = "Price Type"  
  ) +
  scale_color_manual(
    values = c("Nominal Price" = "darkred", "Real Price" = "steelblue")  
  ) +
  theme_minimal()

# Step6: Time-Series Decomposition and Smoothing
price.ts <- ts(soybeans$price, start=c(1969,01), end=c(2025,03), frequency=12)
head(price.ts,24)

#Step7:
# Decompose nominal price time-series
price_components <- decompose(price.ts, type="additive")
plot(price_components)
price_components$figure
max(price_components$figure)

# Step8: 
# Calculate seasonally adjusted prices
price_adjusted <- price.ts - price_components$seasonal
# Plot the adjusted series
plot.ts(price_adjusted, 
        main = "Soybean Prices Seasonally Adjusted from 1969 to 2025",
        ylab = expression(paste("Nominal Price", " ($)")))

price_sma3 <- SMA(price.ts, n=3) # 3-month simple moving average
price_sma6 <- SMA(price.ts, n=6)
price_sma12 <- SMA(price.ts, n=12)
par(mfrow=c(3, 1)) 
plot.ts(price_sma3, main="Soybeans Price - 3 Month SMA", xlab="", ylab="SMA Price ($)")
plot.ts(price_sma6, main="Soybeans Price - 6 Month SMA", xlab="", ylab="SMA Price ($)")
plot.ts(price_sma12, main="Soybeans Price - 12 Month SMA", xlab="", ylab="SMA Price ($)")
par(mfrow=c(1, 1))

#Step9:
price_diff.ts <- diff(price.ts, differences=1)
plot.ts(price_diff.ts, 
        main = "the month-to-month change in soybeans prices",
        ylab = "Nominal Price Change")

log_price.ts <- log(price.ts)
log_price_diff.ts <- diff(price.ts, differences=1)
plot.ts(log_price_diff.ts, main="Month-over-Month % Change in Soybeans Prices Over Time",xlab="", ylab="% Price Change")

# Step10
ar <- ar(log_price_diff.ts, order.max=3)
ar
checkresiduals(ar, lag.max=60)
# Step11
forecast <- forecast(ar, h=6)
forecast
autoplot(forecast, include=36, xlab="", ylab="Soybeans price - monthly % returns")
