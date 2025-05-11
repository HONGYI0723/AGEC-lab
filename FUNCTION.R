myfunction <- function(x,y){
  results <-x+y
return(results)
  }
# First brackets: input
# seconed, define results
# return output
myfunction (5,2)

install.packages("gridExtra")
library(gridExtra)

setwd("D:/systemDir/Downloads/test/AGEC-lab")
WASDE <- read.csv("WASDE.csv")
head(WASDE)
str(WASDE)

# Step3 : Make a time series line plot 
library(ggplot2)
g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) +
    geom_line(color ="red") +
    ggtitle("Corn Prices over Time") +
    labs(y = "Prices", x = "year")
#  +(geom_point() # Show the points on the graph)
print(g_price)

g_supply <-ggplot(data = WASDE, aes(x = year, y = total_supply)) +
  geom_line(color ="purple") +
  ggtitle("Total Corn Supply over Time") +
  labs(y = "total corn supply", x = "year")
print(g_supply)

g_demand <-ggplot(data = WASDE, aes(x = year, y = total_use)) +
  geom_line(color ="blue") +
  ggtitle("Total Corn Demand over Time") +
  labs(y = "total corn demand", x = "year")
print(g_demand)
# Plot corn prices, total corn supply, and total corn demand over time
totalplot <-grid.arrange(g_price, g_demand, g_supply, nrow=3)
print(totalplot)
ggsave("cornprices+corn demand+ corn supply.png",plot=totalplot)

# Step4 Relating price to Corn Stock-to-Use Ratio
library(dplyr)
WASDE$SUR <- WASDE$end_stocks/WASDE$total_use

cornprice_vs_SUR <-ggplot(data = WASDE, aes(x =WASDE$SUR , y = corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "pink") +
  ggtitle("corn price vs. stock-to-use ratio with a regression line") +
  labs(y = "corn_price", x = "stock-to-use ratio ")
print(cornprice_vs_SUR)
ggsave("corn price v.s SUR.png",plot=cornprice_vs_SUR)

# Step5 Estimate linear regression model
head(WASDE)
reg1 <- lm(corn_price~SUR, data = WASDE)
summary(reg1) 
# install.packages("gtsummary")
#library(gtsummary)
#tbl_regression(reg1, intercept = TRUE) %>% add_glance_source_note(include = c(r.squared, nobs))

mean_sur <- mean(WASDE$SUR)
mean_price <-mean(WASDE$corn_price)
Elasticity <- -3.5863*mean_sur/mean_price
print(Elasticity)

# 5.2 Investigate error terms
# Summary statistics of residuals
summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")

# Scatterplot of errors vs SUR
cornprice_vs_SUR <-ggplot(data = WASDE, aes(x =WASDE$SUR , y = corn_price)) +
  geom_point(shape = 1) +
  geom_smooth(method = lm, color = "pink") +
  ggtitle("corn price vs. stock-to-use ratio with a regression line") +
  labs(y = "corn_price", x = "stock-to-use ratio ")

error_vs_sur <-ggplot(data=WASDE, aes(x=SUR, y=resid(reg1))) +
  geom_point(shape = 1) +
  ggtitle("Linear regression errors vs. stock-to-use ratio") +
  labs(y = "errors", x = "stock-to-use ratio ")
print (error_vs_sur)

# step6 Estimating the Inverse SUR Model
WASDE$inverseSUR <- 1/WASDE$SUR
reg2 <- lm(corn_price~inverseSUR, data = WASDE)
summary(reg2)
# Elasticity 
mean_sur <- mean(WASDE$SUR)
mean_price <-mean(WASDE$corn_price)
Elasticity <- -0.173143/(mean_sur*mean_price)
print(Elasticity)

# # Residual analysis
summary(resid(reg2))
hist(resid(reg2), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
library(ggplot2)
ggplot(data=WASDE, aes(x=SUR, y=resid(reg2))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

# Step7 Plot with two different peroids with different regression line

WASDE$period <- ifelse(WASDE$year >= 2006, "2006-2019", "1973-2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)
print(WASDE$P2006)
ggplot(data=WASDE, aes(x=SUR, y=corn_price, color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y="Corn Price ($)", x="Stock-to-Use Ratio")

reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)
summary(reg3)

# Step8: auto-correlation check
#Collect the residuals from the last regression, get errors each year and a one-year lag of the error, then regress the error terms on the lagged error terms
error <- ts(resid(reg3), start=1973, end=2019, frequency=1)   # the ts() function tells R to set the errors as a time-series 
lag_error <- lag(error, -1)                                   # the lag() function creates a one-period lag of the error term
error <- cbind(error, lag_error)                              # cbind() combine a one-year lag of error with error, forming a new data frame

reg4 <- lm(error ~ lag_error, data=error)

summary(reg4)






