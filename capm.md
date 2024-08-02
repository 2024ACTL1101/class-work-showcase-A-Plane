
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
#initialise columns
gspc_df$gspc_daily_return <- NA
amd_df$amd_daily_return <- NA
#AMD Daily Return
for(i in 2:nrow(amd_df)){
amd_df$amd_daily_return[i] <- (amd_df$AMD[i]-amd_df$AMD[i-1])/amd_df$AMD[i-1]
}
#S&P 500 Daily Return
for(j in 2:nrow(gspc_df)){
gspc_df$gspc_daily_return[j] <- (gspc_df$GSPC[j]-gspc_df$GSPC[j-1])/gspc_df$GSPC[j-1]
}

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#initialise columns
rf_df$daily_rf <- NA
#Daily Risk Free Rate
for(i in 1:nrow(rf_df))
rf_df$daily_rf[i] <- (1+rf_df$RF[i]/100)^(1/360)-1
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
amd_rf_df <- merge(amd_df,rf_df,by="Date")
gspc_rf_df <- merge(gspc_df,rf_df, by = "Date")
#initialise columns
gspc_rf_df$gspc_er <- NA
amd_rf_df$amd_er <- NA
#amd excess returns
for(i in 2:nrow(amd_rf_df)){
amd_rf_df$amd_er[i] <- amd_rf_df$amd_daily_return[i]-amd_rf_df$daily_rf[i]
}
#gspc excess returns
for(j in 2:nrow(gspc_rf_df)){
gspc_rf_df$gspc_er[j] <- gspc_rf_df$gspc_daily_return[j]-gspc_rf_df$daily_rf[j]
}

```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
#Linear regression analysis
amd_gspc_rf_df <- merge(amd_rf_df,gspc_rf_df)
regression_model <- lm(amd_er~gspc_er,data=amd_gspc_rf_df)
summary(regression_model)

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
 According to my linear regression analysis, \(\beta\) = 1.5691696. This means that AMD is more volatile
than the market as an increase in the market will result in an increase multiplied by 1.5691696, thus resulting in
a higher value for AMD. If \(\beta\) was less than 1 AMD would be less volatile as an increase or a decrease in the
market would cause a smaller increase/decrease in AMD value.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
#Plotting Model
plot <- ggplot(amd_gspc_rf_df,aes(x = gspc_er,y=amd_er))+geom_point()+geom_smooth(method = "l
m") +
 labs(title = "Linear Regression Model of Excess Return")
print(plot)

```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
#Find Residual Standard Error through regression analysis
se <- summary(regression_model)$sigma
#Number of rows minus NA rows
amd_gspc_rf_df_no_na <- na.omit(amd_gspc_rf_df)
n <- nrow(amd_gspc_rf_df_no_na)
#Calculating X bar
X_bar <- mean(amd_gspc_rf_df$gspc_er,na.rm = TRUE)
#Calculating Rf
Rf <- (1+0.05/100)^(1/360) -1
#Xf changed to daily in the question
Xf <- 0.133/sqrt(252)-Rf
#Calculating daily sf
daily_sf <- se*sqrt(1+1/n+((Xf-X_bar)^2/(sum((amd_gspc_rf_df$gspc_er-X_bar)^2,na.rm=TRUE))))
#Calculating annual sf
annual_sf <- daily_sf*sqrt(252)
#Calculating Expected Return
Yf <- 0.05+coef(regression_model)["gspc_er"]*(0.133-0.05)
#Critical values for prediction interval of 90%
crit_value <- qt(0.95,df=1247)
#Calculating lower and upper bounds
lower_bound <- Yf - crit_value*annual_sf
upper_bound <- Yf + crit_value*annual_sf
print(lower_bound)
print(upper_bound)
```
