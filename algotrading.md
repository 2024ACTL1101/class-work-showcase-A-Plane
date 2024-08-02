
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
#Looping
for (i in 1:nrow(amd_df)) {
# Other Variables
current_day <- as.Date(amd_df$date[i])
current_price <- amd_df$close[i]
#Previous Price
if(i==1){
 previous_price <- 0
 amd_df$previous_price[i] <- previous_price
}else {
 previous_day<- as.Date(amd_df$date[i-1])
 previous_price <- amd_df$close[i-1]
 amd_df$previous_price[i] <- previous_price
 }
final_day <- as.Date(max(amd_df$date))
#Check previous_price, buy/sell and update costs_proceeds
 if(previous_price == 0) {
amd_df$trade_type[i] <-"buy"
amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
}
{
 #Make sure it doesn't buy on final day as well as setting up buy algo
if(current_price < previous_price && current_day != final_day) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
}
if(is.na(amd_df$costs_proceeds[i])){
 amd_df$costs_proceeds[i] <- 0
}
 }
#Check trade_type and update accumulated_shares
{
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
 accumulated_shares<- accumulated_shares+share_size
amd_df$accumulated_shares[i] <- accumulated_shares
} else {
 amd_df$accumulated_shares[i]<- accumulated_shares
 }
}
{
#Last Day
if(current_day == final_day){
 amd_df$trade_type[i] <- "sell"
 amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]*amd_df$close[i]
 amd_df$accumulated_shares[i] <- 0
 }
}
 }
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Initialise start_date/end_date
start_date <- as.Date("2023-04-17") #2023-07-8 (date before floating point precision occurs)
end_date <- as.Date("2024-03-13")
#set trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
#reset the rows in the numbers column
row.names(amd_df) <- NULL
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Find and Print Profit/Loss, rounded to account for floating point precision
total_pl <- sum(amd_df$costs_proceeds)
print(round(total_pl))

#Invested Capital
invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
print(invested_capital)

#Calculate ROI
roi <- total_pl/invested_capital*100
print(roi)

```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# OPTION 1
#initialise column
amd_df$avg_purchase_price <- 0
amd_df$total_investment <- 0
amd_df$PL <- 0
#initialise variables
amd_df$avg_purchase_price[1] <- amd_df$close[1]
amd_df$total_investment[1] <- amd_df$costs_proceeds[1]*-1
amd_df$PL[1] <- amd_df$costs_proceeds[1]
#Loop
for (i in 2:nrow(amd_df)){
 # Update accumulated shares
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy"){
 amd_df$accumulated_shares [i] <- amd_df$accumulated_shares[i-1] + share_size
 }else{amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i]}

 #Find running weighted average purchase price
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy"){
 amd_df$total_investment[i] <- -amd_df$costs_proceeds[i] + amd_df$total_investment[i-1]
 amd_df$avg_purchase_price[i] <- amd_df$total_investment[i]/amd_df$accumulated_shares[i]
 }else{
 amd_df$total_investment[i] <- amd_df$total_investment[i-1]
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 }

 # Create variable when price increases by 30% from avg purchase price
 sell_price_variable <- amd_df$close[i] >= 1.3*amd_df$avg_purchase_price[i]
 # Fix issue with previous buy changing to sell affecting avg_purchase_price
 if(sell_price_variable && amd_df$trade_type[i]=="buy" && !is.na(amd_df$trade_type[i])){
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 }

 if(sell_price_variable){
 # update trade_type
 amd_df$trade_type[i] <- "sell"

 # sell half of accumulated_shares using integer division to not half decimal shares
 amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]-amd_df$accumulated_shares
[i-1]%/%2
 amd_df$total_investment[i] <- amd_df$total_investment[i-1]/2
 # update cost_proceeds
 amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]%/%2*amd_df$close[i]
}


 # Update accumulated shares
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy"){
 amd_df$accumulated_shares [i] <- amd_df$accumulated_shares[i-1] + share_size
 }else{amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i]}

 if(is.na(amd_df$trade_type[i])){
 amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
}
# Account for different scenarios
 if(amd_df$accumulated_shares[i-1] == 1){
 amd_df$accumulated_shares[i] <- 0
 }
 if(amd_df$accumulated_shares[i] == 0){
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 amd_df$total_investment[i] <- amd_df$total_investment[i-1]
 }
 # PL column
 amd_df$PL[i] <- amd_df$PL[i-1] + amd_df$costs_proceeds[i]
}
 # Final Day
 if(current_day == final_day){
 amd_df$trade_type[i] <- "sell"
 amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]*amd_df$close[i]
 amd_df$accumulated_shares[i] <- 0
 }
# Find and Print Profit/Loss
total_pl <- sum(amd_df$costs_proceeds)
print(total_pl)

# Invested Capital
invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
print(invested_capital)

# Calculate ROI
roi <- total_pl/invested_capital*100
print(roi)

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.

Here are my results with ROI rounded to 2 decimal places for simplicity:
 - PL with profit taking strategy = $348,197.2
 - Invested capital with profit taking strategy = $1,283,291
 - Roi with profit taking strategy = 27.13%
 - PL without profit taking strategy = $747,263
 - Invested capital without profit taking strategy = $1,298,032
 - Roi without profit taking strategy = 57.57%

As we can see from the 17th of April 2023 to the 13th of March 2024 both our PL and ROI returned a
decreased value utilising our profit-taking strategy. This is mainly because the final price was only slightly below
the peak height for the stock price in this period, thus when the algorithm made in step two sold on the final day
it had more shares than the algorithm we created with the profit-taking strategy. Our profit-taking algorithm first
approximately 6500 of its shares by the end of December as well as another 1000 shares in mid-January.
In terms of relevant market events, the reason shares rose 30% above the average price by the end of
December was due to the release of the new AMD Instinct MI300 Accelerator which implements advanced AI.
The accelerator was tested on their OCP complaint board known as the MI300 platform and had 2.4 times the
memory of their competitors’ Nvidia H100 HGX board as well as a 30% improvement to the compute server.
This led to a fairly steady incline in closing price all the way through December. However, selling the majority of
our stock in December was not optimal as the announcement for the AMD Ryzen 8000G series occurred on
the 8th of January 2024, with this announcement AMD claims to have created the Ryzen 7 8700G which has
the world’s most powerful in-built graphics. This was likely a major factor to increase the AMD share price by
around 80% by the end of January since the announcement was made. Following this, AMD stock has been
rising since which could also be complemented by the spillover effect of their competitor, Nvidia having a
steadily increasing stock market over 2024. These stable inclines in stock prices led to the peak price in our
trimmed data frame on March 8th 2024.
The final date of our data frame still maintaining a high closing price compared to previous dates meant that
selling all shares on the final day created greater profits than our profit-taking strategy which sold most of its
shares in December and January.
EXTRA WORK BELOW USING MY IDEA FOR AN IMPROVED STRATEGY:
```r
#STRATEGY TO FURTHER IMPROVE PL:
#Set up a system that makes it so that if previous shares are 0, sell isn't prioritised over
buy and adjust AVG PRICE VARIABLE to current close value
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
# Initialise start_date/end_date
start_date <- as.Date("2023-04-17") #2023-07-8 (date before floating point precision occurs)
end_date <- as.Date("2024-03-13")
#set trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
#reset the rows in the numbers column
row.names(amd_df) <- NULL
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
#Looping
for (i in 1:nrow(amd_df)) {
# Other Variables
current_day <- as.Date(amd_df$date[i])
current_price <- amd_df$close[i]
#Previous Price
if(i==1){
 previous_price <- 0
 amd_df$previous_price[i] <- previous_price
}else {
 previous_day<- as.Date(amd_df$date[i-1])
 previous_price <- amd_df$close[i-1]
 amd_df$previous_price[i] <- previous_price
 }
final_day <- as.Date(max(amd_df$date))
#Check previous_price, buy/sell and update costs_proceeds
 if(previous_price == 0) {
amd_df$trade_type[i] <-"buy"
amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
}
{
 #Make sure it doesn't buy on final day as well as setting up buy algo
if(current_price < previous_price && current_day != final_day) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
}
if(is.na(amd_df$costs_proceeds[i])){
 amd_df$costs_proceeds[i] <- 0
}
 }
#Check trade_type and update accumulated_shares
{
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
 accumulated_shares<- accumulated_shares+share_size
amd_df$accumulated_shares[i] <- accumulated_shares
} else {
 amd_df$accumulated_shares[i]<- accumulated_shares
}
 if(is.na(amd_df$trade_type[i])){
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 }
}
{
#Last Day
 if(current_day == final_day){
 amd_df$trade_type[i] <- "sell"
 amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]*amd_df$close[i]
 amd_df$accumulated_shares[i] <- 0
 }
}
 }
View(amd_df)
# Create variable when price increases by 30% from avg purchase price when accumulated shares
are not 0
# OPTION 1
#initialise column
amd_df$avg_purchase_price <- 0
amd_df$total_investment <- 0
amd_df$PL <- 0
#initialise variables
amd_df$avg_purchase_price[1] <- amd_df$close[1]
amd_df$total_investment[1] <- amd_df$costs_proceeds[1]*-1
amd_df$PL[1] <- amd_df$costs_proceeds[1]
#Loop
for (i in 2:nrow(amd_df)){
 # Update accumulated shares
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy"){
 amd_df$accumulated_shares [i] <- amd_df$accumulated_shares[i-1] + share_size
 }else{amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i]}
 # Strategy

 if(amd_df$accumulated_shares[i] == 0){
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 }

 if(amd_df$accumulated_shares[i-1] == 0 && (current_price < previous_price)){
 amd_df$trade_type[i] <- "buy"
 amd_df$costs_proceeds[i] <- amd_df$close[i]*-share_size
 #Reset to new avg purchase price
 amd_df$avg_purchase_price[i] <- amd_df$close[i]
 amd_df$total_investment[i] <- 0
 }
 # Find running weighted average purchase price
 if(!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy"){
 amd_df$total_investment[i] <- -amd_df$costs_proceeds[i] + amd_df$total_investment[i-1]
 amd_df$avg_purchase_price[i] <- amd_df$total_investment[i]/amd_df$accumulated_shares[i]
 }else{
 amd_df$total_investment[i] <- amd_df$total_investment[i-1]
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 }


 # Create variable when price increases by 30% from avg purchase price
 if(amd_df$accumulated_shares[i-1] != 0){
 sell_price_variable <- amd_df$close[i] >= 1.3*amd_df$avg_purchase_price[i]
 }else{
 sell_price_variable <- 0
 }
 # Fix issue with previous buy changing to sell affecting avg_purchase_price
 if(sell_price_variable && amd_df$trade_type[i]=="buy" && !is.na(amd_df$trade_type[i])){
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
 }


 if(sell_price_variable && amd_df$accumulated_shares[i] != 0){
 # update trade_type
 amd_df$trade_type[i] <- "sell"
 # sell half of accumulated_shares using integer division to not half decimal shares
 amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]-amd_df$accumulated_shares
[i-1]%/%2
 amd_df$total_investment[i] <- amd_df$total_investment[i-1]-amd_df$total_investment[i1]%/%2
 # update cost_proceeds
 amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]%/%2*amd_df$close[i]
 }

 # Account for different scenarios
 if(amd_df$accumulated_shares[i-1] == 1){
 amd_df$accumulated_shares[i] <- 0
 }
 if(amd_df$accumulated_shares[i] == 0){
 amd_df$avg_purchase_price[i] <- amd_df$avg_purchase_price[i-1]
amd_df$total_investment[i] <- amd_df$total_investment[i-1]
 }
 if(is.na(amd_df$trade_type[i])){
 amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
 }
 #Update PL
 amd_df$PL[i] <- amd_df$PL[i-1] + amd_df$costs_proceeds[i]
 }
#Last Day
 if(current_day == final_day){
 amd_df$trade_type[i] <- "sell"
 amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]*amd_df$close[i]
 amd_df$accumulated_shares[i] <- 0
 }
# Find and Print Profit/Loss
total_pl <- sum(amd_df$costs_proceeds)
print(total_pl)

#Invested Capital
invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
print(invested_capital)

#Calculate ROI
roi <- total_pl/invested_capital*100
print(roi)



```
I created this improved strategy before I realised we had to use a weighted mean, without a weighted mean
there were a lot of sells to the point where you would often have 0 shares. I have adjusted this strategy to work
for a weighted mean, and thus if there was a date in which this strategy saw frequent selling to the point of 0
accumulated shares, this would readjust the avg_purchase_price so that the weighting is 100% of new shares.
Conclusion achieved using non-weighted mean: As we can see using this updated method we improve the
output of profit we get from `$267,822.6` to `$300,755.2` . The reason that our ROI is so low I believe is because
of the way we were told to calculate invested capital as it doesn’t include reusing that capital when we sell and
then buy again. I believe that the invested capital should be the negative of the minimum value of the
e.g. (-min(amd_df$PL)) which provides us for this set of data using our improved strategy with `$751,083` as
opposed to `$1,225,476` and thus our ROI is 40.4% instead of 24.5%. Whilst this is not larger than the 57%
acheived from step 2, if the final date for step 2 was not as favourable this strategy would achieve a higher ROI
than the ROI of step 2.

