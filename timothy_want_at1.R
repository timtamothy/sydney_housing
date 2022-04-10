# Install Packages
library(ggplot2)
library(magrittr)
library(ISLR)
library(pls)
library(glmnet)
library(readr)

library(tidyverse)
library(dplyr)
library(DataExplorer)
library(funModeling)
library(here)

library(corrplot)
library(lubridate)
library(scales)


# Load Data
df <- read_csv(here('AT1/transactions.csv'))


## Dataset Information
# Total transaction amounts for each customer in a given month.
# Volume changes between product categories and locations
# Sales Manager wishes to have a prediction for next month's monthly_amount


# View data
str(df)
names(df)



## EDA - Exploratory Data Analysis (Initial)

# View histogram of the columns
basic_eda <- function(data) {
  glimpse(data)
  print(status(data))
  freq(data)
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}
# Acknowledging Pablo Casas from datascienceheroes.com
# https://blog.datascienceheroes.com/exploratory-data-analysis-in-r-intro/

basic_eda(df)

# Determine IQR and fences, using p_75 + 1.5* IQR, p_25 - 1.5*IQR
amount_IQR <- 280116.8
amount_upper <- 375439.5 + 1.5 * amount_IQR
amount_lower <- 95323 - 1.5* amount_IQR #lower than zero


# Notes: 
# - industry and location are numeric type which will need to be changed as these are
#   likely factors/categorical values
# - date is a character type which needs to be changed to date type
# - monthly_amount median is 179399, max 100000000, will need to be normalised,
#   reduce outliers in monthly_amount
# - no na values noted in data
# - 47 distinct months, time range of around 4 years
# - 10 distinct industries
# - 10 distinct locations





## Data Cleaning

# 1. Remove outliers
df2 <- df %>% 
  filter(monthly_amount < amount_upper)

# 2. Factor Industry & Location
df2$industry <- df2$industry %>% factor
df2$location <- df2$location %>% factor


# 3. Change date type
df3 <-df2
df3$date <- dmy(df2$date)

df2[1]
df3[1]

# 4. Remove 0 Value from monthly amount

df3 <- df3 %>% 
  filter(monthly_amount >)

basic_eda(df3)

# Notes: 
# - Monthly sales amount appears to have right tail, most sales under 200k
# - Median = 162k, Mean = 227k

# Save Newly cleaned dataset

write_csv(df3, here('AT1/cleaned_transactions.csv'))




## EDA - (Detailed)

colnames(df3)
str(df3)
status(df3)

# Plot some individual histograms

df3 %>% 
  ggplot(aes(x=industry)) +
  geom_histogram(stat='count')

df3 %>% 
  ggplot(aes(x=location)) +
  geom_histogram(stat='count')

# Via Mollie in R-bloggers
# https://www.r-bloggers.com/2013/08/plot-weekly-or-monthly-totals-in-r/
df3 %>% 
  ggplot(aes(x=date, y=monthly_amount)) +
  stat_summary(fun = sum, geom='bar') +
  scale_x_date(
      labels = date_format('%Y-%m')
  ) +
  theme_minimal() +
  labs(title='Monthly Sales from 2013 to 2017',
       x = 'Month',
       y = 'Monthly Sales'
  )

df3 %>% 
  ggplot(aes(x=date, y=monthly_amount)) +
  stat_summary(fun = sum, geom='bar') +
  scale_x_date(
    labels = date_format('%Y-%m')
  ) +
  theme_minimal() +
  labs(title='Monthly Sales from 2013 to 2017',
       x = 'By Industry',
       y = 'Monthly Sales'
  ) +
  facet_wrap(~industry)

df3 %>% 
  ggplot(aes(x=date, y=monthly_amount)) +
  stat_summary(fun = sum, geom='bar') +
  scale_x_date(
    labels = date_format('%Y-%m')
  ) +
  theme_minimal() +
  labs(title='Monthly Sales from 2013 to 2017',
       x = 'By Location',
       y = 'Monthly Sales'
  ) +
  facet_wrap(~location) +
  scale_color_viridis_d()



## Further Data Cleaning

# remove the customer id, leaving only date, industry, location, and monthly sales
df4 <- df3 %>% 
  select(-'customer_id')

# aggregate for monthly amount
df4 <- df4 %>% 
  group_by(date, industry, location) %>% 
  summarise(mean_monthly_sales = mean(monthly_amount))





## Model with Linear Regression for forecasting

# Filter for industry = 1 and location = 1

df_ind1_loc1 <- df4 %>% 
  filter(industry == 1, location == 1)


## 1. Linear Regression

colnames(df_ind1_loc1)

# Plot what average monthly sales are
df_ind1_loc1 %>% 
  ggplot(aes(x=date, y=mean_monthly_sales)) +
  geom_point() +
  theme_minimal() +
  labs(title='Monthly Sales from 2013 to 2017 of Location 1, Industry 1',
       x = 'Date',
       y = 'Average Sales')

# Low sales appear cyclical around the December/January Mark
# Can we use sin/cos to capture this seasonality?
# David Kaleko's blog on using sin/cos for cyclical data
# http://blog.davidkaleko.com/feature-engineering-cyclical-features.html

df_1_1_sincos <- df_ind1_loc1

df_1_1_sincos['month'] <-  month(df_1_1_sincos$date)
df_1_1_sincos['year'] <- year(df_1_1_sincos$date)

df_1_1_sincos['month_sin'] <- sin((df_1_1_sincos$month-1)*(2*pi/12))
df_1_1_sincos['month_cos'] <- cos((df_1_1_sincos$month-1)*(2*pi/12))


# 1. Linear Model using only Date as a variable
sales.lm = lm(formula = mean_monthly_sales ~ date, data = df_ind1_loc1)

summary(sales.lm)
# Adjusted R-Squared = 0.2043

# 2. Linear Model with Cyclical data
sales.lm2 = lm(formula = mean_monthly_sales ~ date + year + month_sin + month_cos, data = df_1_1_sincos)

summary(sales.lm2)
# Adjusted R-Squared = 0.2469

plot(sales.lm2)









