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

# Load Data
df <- read_csv('SydneyHousePrices.csv')

# View Data
str(df)
names(df)

## EDA

# View histogram of the columns
basic_eda <- function(data) {
  glimpse(data)
  print(status(data))
  freq(data)
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

basic_eda(df)

# Determine IQR and fences, using p_75 + 1.5* IQR, p_25 - 1.5*IQR
bed_upper <- 1.5*1.0+4.0
bath_upper <- 1.5*1.0+2.0
car_upper <- 1.5*1.0+2.0
sell_upper <- 1.5*775000 + 1475000

## Data Cleaning

# Change NA values in number of bedrooms and car park to 0 (studio or no park avail)
df2 <- df %>% 
  replace(is.na(.), 0)  

# Remove outliers
df2 <- df2 %>% 
  filter(bed < bed_upper) %>% 
  filter(bath < bath_upper) %>% 
  filter(car < car_upper) %>% 
  filter(sellPrice < sell_upper)

# Remove properties outside of greater Sydney and within borders of Parramatta
df2 <- df2 %>% 
  filter(postalCode < 3000) %>% 
  filter(postalCode < 2150)

# Remove commercial properties
df2 <- df2 %>% 
  filter(propType %in% c('house', 'townhouse', 'duplex/semi-detached', 'villa', 'terrace'))


# Remove unrealistic house prices, only those above 1% percentile
df2 <- df2 %>% 
  filter(sellPrice > 374999)


# Change necessary columns to factor variables
df2$postalCode <- df2$postalCode %>% factor
df2$suburb <- df2$suburb %>% factor
df2$propType <- df2$propType %>% factor  

# Review cleaned EDA

basic_eda(df2)

# much better plots, better normal histograms for sellPrice, bed, bath, car
# bed, bath, car are ordinal values

# Save dataset

write_csv(df2, here('SydneyHousingPrices_v2.0'))

# Review correlation plot to view any correlated variables

num_col <- select_if(df2, is.numeric)

num_col <- num_col %>% select('sellPrice', 'bed', 'bath', 'car')

corrplot(cor(num_col), method = 'number')

# Only a strong-ish correlation between number of bedrooms and number of bathrooms at 0.60

# Model a linear regression for Sell Price in 2019, and removing ID
colnames(df2)

df_2019 <- df2 %>% 
  select(-'Id') %>% 
  filter(Date >= as.Date('2019-01-01')) %>% 
  select(-'Date')

colnames(df_2019)

price.lm = lm(formula = sellPrice ~ ., data = df_2019)

summary(price.lm)
# Adjusted R-squared 0.784, p_value < 2.2e-16

# Will drop suburb information and keep PostCodes as they are correlated (logically speaking)

df_2019 <- df_2019 %>% 
  select(-'suburb')

price.lm2 = lm(formula = sellPrice ~ ., data = df_2019)

summary(price.lm2)
# Adjusted R-Squared 0.764, p_value < 2.2e-16

df_2019_noZip <- df_2019 %>% 
  select(-'postalCode')

price.lm3 = lm(formula = sellPrice ~ ., data = df_2019_noZip)

summary(price.lm3)
# Adjusted R-Squared 0.1794, p_value < 2.2e-16

plot(price.lm2)

