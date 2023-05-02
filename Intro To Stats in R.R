### Stats Fundamentals ###

#######################################
## Set local working directory
setwd("C:\\Users\\Nikki\\Desktop\\MPC\\")
getwd()

## These are all of the libraries needed for this tutorial
# You may need to run install.packages() first
library(tidyverse)
library(broom)
library(ggfortify)
library(yardstick)
library(dplyr)
library(ggplot2)

## These are all of the files needed for this tutorial 
# Note: there is a mix of csv and rds, rds will need to be locally downloaded
# Note 2: '' and "" are interchangeable 
food_consumption <- read_csv('https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/food_consumption.csv')
restaurant_groups <- read_csv("https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/restaurant_groups.csv")
all_deals <- read.csv('https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/all_deals.csv')
taiwan_real_estate <- read.csv("https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/taiwan_real_estate.csv")
sp500_yearly_returns <- read.csv("https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/SP500.csv")
ad_conversion <- read.csv("https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/ad_conversion.csv")
churn <- read.csv("https://raw.githubusercontent.com/perkinsns/Intro-To-Stats-in-R/main/churn.csv")
amir_deals <- readRDS('seller_1.rds')
world_happiness <- readRDS('world_happiness_sugar.rds')
######################################

## Measures of Center (mean and median) ##

# Mean: the sum of all the data points divided by the total number of data 
# points
# Median: the middle value of the dataset where 50% of the data is less than the 
# median, and 50% of the data is greater than the median

# First, always look at the data to get an idea of what it looks like
food_consumption

#Create two data frames: one that holds the rows of food_consumption for 
#“Belgium” and the another that holds rows for “USA”

# Filter for Belgium
belgium_consumption <- food_consumption %>%
  dplyr::filter(country == "Belgium")

# Filter for USA
usa_consumption <- food_consumption %>%
  dplyr::filter(country == "USA")

# Calculate mean and median consumption in Belgium
mean(belgium_consumption$consumption)
median(belgium_consumption$consumption)

# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
median(usa_consumption$consumption)

# Put this data in a tibble for easy viewing
food_consumption %>%
  # Filter for Belgium and USA
  dplyr::filter(country %in% c("Belgium", "USA")) %>%
  # Group by country
  group_by(country) %>%
  # Get mean_consumption and median_consumption
  summarize(mean_consumption = mean(consumption),
            median_consumption = median(consumption))

# Deciding when to use which measure

food_consumption %>%
  #Filter for rice food category
  dplyr::filter(food_category == "Rice") %>%
  # Create histogram of CO2_emission
  ggplot(aes(co2_emmission)) + geom_histogram()

#this data is right-skewed

food_consumption %>%
  # Filter for rice food category
  dplyr::filter(food_category == "Rice") %>% 
  # Get mean_co2 and median_co2
  summarize(mean_co2 = mean(co2_emmission),
            median_co2 = median(co2_emmission))

# given the skew of the data, we should use the median


## Measures of Spread ##
# Variance: measures the average distance from each data pt to the mean
# higher variance = bigger spread
# Standard Deviance (sd) = sqrt(variance)
# This is easier to understand than variance
# Around 68% of scores are within 1 standard deviation of the mean, 
# Around 95% of scores are within 2 standard deviations of the mean, 
# Around 99.7% of scores are within 3 standard deviations of the mean.
# Mean Absolute Deviation: penalizes all distances equally (SD more common)
# Quartiles (quantile()): aka a box plot (use "probs =" to get different tiles)
# Interqartile Range (IQR): data range between 75% and 25%
# Outliers: data < Q1 -1.5*IQR or data > Q3+1.5*IQR

# Calculate the quartiles of co2_emission
quantile(food_consumption$co2_emmission)

# Calculate the quintiles of co2_emission
quantile(food_consumption$co2_emmission, probs = seq(0,1,0.2))

# Calculate the deciles of co2_emission
quantile(food_consumption$co2_emmission, probs = seq(0,1,0.1))

# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarize(var_co2 = var(co2_emmission),
            sd_co2 = sd(co2_emmission))

# Create subgraphs for each food_category: histogram of co2_emission
ggplot(food_consumption, aes(co2_emmission)) +
  # Create a histogram
  geom_histogram(y=5) +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category)

# Finding outliers using IQR
# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emmission))

# Compute the first and third quantiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25)
q3 <- quantile(emissions_by_country$total_emission, 0.75)
iqr <- q3-q1

# Calculate the lower and upper cutoffs for outliers
lower <- q1-1.5*iqr
upper <- q3+1.5*iqr

# Filter emissions_by_country to find outliers
emissions_by_country %>%
  dplyr::filter(total_emission < lower | total_emission > upper)

## end measures of center and spread ##

## Random Numbers and Probability ##

# Calculate probability of picking a deal with each unique product
amir_deals %>%
  # Count the deals for each product (creates a new column "n")
  count(product) %>%
  # make a new column "prob" to hold the probabilities
  mutate(prob = n/sum(n))

# If you randomly select one of Amir’s deals, the probability that it involves
# Product C is 8.43%

# Randomly pick five deals so that you can reach out to each customer and ask 
# if they were satisfied with the service they received.
# Do this with and without replacement
# With replacement: put the marble back in the bag and pick again
# Without replacement: choose another marble without putting it back in the bag

# Set random seed to 31 (this creates "random" reproducible numbers)
set.seed(31)

# Pick 5 deals without replacement 
amir_deals %>%
  sample_n(5)

# Sample 5 deals with replacement
amir_deals %>%
  sample_n(5, replace = TRUE)

# In this case, you would want to choose "without replacement" so you don't 
# choose the same deal more than once.


# Discrete distributions
# Scenario: A new restaurant opened a few months ago, and the restaurant’s 
# management wants to optimize its seating space based on the size of the 
# groups that come most often. On one night, there are 10 groups of people 
# waiting to be seated at the restaurant, but instead of being called in the  
# order they arrived, they will be called randomly. In this exercise, you’ll 
# investigate the probability of groups of different sizes getting picked 
# first. Data on each of the ten groups is contained in the restaurant_groups 
# data frame.

# Remember that expected value can be calculated by multiplying each possible 
# outcome with its corresponding probability and taking the sum.

# Create a histogram with 5 bins of restaurant_groups to look at the data
ggplot(restaurant_groups, aes(group_size)) +
  geom_histogram(bins = 5)

# Create probability distribution
size_distribution <- restaurant_groups %>%
  # Count number of each group size (creates a new column "n")
  count(group_size) %>%
  # make a new column "probability" to hold the probabilities
  mutate(probability = n / sum(n))

# Calculate expected group size
expected_val <- sum(size_distribution$group_size *
                      size_distribution$probability)

# Calculate probability of picking group of 4 or more
size_distribution %>%
  # Filter for groups of 4 or larger
  dplyr::filter(group_size >= 4) %>%
  # Calculate prob_4_or_more by taking sum of probabilities
  summarize(prob_4_or_more = sum(probability))


# Continuous Distributions
# dunif gives the density 
# punif gives the distribution function 
# qunif gives the quantile function 
# runif generates random deviates

# Scenario: The sales software used at your company is set to automatically back
# itself up, but no one knows exactly what time the back-ups happen. It is 
# known, however, that back-ups happen exactly every 30 minutes. Amir comes 
# back from sales meetings at random times to update the data on the client 
# he just met with. He wants to know how long he’ll have to wait for his 
# newly-entered data to get backed up. Use your new knowledge of continuous 
# uniform distributions to model this situation and answer Amir’s questions.

# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(5, min, max)
prob_less_than_5

# Calculate probability of waiting exactly 5 mins
prob_exactly_5 <- dunif(5, min, max)
prob_exactly_5

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- punif(5, min, max, lower.tail = FALSE)
prob_greater_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20, min, max) - punif(10, min, max)
prob_between_10_and_20

# Simulating wait times
# Scenario: To give Amir a better idea of how long he’ll have to wait, you’ll 
# simulate Amir waiting 1000 times and create a histogram to show him what he 
# should expect. Recall from the last exercise that his minimum wait time is 
# 0 minutes and his maximum wait time is 30 minutes.

# Create data frame to hold 1000 values in a column labeled "simulation_nb"
wait_times <- data.frame(simulation_nb = 1:1000)

# Set random seed to 334 for reproducible "random" numbers
set.seed(334)

wait_times %>%
  # Generate 1000 wait times between 0 and 30 mins, save in column "time"
  mutate(time = runif(1000, min = 0, max = 30)) %>%
  # Create a histogram of simulated times (30 bins: 1 for each minute)
  ggplot(aes(time)) +
  geom_histogram(bins = 30) + geom_hline(aes(yintercept = mean(time)), 
                                         color = "blue", 
                                         linetype = "dotted") +
  xlab("Wait Time (min)") + ylab ("Number of Occurrences")

# Binomial Distribution
# dbinom gives the density
# pbinom gives the distribution function
# qbinom gives the quantile function 
# rbinom generates random deviates

# Set random seed to 10 for reproducible "random" numbers
set.seed(10)

# Scenario: Assume that Amir usually works on 3 deals per week, and overall, 
# he wins 30% of deals he works on

# Simulate a single deal (output is 0 or 1)
rbinom(1, 1, 0.3)

# Simulate 1 week of 3 deals (output is 0 to 3)
rbinom(1, 3, 0.3)

# Simulate 52 weeks of 3 deals (output is 52 0s, 1s, 2s or 3s)
deals <- rbinom(52, 3, 0.3)

# Calculate mean deals won per week for the full year
mean(deals)

# Probability of closing exactly 3 out of 3 deals
dbinom(3, 3, 0.3)

# Probability of closing <= 1 deal out of 3 deals
pbinom(1, 3, 0.3)

# Probability of closing > 1 deal out of 3 deals
pbinom(1, 3, 0.3, lower.tail = FALSE)

# Now Amir wants to know how many deals he can expect to close each week if his 
# win rate changes.

# Expected number won with 30% win rate
won_30pct <- 3 * 0.3
won_30pct

# Expected number won with 25% win rate
won_25pct <- 3 * 0.25
won_25pct

# Expected number won with 35% win rate
won_35pct <- 3 * 0.35
won_35pct

