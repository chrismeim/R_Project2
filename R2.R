Introduction

The aim of this excercise is to make some simple calculations on a dataframe that consists a large portfolio of securities.

In the first part of the excercise I create a new dataframe containing the maximum daily return for each month, for each stock.
Each row in the data represents the maximum daily return in a given month, for a given stock

In the second part of the excercise I perform an independent decile group analysis on a specific month within the data set. 
Then I identify a stock in a certain decile based one some criteria.

Part I
Load R packages
Load the packages required. Consider using dplyr , lubridate , knitr
'''{}
library(dplyr)
library(lubridate)
library(knitr)
'''

Load Data
''' {]
comp_data <- read.csv("compustat_sec_2020_2023.csv")
'''

Calculate simple daily returns for each stock. Ensure there are no NA values or Inf values in your
returns column
'''{}
daily_returns <- comp_data %>%
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d")) %>%
  arrange(tic, datadate) %>%
  group_by(GVKEY) %>%
  mutate(return = (prccd/lag(prccd) - 1)) %>%
  na.omit(return) %>%
  filter(is.finite(return)) %>%
  ungroup()
head(daily_returns)
'''

Create a new data frame containing the maximum daily return for each month, for each stock. For each
stock in the data frame, each row should represent the maximum daily return in a given month.
'''{}
monthlyMaxReturns <- daily_returns %>%
3
group_by(tic) %>%
  mutate(month = floor_date(datadate, "month")) %>%
  group_by(tic, month) %>%
  arrange(desc(return)) %>%
  filter(return == max(return)) %>%
  slice(1) %>%
  ungroup()
head(monthlyMaxReturns)
'''

Part II
Consider only the month of March 2023, and create decile groups
'''{}
march_2023_data <- monthlyMaxReturns %>%
  filter(year(datadate) == 2023 & month(datadate) == 3)

march_2023_data <- march_2023_data %>%
  mutate(
    deciles = cut(
      return,
      breaks = quantile(
        return,
        probs = seq(0, 1, by = 0.1),
        type = 5,
        na.rm = TRUE
      ),
      include.lowest = TRUE,
      labels = paste0(seq(10, 100, by = 10), "%")
    )
  )
march_2023_data
'''

Choose the stock matching the following criteria: - lowest return in the 100% decile group
'''{}
selected_stock <- march_2023_data %>%
  filter(deciles == "100%") %>%
  arrange(return) %>%
  head(1)

selected_stock %>%
  select(GVKEY, iid, datadate, conm, volume = cshtrd, closing_price = prccd, return, deciles) %>%
  kable(caption = "Stock with the lowest return in the 100% decile for March 2023")
'''

Consider only the month of February 2023, and create decile groups.
'''{}
march_2023_data <- monthlyMaxReturns %>%
  filter(year(datadate) == 2023 & month(datadate) == 3)

march_2023_data <- march_2023_data %>%
  mutate(
    deciles = cut(
      return,
      breaks = quantile(
        return,
          probs = seq(0, 1, by = 0.1),
          type = 5,
          na.rm = TRUE
        ),
        include.lowest = TRUE, 
        labels = paste0(seq(10, 100, by = 10), "%")
    )
  )
march_2023_data
'''

Choose the stock matching the following criteria: - lowest return in the 80% decile group
'''{}
selected_stock <- february_2023_data %>%
  filter(deciles == "80%") %>%
  arrange(return) %>%
  head(1)

selected_stock %>%
  select(GVKEY, iid, datadate, conm, volume = cshtrd, closing_price = prccd, return, deciles) %>%
  kable(caption = "Stock with the lowest return in the 80% decile for March 2023")  
'''

Consider only the month of January 2023, and create decile groups
'''{}
january_2023_data <- monthlyMaxReturns %>%
  filter(year(datadate) == 2023 & month(datadate) == 1)

january_2023_data <- january_2023_data %>%
  mutate(
    deciles = cut(
      return,
      breaks = quantile(
        return,
        probs = seq(0, 1, by = 0.1),
        type = 5,
        na.rm = TRUE
      ),
      include.lowest = TRUE,
      labels = paste0(seq(10, 100, by = 10), "%")
    )
  )
january_2023_data
'''
Choose the stock matching the following criteria: - highest return in the 20% decile group
'''{}
selected_stock <- january_2023_data %>%
  filter(deciles == "20%") %>%
  arrange(desc(return)) %>%
  head(1)

selected_stock %>%
  select(GVKEY, iid, datadate, conm, volume = cshtrd, closing_price = prccd, return, deciles) %>%
  kable(caption = "Stock with the highest return in the 20% decile for January 2023")
'''

Consider only the month of December 2022, and create decile groups.
'''{}
december_2022_data <- monthlyMaxReturns %>%
  filter(year(datadate) == 2022 & month(datadate) == 12)

december_2022_data <- december_2022_data %>%
  mutate(
    deciles = cut(
      return,
      breaks = quantile(
        return,
        probs = seq(0, 1, by = 0.1),
        type = 5,
        na.rm = TRUE
      ),
      include.lowest = TRUE,
      labels = paste0(seq(10, 100, by = 10), "%")
    )
  )
december_2022_data
'''

Choose the stock matching the following criteria: - highest return in the 60% decile group
'''{}
selected_stock <- december_2022_data %>%
  filter(deciles == "60%") %>%
  arrange(desc(return)) %>%
  head(1)

selected_stock %>%
  select(GVKEY, iid, datadate, conm, volume = cshtrd, closing_price = prccd, return, deciles) %>%
  kable(caption = "Stock with the highest return in the 60% decile for December 2022")
'''

Consider only the month of November 2022, and create decile groups.
'''{}
november_2022_data <- monthlyMaxReturns %>%
  filter(year(datadate) == 2022 & month(datadate) == 11)

november_2022_data <- november_2022_data %>%
  mutate(
    deciles = cut(
      return,
      breaks = quantile(
        return,
        probs = seq(0, 1, by = 0.1),
        type = 5,
        na.rm = TRUE
      ),
      include.lowest = TRUE,
      labels = paste0(seq(10, 100, by = 10), "%")
    )
  )
november_2022_data
'''

Choose the stock matching the following criteria: - lowest return in the 30% decile group
'''{}
selected_stock <- november_2022_data %>%
  filter(deciles == "30%") %>%
  arrange(return) %>%
  head(1)

selected_stock %>%
  select(GVKEY, iid, datadate, conm, volume = cshtrd, closing_price = prccd, return, deciles) %>%
  kable(caption = "Stock with the lowest return in the 30% decile for November 2022")
'''



