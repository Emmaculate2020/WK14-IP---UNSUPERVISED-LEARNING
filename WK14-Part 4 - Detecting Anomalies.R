# Loading the dataset
sales <- read.csv("C:\\Users\\HP\\Downloads\\Supermarket_Sales_Forecasting - Sales.csv", header = TRUE)

# Previewing the dataset
head(sales)

View(sales)
str(sales)
dim(sales)
class(sales)

# Finding out the number of missing values in each column

colSums(is.na(sales))
# our dataset do not have null values

# Collect our time series data
library(tidyverse)
library(anomalize)
tidyverse_cran_downloads

# Detecting our anomalies
tidyverse_cran_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
