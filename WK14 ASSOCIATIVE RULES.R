# Loading the dataset
sales <- read.csv("C:\\Users\\HP\\Downloads\\Supermarket_Sales_Dataset II.csv", header = TRUE)
sales

# Previewing the dataset
head(sales)

View(sales)
str(sales)
dim(sales)
class(sales)

# Data Cleaning
# Looking for null values

is.na(sales)

# Removing empty columns from our dataset
sales[!sapply(sales, function(x) all(x == ""))]


colSums(is.na(sales))

# ASSOCIATIVE ANALYSIS
# Finding association rules

library(arules)
rules <- apriori(sales)
summary(rules)




# Verifying the object's class
 
class(sales)

# Generating a summary of the transaction dataset
# ---
# This would give us some information such as the most purchased items, 
# distribution of the item sets (no. of items purchased in each transaction), etc.
# ---
# 
summary(sales)



