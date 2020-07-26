# Loading the dataset
sales <- read.csv("C:\\Users\\HP\\Downloads\\Supermarket_Dataset_1 - Sales Data.csv", header = TRUE)
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

# Finding out the number of missing values in each column

colSums(is.na(sales))

# Our dataframe do not have any missing values

# Looking for outliers in our numerical columns as follows

boxplot(sales$Unit.price)
boxplot(sales$Quantity)
boxplot(sales$Tax )
boxplot(sales$cogs)
boxplot(sales$gross.margin.percentage)
boxplot(sales$gross.income)
boxplot(sales$Rating)
boxplot(sales$Total) 
# out of the all the numerical columns, the following have outliers
# Tax, Cogs, Gross.Income, Total
# Removing outliers using variable transformation
sales$Tax <- log(sales$Tax)
sales$cogs <- log(sales$cogs)
sales$gross.income <- log(sales$gross.income)
sales$Total <- log(sales$Total)

# Confirming if the outliers have been removed

boxplot(sales$Tax )
boxplot(sales$cogs)
boxplot(sales$gross.income)
boxplot(sales$Total) 



# To remove duplicates if any

sales = unique(sales)
sales

dim(sales)
# The dimension remains the same, meaning our dataframe do not have duplicates

# DIMENSIONALITY REDUCTION 
# t - distributed Stochastic Neighbor Embedding
#Encode the categorical variables 

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}
table(sales[["Branch"]], encode_ordinal(sales[["Branch"]]), useNA = "ifany")
table(sales[["Customer.type"]], encode_ordinal(sales[["Customer.type"]]), useNA = "ifany")
table(sales[["Gender]], encode_ordinal(sales[["Gender"]]), useNA = "ifany")
table(sales[["Product.line"]], encode_ordinal(sales[["Product.line"]]), useNA = "ifany")
table(sales[["Payment"]], encode_ordinal(sales[["Payment"]]), useNA = "ifany")

new_sales <- sales
new_sales[["Branch_encoded"]] <- encode_ordinal(sales[["Branch"]])
new_sales[["Customer.type_encoded"]] <- encode_ordinal(sales[["Customer.type"]])
new_sales[["Gender_encoded"]] <- encode_ordinal(sales[["Gender"]])
new_sales[["Product.line_encoded"]] <- encode_ordinal(sales[["Product.line"]])
new_sales[["Payment_encoded"]] <- encode_ordinal(sales[["Payment"]])

# Drop the categorical columns

new_sales = subset(new_sales, select = -c(Invoice.ID, Date, Time,Branch, Customer.type, Gender, Product.line,Payment) )
View(new_sales)

# Confirm the datatypes of the encoded columns
sapply(new_sales, class)

# Loading our tnse library

library(Rtsne)

# Curating the database for analysis 
# 
Labels<-new_sales$Product.line
new_sales$Product.line<-as.factor(new_sales$Product.line)

# For plotting
#
colors = rainbow(length(unique(new_sales$Product.line)))
names(colors) = unique(new_sales$Product.line)

# Executing the algorithm on curated data
# 
tsne <- Rtsne(new_sales[,-12], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

# Getting the duration of execution
# 
exeTimeTsne <- system.time(Rtsne(new_sales[,-12], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

# Plotting our graph and closely examining the graph
# 
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=new_sales$Product.line, col=colors[new_sales$Product.line])

#FEATURE SELECTION
# 1. Filter Methods
# Loading our caret and corrplot libraries

library(caret)
library(corrplot)

# Calculating the correlation matrix

correlationMatrix <- cor(new_sales)

# Find attributes that are highly correlated

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)


library(clustvarsel)
library(mclust)

# Sequential forward greedy search (default)

out = clustvarsel(new_sales, G = 1:5)
out

# The selection algorithm would indicate that the subset 
# we use for the clustering model is composed of variables X1 and X2 
# and that other variables should be rejected. 
# Having identified the variables that we use, we proceed to build the clustering model:

Subset1 = new_sales[,out$subset]
mod = Mclust(Subset1, G = 1:5)
summary(mod)

plot(mod,c("classification"))

