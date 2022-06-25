# Hotel Bookings - Term Project Code

# Clear Environment
rm(list = ls())

# Import tools
library(MASS)
library(corrplot) 
library(lattice)
library(ggplot2) 
library(caTools) 
library(dplyr) 
library(plotly)

library(tidyverse)
library(scales)
library(summarytools)
library(psych)
library(gridExtra)
library(countrycode)
library(sf)
library(highcharter)
library(repr)
library(scales)

# Set Directory
setwd("/Users/RadioRaheemGSU/Documents/CIS 4930 - Big Data Analytics/Term Project")

# Import Data Set
hotelBook.df <- read.csv("hotel_bookings.csv")

# View Variables
names(hotelBook.df)
glimpse(hotelBook.df)

############## Data Pre-Processing #####################


# Converting date column to string temporarily to allow replacement of "NULL" to NA to work
dftemp1 <- transform(hotelBook.df, reservation_status_date = as.character(reservation_status_date))

# Replacing "NULL" with NA to prep data for colSums to work
dftemp1[dftemp1 == "NULL"] <- NA
dftemp1[dftemp1 == "CN"] <- "CHN"

# Adding country names, continents and regions in addition to iso3c codes 
# We're basically adding a few dummy variables(comes in handy when visualizing) 
dftemp1$country_name <- countrycode(dftemp1$country, "iso3c", "country.name")
dftemp1$continent <- countrycode(dftemp1$country, "iso3c", "continent")
dftemp1$region <- countrycode(dftemp1$country, "iso3c", "region23")

# Converting date column back to date
dftemp2 <- transform(dftemp1, reservation_status_date = as.Date(reservation_status_date))

# Summarise columns with NA values
colSums(is.na(dftemp2)) %>%
  data.frame() %>%
  filter(. !=0) %>%
  t()


############## NA Value Replacements ####################

# Replacing all NAs with 0
dftemp2[is.na(dftemp2)] <- 0

# Replacing 0s in country, agent and company with "None" and preview
dftemp3 <- mutate(dftemp2,
                  country = replace(country, country == 0, "None"),
                  agent = replace(agent, agent == 0, "None"),
                  company = replace(company, company == 0, "None"),
                  country_name = replace(country_name, country_name == 0, "None")
)

############ Data Synthesis #################

# Adding four new columns, total nights, total rate, total guests and total of the three
dftemp3 <- mutate(dftemp2,
                  total_nights = (stays_in_weekend_nights + stays_in_week_nights),
                  total_rate = (total_nights * adr),
                  total_guests = (adults + children + babies),
                  total_nights_rate_guests = (total_nights + total_rate + total_guests)
)

# Adding arrival month_year column to allow for easier visualization later
dftemp4 <- transform(dftemp3, arrival_date_year = as.character(arrival_date_year)) %>%
  mutate(month_year_temp = paste("1",arrival_date_month,arrival_date_year),
         arrival_month_year = as.Date(month_year_temp, format = "%d %B %Y")
)

################ Data Integrity Check ##################

# Counting the number of observations where there's no nights, rates or guest records
filter(dftemp4, total_nights_rate_guests == 0) %>%
  count()



################ Data Finalization #################

# Final data frame creation for subsequent analysis and preview
hotelBookF.df <- filter(dftemp4, total_nights_rate_guests != 0) %>%
  select(., -total_nights_rate_guests, -month_year_temp)

# Sort Dates in Ascending order
hotelBookF.df <- hotelBookF.df[order(as.Date(hotelBookF.df$reservation_status_date, 
                                             format="%m/%d/%Y")),]


# Verify Data set after changes
glimpse(hotelBookF.df)


############## Time-Series ####################
# Let's break this data set down by years
# We want to predict the last quarter of 2017 

library(forecast)

# # Time Series Data Frame (2016 - 2017)
hb2016_17.df <- hotelBookF.df[25112:82908,]
ts2016_17.df <- hb2016_17.df %>% select(total_rate, reservation_status_date)

glimpse(hb2016_17.df)
glimpse(ts2016_17.df)


# Creating a Time Series object (2016 - 2017)
hotelBook.ts <- ts(ts2016_17.df$total_rate, 
                   start = c(2016, 1), end = c(2017, 12), freq = 12)

# Plot (2016 - 2017)
plot(hotelBook.ts, xlab = "Month", ylab = "Total Rate (Revenue)", 
     ylim = c(0, 800))


# Linear Regression Model
hotelBook.lm <- tslm(hotelBook.ts ~ trend + I(trend^2))
summary(hotelBook.lm)

# overlay the fitted values of the linear model
lines(hotelBook.lm$fitted, lwd = 2)

# Verifying trend and seasonality 
hotelZoom <- decompose(hotelBook.ts)
plot(hotelZoom, xlab = "Month", ylab = "Total Rate (Revenue)", 
     ylim = c(0, 800),
     bty = "l", col = "black")
hotelZoom$seasonal


# Partitioning Data
nValid <- 6
nTrain <- length(hotelBook.ts) - nValid

# Training set
train.ts <- window(hotelBook.ts, start = c(2016, 1),
                   end = c(2016, nTrain))
# Validation Set
valid.ts <- window(hotelBook.ts, start = c(2016, nTrain + 1),
                   end = c(2016, nTrain + nValid))

# Performing the seasonal naive forecast
naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h = nValid)


# Plotting the naive forecast
par(mfrow = c(1, 1))


plot(train.ts, ylim = c(0, 800),  ylab = "Total Rate (Revenue)", 
     xlab = "Month", bty = "l", 
     xaxt = "n", xlim = c(2016, 2018), main = "")


axis(1, at = seq(2016, 2017, 1), labels = format(seq(2016, 2017, 1)))
lines(naive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(2017.25 - 3, 2017.25 - 3), c(0, 3500))
lines(c(2017.25, 2017.25), c(0, 3500))
text(2016, 2500, "Training")
text(2016, 2500, "Validation")
text(2016.75, 2500, "Future")

# Naive Seasonal Prediction Accuracy
accuracy(snaive.pred, valid.ts)

# Creating a Regression Model with a linear trend and monthly seasonality
# Training & Seasonality
train.lm.trend.season <- tslm(train.ts ~ trend + season)
summary(train.lm.trend.season)

# Validation & Seasonality
lm.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
accuracy(lm.pred, valid.ts)
# Compared to naive forecast
accuracy(snaive.pred, valid.ts)


################# Naive Bayes ##################

#Naive code
library(forecast)
library(MLmetrics)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#Loading required packages
install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('caretEnsemble')
library(caretEnsemble)
install.packages('psych')
library(psych)
install.packages('Amelia')
library(Amelia)
install.packages('mice')
library(mice)
install.packages('GGally')
library(GGally)
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)

install.packages("e1071")

str(train)
set.seed(1234)
ind <- sample(2, nrow(hotelBookF.df), replace = T, prob = c(0.8, 0.2))
train <- hotelBookF.df[ind == 1,]
test <- hotelBookF.df[ind == 2,]
model <- naive_bayes(is_canceled, data = train, usekernel = T) 
plot(model) 

p <- predict(model, train, type = 'prob')
head(cbind(p, train))
p1 <- predict(model,train)
tab1 <- table(p1)
length(p)
i1 <- sapply(hotelBook.df, is.numeric)
hotelBook.df[i1]
data <- hotelBook.df[i1]
str(i1)
data[['is_canceled']] <- factor(data[['is_canceled']], levels = c(0,1), labels = c("False", "True"))
str(data)
describe(data)
missmap(data)
ggplot(data, aes(adr , colour = is_canceled)) +
  geom_freqpoly(binwidth = 1) + labs(title="rate per day compared with canceled bookings ")
ggpairs(data)
indxTrain <- createDataPartition(y = data[['is_canceled']],p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100
prop.table(table(training[['is_canceled']])) * 100
x = training[,-9]
y = training[['is_canceled']]

#install.packages("e1071")
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
Predict <- predict(model,newdata = testing ) 
confusionMatrix(Predict, testing[['is_canceled']] )
X <- varImp(model)
plot(X)

############### Neural Network #####################
#descending order
hotelBookF.df <- hotelBookF.df[order(as.Date(hotelBookF.df$reservation_status_date,
                                             format="%m/%d/%Y")),]
hotelBookF.df <- hotelBookF.df[58751:80000,]
# Replacing "NULL" with NA to prep data for colSums to work
hotelBookF.df[hotelBookF.df == "NULL"] <- NA
hotelBookF.df[hotelBookF.df == "CN"] <- "CHN"
# Replacing all NAs with 0
hotelBookF.df[is.na(hotelBookF.df)] <- 0

selected.var <- c("is_canceled","adults","adr","children")
# partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:dim(hotelBookF.df)[1]), dim(hotelBookF.df)[1]*0.6)
train.df <- hotelBookF.df[train.index,selected.var]
valid.df <- hotelBookF.df[-train.index,selected.var]

library(neuralnet)
library(NeuralNetTools)
library(nnet)

nn <- neuralnet(is_canceled ~ adults+adr+children, data=train.df, hidden=5)

# neural network results
plotnet(nn)
neuralweights(nn)

# neural network performance
lm.pred.pro.valid2 <- predict(nn, valid.df, type='response')  

confusionMatrix(factor(ifelse(lm.pred.pro.valid2 > 0.5, 1, 0)), as.factor(valid.df$is_canceled))