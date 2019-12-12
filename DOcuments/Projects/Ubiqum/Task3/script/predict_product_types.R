# Predict the sales in four different types of products
# (PC, Laptops, Netbooks, Smartphones)
# while assessing the effects service and customer reviews have on sales
# And, of course, predict sales for the four types in the newproducts

# Load necessary packages ####
library(readr)
library(ggplot2)
library(lattice)
library(corrplot)
library(e1071)
library(randomForest)
library(caret)
library(dplyr)
library(tidyverse)
library(reshape)


# Load the datasets ####
EPA<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task3/data/existingproductattributes2017.csv")
NPA<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task3/data/newproductattributes2017.csv")
EPA <- data.frame(EPA)
NPA <- data.frame(NPA)

# Data exploration ####
summary(EPA)
summary(NPA)
str(EPA)
str(NPA)

# Check for missing values (NA) and remove those observations
summary(EPA)
# There are 15 observations that contain missing values (NA), all of them in the BestSellersRank column
# The bestsellers column will, thus, be removed completely
EPA$BestSellersRank <- NULL
summary(EPA)

# Check for outliers ####
boxplot(EPA[2:ncol(EPA)])
# Remove outliers from the dependent variable
OutlierDataSet <- EPA
OutlierColumn <- OutlierDataSet[,ncol(OutlierDataSet)]
OutlierDataSet <- OutlierDataSet[OutlierColumn > (quantile(OutlierColumn)[[2]] - 1.5*IQR(OutlierColumn)),]
OutlierDataSet <- OutlierDataSet[OutlierColumn < (quantile(OutlierColumn)[[4]] + 1.5*IQR(OutlierColumn)),]
EPA <- OutlierDataSet

# Check for duplicated observations
sum(duplicated(EPA[-which(names(EPA) == 'ProductNum')]))
EPA[!duplicated(EPA[-which(names(EPA) == 'ProductNum')]),]
sum(duplicated(NPA[-which(names(NPA) == 'ProductNum')]))
NPA[!duplicated(NPA[-which(names(NPA) == 'ProductNum')]),]

# Check for duplicates without the price ####
duplicated(EPA[c('ProductType','Price')])
# There are duplicated values with different prices
# The duplicated values will be merged into one single value and the price will be the mean of the duplicated values
# Keep one of the duplicated observations (the first one, for example)
duplicatedproductsvalues <- EPA[duplicated(EPA[,-c(2,3)]),][1,]
# Calculate the mean of the price of the duplicated observations
mean_duplicated <- mean(EPA$Price[duplicated(EPA[-c(2,3)])])
# Remove the duplicated observations
EPA <- EPA[!duplicated(EPA[-c(2,3)]),]
# Add one of the duplicated observations (the first one that had been saved previously)
EPA <- rbind(EPA,duplicatedproductsvalues)
# Change the price to the mean of the price of the duplicated values
EPA[nrow(EPA),'Price'] <- mean_duplicated

# Check for rows with missing reviews ####
nrow(EPA[which(EPA$x4StarReviews==0 & EPA$x3StarReviews == 0 & EPA$x2StarReviews == 0 & EPA$x1StarReviews == 0),])
# There are 3 rows with missing x4,x3,x2,x1 reviews that will be removed
EPA <- EPA[-which(EPA$x4StarReviews==0 & EPA$x3StarReviews == 0 & EPA$x2StarReviews == 0 & EPA$x1StarReviews == 0),]

# Check the variables ####
ggplot(EPA, aes(x=x4StarReviews, y = Volume)) +
  geom_point() +
  geom_smooth()
ggplot(EPA, aes(x=x3StarReviews, y = Volume)) +
  geom_point() +
  geom_smooth()
ggplot(EPA, aes(x=x2StarReviews, y = Volume)) +
  geom_point() +
  geom_smooth()
ggplot(EPA, aes(x=x1StarReviews, y = Volume)) +
  geom_point() +
  geom_smooth()
ggplot(EPA, aes(x=PositiveServiceReview, y = Volume)) +
  geom_point() +
  geom_smooth()
ggplot(EPA, aes(x=NegativeServiceReview, y = Volume)) +
  geom_point() +
  geom_smooth()

# Removing the observations that the graphs before showed were distrustful ####
EPA <- EPA[-which(EPA$ProductNum == 118),]
EPA <- EPA[-which(EPA$ProductNum == 123),]
EPA <- EPA[-which(EPA$ProductNum == 134),]
EPA <- EPA[-which(EPA$ProductNum == 135),]

# Placing the dependent variable in the first position ####
EPA <- subset(EPA, select=c(ncol(EPA),1:(ncol(EPA)-1)))

# Check correlation
# Dummify with feature engineering
newDataFrame <- dummyVars(" ~ .", data = EPA)
EPA_dummy <- data.frame(predict(newDataFrame, newdata = EPA))
CorrData <- cor(EPA_dummy)
# Check correlation with corrplot
corrplot(CorrData)
corrplot(CorrData,tl.pos='n', method='pie')
corrplot(CorrData, type='upper', method='pie')

# Remove attributes ####
# Look for high correlation with the dependent variable and remove attributes
corrDataFrame <- data.frame(CorrData)
corrDataFrame[lower.tri(corrDataFrame,diag=TRUE)] <- NA
correlations <- corrDataFrame %>%
  rownames_to_column("id") %>%
  gather(key = "key", value = "value", -id) %>%
  filter(value > 0.85 | value < -0.85)
#correlationAle <- as.table(corrData)

# Plot the volume of sales by product, colored by product type ####
ggplot(EPA, aes(x=reorder(ProductNum, -Volume), y = Volume, fill = ProductType)) + 
  geom_col(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('Product Number') +
  ylab('Volume') +
  ggtitle('Volume of sales by product') +
  labs(fill = 'Product Type')

# Feature Engineering ####
EPA$TotalServiceReviews <- EPA$PositiveServiceReview + EPA$NegativeServiceReview

# Normalize Data ####
# Normalize function
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# # Normalize the data
# EPA_normalized <- cbind(EPA[c(1,2,3)],normalize(EPA[-c(1,2,3)]))

# Dummify with feature engineering
newDataFrame <- dummyVars(" ~ .", data = EPA)
EPA_dummy <- data.frame(predict(newDataFrame, newdata = EPA))

# Define training and testing sets ####
set.seed(107)
inTrain <- createDataPartition(y=EPA_dummy$Volume,p=0.75,list=FALSE)
training <- EPA_dummy[inTrain,]
testing <- EPA_dummy[-inTrain,]
nrow(training)
nrow(testing)

# Modelling the training data and testing with the test data ####
# Set the model training method to a 1 time repeated cross validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3)
# Train the model
a <- c("Volume ~ x4StarReviews", "Volume ~ x4StarReviews + PositiveServiceReview", "Volume ~ x4StarReviews + TotalServiceReviews", "Volume ~ PositiveServiceReview")
b <- c("knn", "rf","svmLinear")
compare_var_mod <- c()
compare_models <- c()
for ( i in a) {
  for (j in b) {
    model <- train(formula(i), data = training, method = j, trControl = ctrl, preProcess = c('center','scale'), tuneLength = 20)
    pred <- predict(model, newdata = testing)
    pred_metric <- postResample(testing$Volume, pred)
    compare_models <- c(compare_models,model)
    compare_var_mod <- cbind(compare_var_mod , pred_metric)
  }
}
names_var <- c()
for (i in a) {
  for(j in b) {
    names_var <- append(names_var,paste(i,j))
  }
}
colnames(compare_var_mod) <- names_var
compare_var_mod
# Melt the error metrics
compare_var_mod_melt <- melt(compare_var_mod, varnames=c("metric","model"))
compare_var_mod_melt <- as.data.frame(compare_var_mod_melt)
compare_var_mod_melt
# Plot the error metrics
ggplot(compare_var_mod_melt, aes(x=model, y=value, fill = model)) +
  geom_col() +
  facet_grid(metric~., scales="free") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  xlab('') +
  ggtitle('Error metrics comparison') +
  labs(fill = 'Features and model used')

# Predict for the new products ####
# Retrain and retest the model using the best features and algorithm
model <- train(Volume ~ x4StarReviews + TotalServiceReviews, data = training, method = 'rf', trControl = ctrl, preProcess = c('center','scale'), tuneLength=20)
model
pred <- predict(model, newdata = testing)
pred_metric <- postResample(testing$Volume, pred)
pred_metric
error <- data.frame(pred - testing$Volume)
colnames(error) <- c("err")
ggplot(error, aes(err)) + 
  geom_histogram(bins=20)
# Make predictions for the new attributes
NPA_undummy <- NPA
newDataFrame <- dummyVars(" ~ .", data = NPA)
NPA <- data.frame(predict(newDataFrame, newdata = NPA))
NPA <- subset(NPA, select=c(ncol(NPA),1:(ncol(NPA)-1)))
NPA$TotalServiceReviews <- NPA$PositiveServiceReview + NPA$NegativeServiceReview
NPA$BestSellersRank <- NULL
predictionsnew <- predict(model,NPA)
predictionsnew
NPA$Volume <- predictionsnew

# Predictions by product type ####
# Laptop predictions
NPA <- NPA[NPA$ProductTypeLaptop == 1 | NPA$ProductTypePC == 1 |NPA$ProductTypeNetbook == 1 | NPA$ProductTypeSmartphone == 1,]
NPA_undummy$Volume <- predictionsnew
NPA_undummy <- NPA_undummy[NPA_undummy$ProductType == 'PC' | NPA_undummy$ProductType == 'Laptop' |NPA_undummy$ProductType == 'Netbook' | NPA_undummy$ProductType == 'Smartphone',]

ggplot(NPA_undummy, aes(x = ProductType,y = Volume, fill = ProductType)) +
  geom_col()
