library("caret")
library("corrplot")

#load data ####
EPA<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task3/data/existingproductattributes2017.csv")
NPA<-read.csv("C:/Users/Burbu/Documents/Projects/Ubiqum/Task3/data/newproductattributes2017.csv")

#Exploring and preprocessing data ####
summary(EPA)
summary(NPA)
str(EPA)
str(NPA)

#I noticed that there are 15 NA values in Best Sellers Rank
sum(is.na(EPA))
sum(is.na(EPA$BestSellersRank)) #only to confirm that

#Remove NA values####
#I will remove attribute Best Sellers Rank because is the best way to treat NA values in this case
EPA$BestSellersRank <- NULL
summary(EPA)
sum(is.na(EPA))

#Remove outliers####
#Now, let's identify the outliers and assign them to a vector
boxplot(EPA$Volume)
outliers <- boxplot(EPA$Volume, plot=FALSE)$out

# Find in which rows the outliers are
EPA[which(EPA$Volume %in% outliers),]
# Now remove the rows containing the outliers:
EPA <- EPA[-which(EPA$Volume %in% outliers),]
boxplot(EPA$Volume)


# Remove duplicates ####
#I will remove duplicated values

options(max.print = 500)
duplicated(EPA[-2]) #I have to exclude Productnum in the analysis because it's the ID of the product and always is diferent. 
duplicated(EPA[17])#I comproved the duplicates in the dependent variable
duplicated(EPA[c(-2, -3)])#I also noticed that there are some products with the same characteristics but diferent prices.
#I find some duplicate values, but we would treat it later

#Dummify ProductType and do the correlation matrix for feature engineering####
#We will dummify the Product Type attribute to include it in the correlation matrix. 
NewEPA <- dummyVars(" ~ .", data = EPA)
readyData <- data.frame(predict(NewEPA, newdata = EPA))
CorrData <- cor(readyData)
CorrData


#feature engineering ####
#I tried a lot of new variables to find if someone performs better as a predictro with volume
#I have to be careful about magnitudes and metric sistem. The weight is probably on lbs, so I have it present when I will analize the results
EPA$PVolume <- EPA$ProductDepth * EPA$ProductWidth * EPA$ProductHeight
EPA$PVolume
#I tried to create the feature density, but it doesn't work
#EPA$Density <- EPA$ShippingWeight/EPA$Volume
#EPA$Density
EPA$TotalServiceReviews <- EPA$PositiveServiceReview - EPA$NegativeServiceReview
EPA$TotalServiceReviews
CorrData[2,3]
EPA$barTSR <- (EPA$PositiveServiceReview * CorrData[20, 28]) - (EPA$NegativeServiceReview * CorrData[21, 28])
EPA$barTSR

#to look how many product volumes are equal to zero
sum(EPA$PVolume == 0) #that's why density, later, don't work in a correlation matrix


#We also high reviews values (3 and 4 stars reviews) in multiple ways
EPA$HighReviewsmn <- (EPA$x4StarReviews + EPA$x3StarReviews)/2
EPA$HighReviewsbar <- (EPA$x4StarReviews * CorrData[16, 28]) + (EPA$x3StarReviews * CorrData[17, 28])
EPA$HighReviewsmn
EPA$HighReviewsbar

#The same with the lower revies
EPA$LowReviewsmn <- (EPA$x2StarReviews + EPA$x1StarReviews)/2
EPA$LowReviewsbar <- (EPA$x2StarReviews * CorrData[18, 28]) + (EPA$x1StarReviews * CorrData[19, 28])
EPA$LowReviewsbar
EPA$LowReviewsmn

#And the same with all reviews attributes (TSR= Total Star Reviews )
EPA$TSR <- (EPA$x4StarReviews * CorrData[16, 28]) + (EPA$x3StarReviews * CorrData[17, 28]) + 
  (EPA$x2StarReviews * CorrData[18, 28]) + (EPA$x1StarReviews * CorrData[19, 28])
EPA$TSR2 <- EPA$TSR/4
EPA$TSR2
EPA$PosvsNeg <- ((EPA$x4StarReviews * CorrData[16, 28]) + (EPA$x3StarReviews * CorrData[17, 28])) - 
  ((EPA$x2StarReviews * CorrData[18, 28]) + (EPA$x1StarReviews * CorrData[19, 28]))
EPA$TSR3 <- (EPA$x4StarReviews * 1) + (EPA$x3StarReviews * 0.75) + 
  (EPA$x2StarReviews * 0.5) + (EPA$x1StarReviews * 0.25) #I baremate the variables in diferent ways. Here I tried assigning diferent values. 

# Dummify the type again####
#We dummified again the data Becuse we added the other variables in the EPA df
NewEPA <- dummyVars(" ~ .", data = EPA)
EPAdumy <- data.frame(predict(NewEPA, newdata = EPA))


#Correlation Matrix with featuring engineering####
#I again did a correlation matrix with all variables to see how the variables are related
AllCorrData <- cor(EPAdumy)
AllCorrData

#I gonna show a correlation plot of that second correlation
corrplot(AllCorrData)


#modeling ####

#we tried our model with all variables
set.seed(100)
in_training <- createDataPartition(EPAdumy$Volume, p = 0.7, list = F)

trainset <- EPAdumy[in_training,]
testset <- EPAdumy[-in_training,]

a <- c("rf", "knn", "svmLinear", "svmRadial")
compare <- c()
for (i in a) {
  fit <- train(Volume ~ ., trainset, method = i)
  pred <- predict(fit, testset)
  metric <- postResample(testset$Volume, pred)
  compare <- cbind(metric, compare)
}
colnames(compare) <- a
compare
class(compare)

cmm <- as.data.frame(compare)
cmm <- melt(compare, varnames = c("metric", "model"))
cmm

#second modeling ####
#First, we selected the variables
FinalEPA <- EPAdumy[c(1:12,33,28)]

#Then, we run the model
set.seed(123)
in_training <- createDataPartition(FinalEPA$Volume, p = 0.7, list = F)

trainset <- FinalEPA[in_training,]
testset <- FinalEPA[-in_training,]

a <- c("rf", "knn", "svmLinear", "svmRadial")
compare <- c()
for (i in a) {
  fit <- train(Volume ~ ., trainset, method = i)
  pred <- predict(fit, testset)
  metric <- postResample(testset$Volume, pred)
  compare <- cbind(metric, compare)
}
colnames(compare) <- a
compare
class(compare)

cmm <- as.data.frame(compare)
cmm <- melt(compare, varnames = c("metric", "model"))
cmm
