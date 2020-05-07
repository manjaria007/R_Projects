install.packages("ggcorrplot")
install.packages("Hmisc")
install.packages("corrplot")
library(Hmisc)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(glmnet)
library(DataExplorer)
library(data.table)
library(onehot)
install.packages("onehot")

walmartdta <- read.csv("C:/Users/mohit/Downloads/Train (1).csv")
str(walmartdta) #Displaying the structure of data

#Data cleaning for fat content
Fats <- walmartdta$Item_Fat_Content #select the fat content
unique(Fats) #displaying the names of unique values in Fat_content
walmartdta$Item_Fat_Content[Fats == "LF"] <- "Low Fat"  #Replacing LF
walmartdta$Item_Fat_Content[Fats == "low fat"] <- "Low Fat" #Replacing low fat
walmartdta$Item_Fat_Content[Fats == "reg"] <- "Regular" #Replacing regular
walmartdta$Item_Fat_Content <- droplevels(walmartdta$Item_Fat_Content)
unique(walmartdta$Item_Fat_Content) #displaying the names of unique variable

# DATA CLEANING
weight <- walmartdta$Item_Weight   #Displaying the weight column
summary(weight)   #Displaying statistical summary for 'weight' to show count of missing
count_Weight <- is.na(weight)  #condition checking count of missing values in 'weight'
count_Weight  #displaying the count of missing values in weight column
weight_mean <- mean(weight[!count_Weight])  #computing the weight of available value
weight_mean  #displaying the mean weight without na values
walmartdta$Item_Weight[count_Weight] <- weight_mean  #replacing missing values with weight_mean
walmartdta

write.csv(walmartdta,"C:/Users/mohit/OneDrive/Desktop/Intermediate_Analytics/Week6/Walmartnew.csv", row.names = FALSE)
Walmartnew <- read.csv("C:/Users/mohit/OneDrive/Desktop/Intermediate_Analytics/Week6/Walmartnew.csv")

#EDA EXPLORATORY DATA ANALYSIS
#Creating Bar graph for fat content
ggplot(Walmartnew,aes(x=Walmartnew$Item_Fat_Content, fill=Walmartnew$Item_Fat_Content))+geom_bar()+xlab("Fat Content") + ylab('Frequency')+labs(title="Number of items sold as per fat content")

#Bar graph for Store outlet size
ggplot(Walmartnew,aes(x=Walmartnew$Outlet_Size,fill=Walmartnew$Outlet_Size))+geom_bar() + xlab("Outlet size") + ylab('Frequency')+ labs(title="Number of items sold for each outlet size")

#Bar graph for location type
ggplot(Walmartnew,aes(x=Walmartnew$Outlet_Location_Type,fill=Walmartnew$Outlet_Location_Type))+geom_bar() + xlab("Location Type") + ylab('Frequency')+labs(title="Number of items sold for each location")

#Correlation b/w various fields of walmart dataset

#creating correlation b/w various fields of walmart dataset
walmart_wal <- Walmartnew[c(2,4,6,8,12)]


#plotting the correlation matrix
plot_correlation(walmart_wal)


#HYPOTHESIS TESTING

Hyp0_test <- var.test(Walmartnew$Item_Outlet_Sales[Walmartnew$Item_Fat_Content=="Low Fat"],Walmartnew$Item_Outlet_Sales[Walmartnew$Item_Fat_Content=='Regular'])
Hyp0_test #Displaying test results



##Lasso Regularization
## Supressing warning for removed columns and generating one hot encoder for linear regression
options(warn = -1) #To ignore the warnings
encoder <- onehot(Walmartnew, max_levels = 11) #For one hot encoder
options(warn = 0) # To store warnings till the top level functions returns

##COnverting categorical columns 
datas <- data.frame(predict(encoder, Walmartnew))

Walmartnew_sales <- datas[, 27] #response variable :sales
Walmartnew_sales #Display the sales
Walmartnew_predictors <- datas[, -27] #Predictors
Walmartnew_predictors #Display all the predictors : weight, visibility, MRP, Outlet
view(Walmartnew_predictors)

cross_val <- cv.glmnet(as.matrix(Walmartnew_predictors), #Performing cross validation
                       Walmartnew_sales, #specifying response variable : sales
                       nlambda = 500) #number of lambda values to be used

#Building linear regression model
lmd_lse_cv <- glmnet(as.matrix(Walmartnew_predictors), #using greater lambda value
                     Walmartnew_sales, #specifying response variable
                     nlambda = cross_val$lambda.1se) #specifying lambda value

plot(lmd_lse_cv, #Plotting curve for coefficients for lasso regularization
     xvar = "norm", #specifying 'norm'
     label = TRUE, #Displaying labels for each coefficients curve
     main = "Lasso Regularization - Coefficients") #Specifying title of plot



## Clustering - K means algorithm ##
Walmart_Cluster <- Walmartnew[c(6,12)] #looking for only MRP and outlet sales
Walmart_Cluster #viewing the dataset which has only two variables
Walmart_clust <- data.matrix(Walmart_Cluster) #making the data matrix

Wcc <- (nrow(Walmart_clust)-1)*sum(apply(Walmartnew,2,var))

options(warn=-1) #supressing the warning
for(i in 2:10)Wcc[i]<-sum(kmeans(Walmart_clust,centers = i)$withinss)

#Plotting the results within sum of squares to understand where the value
plot(1:10, Wcc, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")


Kcc <- kmeans(Walmart_clust, 5) #K-means clustering with 3clusters as per the above scr
plot(Walmart_clust, col = (Kcc$cluster), main = "K means clustering", pch = 1, cex = 1)
points(Kcc$centers, col='white', pch=8, cex=3) #inserting the point bases on the clusters
