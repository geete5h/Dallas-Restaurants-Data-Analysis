setwd("D:/JSOM/Spring 2020/BUAN 6356.001 - Business Analytics With R - S20/R Scripts and Datasets/All Datasets")


# LIBRARIES

library(forecast)
library(Hmisc)
library(dplyr)
library(leaps)
library(ggplot2)
#setwd("D:/JSOM/Spring 2020/BUAN 6356.001 - Business Analytics With R - S20/R Scripts and Datasets/All Datasets")

# Creating a data frame with the data set
iv.df <- read.csv("inspectionviolationsbest1.csv")
View(iv.df)
str(iv.df)
#dim(iv.df)
# Selecting the required variables from the data set
variables <- c("Inspection.Score", "Inspection.Type","Zip.Code","Season",
               "Facility_Cleanliness","Food_Temperature", "Food_Contamination",
               "Worker_Cleanliness","Hazardous_Chemicals","Facility_Layout",
               "Certification","Documentation","Equipment","Facility_Amenities",
               "Other","Inspection.Year")


# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Setting the seed value and creating 70% Training and 30% Validation datasets.
set.seed(1)
numberOfRows <- nrow(iv.df)
train.index <- sample(numberOfRows, numberOfRows*0.7)
train.df <- iv.df[train.index, variables]
valid.df <- iv.df[-train.index, variables]

# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# --   FULL LINEAR REGRESSION MODEL  ---------- ( MODEL 1 ) -------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating a full linear regression model. 
iv.lm <- lm(Inspection.Score ~ . , data = train.df )
options(scipen=999)
summary(iv.lm)

# Prediciting the Inspection Scores of the Validation dataset and finding the accuracy.

predicteddata <- predict(iv.lm, valid.df )
accuracy(round(predicteddata),valid.df$Inspection.Score)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Residual Plot of Validation data with full linear regression model
all.residuals <- valid.df$Inspection.Score - predicteddata 
hist(all.residuals, breaks = 25, col = "coral2",xlab = "Residuals", main = "Residual Plot of Validation data with Full Linear Regression model.")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating a table with predicted values, actual values and bins to plot a comparision line chart.

df <- data.frame("Predicted" = predicteddata, "Actual" = valid.df$Inspection.Score)
df <- df[order(-df$Actual),] 
df$bin = as.numeric(cut2(df$Actual))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted),
                              min_Actual = min(Actual), min_Predicted = min(Predicted), 
                              max_Actual = max(Actual), max_Predicted = max(Predicted) )

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting actual vs predicted values for Validation.
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Score" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Score")) 

p1

# -----------------------------------------------------------------------------------------------------------------------------------------------------------
#   REGSUBSETS
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Running the regsubsets and finding the best variable in building the model.

model <- regsubsets(Inspection.Score ~ ., data = train.df, nvmax = 15)
sum <- summary(model)
sum
# show models
sum$which

# show metrics
sum$rsq
sum$adjr2


#Rsq
plot(sum$rsq, xlab = "Number of Variables", ylab = "RSquare",type = "l")

#RSS - residual sum of squares
plot(sum$rss, xlab = "Number of Variables", ylab = "RSS",type = "l")

#Adjusted RSq
plot(sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq",type = "l")
points(11,sum$adjr2[11],col="red", cex=2,pch=20)

#par(mfrow=c(1,1))
plot(model, scale="r2",col = hsv(0.5, .35, seq(0,1,length.out = 12)))


# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# ---  PARSIMONIOUS LINEAR REGRESSION MODEL ------------ ( MODEL 2 ) ----------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Building a parsimonious model with 11 best significant variables which are the violation variables.
pariv.lm <- lm(Inspection.Score ~ Food_Contamination + Worker_Cleanliness + Facility_Cleanliness + 
                 Facility_Layout + Food_Temperature + Equipment + Hazardous_Chemicals + Certification + 
                 Facility_Amenities + Other + Documentation , data = train.df )
options(scipen=999)
summary(pariv.lm)

# Prediciting the Inspection Scores of the Validation dataset and finding the accuracy.
parpredicteddata <- predict(pariv.lm, valid.df )
accuracy(round(parpredicteddata),valid.df$Inspection.Score)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Residual Plot of Validation data with parsimonious linear regression model
parall.residuals <- valid.df$Inspection.Score - parpredicteddata 
hist(parall.residuals, breaks = 25,col="darkseagreen4", xlab = "Residuals", main = "Residual Plot of Validation data with Parsimonious Linear Regression model.")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating a table with predicted values, actual values and bins to plot a comparision line chart.

pardf <- data.frame("Predicted" = parpredicteddata, "Actual" = valid.df$Inspection.Score)
pardf <- pardf[order(-df$Actual),] 
pardf$bin = as.numeric(cut2(pardf$Actual))
table(pardf$bin)
parbin_stats = pardf %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted),
                              min_Actual = min(Actual), min_Predicted = min(Predicted), 
                              max_Actual = max(Actual), max_Predicted = max(Predicted) )


# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Plotting actual vs predicted values for Validation data
p2<- ggplot(parbin_stats, aes(bin)) + 
  geom_line(aes(y = parbin_stats$mean_Predicted, color ="Predicted Score" )) + 
  geom_line(aes(y = parbin_stats$mean_Actual, color = "Actual Score")) 

p2

# -----------------------------------------------------------------------------------------------------------------------------------------------------------


