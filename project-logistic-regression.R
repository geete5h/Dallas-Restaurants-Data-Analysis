library(caret)
library(gains)
library(caret)
library(ROCR)
# Creating a data frame with the data set
iv.df <- read.csv("inspectionviolationsbest1.csv")
iv.df$Recommendation <- as.integer(ifelse(iv.df$Recommendation  == "Recommended" , "1", "0"))
str(iv.df)

#View(iv.df)

# Selecting the required variables from the data set
#variable <- c(2,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20)

variable <- c("Recommendation", "Inspection.Type","Zip.Code","Season",
              "Facility_Cleanliness","Food_Temperature", "Food_Contamination",
              "Worker_Cleanliness","Hazardous_Chemicals","Facility_Layout",
              "Certification","Documentation","Equipment","Facility_Amenities",
              "Other","Inspection.Year")

# Setting the seed value and creating 70% Training and 30% Validation datasets.
set.seed(1)
numberOfRows <- nrow(iv.df)
train.index <- sample(numberOfRows, numberOfRows*0.7)
train.df <- iv.df[train.index, variable]
valid.df <- iv.df[-train.index, variable]

#View(train.df)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# --   LOGISTIC REGRESSION MODEL  ---------- ( MODEL 3) -------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating a logistic regression model. 
iv.glm <- glm(Recommendation ~ . , data = train.df, family = "binomial")
options(scipen=999)
summary(iv.glm)


# Prediciting the Recommendation of the Validation dataset and finding the accuracy.
predicted.data <- predict(iv.glm, valid.df, type = "response")
predicted.data.new <- factor(ifelse(predicted.data  >= 0.8, "Recommended", "Not Recommended"))
valid.reco <- factor(ifelse(valid.df$Recommendation  == 1 , "Recommended", "Not Recommended"))
confusionMatrix(predicted.data.new, valid.reco)

# first 20 actual and predicted records
f5<-data.frame(actual = valid.df$Recommendation[1:20], predicted = predicted.data[1:20])
View(f5)

#creating lift chart and decile-wise lift chart
gain <- gains(valid.df$Recommendation, predicted.data, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Recommendation))~c(0,gain$cume.obs), 
     xlab=" ", ylab=" ", main="", type="l")
lines(c(0,sum(valid.df$Recommendation))~c(0, dim(valid.df)[1]), lty=2)


# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Recommendation)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)



# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# --   Decision Tree  ---------- ( MODEL 4 Trial) -------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------------------------------------------------


library(rpart)
library(rpart.plot)
tree <- rpart(Recommendation ~ ., data = train.df, method = "class",
              control = rpart.control(cp = 0.01, minsplit = 1)) 
# use printcp() to print the table. 
printcp(tree)
prp(tree, type = 1, extra = 1, split.font = 1, varlen = -10) 
predicted <- predict(tree,valid.df,type = "class")
# generate confusion matrix for validation data 
confusionMatrix(predicted, valid.df$Recommendation)
#pruning the decision tree.
pruned.tree <- prune(tree, 
                     cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
printcp(pruned.tree)
prp(pruned.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 
predicted <- predict(pruned.tree,valid.df,type = "class")
# generate confusion matrix for validation data 
confusionMatrix(predicted, valid.df$Recommendation)
