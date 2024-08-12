setwd("E:/R prac")
getwd()
library(readr)
credit_record <- read_csv("Chapter 4/CEP/credit_record.csv")
View(credit_record)
head(credit_record)
application_record <- read_csv("Chapter 4/CEP/application_record.csv")
View(application_record)


-------------------------------------------------------------------------------------
##### 1) Check for the missing values. Find a column-wise count of missing values

sapply(application_record, function(x) which(is.na(x)))

sapply(application_record, function(x) sum(is.na(x)))

##### 2). Treat the columns with missing values with an appropriate strategy.
# more than 30% records are na hence column has been omitted.

application_record$OCCUPATION_TYPE = NULL

####### 3). Check for duplicates in the ‘ID’ column and remove any

duplicated(application_record$ID)
sum(duplicated(application_record$ID))
app_nodup <- application_record[!duplicated(application_record$ID,fromLast=T),]
####View(app_nodup)
###################### Feature Analysis



####### 1).What percentage of IDs are common between the two tables?
table1 <- data.frame(application_record$ID)
table2 <- data.frame(credit_record$ID)
# Find common IDs
common_ids <- intersect(table1$application_record.ID, table2$credit_record.ID)

# Calculate the percentage of common IDs
percentage_common <- length(common_ids) / length(unique(table1$application_record.ID)) * 100

cat("Percentage of common IDs:", percentage_common, "%\n")

########## 2).    Visualize the distribution of income types

income_type = application_record$NAME_INCOME_TYPE
table(income_type)
Income_Type=cbind.data.frame(
  table(income_type))

barplot(Income_Type$Freq,names.arg = Income_Type$income_type,col = c(1,2,3,4,5),)



######### 3). Find the age in years and study the age distribution


Age_Years =application_record$DAYS_BIRTH/365*-1
table(Age_Years)
# Age_dist =cbind.data.frame(Age_Years)
hist(Age_Years,col = 'blue',main = 'Age Distribution', xlab = 'Age(Years)')
plot(density(Age_Years), main = 'Age Distribution', xlab = 'Age(Years)', col='red' ,lwd =3,)



########### 4).Study the gender distribution in applicants’ data

gender = application_record$CODE_GENDER
table(gender)
barplot(table(gender) ,main = 'gender distribution', col = rainbow(2),
        xlab = 'Gender',ylab = 'Freq',border = 'green')


########### 5). Study the average annual income across different education levels

agg_inc <- aggregate.data.frame(application_record$AMT_INCOME_TOTAL, 
                     by=list(application_record$NAME_EDUCATION_TYPE), 
                     FUN=mean) 


barplot(with(agg_inc, setNames(x, Group.1)) ,main = 'average annual income across different education levels',
        col = rainbow(2),
        xlab = 'Education Level',ylab = 'Avg Annual Inc',border = 'green')
class(agg_inc)
# 6.Find the count of applicants from different education levels.Also, showcase how many of those 
# own a car. (Hint: Present the data in a wide format for better understanding.)


library(dplyr)
#Install.packages("reshape2")
library(reshape2)


table(application_record$NAME_EDUCATION_TYPE,application_record$FLAG_OWN_CAR)
df = data.frame(table(application_record$NAME_EDUCATION_TYPE,application_record$FLAG_OWN_CAR))
head(df)

wide = dcast(df,Var1~Var2)

#7.Create a stacked column chart to visualize the above


row.names(wide) = wide$Var1
wide$Var1 = NULL
head(wide)

barplot(as.matrix(t(wide)), col = c('lightblue', 'magenta'))


# Feature Analysis

# 1.Convert “STATUS” to a binary variable called ‘target’, 
# where 0 is for good applicants (where STATUS is either ‘C’, ‘X’ or ‘0’) 
# and 1 is for bad applicants (where STATUS is either 1,2,3,4,5)



credit_record$target = ifelse(credit_record$STATUS %in% c('C','X','0') ,0,1)


# 2.Merge the two datasets to get the common records


nodupcr =aggregate(credit_record$target,list(credit_record$ID),sum)
View(nodupcr)
nodupcr$x = ifelse(nodupcr$x>0,1,0)
colnames(nodupcr) = c('ID','Target')
Ap_cr_record = merge(app_nodup,nodupcr, by = 'ID', all = FALSE)
                  
head(Ap_cr_record)


# 3.Study the data distribution in terms of the target

hist(nodupcr$Target,col = 'blue',main = 'Target Distribution', xlab = '')

plot(density(nodupcr$Target), main = 'Target Distribution', xlab = '', col='red' ,lwd =3,)


#******************************************************************************
# 
# 5.Data preparation and modeling:
#   
#   1.Convert categorical columns into factors


names <- c(2:4,7:10,13:16,18)
Ap_cr_record[,names] <- lapply(Ap_cr_record[,names] , factor)
str(Ap_cr_record)
View(Ap_cr_record)
summary(Ap_cr_record)

drop_cols <-
  c(which(sapply(Ap_cr_record,function (x) length(levels(x))== 1) ),
    which(names(Ap_cr_record)=="ID"))
new_data <-
  Ap_cr_record[, -drop_cols]
head(new_data)
View(new_data)
View(drop_cols)
# 2.Use this cleaned data to build a classification model



Install.packages("caTools")
library(caTools)
set.seed(500)
spl = sample.split(new_data$Target, SplitRatio = 0.7)
train = subset(new_data, spl==TRUE)
test = subset(new_data, spl==FALSE)
print(dim(train)); print(dim(test))

# Load the required libraries/packages
# Fit a logistic regression model
#Load the required libraries/packages

Install.packages("glmnet")  

library(glmnet)

model_glm = glm(Target ~ ., family = binomial, data = train)

# Get a summary of the model
summary(model_glm)

#baseline Accuracy

prop.table(table(train$Target))

# Predictions on the training set
predictTrain = predict(model_glm, data = train, type = "response")

# Confusion matrix on training data
install.packages("caret")
library(caret)
predictTrain <- factor(predictTrain, levels = levels(train$Target))

confusionMatrix(predictTrain, train$Target, positive = "1")


str(predictTrain)
str(train$Target)


---------------------------------------------------------------------------------------------------
cm <- table(predictTrain, train$Target)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])



table(train$Target, predictTrain >= 0.5)
(22516+10)/nrow(train) #Accuracy - 88%

---------------------------------------------------------------------------------------------------

#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")

library(caret)

# Ensure factor levels match in predictTest and test$Target
levels(predictTest) <- levels(test$Target)

# Create a confusion matrix using the table function
confusion_matrix <- table(predictTest, test$Target)

# Print the confusion matrix
print(confusion_matrix)



# Ensure factor levels match in predictTest and test$Target
levels(predictTest) <- levels(test$Target)

# Load the 'caret' package if not already loaded
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}
# Ensure factor levels match in predictTest and test$Target
levels(predictTest) <- levels(test$Target)

# Create a confusion matrix using the table function
confusion_matrix <- table(predictTest, test$Target)

# Print the confusion matrix
print(confusion_matrix)
# Confusion matrix on test set

cm <- table(predictTest, test$Target)
---------------------------------------------------------------------------------------------------
accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])

table(test$Target, predictTest >= 0.5)
(9650+7)/nrow(test) #Accuracy - 88%
---------------------------------------------------------------------------------------------------
library(pROC)
# Assuming 'predictTest' contains your predicted probabilities and 'test$Target' contains the true binary labels

roc_obj <- roc(as.numeric(test$Target), as.numeric(predictTest))
plot(roc_obj, print.auc = TRUE, main = "ROC Curve")

