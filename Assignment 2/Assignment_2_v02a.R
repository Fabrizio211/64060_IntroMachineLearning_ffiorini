
library(caret)
library(FNN)
library(ggplot2)
library("gmodels")
library(ISLR)
library(dplyr)
library(fastDummies)



#LOAD

customers <- read.csv("UniversalBank.csv")

#EXPLORATION

head(customers)

str(customers)

summary(customers)

unique(customers$Education)

#DUMMY VARIABLES

dummy_df <- dummy_cols(customers, select_columns = c("Education"))
head(dummy_df)

#SUBSET

subset <- select(dummy_df, 2:4, 6:7, 9:17)
head(subset)

#PARTITIONING

set.seed(123)

sample = createDataPartition(subset$Income, p=0.60, list=FALSE)

train_df = subset[sample, ]
valid_df = subset[-sample, ]

summary(train_df$Income)

summary(valid_df$Income)

ggplot(train_df, aes(x=Income, y=Age, colour=train_df$Personal.Loan)) + geom_point()

#NORMALIZATION

norm_train <- train_df
norm_valid <- valid_df

norm_values <- preProcess(train_df[ , 1:6], method=c("center", "scale"))

norm_train[ , 1:6] <- predict(norm_values, train_df[ , 1:6])
norm_valid[ , 1:6] <- predict(norm_values, valid_df[ , 1:6])

summary(norm_train)
summary(norm_valid)

#TRAINING / K-NN MODEL

#predictors
train_predictors <- norm_train[ , -7]
valid_predictors <- norm_valid[ , -7]
#label
train_labels <- norm_train[ , 7]
valid_labels <- norm_valid[ , 7]

set.seed(1234)
predicted_lables <- knn(train_predictors, valid_predictors, cl=train_labels, k=1)
head(predicted_lables)
summary(predicted_lables)

#CONFUSION MATRIX

c_matrix <- CrossTable(x=valid_labels, y=predicted_lables, prop.chisq=FALSE)

accuracy <- (c_matrix$t[2,2] + c_matrix$t[1,1])/ sum(c_matrix$t)
print(accuracy)
recall <- c_matrix$t[2,2]/ (c_matrix$t[2,2] + c_matrix$t[2,1])
print(recall)
precision <- c_matrix$t[2,2]/ (c_matrix$t[2,2] + c_matrix$t[1,2])
print(precision)
specificity <- c_matrix$t[1,1]/ (c_matrix$t[1,1] + c_matrix$t[1,2])
print(specificity)

#NEW OBSERVATION

new_cust <- data.frame(
  "Age" = as.integer(40), 
  "Experience" = as.integer(10), 
  "Income" = as.integer(84), 
  "Family" = as.integer(2), 
  "CCAvg" = as.double(2), 
  "Mortgage" = as.integer(0), 
  "Personal.Loan" = NA,
  "Securities.Account" = as.factor(0), 
  "CD.Account" = as.factor(0), 
  "Online" = as.factor(1), 
  "CreditCard" = as.factor(1), 
  "Education_1" = as.factor(0), 
  "Education_2" = as.factor(1), 
  "Education_3" = as.factor(0))
head(new_cust)

library(FNN)
nn <- knn(train_predictors, norm_new_cust, cl=train_labels, k=1)
row.names(train.df)[attr(nn, "nn.index")]
nn

#new_cust <- c(40, 10, 84, 2, 2, 0, 0, 0, 1, 1, 0, 1, 0)
#new_cust

norm_new_cust <- new_cust
norm_new_cust[ , 1:6] <- predict(norm_values, new_cust[ , 1:6])
head(norm_new_cust)


# To select only the first six columns that we want to normalize
#x <- as.data.frame(t(new_obs[1:6]))
#colnames (x) <- c('Age', 'Expereince', 'Income', 'Family', 'CCAvg', 'Mortgage')
#To normalize my new obervation
#n=(x-norm_values$mean)/norm_values$std
# To get the same dimensions
#m = c(n$Age,n$Expereince,n$Income,n$Family,n$CCAvg,n$Mortgage,new_obs[7], new_obs[8],
#      new_obs[9], new_obs[10], new_obs[11], new_obs[12], new_obs[13])
# To show how my the variable looks like
#m

# Calculate the probability for the new observation
norm_new_cust <- norm_new_cust[,-7]
norm_new_cust


new_cust$Personal.Loan <- knn(train_predictors, norm_new_cust,
                              cl=train_labels, k=1, prob = TRUE)


# To get the probability output
class_prob2<-attr(predicted_labels_prob2, 'prob')
# Show the first lines
head(class_prob2)

#TUNING

set.seed(1234)
Search_grid <- expand.grid(k=c(1:10,15,20,25))
train_predict_labels <- train_predictors
train_predict_labels$Personal_Loan = train_labels

modeltest<-train(factor(Personal_Loan)~Age+Experience+Income+Family+
                   CCAvg+Mortgage+'Securities Account'+'CD Account'+Online+
                   CreditCard+Education_1+Education_2+Education_3,
                 data = train_predict_labels, method="knn",
                 tuneGrid=Search_grid,
                 preProcess='range')
# To show the result
modeltest
