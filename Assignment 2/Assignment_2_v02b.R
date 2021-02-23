set.seed(1234)

sample2 <- createDataPartition(subset$Income, p=0.80, list=FALSE)

traval_df2 = subset[sample2, ]
test_df2 = subset[-sample2, ]

sample3 = createDataPartition(traval_df2$Income, p=0.50, list=FALSE)

train_df2 = subset[sample3, ]
valid_df2 = subset[-sample3, ]

summary(train_df2$Income)
summary(valid_df2$Income)
summary(test_df2$Income)

train_norm_df1 <- train_df2
valid_norm_df1 <- valid_df2
traval_norm_df1 <- traval_df2
test_norm_df1 <- test_df2

norm_values2 <- preProcess(train_df2[, 1:6], method=c("center", "scale"))

train_norm_df1[, 1:6] <- predict(norm_values2, train_df2[, 1:6])
valid_norm_df1[, 1:6] <- predict(norm_values2, valid_df2[, 1:6])

norm_values3 <- preProcess(traval_df2[, 1:6], method=c("center", "scale"))

traval_norm_df1[, 1:6] <- predict(norm_values3, traval_df2[, 1:6])

test_norm_df1[, 1:6] <- predict(norm_values3, test_df2[, 1:6])

summary(train_norm_df1)
summary(valid_norm_df1)



#Melissa
train_predictors1 <-train_norm_set1[,1:13, drop = TRUE]
valid_predictors1 <-valid_norm_set1[,1:13, drop = TRUE]
train_labels1 <-train_norm_set1[,14, drop = TRUE]
valid_labels1 <-valid_norm_set1[,14, drop = TRUE]
#Run the model using k = 1
set.seed(1234)
my_knn2 <-knn(train_predictors1,
              valid_predictors1,
              cl=train_labels1,
              k=1 )
head(my_knn2)
summary(my_knn2)

