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

