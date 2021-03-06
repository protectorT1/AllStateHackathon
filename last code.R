log_loss = function(actual, predicted, eps = 1e-15) {  
  predicted = pmin(pmax(predicted, eps), 1-eps)  
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}
set.seed(927540672)
train_data_random <- train_data[sample(nrow(train_data)),] # randomize row order
train_fraction <- 0.7
train <- train_data_random[1:floor(train_fraction*nrow(train_data_random)),]
validate <- train_data_random[(floor(train_fraction*nrow(train_data_random)) + 1):nrow(train_data_random),]
identical(train_data_random, rbind(train, validate)) # test that split was done correctly
baseline_model <- gbm(
  formula=response ~ burglary +rodents + garbage + foodType + sanitation + facilityType + pastFail + zipCode + facilityType + catersLiquorLicense + ward+ timeSinceLast,
  distribution='bernoulli',
  data=train,
  n.trees=50,
  interaction.depth=7,
  n.minobsinnode=5,
  shrinkage=0.1,
  bag.fraction=0.75,
  train.fraction=0.7,
  keep.data=FALSE,
  verbose=TRUE
)
num_trees <- gbm.perf(baseline_model, method='test')
train_pred <- predict(baseline_model, train, n.trees=num_trees, type='response')
validate_pred <- predict(baseline_model, validate, n.trees=num_trees, type='response')
test_pred <- predict(baseline_model, test_data, n.trees=num_trees, type='response')
cat('train average')
log_loss(train$response, rep(mean(train$response), nrow(train)))
cat('train baseline')
log_loss(train$response, train_pred)
cat('validation average')
cat('validation baseline')
log_loss(validate$response, validate_pred)


baseline_model <- gbm(
  formula=response ~ sanitation +rodents + zipCode,
  distribution='bernoulli',
  data=train,
  n.trees=10,
  interaction.depth=7,
  n.minobsinnode=5,
  shrinkage=0.01,
  bag.fraction=0.75,
  train.fraction=0.5,
  keep.data=FALSE,
  verbose=TRUE
)

summary(baseline_model, plot = FALSE)
summary(baseline_model, plot = TRUE)

num_trees <- gbm.perf(baseline_model, method='test')
train_pred <- predict(baseline_model, train, n.trees=num_trees, type='response')
validate_pred <- predict(baseline_model, validate, n.trees=num_trees, type='response')
test_pred <- predict(baseline_model, test_data, n.trees=num_trees, type='response')
cat('train average')
log_loss(train$response, rep(mean(train$response), nrow(train)))
cat('train baseline')
log_loss(train$response, train_pred)
cat('validation average')
cat('validation baseline')
log_loss(validate$response, validate_pred)


numberOfObservationsInTestSet = nrow(test_data)
vectorOfPredictions = runif(numberOfObservationsInTestSet, 0, 1)
summary(vectorOfPredictions)

  outputDataSet = data.frame("inspectionId" = test_data$inspectionId,
                             "response" = vectorOfPredictions)
  
  head(outputDataSet)
  
  write.csv(outputDataSet, "submission3.csv", row.names = FALSE)
  
  dim(train_data)
  dim(test_data)
  
  v <- (train_data$sanitation + train_data$rodents + train_data$zipCode)
  
  vector = c()
  for (item in v)
  {
    item <- gbm(
      formula=response ~ item,
      distribution='bernoulli',
      data=train,
      n.trees=10000,
      interaction.depth=7,
      n.minobsinnode=10,
      shrinkage=0.01,
      bag.fraction=0.75,
      train.fraction=0.5,
      keep.data=FALSE,
      verbose=TRUE
    )
    vector <- c(vector, log_loss(validate$response, validate_pred))
    return(vector)
  }
  
  sanitation_model <- gbm(
    formula=response ~ sanitation,
    distribution='bernoulli',
    data=train,
    n.trees=50,
    interaction.depth=7,
    n.minobsinnode=5,
    shrinkage=0.01,
    bag.fraction=0.75,
    train.fraction=0.5,
    keep.data=FALSE,
    verbose=TRUE
  )
  vector_sanitation <- c( log_loss(validate$response, validate_pred))
  
  rodents_model <- gbm(
    formula=response ~ rodents,
    distribution='bernoulli',
    data=train,
    n.trees=50,
    interaction.depth=7,
    n.minobsinnode=5,
    shrinkage=0.01,
    bag.fraction=0.75,
    train.fraction=0.5,
    keep.data=FALSE,
    verbose=TRUE
  )
  vector_rodents <- c( log_loss(validate$response, validate_pred))
  
  zipCode_model <- gbm(
    formula=response ~ zipCode,
    distribution='bernoulli',
    data=train,
    n.trees=50,
    interaction.depth=7,
    n.minobsinnode=5,
    shrinkage=0.01,
    bag.fraction=0.75,
    train.fraction=0.5,
    keep.data=FALSE,
    verbose=TRUE
  )
  vector_zipCode <- c( log_loss(validate$response, validate_pred))
  
  total <- (vector_sanitation + vector_rodents + vector_zipCode)/3
  print(total)
  
  write.csv(outputDataSet, "submission4.csv", row.names = FALSE)