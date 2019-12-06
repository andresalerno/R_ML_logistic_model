setwd("~/OneDrive/Documentos/Private/Salerno/Pessoal/Cursos/datacamp/machine_learning")

library(ggplot2)

data <- ggplot2::diamonds

write.csv2(data, file = "diamonds.csv")

colnames(data)

head(data)

# Fit lm model: model
model <- lm(price ~ ., data)

# Predict on full data: p
p <- predict(model, data, type = "response")

# Compute errors: error
error <- unlist(p - data['price'])

# Calculate RMSE
sqrt(mean((error)^2))

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(data))

# Randomly order data
shuffled_data <- data[rows, ]

# Determine row to split on: split
split <- round(nrow(data) * .80)

# Create train
train <- data[1:split, ]

# Create test
test <- data[(split + 1):nrow(data), ]

# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)

# Compute errors: error
error <- unlist(p - test['price'])

# Calculate RMSE
sqrt(mean(error^2))

# Fit lm model using 10-fold CV: model
library(caret)
model <- train(
  price ~., 
  data,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
print(model)

# Fit lm model using 5-fold CV: model
model <- train(
  price ~., 
  data,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

# Print model to console
print(model)

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  price ~ ., 
  data,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
print(model)

# Predict on full data dataset
predict(model, data)

