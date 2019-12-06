setwd("~/OneDrive/Documentos/Private/Salerno/Pessoal/Cursos/datacamp/machine_learning")

library(caret)

data <- read.csv2("sonar.csv", sep = ",", header = TRUE)

colnames(data)

head(data)

# Shuffle row indices: rows
rows <- sample(nrow(data))

# Randomly order data
shuffled_data <- data[rows, ]

# Determine row to split on: split
split <- round(nrow(data) * .60)

# Create train
train <- data[1:split, ]

# Create test
test <- data[(split + 1):nrow(data), ]

# Fit glm model: model
model <- glm(R ~ ., family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type = "response")

