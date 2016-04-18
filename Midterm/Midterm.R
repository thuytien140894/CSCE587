# Part 1: Setting up the data
dataset <- read.csv("~/Documents/College/Computer Science/CSCE 587/tae.dat")
dataset$Class <- factor(dataset$Class)
# the data in column 6 now has levels after they are converted to a factor
levels(dataset$Class)

test_set <- dataset[1:51, ]
training_set <- dataset[52:151, ]

# Part 2: Naive Bayes Analysis
install.packages("e1071")
library(e1071)

# Train the model
training_model <- naiveBayes(training_set[, 1:5], training_set[, 6])

# Confusion matrix
confusion_matrix <- table(predict(training_model, test_set[, 1:5]), test_set[, 6])

total_observations <- sum(colSums(confusion_matrix))
correct_classifications <- sum(diag(confusion_matrix))

model_accuracy <- correct_classifications / total_observations
model_accuracy
[1] 0.32

# Part 3: Decision Tree Analysis
install.packages("rpart")
library("rpart")

# Train the model
tree1 <- rpart(Class ~ Nat + Inst + C + Sem + Size,
  method = "class", data = training_set,
  control = rpart.control(minsplit=2, cp=0.02))

# Confusion matrix
tree1_confusion_matrix <- table(predict(tree1, test_set[, 1:5], type="class"), test_set[, 6])

tree1_total_observations <- sum(colSums(tree1_confusion_matrix))
tree1_correct_classifications <- sum(diag(tree1_confusion_matrix))
tree1_model_accuracy <- tree1_correct_classifications / tree1_total_observations
tree1_model_accuracy
[1] 0.6470588

training_confusion_matrix <- table(predict(tree1, training_set[, 1:5], type="class"), training_set[, 6])
training_confusion_matrix

     1  2  3
  1 28  6  2
  2  5 25  7
  3  5  5 17

training_total_observations <- sum(colSums(training_confusion_matrix))
training_correct_classifications <- sum(diag(training_confusion_matrix))
training_model_accuracy <- training_correct_classifications / training_total_observations
training_model_accuracy
[1] 0.7
