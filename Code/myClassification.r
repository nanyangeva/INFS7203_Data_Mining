# Student Identity : 44814377
# File Description : this file is for the task 3 of the assignment, which is about the 
#                    classification topic.



# 3.1. Load the preprocessed data file from Task 1 into data frame. Divide the
# dataset into “training” and “test” subsets randomly (70% and 30% respectively).

# extract Data
breast_cancer_data <- readRDS("./Data/bcw_processed.Rda")

# for reproducible result
set.seed(4377)

# set training and test ratio
m = nrow(breast_cancer_data)
training_percentage = 0.7
test_percentage = 0.3

# sample random index
ind <- sample(2, m, replace = TRUE, prob = c(training_percentage, test_percentage))

# select training and test data
training_data = breast_cancer_data[ind==1,]
test_data = breast_cancer_data[ind==2,]

# 3.2. Learn a classification tree from the training data using the default
# parameters of the ctree function from the “party” library. Plot that
# classification tree and provide your comments on its structure (e.g., what
# are the important/unimportant variables? Is there any knowledge we can
# infer from the tree representation that helps in differentiating between the
# classes?). Using the learned tree, predict the class labels of the test data.
# Calculate the accuracy, precision, and recall.

# divide features and labels
training_features <- training_data[,1:9]
training_labels <- training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# install and import "party" library
install.packages("party")
library(stats4)
library(grid)
library(mvtnorm)
library(modeltools)
library(zoo)
library(sandwich)
library(strucchange)
library(party)

# for reproducible result
set.seed(4377)

# specify target(class) and predictors(features)
myFormula <- Class ~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion +
  Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses

# generate classification tree
breast_cancer_data_ctree <- ctree(myFormula, data = training_data)

# visualise the tree
plot(breast_cancer_data_ctree)
plot(breast_cancer_data_ctree, type = "simple")

# predict test labels
  # 2 = True; 4 = False
ctree_pred <- predict(breast_cancer_data_ctree, newdata = test_features)
saveRDS(ctree_pred, file = "./data/ctree_pred.Rda")

class_tag <- data.frame(test_data$Class, ctree_pred)
names(class_tag) <- c("class","predict_class")

a <- nrow(subset(class_tag, class == "2" & predict_class == "2"))    # tp
b <- nrow(subset(class_tag, class == "2" & predict_class == "4"))    # fn
c <- nrow(subset(class_tag, class == "4" & predict_class == "2"))    # fp
d <- nrow(subset(class_tag, class == "4" & predict_class == "4"))    # tn

accuracy <- (a+d)/(a+b+c+d)        # 96.1905%
precision <- a/(a+c)               # 95.6522%
recall <- a/(a+b)                  # 98.5075%


# 3.3. Try building you classification tree again via the ctree function but using
# parameters that are different from the default settings. Can you achieve
# better accuracy or more meaningful representation by tuning some
# parameters? (Note that in the ctree function from “party” library, you
# can modifiy ctree_control parameters. Execute ?ctree form RStudio
# Console for the detailed documentation.)

# install and import "party" library
install.packages("party")
library(stats4)
library(grid)
library(mvtnorm)
library(modeltools)
library(zoo)
library(sandwich)
library(strucchange)
library(party)

# for reproducible result
# set.seed(4377)

# default: control_parameter=ctree_control(mincriterion = 0.95, minsplit=20, minbucket=7)
control_parameter <- ctree_control(mincriterion = 0.95, minsplit=20, minbucket=7, mtry = 5, maxdepth = 0)

# Specify target(class) and predictors(features)
myFormula <- Class ~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion +
  Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses

# generate classification tree
breast_cancer_data_ctree <- ctree(myFormula, data = training_data, controls = control_parameter)

# visualise the tree
plot(breast_cancer_data_ctree)
plot(breast_cancer_data_ctree, type = "simple")

# predict test labels
# 2 = True; 4 = False
ctree_pred <- predict(breast_cancer_data_ctree, newdata = test_features)
saveRDS(ctree_pred, file = "./data/ctree_pred.Rda")

class_tag <- data.frame(test_data$Class, ctree_pred)
names(class_tag) <- c("class","predict_class")

a <- nrow(subset(class_tag, class == "2" & predict_class == "2"))    # tp
b <- nrow(subset(class_tag, class == "2" & predict_class == "4"))    # fn
c <- nrow(subset(class_tag, class == "4" & predict_class == "2"))    # fp
d <- nrow(subset(class_tag, class == "4" & predict_class == "4"))    # tn

accuracy_new <- (a+d)/(a+b+c+d)
precision_new <- a/(a+c)
recall_new <- a/(a+b)

# 3.4 Apply K-NN classification to predict the labels in the test subset and
# calculate the accuracy, precision and recall. Particularly, try different
# values of K (e.g. K = 1, 2, 3, 4, 5), and report your observations on the
# achieved classification.

# install and import "class" library
install.packages("class")
library(class)

# classify using K-NN  (k==1)
knn_pred <- knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 1)

# create the confusion matrix
cm = as.matrix(table(Actual = test_labels,Predicted = knn_pred))

n = sum(cm)                       # number of instances
nc = nrow(cm)                     # number of classes
diag = diag(cm)                   # number of correctly classified instances per class
rowsums = apply(cm, 1, sum)       # number of instances per class
colsums = apply(cm, 2, sum)       # number of predictions per class

# compute accuracy, precision, recall, and f1
accuracy_knn1 = sum(diag) / n
precision_knn1 = diag / colsums
recall_knn1 = diag / rowsums
f1_knn1 = 2 * precision * recall / (precision + recall)

results1 <- data.frame(precision_knn1, recall_knn1, f1_knn1)
accuracy_knn1
results1

# classify using K-NN  (k==2)
knn_pred <- knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 2)

# create the confusion matrix
cm = as.matrix(table(Actual = test_labels,Predicted = knn_pred))

n = sum(cm)                       # number of instances
nc = nrow(cm)                     # number of classes
diag = diag(cm)                   # number of correctly classified instances per class
rowsums = apply(cm, 1, sum)       # number of instances per class
colsums = apply(cm, 2, sum)       # number of predictions per class

# compute accuracy, precision, recall, and f1
accuracy_knn2 = sum(diag) / n
precision_knn2 = diag / colsums
recall_knn2 = diag / rowsums
f1_knn2 = 2 * precision * recall / (precision + recall)

results2 <- data.frame(precision_knn2, recall_knn2, f1_knn2)
accuracy_knn2
results2


# 3
# install and import "class" library
install.packages("class")
library(class)

# classify using K-NN  (k==3)
knn_pred <- knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 3)

# create the confusion matrix
cm = as.matrix(table(Actual = test_labels,Predicted = knn_pred))

n = sum(cm)                       # number of instances
nc = nrow(cm)                     # number of classes
diag = diag(cm)                   # number of correctly classified instances per class
rowsums = apply(cm, 1, sum)       # number of instances per class
colsums = apply(cm, 2, sum)       # number of predictions per class

# compute accuracy, precision, recall, and f1
accuracy_knn3 = sum(diag) / n
precision_knn3 = diag / colsums
recall_knn3 = diag / rowsums
f1_knn3 = 2 * precision * recall / (precision + recall)

results3 <- data.frame(precision_knn3, recall_knn3, f1_knn3)
accuracy_knn3
results3


# 4
# classify using K-NN  (k==4)
knn_pred <- knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 4)

# create the confusion matrix
cm = as.matrix(table(Actual = test_labels,Predicted = knn_pred))

n = sum(cm)                       # number of instances
nc = nrow(cm)                     # number of classes
diag = diag(cm)                   # number of correctly classified instances per class
rowsums = apply(cm, 1, sum)       # number of instances per class
colsums = apply(cm, 2, sum)       # number of predictions per class

# compute accuracy, precision, recall, and f1
accuracy_knn4 = sum(diag) / n
precision_knn4 = diag / colsums
recall_knn4 = diag / rowsums
f1_knn4 = 2 * precision * recall / (precision + recall)

results4 <- data.frame(precision_knn4, recall_knn4, f1_knn4)
accuracy_knn4
results4

# 5
# classify using K-NN  (k==5)
knn_pred <- knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 5)

# create the confusion matrix
cm = as.matrix(table(Actual = test_labels,Predicted = knn_pred))

n = sum(cm)                       # number of instances
nc = nrow(cm)                     # number of classes
diag = diag(cm)                   # number of correctly classified instances per class
rowsums = apply(cm, 1, sum)       # number of instances per class
colsums = apply(cm, 2, sum)       # number of predictions per class

# compute accuracy, precision, recall, and f1
accuracy_knn5 = sum(diag) / n
precision_knn5 = diag / colsums
recall_knn5 = diag / rowsums
f1_knn5 = 2 * precision * recall / (precision + recall)

results5 <- data.frame(precision_knn5, recall_knn5, f1_knn5)
accuracy_knn5
results5
