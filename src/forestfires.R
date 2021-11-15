#---------------------------------------------
# author: Malsha Ranawaka
# date: 18 Sep 2020
#---------------------------------------------


library(ISLR)
library(tree)

# read and attach dataset
fire_data = read.csv("forestfires.csv")
attach(fire_data)

# get first few lines of the dataset
head(fire_data)

# get the size of the dataset
dim(fire_data)

# class of each variable in the dataset
sapply(fire_data, class)

# structure of the dataset
str(fire_data)

# summary of the dataset
summary(fire_data)

# check if dataset contains NA values
colSums(is.na(fire_data))

# plot the distribution of the target variable
par(mfrow=c(1,3))
hist(fire_data$area, main = "area", xlab = "")
hist(log(fire_data$area), main = "log(area)", xlab = "")
hist(log(fire_data$area+1), main = "log(area+1)", xlab = "")
mtext("Distribution of Burnt Area", line = -1, outer = TRUE, cex = 0.8, font=2)

# calculate the percentage of zeros in the target variable
sum(fire_data$area==0)*100/dim(fire_data)[1]

# transform target variable of the dataset
fire_data$area_log <- log(fire_data$area+1)

# map month names to numbers
fire_data$month_num <- match(fire_data$month, tolower(month.abb))

days = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
# map day names to numbers
fire_data$day_num <- match(fire_data$day, tolower(days))

# create the transformed dataset
transformed_data <- fire_data[1:2]
transformed_data$Month <- fire_data$month_num
transformed_data$Day <- fire_data$day_num
transformed_data <- cbind(transformed_data, fire_data[5:8])
transformed_data$Temperature <- fire_data$temp
transformed_data <- cbind(transformed_data, fire_data[10])
transformed_data$Wind <- fire_data$wind
transformed_data$Rain <- fire_data$rain
transformed_data$Area <- fire_data$area_log
head(transformed_data)

# plot linear relationship of target variable against independent variables 
par(mfrow=c(3, 4))
fire_data_x <- transformed_data[-13]
var_names <- colnames(fire_data_x)
color_palette = c(46, 35, 85, 60, 99, 125, 371, 428, 466, 524, 81, 551)

i <- 1
for (col in fire_data_x){
  plot_title <- paste("Area vs", var_names[i])
  
  if (var_names[i] == 'Month') {
    plot(col, transformed_data$Area, 
         main = plot_title, xlab = "", ylab = "", 
         type = "p", col = colors()[81], pch = 20, xaxt = "n")
    axis(1, at=1:12, labels=month.abb) 
  }
  else if (var_names[i] == 'Day'){
    plot(col, transformed_data$Area, 
         main = plot_title, xlab = "", ylab = "", 
         type = "p", col = colors()[81], pch = 20, xaxt = "n")
    axis(1, at=1:7, labels=days) 
  } else {
    plot(col, transformed_data$Area, 
         main = plot_title, xlab = "", ylab = "", 
         type = "p", col = colors()[81], pch = 20)
  }
  i <- i+1
}

# plot the pairwise correlation plots
pairs(transformed_data, panel = panel.smooth)

# get the correlation coefficients 
cor(transformed_data, method = "pearson")

# order correlations with target variable 
target_cor <- cor(transformed_data$Area, transformed_data)
# get the sorted indexes to order variable names
sorted_indexes <- order(target_cor, decreasing = TRUE)
# create sorted dataframe with variable names in the sorted order
sorted_cor <- t(data.frame(colnames(target_cor)[sorted_indexes], sort(target_cor, decreasing = TRUE)))
row.names(sorted_cor) <- NULL
sorted_cor

### decision tree modelling

## regression tree
set.seed(2)

# tree model for the complete dataset
reg_tree1 <- tree(Area~., data = transformed_data)
summary(reg_tree1)
print(reg_tree1)

# plot the tree
plot(reg_tree1, col = colors()[125])
text(reg_tree1, pretty = 0)


# tree model for the train-test split dataset using cross validation

# set the seed for sampling
set.seed(2)
# get the train test split
reg_train_split <- sample(1:nrow(transformed_data), nrow(transformed_data)*0.6)

# train the tree model
reg_tree2 <- tree(Area~., data = transformed_data, subset = reg_train_split)
summary(reg_tree2)
print(reg_tree2)

# plot the trained tree
plot(reg_tree2, col = colors()[125])
text(reg_tree2, pretty = 0)

# predict using the model
reg_predict = predict(reg_tree2, newdata = transformed_data[-reg_train_split,]) 
reg_test = transformed_data[-reg_train_split, 'Area'] 
plot(reg_predict, reg_test, xlab = "Predicted", ylab = "Actual")
abline(0,1)

# calculate the mean squared error
MSE <- mean((reg_predict-reg_test)^2)
cat("MSE: ", MSE, "\t", "Sqrt MSE: ", sqrt(MSE), "\n", sep = "")


# train model using cross validation
reg_cv <- cv.tree(reg_tree2)
reg_cv
# plot the cross validation results, deviation explained by the model against the size of the tree
plot(reg_cv$size, reg_cv$dev, type = "b", col = colors()[371],
     xlab = "Size", ylab = "Deviation")

# prune tree based on the cv results
reg_pruned_tree = prune.tree(reg_tree2, best = 4) 
# plot the pruned tree
plot(reg_pruned_tree, col = colors()[371]) 
text(reg_pruned_tree, pretty = 0)

# predict using the pruned tree
reg_predict2 = predict(reg_pruned_tree, newdata = transformed_data[-reg_train_split,]) 
reg_test2 = transformed_data[-reg_train_split, 'Area'] 
plot(reg_predict2, reg_test2, xlab = "Predicted", ylab = "Actual")
abline(0,1)

# calculate the mean squared error
MSE2 <- mean((reg_predict2-reg_test2)^2)
cat("MSE: ", MSE2, "\t", "Sqrt MSE: ", sqrt(MSE2), "\n", sep = "")

# ====================
## classification tree

# mapping target variable to a categorical variable
# make the target variable for classification task Fire = c('Yes', 'No')
# if the burnt area is zero, Fire variable is set as 'No' 

# remove the last column 'Area'
classification_data <- transformed_data[-13]
# add the new target variable 'Fire'
classification_data$Fire <- as.factor(ifelse(transformed_data$Area==0, "No", "Yes"))
head(classification_data)

# tree model for the complete dataset
cl_tree1 <- tree(Fire~., data = classification_data)

# plot the tree model
plot(cl_tree1, col = colors()[60])
text(cl_tree1, pretty = 0)

# get the summary of the tree
summary(cl_tree1)
print(cl_tree1)

# predict using the classification tree model
cl_predict1 <- predict(cl_tree1, classification_data, type = "class")
# get the confusion matrix
table(cl_predict1, classification_data$Fire)
# calculate the misclassification error rate
misclass.tree(cl_tree1)/dim(classification_data)[1]


# tree model for partitioned dataset

# divide the training and testing datasets to 60:40 ratio
set.seed(2)
cl_train_split <- sample(1:nrow(classification_data), nrow(classification_data)*0.6)
cl_train <- classification_data[cl_train_split,]
cl_test <- classification_data[-cl_train_split,]

dim(cl_train)
dim(cl_test)

# build the tree model for training data
cl_tree2 <- tree(Fire~., data = cl_train)

# plot the tree model
plot(cl_tree2, col = colors()[99])
text(cl_tree2, pretty = 0)

# print the tree details
summary(cl_tree2)
print(cl_tree2)

# predict using the classification tree
cl_predict2 <- predict(cl_tree2, cl_test, type = "class")
# get the misclassification table
table(cl_predict2, classification_data$Fire[-cl_train_split])
# calculate the misclassification error rate
misclass.tree(cl_tree2)/dim(cl_test)[1]


# run k-fold cross validation with k=10 
# use prune.misclass to use misclassification error rate for cross validation instead of deviation
set.seed(2)
cl_tree_cv <- cv.tree(cl_tree2, FUN = prune.misclass)
cl_tree_cv

# plot the cross validation results
# cross validation error of the model against the size of the tree
plot(cl_tree_cv$size, cl_tree_cv$dev, type = "b", col = colors()[99], 
     xlab = "Size", ylab = "Cross Validation Error Rate")

# prune tree based on the cv results
cl_pruned_tree = prune.misclass(cl_tree2, best = 4) 

# plot the pruned tree
plot(cl_pruned_tree, col = colors()[99]) 
text(cl_pruned_tree, pretty = 0)

# predict using the pruned tree
cl_predict3 <- predict(cl_pruned_tree, cl_test, type = "class")
# get the misclassification table
table(cl_predict3, classification_data$Fire[-cl_train_split])
# calculate the misclassification error rate
misclass.tree(cl_pruned_tree)/dim(cl_test)[1]

#=====================
# multiple linear regression

# filter only the numerical variables for regression
numerical_data <- transformed_data[5:13]

# normalise dataset
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
fire_norm <- as.data.frame(lapply(numerical_data, normalise))
head(fire_norm)

# linear model using all the numerical variables
lin_model1 <- lm(Area ~ ., data = fire_norm)
summary(lin_model1)
par(mfrow=c(2,2))
plot(lin_model1)

# linear model using variables with positive correlation
lin_model2 <- lm(Area ~ FFMC+DMC+DC+Temperature+Wind+Rain, data = numerical_data)
summary(lin_model2)
par(mfrow=c(2,2))
plot(lin_model2)

# perform step-wise feature selection
step(lm(Area~., data = numerical_data), direction = 'both')

# linear model using variables selected from step-wise selection
lin_model3 <- lm(Area ~ DMC+RH+Wind, data = numerical_data)
summary(lin_model3)
par(mfrow=c(2,2))
plot(lin_model3)

lin_model4 <- lm(Area ~ ., data = numerical_data[-500,])
summary(lin_model4)
par(mfrow=c(2,2))
plot(lin_model4)


head(transformed_data)
hh = hclust(dist(transformed_data))
h_clusters = cutree(hh, k=2)
plot(hh, main = "Cluster Dendrogram") 
rect.hclust(hh, k=4)

par(mfrow=c(4,4))
par(mar=c(1,1,1,1))
i <- 1
for (col in transformed_data) {
  plot(col, col = h_clusters+1, type = "p", pch = 16, ylab = colnames(transformed_data)[i])
  i <- i + 1
}


month_to_season <- c(4, 4, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4)
fire_data$month_num <- match(fire_data$month, tolower(month.abb))
fireseason_data <- transformed_data
fireseason_data$Season <- month_to_season[fireseason_data$Month]
plot(fireseason_data$Area, 1:dim(fireseason_data)[1], col = fireseason_data$Season+1, type = "p", pch = 16, ylab = "")
legend( "bottomright", c("Spring", "Summer", "Autumn", "Winter"), 
        text.col=c(2,3,4,5) )





