################ Artificial Neural Networks ####################
##### Data Preprocessing########
#Checking the missing values
sum(is.na(stats))
stats$thalach = ifelse(is.na(stats$thalach),
                   ave(stats$thalach, FUN = function(x) mean(x, na.rm = TRUE)),
                   stats$thalach)

stats$Salary = ifelse(is.na(stats$Salary),
                      ave(stats$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                      stats$Salary)
#change column name
names(stats)[names(stats) == "ï..age"] <- "age"
match("target",names(stats))
# Importing the dataset
stats = read.csv('heart.csv')
View(stats)



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(stats$target, SplitRatio = 0.8)
training_set = subset(stats, split == TRUE)
test_set = subset(stats, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Fitting ANN to the Training set
# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'target',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(5,5),
                         epochs = 100,
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-14]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)
# Making the Confusion Matrix
cm = table(test_set[, 14], y_pred)

cm

#Accuracy
Accuracy=(cm[1]+cm[4])/(cm[1]+cm[4]+cm[3]+cm[2])
Accuracy
# Recall
Recall=(cm[4])/(cm[4]+cm[2])
Recall
# Precision
Precision=(cm[4])/(cm[3]+cm[4])
Precision
#F1 Score
F1Score= 2*Precision * Recall / (Precision + Recall)
F1Score

h2o.shutdown()
