set.seed(1101)
library(class)
library(ROCR)
library(e1071)
library("rpart") 
library("rpart.plot")

setwd("/Users/Spare/Desktop/DSA1101")
df= read.csv("diabetes_prediction_dataset.csv")
head(df)
dim(df)
attach(df)
str(df)

################ Exploratory Data Analysis ####################
# Summarising and describing the variables 
prop.table(table(df$diabetes))
summary(df$diabetes)

table(df$gender)
summary(df$gender)

table(df$smoking_history)
summary(df$smoking_history)

summary(df$bmi)
hist(df$bmi, main = paste("Histogram of BMI"),
     xlab = "BMI", ylab="Count", 
     col = "dark green")


summary(df$blood_glucose_level)
hist(df$blood_glucose_level, main = paste("Histogram of Blood Glucose Level"),
     xlab = "Blood Glucose Level", ylab="Count", 
     col = "light green")


summary(df$age)
hist(df$age, main = paste("Histogram of Age"),
     xlab = "Age", ylab="Count", 
     col = "green")

summary(df$age)
hist(df$HbA1c_level, main = paste("Histogram of HbA1c_level"),
     xlab = "HbA1c_level", ylab="Count", 
     col = "green")

odds_ratio = function(level, feature){
  new_col = ifelse(feature==level, 1,0)
  table = table(new_col, {response_variable})
  odds1 = table[2,2]/ table[2,1]
  odds0 = table[1,2]/ table[1,1]
  return (odds1/odds0)
}
# Defining recategorize_smoking function
recategorize_smoking <- function(smoking_status) {
  ifelse(smoking_status %in% c('never', 'not current'), 'Non smoker',
         ifelse(smoking_status == 'current', 'Current smoker',
                ifelse(smoking_status %in% c('ever', 'former'), 'Past smoker',
                       'No Info')))
}

# Recategorizing smoking_history column to reduce dimensionality
df$smoking_history = recategorize_smoking(smoking_history)
attach(df)

# Get ORs for gender
female_odds = odds_ratio('Female', gender); female_odds
male_odds = odds_ratio('Male', gender); male_odds

# Get ORs for smoking_history
current_odds = odds_ratio('Current smoker', smoking_history); current_odds
past_odds = odds_ratio('Past smoker', smoking_history); past_odds
non_odds = odds_ratio('Non smoker', smoking_history); non_odds
noinfo_odds = odds_ratio('No Info', smoking_history); noinfo_odds

# Get OR for hypertension
hyptens_odds = odds_ratio(1, hypertension); hyptens_odds

# Get OR for heart_disease
heartdis_odds = odds_ratio(1, heart_disease); heartdis_odds


# Use boxplots for continuous variables and response variable
par(mfrow = c(1, 4))
boxplot(age ~ diabetes, data = df, xlab = "Diabetes", col = "yellow")

boxplot(bmi ~ diabetes, data = df, xlab = "Diabetes", col = "yellow")

boxplot(HbA1c_level ~ diabetes, data = df, xlab = "Diabetes", col = "yellow")

boxplot(blood_glucose_level ~ diabetes, data = df, xlab = "Diabetes", col = "yellow")

# Reset the plotting layout to default
par(mfrow = c(1, 1))



## Use bar plots for categorical and response variable
# Set up the plotting area to have 1 row and 4 columns
par(mfrow = c(1, 4))
#plot 1: Gender vs Diabetes
barplot(table(gender, diabetes), beside = TRUE, col = c("blue", "red", "yellow"), main = "Gender vs Diabetes")
legend("right", legend = c("Female", "Male", "Other"), fill = c("blue", "red", "yellow"), cex = 1.2, bty = "n")

# Plot 2: Smoking History vs Diabetes - Adjusted for three levels
barplot(table(smoking_history, diabetes), beside = TRUE, col = c("blue", "green", "red"), main = "Smoking History vs Diabetes")
legend("right", legend = c("Current Smoker", "Past Smoker", "Non-Smoker"), fill = c("blue", "green", "red"), cex = 1.2, bty = "n")

# Plot 3: Hypertension vs Diabetes
barplot(table(hypertension, diabetes), beside = TRUE, col = c("blue", "red"), main = "Hypertension vs Diabetes")
legend("right", legend = c("No Hypertension", "Hypertension"), fill = c("blue", "red"), cex = 1.2, bty = "n")

# Plot 4: Heart Disease vs Diabetes
barplot(table(heart_disease, diabetes), beside = TRUE, col = c("blue", "red"), main = "Heart Disease vs Diabetes")
legend("right", legend = c("No Heart Disease", "Heart Disease"), fill = c("blue", "red"), cex = 1.2, bty = "n")

# Reset the plotting layout to default
par(mfrow = c(1, 1))


#Feature scaling and manipulation
# Turn gender into categorical
gender_convert <- function(gender) {
  ifelse(gender == 'Female', 0,
         ifelse(gender == 'Male', 1,
                ifelse(gender == 'Other', 2, NA)))
}
df$gender <- gender_convert(df$gender)

# Turn smoking_history into categorical
numerical_smoking <- function(smoking_status) {
  ifelse(smoking_status == 'Current smoker', 0,
         ifelse(smoking_status == 'No Info', 1,
                ifelse(smoking_status == 'Non smoker', 2,
                       ifelse(smoking_status == 'Past smoker', 3, NA))))
}
df$smoking_history <- numerical_smoking(df$smoking_history)
df$smoking_history = as.factor(df$smoking_history)

############# ML MODELS ################

########## Logistic Regression #########
#Defining accuracy metric, taking in the confusion matrix:
accuracy <- function(matrix){
  return (sum(diag(matrix))/sum(matrix));
}

# Define logistic regression model
lr_model = glm(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
               data=df,
               family=binomial(link="logit"))
summary(lr_model)

# 5 Fold Cross Validation
n <- nrow(df)
n_folds <- 5

# Assign folds in a stratified manner
df$stratified_fold <- rep(NA, n)
for (i in unique(df$diabetes)) {
  indices <- which(df$diabetes == i)
  df$stratified_fold[indices] <- sample(rep(1:n_folds, length.out = length(indices)))
}

# Initialize vectors to store results
lr_auc_folds = numeric(n_folds)
lr_accuracy_folds = numeric(n_folds)

# Plot blank graph
plot(NA, xlim=c(0,1), ylim=c(0,1), 
     xlab="False Positive Rate", ylab="True Positive Rate", 
     main="LR ROC Curve")

#Carry out 5 fold CV 
for (j in 1:n_folds) {
  train_indices = which(df$stratified_fold != j)
  test_indices = which(df$stratified_fold == j)
  
  # Define lr model
  lr_model = glm(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                 data=df[train_indices, ],
                 family=binomial(link="logit"))
  
  # Make predictions
  test_data = df[test_indices, ]
  pred = predict(lr_model, newdata=test_data)
  pred = ifelse(pred>=0, 1, 0)
  confusion_matrix=table(pred, test_data$diabetes)
  lr_accuracy_folds[j] = accuracy(confusion_matrix)
  
  pred_probs = predict(lr_model, newdata=test_data, type='response') 
  
  # Creating a prediction object for ROCR
  pred = prediction(pred_probs, df$diabetes[test_indices])
  perf = performance(pred , "tpr", "fpr")
  
  #Plot ROC curve into existing graph
  plot(perf, add = TRUE, col = 'orange')
  lr_auc_folds[j] = performance(pred , measure ="auc")@y.values[[1]]
}
# Get mean accuracy of LR model
lr_accuracy = mean(lr_accuracy_folds); lr_accuracy
# Mean Logistic Regression accuracy is 0.96026

# Get AUC value of LR model
lr_auc = mean(lr_auc_folds); lr_auc
# Mean Logistic Regression AUC is 0.9618007


######## Naive Bayes #########
nb_auc_folds = numeric(n_folds)
nb_accuracy_folds = numeric(n_folds)

#Plot new blank graph
plot(NA, xlim=c(0,1), ylim=c(0,1), 
     xlab="False Positive Rate", ylab="True Positive Rate", 
     main="Naive Bayes ROC Curve")

for (j in 1:n_folds) {
  train_indices <- which(df$stratified_fold != j)
  test_indices <- which(df$stratified_fold == j)
  nb_model <- naiveBayes(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                         data=df[train_indices, ])
  
  test_data <- df[test_indices, ]
  
  # Predict using nb_model
  pred_probs = predict(nb_model, newdata=test_data, type='raw')
  pred_positive_probs = pred_probs[,2]
  
  # Convert probabilities to binary outcomes for confusion matrix
  pred_class = ifelse(pred_positive_probs > 0.5, 1, 0)
  
  # Create confusion matrix using actual outcomes and predicted classes
  confusion_matrix = table(Predicted=pred_class, Actual=df$diabetes[test_indices])
  
  # Calculate accuracy
  nb_accuracy_folds[j] = accuracy(confusion_matrix)
  
  # AUC calculation
  pred_obj = prediction(pred_positive_probs, df$diabetes[test_indices])
  perf = performance(pred_obj, "tpr", "fpr")
  nb_auc_folds[j] = performance(pred_obj, measure ="auc")@y.values[[1]]
  
  #Get plots for ROC Curve
  plot(perf, col="red", main="Naive Bayes ROC Curve")
  abline(a=0, b=1, lty=2)
  par(new=TRUE)
}
# Get mean accuracy of NB model
nb_accuracy = mean(nb_accuracy_folds); nb_accuracy
#Best accuracy of NB model is 0.90323

# Get AUC value of NB model
nb_auc = mean(nb_auc_folds); nb_auc
#Best AUC for NB Model is 0.9221774



######## Decision Tree (with Hyperparameter Tuning) #########
#Create vector of hyperparameters (cp)
k = c(-7:-1)
cp = 10^k

#Initialize vectors to store results
dt_aucs = numeric(length(cp))
dt_accuracies = numeric(length(cp))

#Carrying out hyperparameter tuning
for (i in 1:length(cp)) {
  dt_auc_folds=numeric(n_folds)
  dt_accuracy_folds = numeric(n_folds)
  
  for (j in 1:n_folds) {
    train_indices <- which(df$stratified_fold != j)
    test_indices <- which(df$stratified_fold == j)
    
 
    tree <- rpart(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                  method="class",
                  data=df[train_indices, ],
                  control=rpart.control(cp=cp[i]), # or max_depth
                  parms=list(split='information'))
    
    #Make predictions
    test_data <- df[test_indices, ]
    pred = predict(tree, newdata=test_data, type='class')
    confusion_matrix=table(pred, test_data$diabetes)
    dt_accuracy_folds[j] = accuracy(confusion_matrix)
    
    pred_probs = predict(tree, newdata=test_data, type='prob')
    
    pred_probs <- pred_probs[,2] 
    #Creating prediction object for ROCR
    pred_rocr = prediction(predictions=pred_probs, labels=df$diabetes[test_indices])
    perf = performance(pred_rocr , "tpr", "fpr")
    dt_auc_folds[j] = as.numeric(performance(pred_rocr, "auc")@y.values[[1]])
  }
  #Finding mean accuracy for folds of each cp
  mean_accuracy = mean(dt_accuracy_folds)
  dt_accuracies[i] = mean_accuracy
  mean_auc = mean(dt_auc_folds)
  # Save the best AUC from each fold
  dt_aucs[i] = mean_auc
}

max_accuracy <- max(dt_accuracies); max_accuracy
max_auc <- max(dt_aucs); max_auc

#Best Decision Tree accuracy is 0.97187
#Best Decision Tree AUC is 0.967135

best_cp_acc_index <- which.max(dt_accuracies)
best_cp_auc_index <- which.max(dt_aucs)

best_cp_acc <- cp[best_cp_acc_index]; best_cp_acc
#Best cp for Accuracy is 1e-03

best_cp_auc <- cp[best_cp_auc_index]; best_cp_auc
#Best cp for AUC is 1e-04

#Plot new blank graph
plot(NA, xlim=c(0,1), ylim=c(0,1), 
     xlab="False Positive Rate", ylab="True Positive Rate", 
     main="DT ROC Curve")

#Add ROC curves for each fold with the best cp 
for (j in 1:n_folds) {
  train_indices <- which(df$stratified_fold != j)
  test_indices <- which(df$stratified_fold == j)
  tree <- rpart(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                method="class",
                data=df[train_indices, ],
                control=rpart.control(cp = best_cp_auc), 
                parms=list(split='information'))
  
  test_data <- df[test_indices, ]
  
  pred_probs = predict(tree, newdata=test_data, type='prob')
  
  pred_probs <- pred_probs[,2] 
  pred_rocr = prediction(predictions=pred_probs, labels=df$diabetes[test_indices])
  perf = performance(pred_rocr , "tpr", "fpr")
  plot(perf, add = TRUE, col = 'dark green')
}

#Plotting Average AUCs against range of log_cp
log_cp = log10(cp) # Calculating the logarithm (base 10) of cp values
plot(log_cp, dt_aucs, type='b', col="blue", pch=19, xlab="log_cp", ylab="Average AUC", main="Average AUC vs Log_cp")


######## K Nearest Neighbours ##########
# Scale only the numeric_features <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")
df_scaled = df[,-ncol(df)]
numeric_features <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")

df_scaled[numeric_features] <- scale(df_scaled[numeric_features])

# Prepare data
X = df_scaled[,-ncol(df_scaled)]
Y = df[,ncol(df)-1]
Y = as.factor(Y)

df_scaled$stratified_fold <- rep(NA, nrow(df))

# Create stratified fold assignments
for(class in levels(Y)) {
  indices <- which(Y == class)
  df_scaled$stratified_fold[indices] <- sample(rep(1:5, length.out = length(indices)))
}


n_folds <- 5

#choosing number of nearest neighbours (K)
num_ks <- 10
k = c(1:num_ks)

#Initialize vectors to store results 
knn_aucs <- numeric(num_ks)
knn_accuracies = numeric(num_ks)
#Carrying out hyperparameter tuning
for (i in k) {
  knn_auc_folds <- numeric(n_folds)
  knn_accuracy_folds = numeric(n_folds)

    for (j in 1:n_folds) {
    train_indices <- which(df_scaled$stratified_fold != j)
    test_indices <- which(df_scaled$stratified_fold == j)  # Remaining data for training
    #Creating knn model
    pred <- knn(train = X[train_indices, ], test = X[test_indices, ], cl = Y[train_indices], k = i)
    #making predictions
    test_data = Y[test_indices]
    confusion_matrix=table(pred, test_data)
    knn_accuracy_folds[j] = accuracy(confusion_matrix)  
    
    pred_numeric <- as.numeric(pred) -1 
    true_numeric <- as.numeric(df_scaled$diabetes[test_indices])
    #Creating prediction object for ROCR
    pred <- prediction(pred_numeric, true_numeric)
    perf = performance(pred, "tpr", "fpr")
    knn_auc_folds[j] <- performance(pred, "auc")@y.values[[1]]
  }
  knn_accuracies[i] = mean(knn_accuracy_folds)
  # Calculate the mean AUC across all folds for the current value of k
  knn_aucs[i] <- mean(knn_auc_folds)
}

max_accuracy <- max(knn_accuracies); max_accuracy
#Best mean kNN accuracy is 0.96238

max_auc <- max(knn_aucs); max_auc
#Best mean kNN AUC is 0.827522

best_k_acc_index <- which.max(knn_accuracies)
best_k_auc_index <- which.max(knn_aucs)

best_k_acc <- k[best_k_acc_index]; best_k_acc
#Best k for accuracy is 9

best_k_auc <- k[best_k_auc_index]; best_k_auc
#Best k for AUC is 1

#Plot new blank graph
plot(NA, xlim=c(0,1), ylim=c(0,1), 
     xlab="False Positive Rate", ylab="True Positive Rate", 
     main="kNN ROC Curve")

#Add ROC curves for each fold with the best k
for (j in 1:n_folds) {
  train_indices <- which(df_scaled$stratified_fold != j)
  test_indices <- which(df_scaled$stratified_fold == j)  # Remaining data for training
  
  pred <- knn(train = X[train_indices, ], test = X[test_indices, ], cl = Y[train_indices], k = best_k_auc)
  
  pred_numeric <- as.numeric(pred) -1 
  true_numeric <- as.numeric(df_scaled$diabetes[test_indices])
  
  pred <- prediction(pred_numeric, true_numeric)
  perf = performance(pred, "tpr", "fpr")
  plot(perf, add = TRUE, col = 'blue')
}

#Plot average AUC values against k 
k_values = seq(1:num_ks)
plot(k_values, knn_aucs, type='b', col="red", pch=19, xlab="K", ylab="Average AUC", main="Average AUC vs k neighbours")

#Plot average accuracy values against k 
plot(k_values, knn_accuracies, type='b', col="red", pch=19, xlab="K", ylab="Average accuracy", main="Average Accuracy vs k neighbours")



##### Compare ROC plots on same split (Logistic Regression, Decision Tree, Naive Bayes) #####
train_indices <- which(df$stratified_fold != 1)
test_indices <- which(df$stratified_fold == 1)
test_data <- df[test_indices, ]


### Logistic Regression
lr_model <- glm(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                data=df[train_indices, ],
                family=binomial(link="logit"))
lr_predictions <- predict(lr_model, newdata=test_data, type='response')
lr_pred <- prediction(lr_predictions, test_data$diabetes)
lr_perf <- performance(lr_pred, "tpr", "fpr")

### Naive Bayes
nb_model <- naiveBayes(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                       data=df[train_indices, ])
nb_predictions <- predict(nb_model, newdata=test_data, type='raw')[,2]
nb_pred <- prediction(nb_predictions, test_data$diabetes)
nb_perf <- performance(nb_pred, "tpr", "fpr")

### Decision Tree
dt_model <- rpart(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level,
                  method="class",
                  data=df[train_indices, ],
                  control=rpart.control(cp=best_cp_auc))# based on my best_cp
dt_predictions <- predict(dt_model, newdata=test_data, type='prob')[,2]
dt_pred <- prediction(dt_predictions, test_data$diabetes)
dt_perf <- performance(dt_pred, "tpr", "fpr")

### KNN
knn_model <- knn(train = X[train_indices, ], test = X[test_indices, ], cl = Y[train_indices], k = best_k_auc)

pred_numeric <- as.numeric(knn_model) -1 
true_numeric <- as.numeric(df_scaled$diabetes[test_indices])
knn_pred <- prediction(pred_numeric, true_numeric)
knn_perf <- performance(knn_pred, "tpr", "fpr")


#Plot ROC Comparison Curves
plot(lr_perf, col="red", main="ROC Curves Comparison")
abline(a=0, b=1, lty=2)
par(new=TRUE)
plot(nb_perf, col="blue", add=TRUE)
par(new=TRUE)
plot(dt_perf, col="green", add=TRUE)
par(new=TRUE)
plot(knn_perf, col="orange", add=TRUE)
legend("bottomright", legend=c("Logistic Regression", "Naive Bayes", "Decision Tree", "KNN"),
       col=c("red", "blue", "darkgreen", "orange"), lwd=2)








