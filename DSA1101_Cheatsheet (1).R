
## ## ## ## ## ## ##
##### Common Funcs #####
set.seed()
setwd("/Users/Spare/Desktop/DSA1101")
df= read.csv("Titanic.csv")
head(df)
dim(df)


#Read text file
df1 = read.table("data1.txt", header=TRUE, sep="\t")
head(df1)

#Get names of col in dataset
names(df)

#proportion table
prop.table(table({variable))


#Change indexes of data in dataset

#Change names of columns in dataset
names(data) = c('Status', 'X1', 'X2', 'X3', 'X4')

# as.factor()
NW = as.factor(NW) # Impt if more than 2 categories in column

#check levels of column
levels(spine)

# Create matrix
rep(); numeric(); cbind()

######### Indexing into a matrix#########
# Extract a single row
data[row_number, ]  
# Extract multiple rows
data[c(row1, row2, ...), ]  

# Extract a single column by number
data[, col_number]  
# Extract a single column by name
data[, "col_name"]  
# Extract a single column by name
data$columnName 
# Extract multiple columns by numbers or names
data[, c(col1, col2, ...)]  

# Extract a single cell
data[row_number, col_number]  
# Extract a submatrix
data[c(row1, row2, ...), c(col1, col2, ...)]  

#Get indexes of something
index = which(FEV %in% c(out))

# which.min or which.max
best_cp = cp[which.min(misclass_cp)]

# ifelse
order.size = ifelse(order<=5, "small", "large")


#Create Dataframe:
df = data.frame (X1= c("value_1", "value_2", "value_3"),
                 X2 = c("value_A", "value_B", "value_C"),
                 X3 = c(),
                 Y = c()
)


# Dataframe slicing (By index with filter)

# Row filter on condition
churn1 = churn[Married == 1,]
# Col filter by name
churn1 = churn[,c('Married', 'Cust_years')]

# Convert response classes to numeric or bool before using Prediction func (ROCR) 
# (If column not 0-1 or True-False, use this line to convert)
actual_class <- Survived == 'Yes' # Convert to True-False

# Convert class response to numeric factor, appending it to dataframe
sur = ifelse(Survived == 'Yes', 1, 0) 
sur = as.factor(sur)
titanic$sur = sur ; head(titanic)

# Logarithms in R
e(1) # for e^1
e(2) # for e^2
log()
eulers = 2.71828

## ## ## ## ## ## ##
##### Data Viz #####
## Parallel Histograms
opar = par(mfrow=c(1,2)) # Arrange figures in a frame of 1 row, 2 columns

# Return back to one figure per frame
par(opar)

# HISTOGRAM 
#remove ylim if need
hist(total, freq=FALSE, main = paste("Histogram of {Name}"),
     xlab = "total sales", ylab="Probability", 
     col = "grey", ylim = c(0, 0.002))


# Normal Density Curve
#Re-define x axis if needed
x <- seq(0, max(total), length.out=n)
y <- dnorm(x, mean(total), sd(total))
lines(x, y, col = "red") 

#OR

x= seq(50,90, length.out = length(X1))
y = dnorm(x, mean(X1), sd(X1))
lines(x,y, col = "red")


#possible comments:
#histogram shows that the sample is unimodel/ bimodal
#the range of histogram is from 0 to 15
#the histogram is right skewed. No gap in the data.

#Get median
median(female)

#Get Interquartile Range
IQR(female)
#Compare median and IQRs of 2 variables

# BOX PLOTS
boxplot(total, xlab = "Total Sales", col = "blue")
outliers = boxplot(total)$out 
length(outliers) # Count outliers
outlier_index = which(total %in% outliers) 
sales[c(outlier_index),] # Show outlier rows

# QQ plot
qqnorm(total, main = "QQ Plot", pch = 20)
qqline(total, col = "red")

#possible comments:
#From the qq plot, on the left tail, the sample quantiles are larger than expected (theoretical quantile) 
#hence the left tail is shorter than normal.

#On the right side, the sample quantiles are larger than expected,
#hence the right tail is longer than normal.

#The left tail and right tail are quite normal.

# The sample of life expectancy of 89 countries is quite normally distributed.
# some might say right tail is slightly longer than normal = acceptable.

# IN GENERAL THE DISTRIBUTION IS NORMAL/ NOT NORMAL

# CORRELATION COEFFICIENT
order = sales$num_of_orders
cor(total, order) #0.75

# SCATTER PLOT
plot(order,total, pch = 20, col = "darkblue")
# Comments: The variability of the total is NOT STABLE when order changes, the plot has a {kind of shape}.
# BOX PLOTS OF MULTIPLE GROUP
boxplot(total ~ sales$gender, col = "blue")

# 3 variables scatter plot - With legend
order = sales$num_of_orders
attach(sales)
plot(order,total, type = "n") # a scatter plot with no point added
# This is to color the points (like using hue in seaborn)
points(order[gender=="M"],total[gender=="M"],pch = 2, col = "blue") # MALE
points(order[gender=="F"],total[gender=="F"],pch = 20, col = "red") # FEMALE
legend(1,7500,legend=c("Female", "Male"),col=c("red", "blue"), pch=c(20,2))
# (x = 1, y =7500) tells R the place where you want to put the legend box in the plot
# do note on the size of the points since the points added latter will overlay on the points added earlier
# hence, the points added latter should be chosen with smaller size so that they will not cover the points earlier

plot(width, weight)
points(width[spine == 1], weight[spine == 1], pch=6, col = "black")
points(width[spine == 2], weight[spine == 2], pch=10, col = "blue")
points(width[spine == 3], weight[spine == 3], pch=20, col = "red")
legend(30, 2, legend = c("Spine = 1", "Spine = 2", "Spine = 3"),
       col = c("black", "blue", "red"), pch = c(6, 10, 20))


#Creating new column with conditions from other columns
n = length({column})
data$col = numeric(n)

data$col[which(color <=3)] = "light"
data$col[which(color >3)] = "dark"

# Table gives frequency for each value
count = table(gender)
count # frequency table

# BARPLOT FOR CATEGORICAL VARIABLE (Input freq table)
count = table(gender)
barplot(count)

# PIE CHART (Input freq table)
count = table(gender)
pie(count)

# CONTINGENCY TABLE
table = table(gender,order.size)

tab = prop.table(table, "gender")# proportion by gender

table(Churned)
prop.table(table(Churned))

#> table(Churned)
#Churned
#0    1 
#6257 1743 
#> prop.table(table(Churned))
#Churned
#0        1 
#0.782125 0.217875 

## ## ## ## ## ## ##
##### Train-Test Split #####
# (with standardisaton)
#specify df2 properly
standardized.X = scale(df2) 

test = sample(1:length(Purchase), 1000); test # sample a random set of 1000 indexes, from 1:n.
train.X = standardized.X[-test,] #training set
test.X = standardized.X[test,] # test set
train.Y = caravan$Purchase[-test] # response for training set
test.Y = caravan$Purchase[test]


# (with even split)
iris = read.csv("iris.csv", header=TRUE)
t1 = iris[which(species =="Iris-virginica"),]
rownames(t1) <- NULL                             ## Resets index to start from 1
t2 = iris[which(species =="Iris-versicolor"),]
rownames(t2) <- NULL
t3 = iris[which(species =="Iris-setosa"),]
rownames(t3) <- NULL

# Data to feed model for training and test
n_folds=5
folds_j <- sample(rep(1:n_folds, length.out = dim(t1)[1]))

data=data.frame(rbind(t1[-test_j,], t2[-test_j,], t3[-test_j,]))
to_predict = data.frame(rbind(t1[test_j,], t2[test_j,], t3[test_j,]))

## ## ## ## ## ## ##
##### Linear Regression #####

# Manual Linear Regression function
simple <- function(x , y) {
  beta_1 <- (sum(x*y)- mean (y)* sum (x ))/( sum(x^2)- mean(x)* sum(x))
  beta_0 <- mean(y)- beta_1* mean(x) 
  return(c( beta_0 , beta_1)) 
  }


M1 = lm(y ~ x+NW, data)
summary(M1)

#How to write fitted model:
# y-hat = Intercept - 1.2*I(sex=Infant) - 0.18*I(Sex=Male) - 0.052 * length + .......

#A variable is significanat if their p value is small

#QN: Is model significant?
#The fitted model, M, has F-test for the overall significance of the model with extremely small p-value. 
#Hence, model M is significant.


# goodness-of-fit: the significance test has p-value 4.324e-06 or 0.000004324, very small, 
# meaning model M1 is significant. However, R^2 = 0.3, very low. 
# Model M1 is not very good fit.


## Comments on suitability to fit a linear model:
# x is Symmetric/ NOT Symmetric (Plot the histogram to find out if symmetric or not)
# The variability of x is NOT STABLE/ STABLE when {other factor} increases


## Interpreting Coefficients:
Given that this is the model summary:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.0438     0.1679  12.171  < 2e-16 ***
#  Class2nd     -1.0181     0.1960  -5.194 2.05e-07 ***
#  Class3rd     -1.7778     0.1716 -10.362  < 2e-16 ***
#  ClassCrew    -0.8577     0.1573  -5.451 5.00e-08 ***
#  SexMale      -2.4201     0.1404 -17.236  < 2e-16 ***
#  AgeChild      1.0615     0.2440   4.350 1.36e-05 ***

#Coefficient of Sex: FEMALE IS REFERENCE. MALE IS INDICATED BY INDICATOR.
# coefficient is estimated = -2.4201. 
# It means, given the same condition on the class and age,
# when comparing to a female, the LOG-ODDS of survival for a male is less than by 2.42.
# It means, the ODDS of survival of a male passenger will be less than that of a female by
# e^2.42 = 11.25 TIMES.


#Coefficient of Age "ADULT" IS CHOSEN AS REFERENCE. CATEGORY "CHILD" IS INDICATED BY AN INDICATOR.
# coefficient is estimated = 1.0615.
# It means, given the same condition on the class and gender,
# when comparing to an adult, the LOG-ODDS of survival of a child is larger by 1.0615.
# That means, the ODDS of survival of a child passenger is larger than that of an adult passenger by 
# e^1.0615 = 2.89 TIMES.


#Check number of fitted values n less than 0:
length(which(M1$fitted<0) 

to_predict = data.frame(x= 4000, NW = "1")  # Put as string since declared as factor
predict(model_2, to_predict)

#Comments about R Squared:
#It shows a quite strong positive association and 
#quite linear between price and size of a house. 
#This agrees with the correlation value of 0.76.

#More Comments about R Squared:
#The fitted Model 2 has R2 = 0.6352.
#It means, Model 2 can explain 63.52% variance in the observed response.

## ## ## ## ## ## ##
##### KNN, Confusion Matrix & N-Fold #####
# Requires Standardization
standardized.X = scale(df[,1:5]) 


#calculate euclidean distance function
CalculateEuclideanDistance = function(vect1, vect2){
  return (sqrt(sum((vect1 - vect2)^2)))
}
   

######## K Nearest Neighbours ##########
# Scale only the numeric_features <- c("age", "bmi", "HbA1c_level", "blood_glucose_level")
df_scaled = df[,-ncol(df)]
numeric_features <- c("")

df_scaled[numeric_features] <- scale(df_scaled[numeric_features])

# Prepare data
X = df_scaled[,-ncol(df_scaled)]
Y = df[,ncol(df)-1]
Y = as.factor(Y)

n_folds <- 5
#Get indices for K fold


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
    true_numeric <- as.numeric(df_scaled$response[test_indices])
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
#Best mean kNN accuracy is 

max_auc <- max(knn_aucs); max_auc
#Best mean kNN AUC is 

best_k_acc_index <- which.max(knn_accuracies)
best_k_auc_index <- which.max(knn_aucs)

best_k_acc <- k[best_k_acc_index]; best_k_acc
#Best k for accuracy is 

best_k_auc <- k[best_k_auc_index]; best_k_auc
#Best k for AUC is 

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
  true_numeric <- as.numeric(df_scaled$response[test_indices])
  
  pred <- prediction(pred_numeric, true_numeric)
  perf = performance(pred, "tpr", "fpr")
  plot(perf, add = TRUE, col = 'blue')
}

#Plot average AUC values against k 
k_values = seq(1:num_ks)
plot(k_values, knn_aucs, type='b', col="red", pch=19, xlab="K", ylab="Average AUC", main="Average AUC vs k neighbours")

#Plot average accuracy values against k 
plot(k_values, knn_accuracies, type='b', col="red", pch=19, xlab="K", ylab="Average accuracy", main="Average Accuracy vs k neighbours")

# init zero vectors to store values for whole run
ave.type1 = numeric(50)
ave.type2 = numeric(50)
for (i in 1:50) {
  # init zero vectors to store values for curr k value
  type.1=numeric(n_folds)
  type.2=numeric(n_folds)
  for (j in 1:n_folds) {
    #indexes for the test data after k fold X validation
    test_j = which(folds_j == j) 
    pred = knn(train=X[-test_j,], test=X[test_j,], cl=Y[-test_j], k=i)
    
    confusion_matrix=table(pred, Y[test_j])

  }
  # update metric value for current k value
  ave.type1[i]=mean(type.1)
  ave.type2[i]=mean(type.2)
}


# Accuracy of a confusion matrix
accuracy <- function(matrix){
  return (sum(diag(matrix))/sum(matrix));
}


# Precision of a confusion matrix
precision <- function(matrix){
  return (matrix[2,2]/sum(matrix[2,]));
}


# TPR of a confusion matrix
tpr <- function(matrix){
  return (matrix[2,2]/sum(matrix[,2]));
}

# FNR of a confusion matrix - Type 2 error rate
fnr <- function(matrix){
  return (matrix[1,2]/sum(matrix[,2]));
}

# FPR of a confusion matrix - Type 1 error rate
fpr <- function(matrix){
  return (matrix[2,1]/sum(matrix[,1]));
}


##Scale a new point with relevance to data
new = data.frame(X1 = 83, X2 = 57, X3= 2, X4 = 3)
standard = scale(rbind(data[,2:5], new) ) 
# add the new point to the x-values of the whole data, then standardizing all


standard.new = standard[90,] # x values of the new point after standardizing
standard.new



## ## ## ## ## ## ##
##### Decision Trees, tree plot & N-fold (see misclass count version from tut 7) #####
library("rpart") # load libraries
library("rpart.plot")
tree <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class",
             data=play_decision,
             control=rpart.control(minsplit=1), # or max_depth
             parms=list(split='information')) # or gini
rpart.plot(tree , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)

newdata <- data.frame(Outlook="rainy", Temperature="mild",
                      Humidity="high", Wind=FALSE)

predict(tree,newdata=newdata,type="prob")
predict(tree,newdata=newdata,type="class")

#Important features of the dataset are the ones included in the decision nodes

# ROC, AUC
score <- tree_pred[,c("Yes")] # score = tree_pred[,2] to return the second col in "raw" table
pred <- prediction(score , actual_class) 
perf <- performance(pred , "tpr", "fpr")
plot (perf, lwd =2, col="black", add=TRUE)

#Tutorial qn on how to stratify the train and test set such that equal amount of class in each set:
n_folds=5 #

folds_j_1 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 1 = setosa

folds_j_2 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 2 = versicolor

folds_j_3 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 2 = virginica


table(folds_j_1)
table(folds_j_2)
table(folds_j_3)

data1 = iris[1:50,] # data for type 1 = setosa
data2 = iris[51:100,] # data for type 2 = versicolor
data3 = iris[101:150,] # data for type 3 = virginica

acc=numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  test3 <- which(folds_j_3 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  train.3=data3[ -test3, ]
  
  train = rbind(train.1, train.2, train.3) # this is the training data set
  
  test = rbind(data1[test1,], data2[test2,], data3[test3,] ) # test data 
  
  fit.iris <- rpart(class ~ .,
                    method = "class", data =train, control = rpart.control( minsplit =1),
                    parms = list( split ='gini'))
  
  
  pred = predict(fit.iris, newdata = test[,1:4], type = 'class')
  
  confusion.matrix = table(pred, test[,5])
  
  acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
  
}
acc
mean(acc)

# Entropy of a purity
entropy <- function(p){
  if (p==0 | p==1){
    return (0)
  } else {
    return (-(log2(p)*p+log2(1-p)*(1-p)))
  }
}


# Gini index of a purity
gini <- function(p){
  return (2*p*(1-p))
}

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
    
 
    tree <- rpart(response ~ .,
                  method="class",
                  data=df[train_indices, ],
                  control=rpart.control(cp=cp[i]), # or max_depth
                  parms=list(split='information'))
    
    #Make predictions
    test_data <- df[test_indices, ]
    pred = predict(tree, newdata=test_data, type='class')
    confusion_matrix=table(pred, test_data$response)
    dt_accuracy_folds[j] = accuracy(confusion_matrix)
    
    pred_probs = predict(tree, newdata=test_data, type='prob')
    
    pred_probs <- pred_probs[,2] 
    #Creating prediction object for ROCR
    pred_rocr = prediction(predictions=pred_probs, labels=df$response[test_indices])
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

#Best Decision Tree accuracy is 
#Best Decision Tree AUC is 

best_cp_acc_index <- which.max(dt_accuracies)
best_cp_auc_index <- which.max(dt_aucs)

best_cp_acc <- cp[best_cp_acc_index]; best_cp_acc
#Best cp for Accuracy is 

best_cp_auc <- cp[best_cp_auc_index]; best_cp_auc
#Best cp for AUC is 


##### Naive Bayes (see tut 8 for manual calculation) and ROC, AUC #####
# Manual Calculation: Proportion that point 15 will be "Yes" for the outcome is proportional to:
prob_yes <-
  ageCounts["Yes",testdata[,c("Age")]]*
  incomeCounts["Yes",testdata[,c("Income")]]*
  jsCounts["Yes",testdata[,c("JobSatisfaction")]]*
  desireCounts["Yes",testdata[,c("Desire")]]*
  tprior["Yes"]

#Calculating Conditional Probability:
ClassCounts <- table(titanic[,c("Survived", "Class")]); classCounts
#Divide by the row Sum
classCounts <- classCounts/rowSums(classCounts); classCounts

genderCounts <- table(titanic[,c("Survived", "Sex")]); genderCounts
genderCounts <- genderCounts/rowSums(genderCounts) ; genderCounts


library(e1071)
model <- naiveBayes(Survived ~ ., data)

results <- predict(model,testdata,"raw") # or class

# ROC (return raw or prob type from predict before plotting ROC)
library(ROCR)
results <- predict(model, data=data, "raw")
score <- results_2[,c("Yes")] # OR score = results_2[,2] to return the second col in "raw" table. Need to tell pred func which is positive
# score is the conditional prob from Naive Bayes classifier for each test point

# Convert response classes to numeric or bool (If column not 0-1 or True-False, use this line to convert)
actual_class <- Survived == 'Yes' # Convert to True-False

pred <- prediction(score , actual_class) 
# this is to "format" the input so that we can use the function in ROCR to get TPR and FPR
perf <- performance(pred , "tpr", "fpr")

plot (perf, lwd =2, col="red") # lwd is to specify how thick the curve is10
abline (a=0, b=1, col ="blue", lty =3) # Line for comparison

# print AUC in title
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))

# COMPUTE AUC FOR NAIVE BAYES CLASSIFIER:
auc <- performance(pred , "auc")@y.values[[1]]
# OR auc <- unlist(slot (auc , "y.values")) to extract value from list
auc # 0.7164944


for (j in 1:n_folds) {
  train_indices <- which(df$stratified_fold != j)
  test_indices <- which(df$stratified_fold == j)
  nb_model <- naiveBayes(response ~ .,
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
#Best accuracy of NB model is 

# Get AUC value of NB model
nb_auc = mean(nb_auc_folds); nb_auc
#Best AUC for NB Model is 


## ## ## ## ## ## ##
##### Logistic Regression #####

# Convert response var to numbers
Surv = ifelse(Survived == "Yes", 1, 0)
M2 = glm(Surv ~ ., data=data[,1:3], family = binomial)
summary(M2)

# log odds
## Eqn: log(p(survived = Yes)/ 1-p(survived = yes)) = 
##      2.0438 -1.0181 I(Class = 2nd) -1.7778*I(Class = 3nd) 
##      -0.8577*I(Class = Crew) -2.4201*I(Sex = Male) 
##      +1.0615*I(Age = Child)

## If passenger is Male, log odds decreases by 2.4201
## Female is reference, male is indicated by the indicator.
## Given same condition on class and age, when comparing to female, the log-odds of survival
## for male is less than females by 2.42
## e^2.42 = 11.25 times

# predict type = c("link", "response", "terms")
# link returns log-odds, repsonse returns probabilities, terms return log-odds and vals for each feature.

# ROC
glm_pred = predict(M2, type = 'response')
pred <- prediction(glm_pred , Surv) 
perf <- performance(pred , "tpr", "fpr")
plot (perf, lwd =2, col="red")

#Carry out 5 fold CV for Log regression
for (j in 1:n_folds) {
  train_indices = which(df$stratified_fold != j)
  test_indices = which(df$stratified_fold == j)
  
  # Define lr model
  lr_model = glm(response ~ .,
                 data=df[train_indices, ],
                 family=binomial(link="logit"))
  
  # Make predictions
  test_data = df[test_indices, ]
  pred = predict(lr_model, newdata=test_data)
  pred = ifelse(pred>=0, 1, 0)
  confusion_matrix=table(pred, test_data$response)
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
# Mean Logistic Regression accuracy is 

# Get AUC value of LR model
lr_auc = mean(lr_auc_folds); lr_auc
# Mean Logistic Regression AUC is 
## ## ## ## ## ## ##





##### K-means #####
#How to do a K means question
df= read.csv("hdb-2012-to-2014.csv")
df2 = df[,c("resale_price","floor_area_sqm")]

standardized.X = scale(df2) 

#Choosing number of cluster options
K = 10 
wss <- numeric(K)
for (k in 1:K) { 
  wss[k] <- sum(kmeans(standardized.X,centers=k)$withinss)
}

#Plot Inertia Graph
plot(1:K, wss, col = "red", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")

#Assume optimal K is 3
attach(df)
kout = kmeans(standardized.X,centers=3)

#Plot all the points in black
plot(resale_price,floor_area_sqm, pch = 2, col = "black")

#Assigning each point to a cluster
df$clusters = kout$cluster

#Colouring each point in each cluster
points(resale_price[clusters==1],floor_area_sqm[clusters==1],pch = 2, col = "blue") 
points(resale_price[clusters==2],floor_area_sqm[clusters==2],pch = 20, col = "red") 
points(resale_price[clusters==3],floor_area_sqm[clusters==3],pch = 20, col = "green") 

#Add legend
legend("bottomright",legend=c("Cluster 1", "Cluster 2", "Cluster 3"),col=c("blue", "red", "green"), pch=c(2,20,20))

#Getting the points of the Cluster centres
data = data.frame(kout$centers)

#Plotting them on the same graph, using par(new=TRUE)
par(new=TRUE)
plot(data$resale_price,data$floor_area_sqm, add=TRUE, pch=20, col='black')

#Getting the size of each respective cluster
kout$size

# kmeans attributes
kout$cluster # A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
kout$centers # A matrix of cluster centres.
kout$size # The number of points in each cluster.
kout$withinss # Vector of SS_k, one value per cluster
kout$tot.withinss # Total within-cluster sum of squares = WSS
## ## ## ## ## ## ##
##### Association Rules #####

library('arules') # arules has its own dataset type (sparse dataset) row and cols switch
library('arulesViz')


#View first 10 transactions
inspect(head(Groceries, 10))


# view rows 1 to 5 (rows and cols swapped compared to normal dataframes)
apply(Groceries@data[,1:5], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=", "))

# Frequent 1-itemsets, min support to consider frequent = 0.02
itemsets.1 <- apriori(Groceries, parameter=list(minlen=1, maxlen=1,
                                                support=0.02, target="frequent itemsets"))
# list the most 10 frequent 1-itemsets:
inspect(head(sort(itemsets.1, by = "support"), 10))
#      items                                           support    count
#[1]   {whole milk}                                    0.25551601 2513 
#[2]   {other vegetables}                              0.19349263 1903 
#[3]   {rolls/buns}                                    0.18393493 1809 
#[4]   {soda}                                          0.17437722 1715 
# ......

# Return frequent itemsets, minimum size is 1-itemset  (apriori target="frequent itemsets")
itemsets<- apriori( Groceries , parameter = list( minlen=1,
                                                  support =0.02 , target ="frequent itemsets"))
summary(itemsets) # summary here shows most frequent indiv items, count of each itemset type that is considered frequent

#Finding itemsets with 2 items
itemsets.2 <- apriori(Groceries, parameter=list(minlen=2, maxlen=2,
          support=0.02, target="frequent itemsets"))
summary(itemsets.2)


# Return rules (apriori target="rules", include min confidence value)
rules <- apriori(Groceries, parameter=list(support=0.001,
                                           confidence=0.6, target = "rules"))
plot(rules)
# Scatter plot with customized measures and can add limiting the plot to the 100 rules with the 
# largest value for for the shading measure. 
plot(rules, measure = c("support", "confidence"), shading = "lift", col = "black", limit = 100)


# the top 5 rules sorted by LIFT
inspect(head(sort(rules, by="lift"), 5))
highLiftRules <- head(sort(rules, by="lift"), 5)
#    lhs                                                        rhs              support     confidence coverage    lift     count
#[1] {Instant food products, soda}                           => {hamburger meat} 0.001220132 0.6315789  0.001931876 18.99565 12   
#[2] {soda, popcorn}                                         => {salty snack}    0.001220132 0.6315789  0.001931876 16.69779 12   
#[3] {ham, processed cheese}                                 => {white bread}    0.001931876 0.6333333  0.003050330 15.04549 19   
#[4] {tropical fruit, other vegetables, yogurt, white bread} => {butter}         0.001016777 0.6666667  0.001525165 12.03058 10   
#[5] {hamburger meat, yogurt, whipped/sour cream}            => {butter}         0.001016777 0.6250000  0.001626843 11.27867 10   

# ^^ Above rows are called rules 

plot(highLiftRules, method = "graph", engine = "igraph",
     edgeCol = "blue", alpha = 1)
# alpha = c(0,1)
# the size of the node is sorted by the support.
# the darkness of the color represents the change in lift

plot(highLiftRules, method = "graph", engine = "igraph",
     nodeCol = "red", edgeCol = "blue", alpha = 1)
# this will fix the color be "red" for all lift values, 
# only the size of the node is sorted by the support.


