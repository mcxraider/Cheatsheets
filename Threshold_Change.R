#Variations for metric calculation 
#### MAKE SURE THAT PREDICTED IS ON TOP, ACTUAL ON LEFT
#### True Positive Rate (TPR):
Formula: TP/ (TP + FN)
#### False Positive Rate (FPR):
Forumla: FP/ (FP+TN)
#Also known as Type 1 Error rate
#### False Negative Rate (FNR):
Formula: FN/ (TP + FN)
#Also known as Type 2 error rate
#### Precision:
Formula: TP/ (TP + FP)
#### Accuracy:
Forumla: (TP+ TN)/ (TP + TN + FP + FN)


confusion.matrix <- table(Actual, pred)
# When Positive is on the left
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
TPR <- confusion.matrix[1, 1] / sum(confusion.matrix[, 1])
FPR <- confusion.matrix[1, 2] / sum(confusion.matrix[, 2]) # Type I error
FNR <- confusion.matrix[2, 1] / sum(confusion.matrix[, 1]) # Type II error
precision <- confusion.matrix[1, 1] / sum(confusion.matrix[1, ])

# When Positive is on the right
confusion.matrix <- table(Actual, pred)

# When Positive is on the right
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
TPR <- confusion.matrix[2, 2] / sum(confusion.matrix[, 2])
FPR <- confusion.matrix[2, 1] / sum(confusion.matrix[, 1]) # Type I error
FNR <- confusion.matrix[1, 2] / sum(confusion.matrix[, 2]) # Type II error
precision <- confusion.matrix[2, 2] / sum(confusion.matrix[2, ])



##### LOGISTIC REGRESSION #######
library(ROCR)
prob = predict(M3, type ="response")
pred = prediction(prob , Churned )
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))

# HOW TPR, FPR CHANGE WHEN THRESHOLD CHANGES:
# extract the alpha(threshold), FPR , and TPR values from roc
alpha <- round (as.numeric(unlist(roc@alpha.values)) ,4)
length(alpha) 
fpr <- round(as.numeric(unlist(roc@x.values)) ,4)
tpr <- round(as.numeric(unlist(roc@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(alpha ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(alpha ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis( side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.18 ,0.18 , "FPR")
text(0.58 ,0.58 , "TPR")


# there are some metrics that can help to choose a threshold: G-mean; Youden’s J statistic; etc
cbind(alpha, tpr, fpr)
# To find the range of alpha that is somewhat good, look at the proportion table
prop.table(table(Churned))
# 0        1 
#0.782125 0.217875
# Since 0.217 of the people Churned, then alpha value should be around there





#### Naive Bayes #####

library(ROCR)
score <- nb_prediction[, c("yes")] 

actual_class <- banktest$subscribed == 'yes' # actual response is 0 or 1
pred <- prediction(score , actual_class) 
perf <- performance(pred , "tpr", "fpr")
plot (perf, lwd =2) # lwd is to specify how thick the curve is
abline (a=0, b=1, col ="blue", lty =3)

# COMPUTE AUC FOR NAIVE BAYES CLASSIFIER:
auc <- performance(pred , "auc")@y.values[[1]]
#auc <- unlist(slot (auc , "y.values"))
auc

# HOW TPR, FPR CHANGE WHEN THRESHOLD CHANGES:
alpha <- round (as.numeric(unlist(perf@alpha.values)) ,4)
fpr <- round(as.numeric(unlist(perf@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(alpha ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(alpha ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis(side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.4 ,0.05 , "FPR")
text(0.6 ,0.35 , "TPR")

# there are some metrics that can help to choose a threshold: G-mean; Youden’s J statistic; etc
cbind(alpha, tpr, fpr)
# To find the range of alpha that is somewhat good, look at the proportion table
prop.table(table({Response}))


### Decision Tree calculation

# Function to calculate entropy
calculate_entropy <- function(data, target_column) {
  class_counts <- table(data[[target_column]])
  probabilities <- class_counts / sum(class_counts)
  entropy <- -sum(probabilities * log2(probabilities))
  return(entropy)
}

# Function to calculate Gini index
calculate_gini <- function(data, target_column) {
  class_counts <- table(data[[target_column]])
  probabilities <- (class_counts / sum(class_counts))^2
  gini_index <- 1 - sum(probabilities)
  return(gini_index)
}



