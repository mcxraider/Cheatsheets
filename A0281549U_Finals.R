
#Q1(a)
#False

#Q1(b)
#True

#Q1(c)
#False

#1(d)
#False

#1(e)
#True

#Q2(a)
#Ans: B

#2(b)
#Ans: C

#2(c)
#Ans: A

#2(d)
#Ans: C


#2(e)
#Ans: A



library(class)
library(ROCR)
library(e1071)
library("rpart") 
library("rpart.plot")
library('arules') # arules has its own dataset type (sparse dataset) row and cols switch
library('arulesViz')
##### Common Funcs #####
set.seed(666)
setwd("/Users/Jerry/DSA1101_Finals")
df1= read.csv("crab.csv")
head(df1)
dim(df1)
str(df1)
attach(df1)

##### Q3 #####

#(1)
hist(satell, freq=TRUE, main = paste("Histogram of satell"),
     col = "grey")
# The histogram shows that the sample is unimodel.
# the range of histogram is from 0 to 15
# the histogram is right skewed, and there is no gap in the data.

#(2)
plot(width,satell, pch = 20)
# There is a positive relationship; might be linear, however, it is a weak positive relationship
# The variability of the total is NOT STABLE when order changes, the plot has a Funnel like shape}.


#(3)
df1$spine = as.factor(df1$spine)
df1$color = as.factor(df1$color)
str(df1)

M1 = lm(satell ~ color + spine + width + weight, data = df1)
summary(M1)
# R^2 = 0.151, very low, hence, Model M1 is not very good fit.
# It means, Model M1 can explain 15.1% of the variance in the observed response, satell.

#(4)
# The feature that is most insignificant is I(spine=3),
# as the significance test has p-value 0.918, very large.
# A variable is insignificant if their p value is very large.
# Spine is a categorical variable, not a continuous variable.
# The assumptions of a linear model state that relationships between the input variables and the outcome variable should be linear.


#(5)
sur = ifelse(satell >= 1, 1, 0) 
sur = as.factor(sur)
df1$status = sur
head(df1)
attach(df1)

#(6)
table(status)
#status
# 0   1 
# 62 111 
# Hence, there are 111 female crabs with at least one satellite in the data


#(7)
CS.table = table(color, status); CS.table
#Number of female crabs = 69


#(8)
tab = prop.table(CS.table); tab
#Comments:
# There seems to be no linear association, between the color and probability.
# As color darkens, the probability it has satells does not increase
# The distribution is between color and satells seems to be slightly left skewed,
# With the medium color having the highest probability of having satells


#(9)
head(df1)
str(df1)
df1$status = as.factor(df1$status)
M2 = glm(status ~ width + weight + color,
         data=df1,family=binomial(link="logit"))

summary(M2)
#Fitted equation: 
# log(p(status = 1)/ 1-p(status = 1)) = -8,6445 + 0.2906*width + 0.7727*weight + 0.1310*I(color=3) -0.1610*I(color=4) - 1.2453*(color=5)


#(10)
exp(0.7727)
# Coefficient of weight = 0.7727, e^0.7727 = 2.165606
# The odds that a female crab is a attached to more than one satellite increase by 2.17 unit per unit of weight



#(11)
odds_ratio = function(level, feature){
  new_col = ifelse(feature==level, 1,0)
  table = table(new_col, status)
  odds1 = table[2,2]/ table[2,1]
  odds0 = table[1,2]/ table[1,1]
  return (odds1/odds0)
}
#female_odds = odds_ratio('Female', gender); female_odds

medium_odds = odds_ratio(3, color);medium_odds
# The odds of medium color female crab having satellites is 2.27

darker_odds = odds_ratio(4, color);darker_odds
# The odds of medium color female crab having satellites is 0.748


#(12)
crab_A = data.frame(color = "4",spine = "1", width = 26, weight = 2.6)
crab_B = data.frame(color = "2",spine = "3", width = 30, weight = 4.0)

crab_A_pred = predict(M2, crab_A, type="response"); crab_A_pred
#Probability that Crab A having satellites is 0.681

crab_B_pred = predict(M2, crab_B, type="response"); crab_B_pred
#Probability that Crab B having satellites is 0.960

#(13)
prob = predict(M2, type ="response")
pred = prediction(prob , df1$status )
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))
#AUC value of M2 is 0.778


#(14)
alpha <- round (as.numeric(unlist(roc@alpha.values)) ,4)
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
text(0.4 ,0.3 , "FPR")
text(0.7 ,0.8 , "TPR")

#(15)
cbind(alpha, tpr, fpr)
# For threshold of  δ=0.5995,  tpr=0.775, fpr=0.323
# For threshold of  δ=0.5981,  tpr=0.784, fpr=0.323
# For threshold of  δ=0.5925,  tpr=0.802, fpr=0.323

#(16)
accuracy <- function(matrix){
  return (sum(diag(matrix))/sum(matrix));
}

pred = predict(M2, newdata=df1)
pred = ifelse(pred>=0.5, 1, 0)
confusion_matrix=table(pred, df1$status)
lr_accuracy = accuracy(confusion_matrix); lr_accuracy
# Accuracy is 0.712


#(17)
head(df1)
str(df1)
M3 <- naiveBayes(status ~ color + spine+ width + weight,data=df1)



#(18)
pred_probs = predict(M3, newdata=df1, type='raw')
pred_positive_probs = pred_probs[,2]
pred_class = ifelse(pred_positive_probs > 0.5, 1, 0)
confusion_matrix = table(Actual=df1$status,Predicted=pred_class)
nb_accuracy = accuracy(confusion_matrix); nb_accuracy
# Accuracy is 0.705


#(19)
# PREDICT PROBABILITY
crab_A = data.frame(color = "4",spine = "1", width = 26, weight = 2.6)
crab_B = data.frame(color = "2",spine = "3", width = 30, weight = 4.0)
probs_A = predict(M3, newdata=crab_A, type='raw'); probs_A
# The probability of Crab A having satellites is 0.676

probs_B = predict(M3, newdata=crab_B, type='raw'); probs_B
# The probability of Crab A having satellites is 1.00


####### Q4 #########
data = read.csv("penguins-dsa1101.csv")
head(data)
dim(data)
str(data)
attach(data)

#(1)
plot(bill_depth, mass, pch = 20)
# Based on the scatterplot, I would choose 2 clusters


#(2)
standardized.X = scale(data) 

#Choosing number of cluster options
K = 8
wss <- numeric(K)
for (k in 1:K) { 
  wss[k] <- sum(kmeans(standardized.X,centers=k)$withinss)
}
wss
# The wss vector has 8 values of the within sum of squares of each k, starting from k =1


#(3)
plot(1:K, wss, col = "red", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")
# Based on the plot, I would choose K =2, 2 clusters.
# Explanation: From the plot, we see that There is a sharp
# decrease in Within sum of squares from K =1 to K = 2, then there is a more 
# gradual decrease in within sum of squares from  k =2 to K =3.

# Using the elbow method, I can then choose the K=2 as the optimal number of clusters


#(4)
kout = kmeans(standardized.X,centers=2)
kout$centers 
kout$size 

# Since K =2, there are 2 centroids:
# Centroid 1 center: (0.617, -0.612)
# Centroid 1 has 219 observations

# Centroid 2 center: (-1.10,  1.09)
# Centroid 2 has 123 observations













