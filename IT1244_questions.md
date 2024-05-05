

Meaning of precision and accuracy, F1 score so dont have to memorise

- Precision, recall, and F1-score are common evaluation metrics used in classification tasks to assess the performance of a model. 

- Precision measures the accuracy of positive predictions made by the model. It answers the question: "Of all the instances predicted as positive, how many are actually positive?"

- Recall, also known as sensitivity or true positive rate (TPR), measures the ability of the model to correctly identify all positive instances in the dataset. It answers the question: "Of all the actual positive instances, how many did the model correctly identify?"


- F1-score is the harmonic mean of precision and recall. It provides a balance between precision and recall and is particularly useful when there is an imbalance between the number of positive and negative instances in the dataset.

---

Concept of underfitting:
- Underfitting occurs when a machine learning model is too simple to capture the underlying structure of the data. 
- It typically occurs when the model lacks the capacity or flexibility to learn from the training data effectively

---

When to use each activation function
- Softmax for multi class represntation
- Sigmoid for binary classification
- Relu for hidden layers of deep neural networks for most types of problems, for NLP and CV.
- Leaky Relu for hidden layers of deep neural networks, especially when addressing the dying ReLU problem

---

"If K=N, then it is called Leave one out cross validation, where  N is the number of observations." - This statement is True. Leave-One-Out Cross-Validation (LOOCV) is a special case of K-fold cross-validation where K, the number of folds, equals  N, the number of observations in the dataset. In LOOCV, each fold contains exactly one data point as the validation set and the remaining N−1 points as the training set.

---

For K-Nearest Neighbors (KNN), it's generally advisable to apply feature scaling to ensure that all features contribute equally to the distance calculations. Since the energy density and polarization features have different ranges, normalization, which scales the features to a range of [0,1], would be appropriate in this case.

---

Role of validation, test, esp for the part where testing, selecting best model etc

---

Is Pooling layer is always applied before the Activation Layer?

What then is this activation layer?

---

Purpose of convolutional layer

---

KNN can be used for regression tasks, u average out the y val

---

Calculateing euclidean distance for R^3 vectors:
(x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2

---

Increasing the value of 'k' in the k-Nearest Neighbors (KNN) algorithm does not always reduce the impact of noise on the model's predictions. While a larger 'k' can indeed help in reducing the effect of noise by averaging over more neighboring points, it can also make the algorithm less sensitive to the actual structure of the data, potentially leading to oversmoothing and poorer performance on complex datasets. 

---






Add to anki:
F1 score, precision, recall formulas 
Type 1 and 2 error rates



