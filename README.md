# BFRSS-Depression Classifier

## Overview
This project aims to build a predictive model for identifying individuals with depressive disorders using data from the 2020 Behavioral Risk Factor Surveillance System (BRFSS). The dataset consists of 5000 individuals and 276 attributes. Multiple machine learning classification models were applied to determine the most effective model.

## Key Objectives
- **Data Preprocessing:** Handle missing values, clean and reduce the data, and ensure the dataset is ready for modeling.
- **Model Training:** Apply various classification algorithms including Logistic Regression, Random Forest, SVM, K-Nearest Neighbors (KNN), XGBoost, and Naive Bayes.
- **Feature Selection:** Use techniques like Boruta, Information Gain, and Correlation-based Feature Selection (CFS) to refine feature sets.
- **Model Evaluation:** Compare the performance of different models and identify the best performing one.

## Tools Used
- **Caret**: For model training and tuning.
- **Random Forest**: For classification and regression tasks.
- **e1071**: Includes functions for statistical learning such as support vector machines (SVM).
- **XGBoost**: An efficient gradient boosting framework.
- **pROC**: Used for visualizing and analyzing model performance via ROC curves.
- **Boruta**: For feature selection.
- **kknn**: Implementation of K-nearest neighbors algorithm.

## Data Preprocessing
The dataset initially contained 5000 rows and 276 attributes, but after processing, the final data included 66 attributes. Data preprocessing involved:
- Handling missing values.
- Dropping irrelevant attributes.
- Balancing the dataset using oversampling and undersampling techniques.
- Splitting the dataset into training (66%) and testing (34%).

## Feature Selection Methods
Three feature selection methods were used:
- **Boruta**: Identifies important features using random forest-based shadow features.
- **Information Gain**: Measures reduction in entropy after a split.
- **Correlation-based Feature Selection (CFS)**: Selects features based on their correlation with the target variable.

## Classification Algorithms
The following algorithms were applied:
- Logistic Regression
- Random Forest
- Support Vector Machines (SVM)
- Naive Bayes
- K-Nearest Neighbors (KNN)
- XGBoost

## Model Results
After evaluating 36 model combinations, the **Overbalanced Boruta** approach combined with **Logistic Regression** showed the best performance. Below are the key performance metrics for the best model:

| Metric     | Class Y | Class N |
|------------|---------|---------|
| TPR        | 0.645   | 0.714   |
| FPR        | 0.285   | 0.354   |
| Precision  | 0.347   | 0.895   |
| Recall     | 0.645   | 0.714   |
| F-Measure  | 0.451   | 0.794   |
| ROC        | 0.723   | 0.723   |

## Conclusion
The **Logistic Regression** model using the **Overbalanced Boruta** method was selected as the best model due to its robust performance in predicting depressive disorders. This project highlights the importance of thorough data preprocessing, feature selection, and model evaluation in achieving accurate predictions.

## Future Improvements
- Further tuning of hyperparameters across all models.
- Experimenting with more complex algorithms like deep learning models.
- Expanding the dataset for better generalization.
