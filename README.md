# Customer-Life-Time-Prediction-AutoInsurance
Aim of this Project is to Explore how the Customer Life Time Value is Predicted. Which will be useful to identify the right sets of Customers and could be used to target them for better conversion Rate.

1. Initialy we did the Exploratory data analysis to understand the data spread, Visualize the data
2. Understand the outliers and apply the 97th percentile to bring them back into the range.
3. Used OneHotEncoding ( Feature engineering ) to or Factor the data to convert the categorical variables.
4. Split the dataset to training and testset
5. Build various models to understand the correlation between the variables.



##### For this Project I have used the below MLs:

1. Multiple Linear Regression
2. Random Forest Regression
3. XG Boost

After Evaluating all the Model based on accuracy on testing data we found XG Boost Model is stable and provide better Accuracy for Predicting CLTV.

###### With Outliers the Model Accuracy     : 77.48%(R^2)  | RMSE (2647.7554095)   | MAE (1253.8750966)

###### Without Outliers the Model Accuracy : 84.43%(R^2) | RMSE (2239.0906101) | MAE (1041.9238513)
