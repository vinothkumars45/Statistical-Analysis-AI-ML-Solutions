Conversion probability of new pipeline deals analysis:
The goal of this analysis is develop ML model to predict the conversion probabilities of new pipeline deals based on  
• Age, stage duration, days passed EBD
• CX Category
• SCMS/Tier

R Script Overview
This R script:

Library Imports: The script starts by loading several R libraries necessary for data manipulation, modeling, and visualization.

Set Working Directory: The working directory is set to a specific folder on the user's desktop.

Data Preparation:
• Reading Data: Reads an Excel file (Inter_file.xlsx) into a dataframe and selects specific columns.
• Missing Values: Checks and handles missing values by replacing them with median values or converting 

categorical variables.
• Data Filtering: Filters out outliers based on certain numerical columns.
• Feature Visualization:
• Categorical Variables: Plots bar charts to visualize the distribution of categorical variables.
• Numerical Variables: Creates scatter plots to visualize the relationship between numerical variables and the target variable.

Data Transformation:
• Creating Dummy Variables: Converts categorical variables into dummy/indicator variables.
• Combining Data: Merges the dummy variables with the original dataset and cleans up unnecessary columns.
• Feature Significance and Modeling:
• Data Splitting: Splits the data into training and testing sets.
• Balancing Data: Applies SMOTE (Synthetic Minority Over-sampling Technique) to balance the classes in the training set.

Logistic Regression Model: Trains a logistic regression model and performs feature selection using LASSO regression.
• Model Evaluation: Evaluates the logistic regression model.
• Model Training and Evaluation:

K-Nearest Neighbors (KNN): Trains a KNN model and evaluates its performance using a confusion matrix.

Decision Trees: Trains a decision tree model, evaluates its performance, and performs cross-validation and pruning.

Random Forest: Trains a random forest model and evaluates its performance.
• Prediction on New Data
• Prediction: Uses the trained random forest model to predict probabilities for the new data.

Output Processing: Adds additional columns to categorize the prediction probabilities and deal sizes, and then saves the output to a new Excel file 


