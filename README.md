# Brooklyn-_Home_Sales_Price-Prediction
Academic Project for ISEN Engineering Data Analysis

## INTRODUCTION

For this project, we analyzed a data set corresponding obtained from NYC Department of Finance website to develop important insights between different features and to write a predictive model to estimate the value of a property for Brooklyn City for given values of these parameters. 

Planning has become a basic need of this decade. Planning allow both individuals and organizations to avoid undesired situations, gain better economic stability and thus, obtain a better quality in operations and living. 

We did feature engineering, to create new attributes which categorized variables in a new way, and thus, makes it easier to understand and straight forward to model. The original data set was cleaned, and variables that not generated added value were removed. After obtaining a new data set, both univariate and bivariate analysis was performed.

In the Univariate Analysis each attribute was, removing outliers and ambiguous values. For example, Sale price values of $1, zip codes with null values and empty cells for any given attribute. Moreover, the Bivariate Analysis consisted in checking for correlation between attributes and dropping a few more predictors which we found to be highly correlated. After the dataset was cleaned, and both Univariate and Bivariate analysis were completed, we continued with the model fitting stage. 

For model fitting, we tried different methods. Among those were Tree Regression (Boosting & Random Forest), K Nearest Neighbor, Multiple Linear Regression, Ridge Regression and Lasso Regression. 

After fitting and analyzing each model, we found that Boosting and Random forest produce good accuracy, but yield poor interpretability. Whereas, Multiple Linear Regression, Lasso Regression, Ridge Regression produce less accurate result, but more interpretable models.

