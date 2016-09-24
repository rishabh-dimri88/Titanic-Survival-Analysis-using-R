# Titanic-Survival-Analysis-using-R


Two Approach for survival analysis both using R

First Approach: (Learning_from_disaster_titanic.R)
1. Some basic analysis using density plots, tables and proportion tables 
2. Trained a decision tree model for prediction using all the other variables as predictors
3. Applied some feature engineering techniques for example, split the names into three parts, used Title to bin between Rich, middle class and poor, identified the family using surname and family size  etc
4. Trained a feature engineering decision tree model for prediction using all the other variables as predictors
5. Imputed the Age and Embarked missing values using decision tree and Mode imputation respectively
6. Trained a Random forest model.

Second Approach:(Another_approach_Learning_from_disaster_titanic.R)
1. Some basic analysis using density plots, bar plots, tables and proportion tables 
2. Imputed the missing Age values using mean imputation across each Title
3. Created new variable chi
