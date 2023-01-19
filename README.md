# Linear-Regression_R
This project investigates the relationship between income and 3 sociodemographic predictors (education, gender, age) in Italy using the European Values Study Wave 5 dataset (https://doi.org/10.4232/1.13897)

By means of EDA (multivariate plots) and a stepwise linear regression model, the following questions are answered:

1.	What is the relationship between education and income?
2.	Does it vary when age and gender are taken into account?

#Workflow steps

1. Subsetting the target and regressor variables 
2. Assign meaningful variable names
3. Recode missing values and perform a listwise deletion (final sample of 1556 complete observations)
4. Convert data type and grand mean center age
5. Generate multivariate graphs with ggplot
6. Fitting 2 linear models in a stepwise fashion 

#Results 

1. In Italy the differences in income are significant across the educational levels.
2. The association between income and education does not vary significantly between men and women, whereas it does based upon age that makes it
stronger (suppressor variable).

#Model goodness of fit

The F statistic for both regression models yielded statistically significant results meaning that there are statistically significant linear relationships
between the outcome and predictor variables.

