# MechaCar_R-Statistical_Analysis

Implementing statistical analysis using R on a car testing datasets.

MechaCar_mpg.csv, Suspension_Coil.csv - source data for analysis.

MechaCarChallenge.R - calculations in R-language.

# MPG Regression

Based on the MechaCar_mpg.csv dataset and using R-language we have designed the
following multiple linear regression model for mpg prediction:

mpg = 6.27*vlength + 0*vweight + 0.07*spangle + 3.55*grclearance - 3.41*AWD - 104

From this formula we can already see that vehicle length and spoiler angle have
no (or almost no) impact on the mpg of the vehicle so we can rewrite our model:

mpg = 6.27*vlength + 3.55*grclearance - 3.41*AWD - 104

Thus we can already see that only vehicle length, ground clearance and AWD have 
impact on an mpg parameter of the vehicle.

From the generated by summary statistics p-values, Pr(>|t|), for the variables 
in the dataset we can state that the following variables/coefficients provide 
a non-random amount of variance to the mpg values in the dataset:
 - vehicle length: Pr(>|t|) = 2.60e-12
 - ground clearance: Pr(>|t|) = 5.21e-08
 - (Intercept): Pr(>|t|) = 5.08e-08

P-values for those variables are much smaller than our significance level of 0.05. 
Therefore, we have sufficient evidence to state that those parameters have 
significant impact on the mpg values in the dataset.
Unfortunately, Intersept value in our model is statistically significant too which
means that there are other variables and factors that contribute to the variation
in mpg that have not been included in our model (and into the dataset) and those 
values may still need to be collected or observed.

Despite that, according to the summary output, the r-squared value of our multiple
linear regression model is 0.71, which means that roughly 71% of all mpg predictions
will be correct while using this regression model.

In addition, the p-value of our linear regression is 5.35e-11, which is much smaller
than our assumed significance level of 0.05. Therefore, we can state that there is
sufficient evidence to reject our null hypothesis, which means that the slopes of 
a multiple linear regressions model are not zero for 3 variables: vlength, 
grclearance and AWD.

# Suspension Coil Summary

Suspension coil presented in the dataset is normally distributed and densely gathered
around the mean and median which are equal to 1498.78 and 1500 respectively.

Standard Deviation of the PSI variable is very small 7.89.
Variance of the PSI sample distribution is 62.29 which is smaller than 100 pounds per
inch so our suspension coils data meets the design specifications for the MechaCar.

# Suspension Coil T-Test

H0: There is no statistical difference between the observed sample mean and its
population mean.
Ha: There is a statistical difference between the observed sample mean and its
population mean.
Population mean is 1500 pounds per inch.

P-value we've calculated using R-language t.test() function is 0.06.
Assuming our significance level was the common 0.05 percent, our p-value is above
that significance level. Therefore we don't have sufficient evidence to reject our
null hypothesis, so we would state that our dataset mean and population mean are
statistically similar. This also means that our suspension coils dataset is 
statistically representative.

# Design your own study

If we are planning to sell our vehicle at the same price as our competitors and
want to outperform the competition we migth want to test if our prototype
vehicle is more fuel efficient than other comparable vehicles on the market. 

We would need to collect the fuel efficiency mpg levels for our car prototypes at 
different distances and weather conditions and test if the mean of this set is NOT 
SMALLER than the mean fuel efficiency mpg for comparable vehicles at the market.
We would also need to obtain the fuel effcicency mpg levels for our competitors
vehicles at different distances and weather conditions.

Our collected mpg data should follow the following assumptions:
1. The input data is numerical and continuous;
2. Each sample data is selected randomly from its population.
3. The input data is considered to be randomly distributed (if our data is slightly 
skewed we can use logarithm base 10 to fix that).
4. The input data is reasonably large.
5. The variance of the two datasets should be similar.

Our hypothesis would be the following:
H0: There is no statistical difference between our mpg dataset and other comparable
vehicles mpg dataset on the market.
Ha: The true mean of our mpg dataset is GREATER than the mean of our competitors
fuel efficiency values dataset.

We would need to use a paired t-test to compare our datasets providing the following
parameters:
x - our prototype vehicle mpg dataset as vector of numbers;
y - our competitors mpg values dataset as vector of numbers;
paired - must be set to TRUE since we're comparing two sets;
alternative - must be set to "greater" or "g" to specify that our alternative 
hypothesis states that x mean is greater than y mean.

If the resulting p-value would be smaller than 0.05 we would have enougth 
statistical evidence to reject our null hypothesis and state that our Ha is true,
which would mean that our prototype vehicle outperforms the competition in fuel
efficiency.
