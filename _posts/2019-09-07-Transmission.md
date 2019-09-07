---
title: "Are manual or automatic cars more fuel efficient?"
date: 2019-09-07
tags: [data science, linear regression, data visualisation]
header:
  image: "/images/mpg.jpg"
excerpt: "Data Science, Linear Regression, Data Visualisation"
---

Introduction
------------

Car owners are becoming increasingly conscious of the impact their vehicles have on the environment. This coupled with the financial cost of filling a car with petrol has made miles per gallon (mpg) a key metric in car performance. Thus understanding the factors which may affect miles per gallon is a worthy area of investigation.

Aims and data
-------------

This analysis will focus on how the transmission of a car affects its miles per gallon. In particular, it will aim to do the following:

1.  Answer whether an automatic or manual transmission is better for miles per gallon
2.  Quantify the miles per gallon difference between automatic and manual transmissions

The data comes from the Motor Trend Car Road Tests (mtcars) dataset in R. "The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles.

Data Wrangling
--------------

We begin by loading some packages that we will use for data manipulation and visualisation.

``` r
# Load packages
require(ggplot2, warn.conflicts = F, quietly = T)
require(tidyr, warn.conflicts = F, quietly = T)
require(purrr, warn.conflicts = F, quietly = T)
require(MASS, warn.conflicts = F, quietly = T)
require(lmtest, warn.conflicts = F, quietly = T)
```

Next we load the mtcars dataset and perform some initial data manipulation. In particular we convert the engine shape (vs) and transmission type (am) to factors.

``` r
data(mtcars)
mtcars$vs <- factor(mtcars$vs)
levels(mtcars$vs) = c("V", "S")
mtcars$am <- factor(mtcars$am)
levels(mtcars$am) = c("A", "M")
```

Exploratory Analysis
--------------------

Let's take an initial look at the contents of our dataset.

``` r
str(mtcars)
```

    ## 'data.frame':    32 obs. of  11 variables:
    ##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
    ##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
    ##  $ disp: num  160 160 108 258 360 ...
    ##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
    ##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
    ##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
    ##  $ qsec: num  16.5 17 18.6 19.4 17 ...
    ##  $ vs  : Factor w/ 2 levels "V","S": 1 1 2 2 1 2 1 2 2 2 ...
    ##  $ am  : Factor w/ 2 levels "A","M": 2 2 2 1 1 1 1 1 1 1 ...
    ##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...

The dataset contains 32 observations of the following 11 variables: 1. mpg: Miles/(US) gallon
2. cyl: Number of cylinders
3. disp: Displacement (cu.in.)
4. hp: Gross horsepower
5. drat: Rear axle ratio
6. wt: Weight (1000 lbs)
7. qsec: 1/4 mile time
8. vs: Engine shape (V = V-shaped or S = straight)
9. am: Transmission (A = automatic, M = manual)
10. gear: Number of forward gears
11. carb: Number of carburetors

A plot of mpg by transmission type reveals that mpg is (on average) higher for manual cars.

``` r
mtcars %>%
  ggplot(aes(x = am, y = mpg, fill = am)) + geom_boxplot()
```

![](./Course_Project_files/figure-markdown_github/mpg_by_am-1.png)

A t-test is performed to assess whether this difference is statistically significant.

``` r
t.test(mtcars$mpg[mtcars$am == "A"], mtcars$mpg[mtcars$am == "M"], alternative = c("less"))
```

    ##
    ##  Welch Two Sample t-test
    ##
    ## data:  mtcars$mpg[mtcars$am == "A"] and mtcars$mpg[mtcars$am == "M"]
    ## t = -3.7671, df = 18.332, p-value = 0.0006868
    ## alternative hypothesis: true difference in means is less than 0
    ## 95 percent confidence interval:
    ##       -Inf -3.913256
    ## sample estimates:
    ## mean of x mean of y
    ##  17.14737  24.39231

With a p-value of 0.0006868, we can reject the null hypothesis (at the 1% significance level) that the mean mpg across the two transmission types is the same.

Whilst we have seen that the mpg for manual cars is higher, we need to investigate whether this is due to other confounding factors. We make some further exploratory plots.

![](Course_Project_files/figure-markdown_github/Exploratory_plots-1.png)![](Course_Project_files/figure-markdown_github/Exploratory_plots-2.png)

From these exploratory graphs, the following initial observations are made:
\* Weight and horsepower have a strong negative trend with mpg. Also automatic cars tend to be heavier and more powerful.
\* Quarter mile time (qsec) and mpg have a positive trend. There is no association between qsec and transmission, perhaps because whilst automatic cars tend to be heavier, they are also more powerful, both of which will affect their quarter mile time.
\* Rear axel ratio has a positive trend with mpg. Also automatic cars tend to have a lower ratio.
\* There is a strong negative trend between displacement and mpg. Automatic cars tend to have higher displacement.
\* Displacement is a measure of the volume of the cylinders within an engine, hence unsurprisingly the result with displacement is mirrored with cylinders. That is more cylinders (hence higher displacement) corresponds to fewer mpg.
\* V-shaped engines tend to have lower mpg than straight engines. There does not seem to be an association between transmission and engine shape.
\* Automatic cars tend to have fewer gears. There is some evidence that having four gear gives the best mpg and three gears gives the worst mpg.
\* There is a negative trend between number of carburetors and mpg. There is a some evidence that automatic cars have fewer carburetors.

Following this initial exploratory analysis we expect that horsepower, weight, and displacement may be relevant as they exhibited strong trend with mpg and strong association with transmission type.

Model Selection
---------------

In order to try to better understand the factors which affect mpg, we fit a linear model. Starting with a linear model fitting all the predictors against mpg, the stepAIC algorithm is performed to determine the model with the lowest Akaike information criterion (AIC).

``` r
fit1 <- lm(formula = mpg ~ ., data = mtcars)
summary(stepAIC(fit1, trace = 0, direction = "both"))
```

    ##
    ## Call:
    ## lm(formula = mpg ~ wt + qsec + am, data = mtcars)
    ##
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max
    ## -3.4811 -1.5555 -0.7257  1.4110  4.6610
    ##
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   9.6178     6.9596   1.382 0.177915    
    ## wt           -3.9165     0.7112  -5.507 6.95e-06 ***
    ## qsec          1.2259     0.2887   4.247 0.000216 ***
    ## amM           2.9358     1.4109   2.081 0.046716 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##
    ## Residual standard error: 2.459 on 28 degrees of freedom
    ## Multiple R-squared:  0.8497, Adjusted R-squared:  0.8336
    ## F-statistic: 52.75 on 3 and 28 DF,  p-value: 1.21e-11

The model with the lowest AIC contains the weight, horsepower and transmission type as the predictors.

Model Residuals and Diagnostics
-------------------------------

Before we can interpret the model, we must examine the residuals to ensure our assumptions are satisfied. Here we have assumed that the residuals are normally distributed with mean zero and constant variance.

![](Course_Project_files/figure-markdown_github/residuals-1.png)

The first plot shows the residuals vs fitted values. The points seem fairly evenly spread above and below the line at zero indicating their mean is about zero. Indeed the mean of the residuals is 5.204170410^{-17}.

However there appears to be some heteroskedasticity; the variance of the residuals appears to increase to larger fitted values. We perform a Breusch-Pagan test to assess this.

    ##
    ##  studentized Breusch-Pagan test
    ##
    ## data:  fit2
    ## BP = 6.1871, df = 3, p-value = 0.1029

The p-value of 0.1029 indicates that we do not reject the null hypothesis that the variance of the residuals is constant. So our assumption of constant variance in the residuals is met.

The second plot is the normal Q-Q plot. Since the points lie roughly on the diagonal line we can be confident the residuals are indeed normally disributed.

The third plot shows the fitted values vs the square root of the standardised residuals. This plot is more concerning as the strong upwards trend is evidence of heteroskedasticity; larger fitted values have larger residuals. However since we have already performed a Breusch-Pagan test for heteroskedasticity (and failed to detect it), we will move on.

Finally in the residuals vs leverage plot the points are spaced fairly evenly above and below the line at zero. This indicates that points with high leverage (i.e. far from the mean of the fitted values) do not have larger or smaller standardised residuals. This is more evidence of homoskedasticity.

Moreover there are no points outside of Cook's distance indicating that there are no influential points. That is there are no outlying points which are dramatically affecting the fit.

Overall we can confident the model meet the assumptions regarding the residuals and that there are no outlying points that need to be addressed. Thus we may continue on to model interpretation.

Model Interpretation
====================

Below is the summary of the model.

    ##
    ## Call:
    ## lm(formula = mpg ~ wt + qsec + am, data = mtcars)
    ##
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max
    ## -3.4811 -1.5555 -0.7257  1.4110  4.6610
    ##
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   9.6178     6.9596   1.382 0.177915    
    ## wt           -3.9165     0.7112  -5.507 6.95e-06 ***
    ## qsec          1.2259     0.2887   4.247 0.000216 ***
    ## amM           2.9358     1.4109   2.081 0.046716 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##
    ## Residual standard error: 2.459 on 28 degrees of freedom
    ## Multiple R-squared:  0.8497, Adjusted R-squared:  0.8336
    ## F-statistic: 52.75 on 3 and 28 DF,  p-value: 1.21e-11

With an adjusted R-squared of 0.8336, the model explains a large amount of the variation in mpg so is a good fit to the data. All the predictors are significant we can also be confident that they are all pertinent and we have not overfit the data.

The coefficient of transmission type (amM) is 2.9358 with a corresponding p-value of 0.046716. Thus it is statistically significant at the 5% significance level. This shows that, on average, the mpg of manual cars is 2.9358 higher than automatic cars, *ceteris paribus*. A 95% confidence interval for the transmission type is `0.04570236 5.82589764`.

Conclusion
==========

To conclude, we complete the two assign tasks.

1.  Answer whether an automatic or manual transmission is better for miles per gallon
    A manual transmission is better for miles per gallon.

2.  Quantify the miles per gallon difference between automatic and manual transmissions
    With 95% confidence, we can say that the miles per gallon of a manual car is between 0.04570236 and 5.82589764 mpg better than the mpg of an automatic car. A manual car is on average 2.9358 mpg better than an automatic car.
