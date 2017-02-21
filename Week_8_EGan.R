# Emilia Gan
# COM 521 HW 8
# 2/21/2017

# PC0. Load the CSV file from weeks 3 into R  

setwd("C:/Users/Emilia/Documents/AAAA_2017winter/Stats/COM521_EG/")
data3 <- read.csv("week3_dataset-emilia.csv", header = TRUE)

# PC1. If you recall from Week PC6, x and y seemed like they linearly related. 
#      We now have the tools and terminology to describe this relationship and 
#      to estimate just how related they are. Run a t.test between x and y in 
#      the dataset and be ready to interpret the results for the class.

View(data3) # Since I don't remember it at all...

t.test(data3$x, data3$y)
# Welch Two Sample t-test
# 
# data:  data3$x and data3$y
# t = -10.747, df = 108.83, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -253.3555 -174.4595
# sample estimates:
#     mean of x     mean of y 
#     41.78622      255.6937

# Based on the results of the t-test, the null that the mean of x = mean of y 
# can be rejected.

#--------------------------------------------------------------------
# PC2. Estimate how correlated x and y are with each other.

# eruption.lm = lm(eruptions ~ waiting, data=faithful)
corr_xy <- lm(x ~ y, data = data3)
corr_xy
summary(corr_xy)$r.squared 
# Call:
#     lm(formula = x ~ y, data = data3)
# 
# Coefficients:
#     (Intercept)            y  
#         -6.1430       0.1874

corr_xy <- lm(x ~ y, data = data3)
corr_xy
# Call:
#     lm(formula = y ~ x, data = data3)
# 
# Coefficients:
#     (Intercept)            x  
#          98.384        3.765

# Depending on whether we want to make x or y our dependent variable, 
# we will come up with a linear regression equation for use in 
# predicting the value of the dependent variable.
# We can get R-squared for each case
summary(corr_xy)$r.squared 
# [1] 0.7056724
# to find R (the coefficient of correlation), we can take the square root:
summary(corr_xy)$r.squared**0.5
# [1] 0.8400431
# strong correlation between x and y - since slope of x is positive, the 
# correlation is also positive. Since R^2 is the same for both versions,
# R will be also (which makes sense).

#--------------------------------------------------------------------
# PC3. Recode your data in the way that I laid out in Week 3 PC7.

data3$i <- as.logical(data3$i)
data3$j <- as.logical(data3$j)

data3$k <- as.factor(data3$k)
summary(data3$k)

library(plyr)

levels(data3$k)
revalue(data3$k, c("0"="none", "1"="some", "2"="lots", "3"="all"))

View(data3)

#--------------------------------------------------------------------
# PC4. Generate a set of three linear models and be ready to intrepret the coefficients, 
#      standard errors, t-statistics, p-values, and R^2 for each:
#      (a) y_hat = B_0 + B_1 x + epsilon
#      (b) y_hat = B_0 + B_1 x + B_2 i + B_3 j + epsilon
#      (c) y_hat = B_0 + B_1 x + B_2 i + B_3 j + B-4 k + epsilon

# fit <- lm(y ~ x1 + x2 + x3, data=mydata)
fit_a <- lm(y ~ x, data=data3)
summary(fit_a)
# Call:
#     lm(formula = y ~ x, data = data3)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -243.76  -77.55    6.73   74.39  201.84 
# 
# Coefficients:
#              Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)  98.3837    14.7485    6.671   1.52e-09 ***
#     x        3.7646     0.2456     15.328  < 2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 105.9 on 98 degrees of freedom
# Multiple R-squared:  0.7057,	Adjusted R-squared:  0.7027 
# F-statistic:   235 on 1 and 98 DF,  p-value: < 2.2e-16

fit_b <- lm(y ~ x + i + j, data=data3)
# summary(fit_b)
# Call:
#     lm(formula = y ~ x + i + j, data = data3)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -240.071  -72.797    5.627   65.783  222.141 
# 
# Coefficients:
#               Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)   38.217     22.669     1.686   0.09506 .  
#     x         3.936      0.240      16.400  < 2e-16 ***
#     iTRUE     55.725     20.692     2.693  0.00835 ** 
#     jTRUE     51.044     20.793     2.455  0.01589 *  
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 101.1 on 96 degrees of freedom
# Multiple R-squared:  0.7371,	Adjusted R-squared:  0.7289 
# F-statistic: 89.72 on 3 and 96 DF,  p-value: < 2.2e-16

fit_c <- lm(y ~ x + i + j + k, data=data3)
summary(fit_c)
# Call:
#     lm(formula = y ~ x + i + j + k, data = data3)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -235.835  -67.669    8.195   60.736  213.950 
# 
# Coefficients:
#              Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)  20.1555    38.6749    0.521   0.6035    
#     x        3.9281     0.2432     16.148   <2e-16 ***
#     iTRUE    58.2455    20.7542    2.806   0.0061 ** 
#     jTRUE    53.0371    20.7498    2.556   0.0122 *  
#     k1       26.6823    36.3014    0.735   0.4642    
#     k2       -5.8727    36.8375   -0.159   0.8737    
#     k3       50.0877    41.2079    1.215   0.2273    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 100.6 on 93 degrees of freedom
# Multiple R-squared:  0.7481,	Adjusted R-squared:  0.7318 
# F-statistic: 46.03 on 6 and 93 DF,  p-value: < 2.2e-16

#--------------------------------------------------------------------
# PC5. Generate a set of residual plots for the final model (c) 
#      and be ready to interpret your model in terms of each of these:
#      (a) A histogram of the residuals.
#      (b) Plot the residuals by your values of x, i, j, and k (four different plots).
#      (c) A QQ plot to evaluate the normality of residuals assumption.

fit_c.res = resid(fit_c)
View(fit_c.res)

hist(fit_c.res)
plot(data3$x, fit_c.res)
abline(0, 0)

plot(data3$i, fit_c.res)
plot(data3$j, fit_c.res)
plot(data3$k, fit_c.res)

qqnorm(fit_c.res)

#--------------------------------------------------------------------
# PC6. Generate a nice looking publication-ready table with a series of fitted models 
#      and put them in a Word document.

library(stargazer)
stargazer(fit_a, fit_b, fit_c, type="text", out="models.txt")

#--------------------------------------------------------------------
# Now, lets go back to the Michelle Obama dataset we used last week the week 7 
#      problem set's programming challenges.
# 
# PC7. Load up the dataset once again and fit the following linear models and be ready to 
#      interpret them similar to the way you did above in PC4:

library("readstata13")
setwd("C:/Users/Emilia/Documents/AAAA_2017Winter/Stats")
halloween <- read.dta13("Halloween.dta")
View(halloween)
obama_fruit <- halloween[, 1:2]
View(obama_fruit)

#      (a) fruit-hat = B_0 + B_1 obama + epsilon 

fruit.lm <- lm(fruit ~ obama, data=obama_fruit)
summary(fruit.lm)
# Call:
#     lm(formula = fruit ~ obama, data = obama_fruit)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.2748 -0.2748 -0.2378  0.7252  0.7622 
# 
# Coefficients:
#              Estimate   Std. Error t value  Pr(>|t|)    
# (Intercept)  0.23779    0.01555    15.293   <2e-16 ***
#     obama    0.03699    0.02580    1.434    0.152    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4337 on 1220 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.001682,	Adjusted R-squared:  0.0008639 
# F-statistic: 2.056 on 1 and 1220 DF,  p-value: 0.1519

# ------------------------------
# (b) Add a control for age and a categorical version of a control for year to the model in (a).
fruit_age.lm <- lm(fruit ~ obama + age, data=halloween)
summary(fruit_age.lm)
# Call:
#     lm(formula = fruit ~ obama + age, data = halloween)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.2989 -0.2644 -0.2366  0.7074  0.7772 
# 
# Coefficients:
#             Estimate   Std. Error t value  Pr(>|t|)    
# (Intercept) 0.218199   0.037631   5.798    8.52e-09 ***
#     obama   0.036991   0.025803   1.434    0.152    
# age         0.002300   0.004023   0.572    0.568    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4338 on 1219 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.00195,	Adjusted R-squared:  0.0003123 
# F-statistic: 1.191 on 2 and 1219 DF,  p-value: 0.3044

year_levels <- as.factor(halloween$year)
halloweeny <- halloween
halloweeny$year <- year_levels
View(halloweeny)

fruit_age_year.lm <- lm(fruit ~ obama + age + year, data=halloweeny)
summary(fruit_age_year.lm)
# Call:
#     lm(formula = fruit ~ obama + age + year, data = halloweeny)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.3176 -0.2608 -0.2466  0.6833  0.8013 
# 
# Coefficients:
#             Estimate Std. Error t value   Pr(>|t|)    
# (Intercept) 0.192038   0.048523   3.958   8.01e-05 ***
#     obama   0.044241   0.026256   1.685   0.0922 .  
# age         0.001798   0.004037   0.445   0.6562    
# year2014    0.001289   0.040364   0.032   0.9745    
# year2015    0.052566   0.038826   1.354   0.1760    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4334 on 1217 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.005411,	Adjusted R-squared:  0.002142 
# F-statistic: 1.655 on 4 and 1217 DF,  p-value: 0.1581


# PC8. Take a look at the residuals for your model in (a) and try to interpret these as you would in PC4 above. 
#      What do you notice?

# Year 2012 was left out -- not sure why

fruit_ay.res <- resid(fruit_age_year.lm)
qqnorm(fruit_ay.res)

# The residuals are not normally distributed
plot(halloweeny$obama, fruit_ay.res)
dim(halloweeny)
length(fruit_ay.res)
# -- Plotting doesn't work because the lm functin eliminated one observation
# due to "missingness"
# not sure what is meant by this, as all values seem present in the data


# PC9. Run the simple model in (a) three times on three subsets of the dataset: 
#      just 2012, 2014, and 2015. Be ready to talk through the results.

halloween_2012 <- halloween[halloween$year == "2012",]
fruit_ay_2012.lm <- lm(fruit ~ obama + age + year, data=halloween_2012)
summary(fruit_ay_2012.lm)
# Call:
#     lm(formula = fruit ~ obama + age + year, data = halloween_2012)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.3907 -0.2454 -0.2019 -0.1557  0.8443 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.08308    0.11229   0.740    0.460
# obama        0.06065    0.06756   0.898    0.371
# age          0.01453    0.01213   1.197    0.233
# year              NA         NA      NA       NA
# 
# Residual standard error: 0.4232 on 161 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.01218,	Adjusted R-squared:  -8.701e-05 
# F-statistic: 0.9929 on 2 and 161 DF,  p-value: 0.3728

qqnorm(resid(fruit_ay_2012.lm))
plot(halloween_2012$obama, resid(fruit_ay_2012.lm))
dim(halloween_2012)
length(resid(fruit_ay_2012.lm))

# Missing one of the 2012 values


halloween_2014 <- halloween[halloween$year == "2014",]
fruit_ay_2014.lm <- lm(fruit ~ obama + age + year, data=halloween_2014)
summary(fruit_ay_2014.lm)
# Call:
#     lm(formula = fruit ~ obama + age + year, data = halloween_2014)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.2519 -0.2310 -0.2184 -0.2120  0.7896 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.205546   0.059304   3.466 0.000583 ***
#     obama       0.015840   0.042378   0.374 0.708752    
# age         0.001604   0.006433   0.249 0.803215    
# year              NA         NA      NA       NA    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.419 on 419 degrees of freedom
# Multiple R-squared:  0.0005263,	Adjusted R-squared:  -0.004244 
# F-statistic: 0.1103 on 2 and 419 DF,  p-value: 0.8956

qqnorm(resid(fruit_ay_2014.lm))
plot(halloween_2014$obama, resid(fruit_ay_2014.lm))
dim(halloween_2014)
length(resid(fruit_ay_2014.lm))

halloween_2015 <- halloween[halloween$year == "2015",]
fruit_ay_2015.lm <- lm(fruit ~ obama + age + year, data=halloween_2015)
summary(fruit_ay_2015.lm)
# Call:
#     lm(formula = fruit ~ obama + age + year, data = halloween_2015)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -0.3196 -0.2557 -0.2544  0.6817  0.7480 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.2574160  0.0542600   4.744 2.59e-06 ***
#     obama        0.0632093  0.0385150   1.641    0.101    
# age         -0.0003352  0.0057515  -0.058    0.954    
# year                NA         NA      NA       NA    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4459 on 633 degrees of freedom
# Multiple R-squared:  0.004242,	Adjusted R-squared:  0.001095 
# F-statistic: 1.348 on 2 and 633 DF,  p-value: 0.2605

qqnorm(resid(fruit_ay_2015.lm))
plot(halloween_2015$obama, resid(fruit_ay_2015.lm))


