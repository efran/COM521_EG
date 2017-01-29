# Week 5 - COM 521
# Emilia Gan

setwd("C:/Users/Emilia/Documents/AAAA_2017Winter/Stats/COM521_EG")

# PC0. I've provided the full dataset from which I drew each of your samples in a TSV file 
# in the directory week_05 in class assignment git repository. These are tab delimited, 
# not comma delimited. TSV, is related to CSV and is also a common format. Go ahead and load 
# it into R (HINT: read.delim()). Take the mean of the variable x in that dataset. That is the 
# true population mean — the thing we have been creating estimates of in week 2 and week 3.

# PC1. Go back to the dataset I distributed for the week 3 problem set. You've already computed 
# the mean for this in week 2. You should compute the 95% confidence interval for the variable x 
# in two ways:
#    (a) By hand using the normal formula for standard error: sigma / sqrt(n)
#    (b) Using the appropriate built-in R function. These number should be the same or very close. 
#        After reading the OpenIntro, can you explain why they might not be exactly the same?
#    (c) Compare the mean from your sample — and your confidence interval — to the true population mean. 
#        Is the true mean inside your confidence interval?

# PC2. Let's look beyond the mean. Compare the distribution from your sample of x to the true population. 
# Draw histograms and compute other descriptive and summary statistics. What do you notice? 
# Be ready to talk for a minute or two about the differences.

# PC3. Compute the mean of y from the true population and then create the mean and confidence interval 
# from the y in your sample. Is it in or out?

# PC4. I want you to run a simple simulation that demonstrates one of the most fundamental insights of 
# statistics:
#    (a) Create a vector of 10,000 randomly generated numbers that are uniformly distributed between 0 and 9.
#    (b) Take the mean of that vector. Draw a histogram.
#    (c) Create 100 random samples of 2 items each from your randomly generated data and take the mean of 
#        each sample. Create a new vector that contains those means. Describe/display the distribution of 
#        those means.
#    (d) Do (c) except make the items 10 items in each sample instead of 2. Then do (c) again except with 
#        100 items. Be ready to describe how the histogram changes as the sample size increases. 
#        (HINT: You'll make me very happy if you write a function to do this.)

# PC5. Do PC4 again but with random data drawn from a normal distribution N(mu=42, sigma=42) instead of 
# a uniform distribution. How are your results different than in PC4?

# PC 0 ----------------------------------------- LOADING DATA AND  CALCULATING MEAN 

# Opened file in Excel first to take a look, then saved as a .csv with a new name
# Original data file is in the working directory and named: com521_population.tsv

data <- read.csv("Week_5_data.csv")
View(data)

mean_x = mean(data$x)
mean_x

# [1] 41.87806

# PC 1 ----------------------------------------- COMAPRISON WITH WEEK 3 DATASET 

wk3_data <- read.csv("week3_dataset-emilia.csv")
View(wk3_data)
mean_wk3 <- mean(wk3_data$x)
mean_wk3

# [1] 41.78622

#  Part a) CONFIDENCE LEVEL DETERMINATION: BY HAND

stdev_wk3 = sd(wk3_data$x)
stdev_wk3

# [1] 43.3459

wk3_LCI <- mean_wk3 - 1.96 * (stdev_wk3/sqrt(length(wk3_data$x)))
wk3_LCI

# [1] 33.29042

wk3_UCI <- mean_wk3 + 1.96 * (stdev_wk3/sqrt(length(wk3_data$x)))
wk3_UCI

error_hand <- 1.96 * (stdev_wk3/sqrt(length(wk3_data$x)))
error_hand
# [1] 8.495796

# Upper CI: 50.28202

# CI BY HAND USING Z=1.96: (33.29042, 50.28202)

# Part b) CONFIDENCE LEVEL DETERMINATION: USING BUILT-IN R FUNCTION

R_CI <- t.test(wk3_data$x)
R_CI[4]

# [1] 33.18545 50.38699
# attr(,"conf.level")
# [1] 0.95

# CI USING BUILT IN R FXN: (33.18545, 50.38699)

# ANOTHER METHOD: USING A T-DISTRIBUTION RATHER THAN A NORMAL DISTRIBUTION

n = length(wk3_data$x)
s = stdev_wk3
error <- qt(0.975,df=n-1)*s/sqrt(n)

error
R_CI[1]

# [1] 8.600766

LCI <- mean_wk3 - error
UCI <- mean_wk3 + error
LCI
UCI

# CI USING QT: (33.18545, 50.38699)

# REPEATING WITH QNORM:

error_norm <- qnorm(0.975)*s/sqrt(n)
LCI_norm <- mean_wk3 - error_norm
UCI_norm <- mean_wk3 + error_norm
LCI_norm
UCI_norm

# CI USING QNORM: (33.29058, 50.28186)

# The difference seems to be related to using a t distribution rather than a
# normal distribution. At an n of 100, the two methods should return similar, 
# but clearly not identical, results.

# Part c)

# The week 3 mean was 41.786, which was pretty close to the true mean of 41.878
# The week 3 std dev was 43.346, resulting in a std error value of close to 8.5
# The true mean, 41.878, falls within the week 3 confidence interval.

# PC 2 ----------------------------------------- COMAPRISON WITH WEEK 3 DATASET 

hist(data$x)
# The histogram looks symmetrical

summary(data$x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -131.20   13.10   41.91   41.88   70.51  232.80 

# The mean and the median are very close -- very little skew (essentially none)

hist(wk3_data$x)
# The histogram does NOT look symmetrical 

summary(wk3_data$x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -73.25   13.00   44.93   41.79   71.92  173.10 

# The mean is less than the median, indicating that the data are left-skewed.

# PC 3 ----------------------------------------- COMAPRISON WITH WEEK 3 DATASET: Y VARIABLE

mean_y <- mean(data$y)
mean_y

# [1] 241.5074

mean_y_wk3 <- mean(wk3_data$y)
mean_y_wk3

# [1] 255.6937

hist(wk3_data$y)
summary(wk3_data$y)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -286.7   151.4   266.8   255.7   385.8   833.4 

# left-skewed

# Since my sample is not normally distributed, I will use the built-in R t .test function

y_CI <- t.test(wk3_data$y)[4]
y_CI

# [1] 217.1496 294.2379
# attr(,"conf.level")
# [1] 0.95

# The true y mean falls within the CI calculated using the sample data from Week 3.

# PC 4 ----------------------------------------- SIMULATION STARTING FROM RUNIF

# Part a) -- generating a vector draw from a uniform distribution
generated_vector <- runif(n = 10000, min = 0, max = 9)

# Part b) -- looking at the distribution of values and finding their mean
mean_simulated <- mean(generated_vector)
mean_simulated

# [1] 4.492335

std_dev_simulated <- sd(generated_vector)
std_dev_simulated

# [1] 2.599627

hist(generated_vector)
# Looks pretty flat - values all between 0 and 9

# Part c) -- drawing 100 samples of size 2 from the generated vector and calculating means

means_n_of_2 <- replicate(100, {
    sample_sim <- sample(x = generated_vector, size = 2, replace = TRUE)
    mean(sample_sim)
})

# looking at the resulting distribution of mean values

hist(means_n_of_2)
# Not looking quite so uniform, but seeing "bunching" towards "interior" (i.e. around the mean)

summary(samples_n_of_2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7416  2.8540  4.3570  4.5100  5.9690  8.4000

# Part d) -- do the same as above using samples of size 10 and then of size 100

# Creating a function to do this using the steps followed above
get_means <- function(data, size) {
    return(replicate(100, {
        sample_sim <- sample(data, size, replace = TRUE)
        mean(sample_sim)
    }))
}

test_10 <- get_means(generated_vector, 10)

hist(test_10)
# Looking more left_skewed, but also fewer values near tails

summary(test_10)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.995   3.998   4.559   4.464   5.029   6.175

test_100 <- get_means(generated_vector, 100)
hist(test_100)
# the max and min are getting closer to the mean

summary(test_100)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.802   4.353   4.510   4.503   4.630   5.093  

# PC 5 ----------------------------------------- SIMULATION STARTING FROM RNORM

# Part a)
generated_vector_norm <- rnorm(n=10000, mean = 42, sd = 42)

# Part b)
mean_simulated_norm <- mean(generated_vector_norm)
mean_simulated_norm

# [1] 42.35786

hist(generated_vector_norm)
# Looks pretty darn normal from the start
# Range wide -- due to large sd value

summary(generated_vector_norm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -105.10   13.87   42.50   42.36   70.34  199.70  

# Part c) - Can use the function created above

test_2_norm <- get_means(generated_vector_norm, 2)

hist(test_2_norm)
# Range much narrowed

summary(test_2_norm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -22.54   22.82   44.83   43.74   61.44  117.80 

# Part d)

test_10_norm <- get_means(generated_vector_norm, 10)
hist(test_10_norm)
# Looking still not quite "normal" ?

summary(test_10_norm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.877  32.760  40.630  40.650  47.980  67.500

test_100_norm <- get_means(generated_vector_norm, 100)
hist(test_100_norm)
# the max and min are getting closer to the mean, but still not quite looking normal

summary(test_100_norm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.42   38.68   41.18   41.22   44.50   50.81

# The median and mean are both close to the "true" mean that was used to generate the dataset.
# But I thought they'd be closer -- maybe we need to draw more than 100 samples...

# Modifying the function to test this

# Pushing the 10% rule we discussed last week...
get_means_v2 <- function(data, size) {
    return(replicate(1000, {
        sample_sim <- sample(data, size, replace = TRUE)
        mean(sample_sim)
    }))
}

test_100_norm_v2 <- get_means_v2(generated_vector_norm, 100)
hist(test_100_norm_v2, breaks = 20)
# the max and min are getting closer to the mean, and looking more "normal"
# but still left-skewed

summary(test_100_norm_v2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 23.17   39.77   42.59   42.56   45.48   54.78 

# Main difference I noticed from the uniform distribution was that the std dev 
# was large relative to the mean, so the range of values was also large (3* mean)
# The std dev for the uniform distribution was only about 1/2 the magnitude of the mean.
# And, since the vlues came from a uniform distribution, the range of values for the means
# were limited to falling within this same uniform distribution.