# Emilia Gan
# Week 6 COM 521
# Feb 7, 2017

#----------------------------------------

# Using data from Red Dye Number 40 as original data set
# http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/owan/frames/frame.html
# Modified dataset in COM521_EG folder: red_dye_data.csv
# -- already replaced column headers X1 --> X4 with control --> high

setwd("C:/Users/Emilia/Documents/AAAA_2017Winter/Stats/COM521_EG")

# PC1. Load the data into R. Now get to work on reshaping the dataset. 
#     I think a good format would be a data frame with two columns: group, 
#     time of death (i.e., lifespan).

data <- read.csv("red_dye_data.csv", stringsAsFactors = FALSE)
 
# Reshape data -- already replaced column headers X1 --> X4 with control --> high
# Avoid using cbind, since it creates a matrix, which can only contain ONE type.
# Consequently, all values in the dataset become levels (factor data).

control_data <- data.frame("control", data$control)
colnames(control_data) <- c("group", "tod")
low_data <- data.frame("low", data$low)
colnames(low_data) <- c("group", "tod")
med_data <- data.frame("med", data$med)
colnames(med_data) <- c("group", "tod")
high_data <- data.frame("high", data$high)
colnames(high_data) <- c("group", "tod")

DF <- rbind.data.frame(control_data, low_data, med_data, high_data) 

DF <- DF[! is.na(DF$tod),]

#----------------------------------------

# PC2. Create summary statistics and visualizations for each group. 
#     Write code that allows you to generate a useful way to do both 
#     (a) get a visual sense both for the shape of the data and its relationships and 
#     (b) the degree to which the assumptions for t-tests and ANOVA hold. 
#     (c) What is the global mean of your dependent variable?

library(ggplot2)

ggplot(DF,aes(x=as.integer(tod)))+geom_histogram(bins=10)+facet_grid(~group)+theme_bw()
table(DF$group)
tapply(DF$tod, DF$group, summary)
mean(DF$tod)

# a) see output
# b) they don't seem to hold all that well...
# c) global mean of dependent variable (tod) = 75.55263

#----------------------------------------

# PC3. Do a t-test between mice without any RD40 and mice with at least a small amount. 
#     Run a t-test between the group with a high dosage and control group. 
#     How would you go about doing it using formula notation? 
#     Be ready to report, interpret, and discuss the results in substantive terms.

RD40 <- DF

levels(RD40$group) <- c("control" = "control","low" = "RD40","med" = "RD40","high" = "RD40")
t.test(tod ~ group, data = RD40)

controls <- DF$tod[DF$group=="control"]
hi_dose <- DF$tod[DF$group=="high"]
t.test(controls, hi_dose)

# Welch Two Sample t-test -- CONTROL and RD40

# data:  tod by group
# t = 4.2065, df = 33.732, p-value = 0.0001806
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#    11.49879 33.00626
# sample estimates:
#    mean in group control    mean in group RD40 
#                 91.36364              69.11111


# Welch Two Sample t-test -- CONTROL (x) and HIGH DOSE (y)

# data:  controls and hi_dose
# t = 2.4958, df = 8.5799, p-value = 0.0353
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#    2.266399 49.960874
# sample estimates:
#    mean of x   mean of y 
#     91.36364    65.25000 

control_high <- DF[DF$group=="control"|DF$group=="high",]
t.test(tod ~ group, data = control_high)

# Welch Two Sample t-test

# data:  tod by group
# t = 2.4958, df = 8.5799, p-value = 0.0353
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#    2.266399 49.960874
# sample estimates:
#    mean in group control    mean in group high 
#                 91.36364              65.25000

#----------------------------------------

# PC4. Estimate an ANOVA analysis using aov() to see if there is 
#     a difference between the groups. Be ready to report, interpret, and 
#     discuss the results in substantive terms.

# One Way Anova (Completely Randomized Design)
fit <- aov(tod ~ group, data=DF)

fit

# Call:
#    aov(formula = tod ~ group, data = DF)

# Terms:
#                    group Residuals
# Sum of Squares   4051.96  12937.43
# Deg. of Freedom        3        34

# Residual standard error: 19.50674
# Estimated effects may be unbalanced

summary(fit)

#             Df Sum Sq Mean Sq F value Pr(>F)  
# group        3   4052  1350.7    3.55 0.0245 *
# Residuals   34  12937   380.5                 
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-------------------OPEN DATA QUESTIOND

# Q1 - 6.12 CI interval for proportion

pbar = 0.48
n = 1259

SE = sqrt(pbar*(1 - pbar)/n); SE     # standard error
E = qnorm(0.975) * SE; E              # margin of error
pbar + c(-E, E) 

# [1] 0.4524033 0.5075967

# Q2 - 6.20

qbar = 1 - pbar

# n = pq/SE^2
# SE = E/ qnorm(0.975)
SE = 0.02 / qnorm(0.975)
n = (pbar * qbar) / (SE * SE)
n

# [1] 2397.07 --> 2398

#6.50 - chi square
chi_2 = (83-90)^2/90 + (121-110)^2/110 + (193-185)^2/185 + (103-115)^2/115
chi_2

# [1] 3.242564

data <- rbind(c(83,90), c(121, 110), c(193, 185), c(103, 115))
colnames(data) <- c("sample", "US")
row.names(data) <- c("NE", "NC","S","W")

results <- chisq.test(data)
results

# Pearson's Chi-squared test

# data:  data
# X-squared = 1.6369, df = 3, p-value = 0.6511
                 