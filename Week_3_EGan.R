# Emilia Gan
# COM 521 HW 3
# 1/17/2017

# PC3. Load the CSV file into R. Also make sure that you loaded the week 2 dataset file.

setwd("C:/Users/Emilia/Documents/AAAA_2017winter/Stats/COM521_EG/")
data2 <- week2.dataset
data3 <- read.csv("week3_dataset-emilia.csv", header = TRUE)

#----------------------------------------------------------------------------------------
# PC4. Get to know your data! Do whatever is necessary to summarize the new dataset. 
#      Now many columns and rows are there? What are the appropriate summary statistics 
#      to report for each variable? What are the ranges, minimums, maximums, means, medians, 
#      standard deviations of the variables of variables? Draw histograms for all of the 
#      variables to get a sense of what it looks like. Save code to do all of these things.




summary(data3)

lapply(data3, summary)

str(data3)

dim(data3)

hist(data3$x)
hist(data3$j)
hist(data3$i)
hist(data3$k)
hist(data3$y)

table(data3)

table(data3$k)

str(data3)

plot(data3)
#----------------------------------------------------------------------------------------
# PC5. Compare the week2.dataset vector with the first column (x) of the data frame.
#      I mentioned in the video lecture that they are similar? Do you agree? How similar? 
#      Write R code to demonstrate or support this answer convincingly.

summary(data2)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-73.25   13.00   44.93   41.79   71.92  173.10

summary(data3$x)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-73.25   13.00   44.93   41.79   71.92  173.10 

# They have identical descriptive statistics

hist(data2)
hist(data3$x)

# They have indentical histograms

# length(data2)
# length(data3$x)

# Same length

matching = data2[data2 %in%  data3$x]  # only seem to share 10 values exactly
matching

data2_sorted = sort(data2)
data3_x_sorted = sort(data3$x)
View(data2_sorted)
View(data3_x_sorted)

matching_again = data2_sorted[data2_sorted %in% data3_x_sorted]
matching_again

data2_sorted %in% data3_x_sorted
#----------------------------------------------------------------------------------------
# PC6. Visualize the data using ggplot2 and the geom_point() function. Graphing the x on 
#      the x-axis and y on the y-axis seem pretty reasonable! If only it were always so easy! 
#      Graph i, j, and k on other dimensions (e.g., color, shape, and size seems reasonable). 
#      Did you run into any trouble? How would you work around this?

library(ggplot2)

# Yes - ran into some trouble. Got an error message when trying to load ggplot2. 
# Installed a bunch of updates and restarted R.

# More Trouble:

# p = ggplot(data3$x)
# Error: ggplot2 doesn't know how to deal with data of class numeric

data3_DF <- as.data.frame(data3_x_sorted)
data2_DF <- as.data.frame(data2_sorted)
combined <-merge(data3_DF, data2_DF) 

p = ggplot(combined)
p <- p + aes(x = data3_DF, y = data2_DF) + geom_point(cex=3)

p

# continuing from previous question -- they actually look pretty darn identical

# Yet more trouble

# Error in eval(expr, envir, enclos) : object 'x' not found

p2 = ggplot(as.data.frame(data3))

p2 <- p2 + aes(x = data3$i, y = data3$j, color = 'red') + geom_point(aes(shape = factor(data3$k)))

p2

p3 = ggplot(as.data.frame(data3))

p3 <- p3 + aes(x = data3$x, y = data3$y) + geom_point(aes(color = factor(data3$k)))

p3

df3 <- as.data.frame(data3)

p4 <- ggplot(df3)

p4 + ggplot(df3) + aes(x=x, y=y, shape=j) + geom_point()

#----------------------------------------------------------------------------------------
# PC7. A very common step when you import and prepare for data analysis is going to be cleaning 
#      and coding data. Some of that is needed here. As is very common, i, j are really 
#      dichotomous "true/false" variables but they are coded as 0 and 1 in this dataset. 
#      Recode these columns as logical. The variable k is really a categorical variable. 
#      Recode this as a factor and change the numbers into textual "meaning" to make it easier. 
#      Here's the relevant piece of the codebook (i.e., mapping): 0=none, 1=some, 2=lots, 3=all. 
#      The goals is to end up with a factor where those text strings are the levels of the factor. 
#      I haven't shown you how to do exactly this but you can solve this with things I have shown you. 
#      Or you can try to find a recipe online.

data3$i <- as.logical(data3$i)
data3$j <- as.logical(data3$j)

data3$k <- as.factor(data3$k)
summary(data3$k)

library(plyr)

levels(data3$k)
revalue(data3$k, c("0"="none", "1"="some", "2"="lots", "3"="all"))

data3$k

View(data3)

testing <- ifelse(data3$i == 0, FALSE, TRUE)
testing

# Can also change factors thusly:

# data.factor[data == 0] <- "none"
# data.factor[data == 1] <- "one"
# etc.
#----------------------------------------------------------------------------------------
# PC8. Take column i and set it equal to NA when if it is FALSE (i.e., 0). Then set all the values 
#      that are NA back to 1. Sorry for the busy work! ;)

# Just wanted to check
data3$i == FALSE

data3$i[data3$i == FALSE] <- NA
data3$i[is.na(data3$i)] <- 1

#----------------------------------------------------------------------------------------
# PC9. Now that you have recoded your data in PC7, generate new summaries for those three variables. 
#      Also, go back and regenerate the visualizations. How have these changed? 
#      How are these different from the summary detail you presented above?

# Well.... now column i contains all 1s
# going back to BEFORE we did this...
data3 <- read.csv("week3_dataset-emilia.csv", header = TRUE)
data3$i <- as.logical(data3$i)

summary(data3$i)

# Mode   FALSE    TRUE    NA's 
# logical      47      53       0

table(data3$i)

some.data <- c(47, 53)
names(some.data) <- c("False", "True")
barplot(some.data)

# can't do histogram

# versus OLD version
data3old <- read.csv("week3_dataset-emilia.csv", header = TRUE)
summary(data3old$i)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    1.00    0.53    1.00    1.00

hist(data3old$i)

# ---------------
data3 <- read.csv("week3_dataset-emilia.csv", header = TRUE)
data3$j <- as.logical(data3$j)

summary(data3$j)

# Mode   FALSE    TRUE    NA's 
# logical      54      46       0

table(data3$j)

some.data <- c(54, 46)
names(some.data) <- c("False", "True")
barplot(some.data)

# can't do histogram

# versus OLD version
data3old <- read.csv("week3_dataset-emilia.csv", header = TRUE)
summary(data3old$j)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    0.00    0.46    1.00    1.00

hist(data3old$j)

# --------------------------
data3$k <- as.factor(data3$k)
summary(data3$k)

library(plyr)

levels(data3$k)
data3k_levels <- revalue(data3$k, c("0"="none", "1"="some", "2"="lots", "3"="all"))

data3table <- table(data3k_levels)

data3table

some.data <- c(as.numeric(data3table[1:4]))
names(some.data) <- c(levels(data3k_levels))
barplot(some.data)

# ---------------------------------
# Last stats problem
# A 2005 Gallup Poll found that 7% of teenagers (ages 13 to 17) suffer
# from arachnophobia and are extremely afraid of spiders. At a summer camp there are 10 teenagers
# sleeping in each tent. Assume that these 10 teenagers are independent of each other.

# a) At least 1 = 1, 2, 3, 4, 5, 6, 7, 8, 9. or 10 -- i.e. all except 0
1 - dbinom(0, size=10, prob=0.07) 
# 0.5160177

# b) exactly 2
dbinom(2, size=10, prob=0.07) 
# 0.1233878

# c) at most 1 = 0 or 1 or 1, cumulative
pbinom(1, size=10, prob=0.07) 
# 0.8482701
