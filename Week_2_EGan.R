# Emilia Gan
# COM 521 Week 2 Programming Challenges
# 1/4/2017


#--------------------------------------------------------------------------------
# PC3. Once you've found the variable, compute and present:
#   the mean, median, variance, standard deviation, and interquartile range
#--------------------------------------------------------------------------------
data <- week2.dataset
summary <- summary(data)
meanData <- as.numeric(summary[4])
medianData <- as.numeric(summary[3])
varianceData <- var(data)
sdData <- sd(data)
Q1 <- summary[2]
Q3 <- summary[5] 
IQRData <- as.numeric(Q3 - Q1)

meanData
medianData
varianceData
sdData
IQRData

#------------------- OUTPUT
# > meanData
# [1] 41.79
# > medianData
# [1] 44.93
# > varianceData
# [1] 1878.867
# > sdData
# [1] 43.3459
# > IQRData
# [1] 58.92
#-------------------

# Another presentation of the same data

dataLabels = c("mean", "median", "variance", "std dev", "IQR")
dataTable <- matrix(c(meanData, medianData, varianceData, sdData, IQRData),ncol=5)
colnames(dataTable) <- dataLabels
rownames(dataTable) <- c("emilia_dataset")
dataTable <- as.table(dataTable)
dataTable

#------------------- OUTPUT
#                     mean    median  variance   std dev       IQR
# emilia_dataset   41.7900   44.9300 1878.8668   43.3459   58.9200
#-------------------


#--------------------------------------------------------------------------------

# PC4. Write R code to compute these three statistics by hand: 
#   mean, median, and mode. 
#--------------------------------------------------------------------------------
mean = sum(data)/length(data)
mean

#------------------- OUTPUT
# > mean
# [1] 41.78622
#-------------------

dataSorted = sort(data)
if(length(data)%%2 == 0) {
    val1 = dataSorted[as.integer(length(data)/2)]
    val2 = dataSorted[as.integer(length(data)/2)+1]
    median = (val1 + val2)/2
} else {
    median = dataSorted[as.integer(length(data)/2)+1]
}
median

#------------------- OUTPUT
# > median
# [1] 44.93271
#-------------------

# http://www.cazaar.com/ta/econ113/mode-code-for-r
newData = data
newDataTable = table(as.vector(newData))
mode = as.numeric(names(newDataTable)[newDataTable == max(newDataTable)])
mode

#------------------- OUTPUT - NOTE: Since ALL values are unique, they are ALL modes (or NONE are modes)
# > mode
# [1] -73.251569 -48.224645 -47.048314 -42.430472 -32.997960 -29.244482 -25.408065 -22.471411 -19.628394 -16.559142
# [11] -15.625885 -14.668663  -7.203937  -6.806269  -6.372023  -3.968044  -3.600852  -3.035729  -1.198883   1.955407
# [21]   2.071277   4.413195   6.488674   8.263368   8.346879  14.557593  15.003308  17.881705  18.481827  19.730671
# [31]  20.194318  21.017710  23.345181  23.648405  24.993049  27.089830  27.337348  28.092501  29.479629  29.814310
# [41]  33.038739  37.172461  37.386132  39.368928  39.422979  40.389310  41.199945  41.254385  42.612807  42.880321
# [51]  46.985094  47.082456  48.400580  49.477602  50.640244  50.645756  51.408727  51.647073  53.909685  55.673437
# [61]  56.917987  58.000237  59.613569  62.166874  62.699183  64.131192  65.259367  65.395346  65.462426  67.044358
# [71]  68.819957  69.711417  69.760272  70.869377  71.522558  73.112357  75.180329  76.634147  77.003084  80.347516
# [81]  80.458012  81.975910  82.365691  83.232882  84.961351  85.228928  85.792784  86.560569  89.365453  92.628916
# [91]  94.980226  96.974807  98.213105 101.057188 102.003666 102.112488 107.866525 113.797645 123.229640 173.106545
#-------------------

# PC5. Create a number of visualizations of your dataset: 
#   at the very least, create a boxplot, histogram, and density plot.

boxplot(data, main = 'Boxplot of dataset-emilia')
hist(data, main = 'Histogram of Values')
par(mfrow=c(2,2))
densityPlot(data)

# Made using code in ?densityPlot help section

par(mfrow=c(2,1))

densityPlot(data, histo='faded', breaks=15, main='Density with Solid Histogram')
densityPlot(data, histo='hollow', breaks=30, fadingBorder='66',
            lty=1, main='Density with Hollow Histogram')

# close the open graphics device
dev.off()
#--------------------------------------------------------------------------------

# PC6. Some of you will have negative numbers. Whoops! Those were not supposed to be there. 
#   Recode all negative numbers as missing in your dataset. 
#   Now compute a new mean and standard deviation. 
#   How does it change?

# Copy the data
noNegs = data

# recode negative values to NA
noNegs[noNegs < 0] <- NA

meanNoNegs <- mean(noNegs)
meanNoNegs

#------------------- OUTPUT
# > meanNoNegs
# [1] NA
#-------------------

sdNoNegs <- sd(noNegs)
sdNoNegs

#------------------- OUTPUT
# > sdNoNegs
# [1] NA
#-------------------

# Since the dataset includes missing values, the standard R functions will not produce an answer
# This can be modified:

meanNoNegs2 <- mean(noNegs, na.rm = TRUE )
meanNoNegs2

#------------------- OUTPUT - NOTE: Higher mean value than when negative values were included
# > meanNoNegs
# [1] 56.76996
#-------------------

sdNoNegs2 <- sd(noNegs, na.rm = TRUE)
sdNoNegs2

#------------------- OUTPUT - NOTE: std dev smaller than when negative values were included
# > sdNoNegs
# [1] 32.30143
#-------------------

#--------------------------------------------------------------------------------

# PC7. Log transform your dataset. 
#   Create new histograms, boxplots, and means, median, and standard deviations.

logData = log(noNegs)

# Open a new device for graphics operations
dev.new()
hist(logData, main = 'Histogram of Log-transformed Values')
boxplot(logData, main = 'Boxplot of Log-transformed dataset-emilia')
#--------------------------------------------------------------------------------

# PC8. Commit the code that does all of these into a folder called "week_02" in your git repository. 
#   Publish this on Github and email me with the link to your published Github folder.