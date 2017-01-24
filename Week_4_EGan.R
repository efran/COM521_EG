# NEED TO LOOK AT MAKO'S SOLUTIONS -- Missed the boat on this one
# ALSO -- read up on tapply() and merge()

# merge(x,y,by)

# And maybe watch the video for this week - probably would have saved much time :-P

# views <- merge(mobile.views, total.views, all,x=TRUE, all.y=TRUE, by.x = c("city","state"), by.y=c("city.name", "state.abbrev"))
# DOING JOINING - two datasets with one column the same and finding ALL the values for that common value

setwd("C:/Users/Emilia/Documents/AAAA_2017winter/Stats/COM521_EG/")

library(plyr)

# PC1. Download these two datasets from data.seattle.gov, save them into your git repository: 
#      COS-Statistics-Top5000-Pages, 
#            https://data.seattle.gov/City-Business/COS-Statistics-Top5000-Pages/yuhv-gvtm
#      COS-Statistics-Mobile Sessions, 
#            https://data.seattle.gov/City-Business/COS-Statistics-Mobile-Sessions/2u47-byfn
#      Assume, for the purposes of this assignment that the first dataset on views to the 
#      top 5000 pages is all views made to the http://www.seattle.gov website
# PC2. Load both datasets into R as separate data frames. Explore the data to get a sense of 
#      the structure of the data. What are the columns, rows, missing data, etc? Write code 
#      to take (and then check/look at) several random subsamples of the data.

# PC2 ---------------------------------------------------

dataMobile <- read.csv("MobileData.csv", header = TRUE)
dataTop5K <- read.csv("Top5K.csv", header = TRUE)

View(dataMobile)
View(dataTop5K)

#--------------------------
monthlyViews <- tapply(dataTop5K$Pageviews, dataMobile$Sessions, sum)

table(dataTop5K$Month)


#--------------------------

# Did some cleaning of dataMobile in Excel -- removed blank rows in the middle of the table

str(dataMobile)
os_table <- table(dataMobile$Operating_System)
colnames(dataMobile)
levels(dataMobile$Operating_System)

# renaming levels to remove spaces in names
levels(dataMobile$Operating_System) <- c( "NotSet", "Android", "Bada", "BlackBerry", "Firefox_OS", 
                                          "iOS", "LG", "LGE", "MOT", "Nintendo3DS", "Nokia", "PlaystationVita", 
                                          "Samsung", "Series40", "SymbianOS", "Windows", "WindowsPhone")

os_counts <- as.vector(os_table)
names(os_counts) <- names(os_table)

# Seeing how frequently each OS appears in the data
barplot(os_counts, cex.names = 0.5)

# There's a monthly pattern here, but not sure how to plot it -- but I think there's a problem on this
sessionsCts <- as.data.frame(cbind(as.vector(dataMobile$Operating_System),as.vector(dataMobile$Sessions), as.vector(dataMobile$Month)))
colnames(sessionsCts) <- c("OpSys", "Sessions", "Month")  
View(sessionsCts)

# changed percents to decimals in Excel, so the data could be visualized in histograms
hist(dataMobile$New_Sessions)
hist(dataMobile$Bounce_Rate)

# would prefer the duration values to be in seconds rather than HH:MM:SS format
duration <- dataMobile$AvgSessionDuration
duration <- as.vector(duration)
duration

# transform each entry into a 3-item list
done <- strsplit(duration[1:length(duration)], ":")
str(done)

#View(done)

#done <- as.matrix(done)

# Extract just the minutes using do.call function
# this command creates a row vector from the values returned by lapply, I think
# I don't completely follow the "[[", but the effect is that the second item 
#      in each list in the list of lists is returnd and added to the result.
# Since I want to do arithmetic with these values, I have R convert them to integers.
minutes <- as.integer(do.call("rbind", lapply(done, "[[", 2)))
View(minutes)

seconds <- as.integer(do.call("rbind", lapply(done, "[[", 3)))
View(seconds)

# created a data frame from the minutes and seconds and used them to create a new column in my data.
times <- as.data.frame(cbind(minutes, seconds))


secondsFromMinutes<- (times$minutes)*60
totSeconds <- secondsFromMinutes + times$seconds
View(totSeconds)

dataMobile$AvgSessionDurationSeconds <- totSeconds
View(dataMobile)

# And visualizing this is now much easier
hist(dataMobile$AvgSessionDurationSeconds)

summary(dataTop5K)
View(dataTop5K)


str(dataTop5K)
# Pageviews has some very large numbers -- just want an overview
dataTop5K$PageviewsQualitative <- dataTop5K$Pageviews
dataTop5K$PageviewsQualitative[dataTop5K$Pageviews >= 20000] <- ">= 20000"
dataTop5K$PageviewsQualitative[dataTop5K$Pageviews < 20000] <- "10000 - 20000"
dataTop5K$PageviewsQualitative[dataTop5K$Pageviews < 10000] <- "5000 - 10000"
dataTop5K$PageviewsQualitative[dataTop5K$Pageviews < 5000] <- "1000 - 5000"
dataTop5K$PageviewsQualitative[dataTop5K$Pageviews < 1000] <- "100 - 1000"
dataTop5K$PageviewsQualitative[dataTop5K$Pageviews < 100] <- "< 100"

table(dataTop5K$PageviewsQualitative)

# PC3. Using the top 5000 dataset, create a new data frame where one column is each month 
#      (as described in the data) and a second column is the total number of views made to 
#      all pages in the dataset over that month.

# Just testing that I can make a copy of the entire dataframe
newTop500 <- as.data.frame(dataTop5K[,])

monthViews <- as.data.frame(cbind(as.vector(newTop500$Month), as.vector(newTop500$Pageviews)))
colnames(monthViews) <- c("Month", "Views")
View(monthViews)

library(lubridate)
monthViews$monthOnly <- month(monthViews$Month)
 
# odd -- November seems to be absent
 top5K_monthSums <- aggregate(as.numeric(monthViews$Views),by=list(monthViews$monthOnly),sum) 
 colnames(top5K_monthSums) <- c("Month", "TotViews")
 top5K_monthSums
# PC4. Using the mobile dataset, create a new data frame where one column is each month 
#      described in the data and the second is a measure (estimate?) of the total number of 
#      views made by mobiles (all platforms) over each month. 
#      This will will involve at least two steps since total views are included. You'll need 
#      to first use the data there to create a measure of the total views per platform.

dataMobile <- read.csv("MobileData.csv", header = TRUE)
newMobile <- as.data.frame(dataMobile[,])
View(newMobile)

monthMobile <- as.data.frame(cbind(as.vector(newMobile$Operating_System), as.vector(newMobile$Sessions * newMobile$PagesPerSession), as.vector(newMobile$Month)))
View(monthMobile)
colnames(monthMobile) <- c("OS", "Views", "Month")

monthMobile$monthOnly <- month(monthMobile$Month)
data <- aggregate(as.numeric(monthMobile$Views),by=list(monthMobile$monthOnly),sum)
data$x <- data$x/15
View(data)
colnames(data) <- c("Month", "AvgMobileViews")
data

# PC5. Merge your two datasets together into a new dataset with columns for each month, 
#      total views (across the top 5000 pages) and total mobile views. Are there any missing data? 
#      Can you tell why?

monthVector <- as.vector(data$Month)
topVector <- as.vector(top5K_monthSums$TotViews)
mobileVector <- as.vector(data$AvgMobileViews)

mergedData <- as.data.frame(cbind(monthVector, topVector, mobileVector))
mergedData

monthVector <- as.vector(data$Month)
topVector <- as.vector(top5K_monthSums$TotViews)
mobileVector <- as.vector(data$AvgMobileViews)

mergedData <- as.data.frame(cbind(monthVector, topVector, mobileVector))
mergedData

# Not sure what we are trying to show here
# I don't know why November is missing from both datasets


# PC6. Create a new column in your merged dataset that describes your best estimate of the 
#      proportion (or percentage, if you really must!) of views that comes from mobile. Be able to
#      talk about the assumptions you've made here. Make sure that date, in this final column, 
#      is a date or datetime object in R.

# I think maybe I wasn't supposed to aggregate by ACTUAL month - but instead by Date?
# I also think somewhere along the way I forgot what question I was supposed to be answering.
# Obviously, I should have been sleeping instead of "doing" R homework 

# Redo Top 5K work with Dates
newTop500 <- as.data.frame(dataTop5K[,])

monthViews_Top5K <- as.data.frame(cbind(as.vector(newTop500$Month), as.vector(newTop500$Pageviews)))
colnames(monthViews_Top5K) <- c("Month", "Views_5K")
View(monthViews_Top5K)

data_top5 <- aggregate(as.numeric(monthViews_Top5K$Views_5K),by=list(monthViews_Top5K$Month),sum)
colnames(data_top5) <- c("Month", "sum5K")
View(data_top5)


# Redo Mobile work with Dates
dataMobile <- read.csv("MobileData.csv", header = TRUE)
newMobile <- as.data.frame(dataMobile[,])
View(newMobile)

monthViews_Mobile <- as.data.frame(cbind(as.vector(newMobile$Operating_System), as.vector(newMobile$Sessions), as.vector(newMobile$Month)))
colnames(monthViews_Mobile) <- c("OS", "Views_Mobile", "Month")
View(monthViews_Mobile)

data_Mobile <- aggregate(as.numeric(monthViews_Mobile$Views_Mobile),by=list(monthViews_Mobile$Month),sum)
colnames(data_Mobile) <- c("Month", "Mobilesum")
View(data_Mobile)

# combine
combined_months <- c(as.vector(data_top5K$Month), as.vector(data_Mobile$Month))
combined_months <-sort(combined_months)

combined_months <- unique(combined_months)
combined_months

merged <- as.data.frame(combined_months)
merged_t = t(merged)
colnames(merged_t) <- c(merged_t)

top5_t = t(data_top5)
mobile_t = t(data_Mobile)
colnames(top5_t) <- data_top5$Month
colnames(mobile_t) <- data_Mobile$Month

mobile <- mobile_t["Mobilesum",]
top5 <- top5_t["sum5K",]

mobile <- as.data.frame(t(cbind(as.vector(mobile))))
top5 <- as.data.frame(t(cbind(as.vector(top5))))
colnames(top5) <- data_top5$Month
colnames(mobile) <- data_Mobile$Month



l=list(mobile,top5)
merged <- do.call(rbind.fill, l)
row.names(merged) = c("mobile", "top5")

mdates <- colnames(merged)
monthDates <- t(as.data.frame(mdates))
monthDates
merged

dateRow <- colnames(merged)
colnames(monthDates) <- colnames(merged)
dates<-as.data.frame(t(dateRow[1:length(dateRow)]))
colnames(dates) <- c(colnames(merged)[1:19])

l2 = list(mobile, top5, dates )
final <- do.call(rbind.fill, l2)
final

row.names(final) <- c("mobile", "top5", "month")
final
final_temp <- as.data.frame(final)
final_temp
final_temp <- t(final_temp)
final_temp <- as.data.frame(final_temp)
row.names(final_temp) <- c()
FTmobile <- final_temp$mobile
FTmobile <- as.numeric(as.vector(FTmobile))

FTtop <- final_temp$top5
FTtop <- as.numeric(as.vector(FTtop))

FTdate <- final_temp$month
FTdate <- as.Date(as.vector(FTdate))

# Perhaps can combine these into a data frame

willThisWork <- as.data.frame(cbind(as.character(FTdate), FTtop, FTmobile))
colnames(willThisWork) = c("Month", "Top", "Mobile")
View(willThisWork)

# PC6. Graph this over time and be ready to describe: 
#      (a) your best estimate of the proportion of views from mobiles 
#             to the Seattle City website over time and 
#      (b) an indication of whether it's going up or down

library(ggplot2)


ggplot(willThisWork, aes(x = willThisWork$Month, y = willThisWork$Top)) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "Month", y = "visits")

ggplot(willThisWork, aes(x = willThisWork$Month, y = willThisWork$Mobile)) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "Month", y = "visits")

ggplot(willThisWork, aes(x = willThisWork$Month, y = c(willThisWork$Top,willThisWork$Mobile))) + 
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "Month", y = "visits")

stackedDF <- as.data.frame( x = c(as.Date(willThisWork$Month), as.Date(willThisWork$Month)))
platform = as.data.frame(c(rep("Top", length(willThisWork$Top)), rep("Mobile", length(willThisWork$Mobile))))
values = as.data.frame(c(as.vector(willThisWork$Top), as.vector(willThisWork$Mobile)))
colnames(values) <- c("x")


newDF <- as.data.frame(cbind(stackedDF, platform, values))
colnames(newDF) <- c("x","variable","y")                         

ggplot(newDF, aes(x = newDF$x, y = newDF$y, color = newDF$variable)) + 
    geom_line(size = 1) + geom_point(size = 3) + 
    scale_color_brewer(palette = "Set1")

# complete case returns true for every row where there is complete data for all columns