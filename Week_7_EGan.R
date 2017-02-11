# Emilia Gan
# Week 7 COM 521
# Feb 14, 2017

#----------------------------------------

setwd("C:/Users/Emilia/Documents/AAAA_2017Winter/Stats")

library("foreign")

data <- read.dta("lilypad_anonymized.dta", convert.dates = TRUE, convert.factors = TRUE,
         missing.type = FALSE,
         convert.underscore = FALSE, warn.missing.labels = TRUE)

View(data)

# PC 1:
# (a) Reproduce both Table 1 and Table 2 (just US users) using the dataset 
#      (as closely as possible)

US_data <- data[data$country == "United States",]
View(US_data)

# FROM THE ARTICLE:
# Table 1. LilyPad/Arduino customer contingency table.

#             Arduino     Both    LilyPad
# Unknown     1332        41      91
# Male        7687        250     598
# Female      890         79      367 

table_1 <- table(data$gender, data$order_type)
table_1

#         arduino only both lilypad only
# female           890   79          367
# male            7687  250          598
# unknown         1332   41           91

#---------------------------------------------------
# FROM THE ARTICLE:
# Table 2. Customer contingency table, US data only.

#               Arduino     Both    LilyPad
# Unknown       890         17      52 
# Male          6724        178     382 
# Female        810         61      279 

table_2 <- table(US_data$gender, US_data$order_type)
table_2

#         arduino only both lilypad only
# female           810   61          279
# male            6724  178          382
# unknown          890   17           52

# PC 1:
# (b) Run a chi-square-test on both tables. 
#      Compare to the paper (for Table 1, there doesn't seem to be one for Table 2). 
#      Did you reproduce it?

library(MASS)  
chisq.test(table_1)

# Pearson's Chi-squared test

# data:  table_1
# X-squared = 644.3, df = 4, p-value < 2.2e-16

# FROM THE ARTICLE:
# These differences were highly statistically significant 
# (chi-square(4, N=11335)=644, p<0.001)

# I would say this result is reproduced in the test just performed above.
#---------------------------------------------------

chisq.test(table_2)

# Pearson's Chi-squared test

# data:  table_2
# X-squared = 567.32, df = 4, p-value < 2.2e-16

# The results when restrict data to US only are just as significant.
#---------------------------------------------------

# PC 1:
# (c) Install the package "gmodels" and try to display the table using the function CrossTable()

library("gmodels")
CrossTable(table_1)
CrossTable(table_2)

#       Cell Contents: TABLE 1
#     |-------------------------|
#     |                       N |
#     | Chi-square contribution |
#     |           N / Row Total |
#     |           N / Col Total |
#     |         N / Table Total |
#     |-------------------------|
#     
#     
#     Total Observations in Table:  11335 
# 
# 
#         |  
#         | arduino only |         both | lilypad only |    Row Total | 
#           -------------|--------------|--------------|--------------|--------------|
# female  |          890 |           79 |          367 |         1336 | 
#         |       66.136 |       28.719 |      472.605 |              | 
#         |        0.666 |        0.059 |        0.275 |        0.118 | 
#         |        0.090 |        0.214 |        0.348 |              | 
#         |        0.079 |        0.007 |        0.032 |              | 
#           -------------|--------------|--------------|--------------|--------------|
# male    |         7687 |          250 |          598 |         8535 | 
#         |        6.830 |        2.936 |       48.879 |              | 
#         |        0.901 |        0.029 |        0.070 |        0.753 | 
#         |        0.776 |        0.676 |        0.566 |              | 
#         |        0.678 |        0.022 |        0.053 |              | 
#           -------------|--------------|--------------|--------------|--------------|
# unknown |         1332 |           41 |           91 |         1464 | 
#         |        2.127 |        0.964 |       15.106 |              | 
#         |        0.910 |        0.028 |        0.062 |        0.129 | 
#         |        0.134 |        0.111 |        0.086 |              | 
#         |        0.118 |        0.004 |        0.008 |              | 
#           -------------|--------------|--------------|--------------|--------------|
# Col Tot |         9909 |          370 |         1056 |        11335 | 
#         |        0.874 |        0.033 |        0.093 |              | 
#           -------------|--------------|--------------|--------------|--------------|
#     
#---------------------------------------------------
# 
#       Cell Contents: TABLE 2
#     |-------------------------|
#     |                       N |
#     | Chi-square contribution |
#     |           N / Row Total |
#     |           N / Col Total |
#     |         N / Table Total |
#     |-------------------------|
#     
#     
#     Total Observations in Table:  9393 
# 
# 
#         |  
#         | arduino only |         both | lilypad only |    Row Total | 
#           -------------|--------------|--------------|--------------|--------------|
# female  |          810 |           61 |          279 |         1150 | 
#         |       47.512 |       28.063 |      421.007 |              | 
#         |        0.704 |        0.053 |        0.243 |        0.122 | 
#         |        0.096 |        0.238 |        0.391 |              | 
#         |        0.086 |        0.006 |        0.030 |              | 
#           -------------|--------------|--------------|--------------|--------------|
#  male   |         6724 |          178 |          382 |         7284 | 
#         |        5.610 |        2.121 |       52.830 |              | 
#         |        0.923 |        0.024 |        0.052 |        0.775 | 
#         |        0.798 |        0.695 |        0.536 |              | 
#         |        0.716 |        0.019 |        0.041 |              | 
#           -------------|--------------|--------------|--------------|--------------|
# unknown |          890 |           17 |           52 |          959 | 
#         |        1.042 |        3.194 |        5.941 |              | 
#         |        0.928 |        0.018 |        0.054 |        0.102 | 
#         |        0.106 |        0.066 |        0.073 |              | 
#         |        0.095 |        0.002 |        0.006 |              | 
#           -------------|--------------|--------------|--------------|--------------|
# Col Tot |         8424 |          256 |          713 |         9393 | 
#         |        0.897 |        0.027 |        0.076 |              | 
#           -------------|--------------|--------------|--------------|--------------|
#     

# PC 1
# (c) It's important to be able to import tables directly into your word processor.
#     Can you export the output of your table?
#     I used the "xtable" package but I think that write.table() 
#     and Excel would do the job just as well.

write.table(table_1, "table1_eg")
write.table(table_2, "table2_eg")

# Created tables that could be opened in Excel as "space delimited" data.

#---------------------------------------------------

# PC2. At the Community Data Science Workshops we had two parallel afternoon sessions on Day 1. 
# In my session, there were 42 participants. In Tommy Guy's session, there were only 19. 
# The next week (Day 2), we asked folks to raise their hands if they had been in Tommy's session 
# (14 did ) and how many had been in mine (31 did). There was clearly attrition in both groups! 
# Was there more attrition in one group than another? Try answering this both with a test of 
# proportions (prop.test()) and with a chi-square test (chisq.test()). 
# Compare your answers. Is there convincing evidence that there is a dependence between instructor 
# and attrition?

# From: http://www.dummies.com/programming/r/how-to-create-a-two-way-data-table-with-r/
# trial <- matrix(c(34,11,9,32), ncol=2)
# colnames(trial) <- c('sick', 'healthy')
# rownames(trial) <- c('risk', 'no_risk')
# trial.table <- as.table(trial)

cdsw <- matrix(c(42, 31, 19,  14), ncol=2)
colnames(cdsw) <- c('Mako', "TGuy")
rownames(cdsw) <- c('day_1', 'day_2')
cdsw.table <- as.table(cdsw)
cdsw.table

#       Mako TGuy
# day_1   42   19
# day_2   31   14

prop.test(cdsw.table)

# 2-sample test for equality of proportions with continuity correction

# data:  cdsw.table
# X-squared = 1.6817e-30, df = 1, p-value = 1
# alternative hypothesis: two.sided 
# 95 percent confidence interval:
#    -0.1790573  0.1783287
# sample estimates:
#    prop 1    prop 2 
# 0.6885246 0.6888889 

chisq.test(cdsw.table)

# Pearson's Chi-squared test with Yates' continuity correction

# data:  cdsw.table
# X-squared = 3.0045e-31, df = 1, p-value = 1

# There does not seem to be a difference in attrition based on instructor.
# Both groups had ~69% of the original group return.
# Both tests done confirmed this conclusion (i.e. confirmed the null hypothesis)

# PC 3
# Download this dataset that was just published on "The Effect of 
#      Images of Michelle Obama’s Face on Trick-or-Treaters’ Dietary Choices: 
#      A Randomized Control Trial.
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2NJV2P

#      (a) Download and import the data into R. I needed to install the "readstata13" 
#          package to do so.

library("readstata13")

halloween <- read.dta13("Halloween.dta")
View(halloween)

#      (b) Take a look at the codebook if necessary. Recode the data on being presented 
#          with Michelle Obama's face and the data on whether or not kids picked up fruit. 
#          we'll leave it at that for now.

# Not sure what "recode" means here. The current column names seems fine, and the data is 0s and 1s,
# which also seems perfectly fine. Maybe, make a new dataset with JUST this data?

obama_fruit <- halloween[, 1:2]
View(obama_fruit)

#      (c) Do a simple test on whether or not the two groups are dependent. 
#          Be ready to report those tests. The results in the paper will use linear regression. 
#          Do you have a sense, from your reading, why your results using the coding material 
#          we've learned might be different?

obama_fruit_contigency_table <- table(obama_fruit)
colnames(obama_fruit_contigency_table) <- c('no_fruit', 'fruit')
rownames(obama_fruit_contigency_table) <- c('no_obama', 'obama')
obama_fruit_contigency_table <- t(obama_fruit_contigency_table)

#                     obama
# fruit         no_obama obama
#      no_fruit      593   322
#      fruit         185   122


prop.test(obama_fruit_contigency_table)

# 2-sample test for equality of proportions with continuity correction
# 
# data:  obama_fruit_contigency_table
# X-squared = 1.8637, df = 1, p-value = 0.1722
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#     -0.01595960  0.08993075
# sample estimates:
#     prop 1    prop 2 
# 0.7622108 0.7252252

# The proportions test finds no difference in the two groups 
# (those who chose fruit and those who didn't) based on whether or not 
# they were shown a picture of Michelle Obama. I think the result is different 
# because when a linear regression analysis is done, the goal is to fit a straight
# line to the data -- and it will do this. It may thus pick up a trend in the data,
# whereas just looking at the counts will miss this. Also, not sure exactly how they 
# are doing linear regression with this binary data... It is probably just one feature 
# in an additive linear model.

