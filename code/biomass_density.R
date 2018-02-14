# code to explore alternative methods to estimate sea cucumber density in southeast alaska
# this code will explore the current method (that uses Excel) and new methods that may fit the data better

# K.Palof    ADF&G 
# katie.palof@alaska.gov
#2018-02-13

# load ---
source('./code/packages.R')
#library(readxl)
 
# data ---
data <- read.csv('data/count_113_60s.csv')


# clean and reshape data ---
