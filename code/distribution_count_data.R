# code to explore the distribution of count data from sea cucumber transect surveys in southeast alaska
# Goal: to determine if assumptions of normallity are being met?  and if log normal is appropriate

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-03-02

# load ---
source('./code/packages.R')

# data ---
data <- read.csv('data/count_113_60s.csv', header = T, check.names = F) # added check.names = F to bring in year as column name without an X in front

# clean and reshape data ---
gather(data, "year", "n", 2:23) -> data2
data2 %>% na.omit -> data2 # remove rows with no data for simplification in analyses
head(data2)

# normality? ----
#Determine if the data is normally distributed (p should be >0.05)
# if p <0.05 data is not normally distributed
eda.norm <- function(x, ...)
{
  par(mfrow=c(2,2))
  if(sum(is.na(x)) > 0)
    warning("NA's were removed before plotting")
  x <- x[!is.na(x)]
  hist(x, main = "Histogram and non-\nparametric density estimate", prob = T)
  iqd <- summary(x)[5] - summary(x)[2]
  lines(density(x, width = 2 * iqd))
  boxplot(x, main = "Boxplot", ...)
  qqnorm(x)
  qqline(x)
  plot.ecdf(x, main="Empirical and normal cdf")
  LIM <- par("usr")
  y <- seq(LIM[1],LIM[2],length=100)
  lines(y, pnorm(y, mean(x), sqrt(var(x))))
  shapiro.test(x)
}

data2 %>% filter(year == 1994) -> data2_94
eda.norm(data2_94$n)

data2 %>% 
  


