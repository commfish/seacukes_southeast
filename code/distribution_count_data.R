# code to explore the distribution of count data from sea cucumber transect surveys in southeast alaska
# Goal: to determine if assumptions of normallity are being met?  and if log normal is appropriate

# K.Palof    ADF&G 
# katie.palof@alaska.gov
# 2018-03-02

# load ---
source('./code/packages.R')
library(rcompanion)

# data -----
data <- read.csv('data/count_113_60s.csv', header = T, check.names = F) # added check.names = F to bring in year as column name without an X in front

# clean and reshape data -----
gather(data, "year", "n", 2:23) -> data2
data2 %>% na.omit -> data2 # remove rows with no data for simplification in analyses
head(data2)

# graphical representation ------
ggplot(data2, aes(n)) + geom_histogram(binwidth = 5) + facet_wrap (~ year)

# all years combined
dev.off()
# Normal 
plotNormalHistogram(data2$n)
qqnorm(data2$n)
qqline(data2$n, col = "red")


# data transformations -----------

# log transformation
data2 %>% mutate(n_001 = n + 0.0000001, n_log = log(n_001)) -> data2

plotNormalHistogram(data2$n_log)
qqnorm(data2$n_log)
qqline(data2$n_log, col = "red")

# square root transformation 
data2 %>% mutate(n_sqrt = sqrt(n)) -> data2
plotNormalHistogram(data2$n_sqrt)
qqnorm(data2$n_sqrt)
qqline(data2$n_sqrt, col = "red")

# cube root transformation
data2 %>% mutate(n_cube = sign(n) * abs(n^(1/3))) -> data2
plotNormalHistogram(data2$n_cube)
qqnorm(data2$n_cube)
qqline(data2$n_cube, col = "red")


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
eda.norm(data2_94$n) # W = 0.51792, p-value = 4.637e-07

data2 %>% filter(year == 1997) -> data2_97
eda.norm(data2_97$n) # W = 0.32057, p-value = 1.706e-07

data2 %>% filter(year == 2000) -> data2_00
eda.norm(data2_00$n) # W = 0.51506, p-value = 4.362e-07

data2 %>% filter(year == 2003) -> data2_03
eda.norm(data2_03$n) # W = 0.47634, p-value = 3.118e-07

data2 %>% filter(year == 2006) -> data2_06
eda.norm(data2_06$n) # W = 0.66672, p-value = 3.359e-05

data2 %>% filter(year == 2008) -> data2_08
eda.norm(data2_08$n) # W = 0.34975, p-value = 1.799e-08

data2 %>% filter(year == 2009) -> data2_09
eda.norm(data2_09$n) # W = 0.49681, p-value = 4.959e-09

data2 %>% filter(year == 2012) -> data2_12
eda.norm(data2_12$n) # W = 0.53012, p-value = 1.132e-08

data2 %>% filter(year == 2015) -> data2_15
eda.norm(data2_15$n) # W = 0.6236, p-value = 2.933e-08


