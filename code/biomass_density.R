# code to explore alternative methods to estimate sea cucumber density in southeast alaska
# this code will explore the current method (that uses Excel) and new methods that may fit the data better

# K.Palof    ADF&G 
# katie.palof@alaska.gov
#2018-02-13

# load ---
source('./code/packages.R')
#library(readxl)
 
# data ---
data <- read.csv('data/count_113_60s.csv', header = T, check.names = F) # added check.names = F to bring in year as column name without an X in front

# weight data ---
wt_data <- read.csv('data/wt_113_60s.csv', header = F, check.names = F)

# clean and reshape data ---
gather(data, "year", "n", 2:23) -> data2
data2 %>% na.omit -> data2 # remove rows with no data for simplification in analyses
head(data2)

# reshape data to have row for avg weight and n - sample size 
col.names <- c("transect", tail(paste(t(wt_data[1,]),t(wt_data[2,]),sep="."),n=20))
df <- wt_data[-(1:2),]
df <- setNames(df, col.names)
df %>% 
  gather(year, value, -transect) %>% 
  mutate(year = as.character(year)) %>% 
  separate(year, c("year", "type")) %>% 
  mutate(year = as.numeric(year), value = as.numeric(value)) -> wt_data2

wt_data2 %>% 
  filter(type == "avg") %>% 
  na.omit-> wt_data_avg

# mean count ----
data2 %>% 
  group_by(year) %>% 
  summarise(mean_n = mean(n, na.rm = TRUE), var_n = var(n, na.rm = TRUE)) ->data2_sum
# formulas according to 2012 document - ADFG_FDS_12_26 =_Hebert
data2 %>% 
  group_by(year) %>% 
  summarise(n_trans = n(), mean_d = mean(n, na.rm = TRUE), 
            var_d = var(n, na.rm = TRUE)/n_trans) %>% 
  mutate(se_d = sqrt(var_d/n_trans), sd_d = sqrt(var_d), 
         ps_se = (1- (1.31*(se_d)/mean_d))*100, ps_sd = (1- (1.31*(sd_d)/mean_d))*100 ) -> density_sum

# mean weight -----
# weight is in grams and converted to pounds 
wt_data_avg %>%     # data just with avg weights removed sample sizes for these weights
  group_by(year) %>% 
  summarise(n_trans_wt = n(), mean_wt = mean(value, na.rm = TRUE)/454, 
            var_wt = var(value, na.rm = TRUE)/n_trans_wt/454^2) %>% 
  mutate(se_wt = sqrt(var_wt/n_trans_wt), sd_wt = sqrt(var_wt), 
         ps_se = (1- (1.38*(se_wt)/mean_wt))*100, ps_sd = (1- (1.38*(sd_wt)/mean_wt))*100 ) -> weight_sum


# density and weight -----
density_sum %>% 
  mutate(year = as.numeric(year)) %>% 
  select(year, n_trans, mean_d, var_d) -> temp1

weight_sum %>% 
  select(year, n_trans_wt, mean_wt, var_wt) -> temp2

temp1 %>% 
  left_join(temp2) -> all_summary
# log normal variance -----

# 



