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


# clean and reshape data ---
gather(data, "year", "n", 2:23) -> data2
data2 %>% na.omit -> data2 # remove rows with no data for simplification in analyses
head(data2)

# mean count ----
data2 %>% 
  group_by(year) %>% 
  summarise(mean_n = mean(n, na.rm = TRUE), var_n = var(n, na.rm = TRUE)) ->data2_sum
# formulas according to 2012 document - ADFG_FDS_12_26 =_Hebert
data2 %>% 
  group_by(year) %>% 
  summarise(n_trans = n(), mean_d = mean(n, na.rm = TRUE), 
            var_d = var(n, na.rm = TRUE)/n_trans) %>% 
  mutate(se_d = sqrt(var_d/n_trans), ps = 1- (1.31*(se_d)/mean_d) ) -> density_sum


