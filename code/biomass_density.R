# code to explore alternative methods to estimate sea cucumber density in southeast alaska
# this code will explore the current method (that uses Excel) and new methods that may fit the data better

# K.Palof    ADF&G 
# katie.palof@alaska.gov
#2018-02-13

# load ---
source('./code/packages.R')

#library(readxl)
 
# data -----
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
density_sum
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

all_summary %>% 
  mutate(lb_m = mean_d*mean_wt) %>% 
  mutate(var_lb_m = ((mean_wt^2)*var_d)+((mean_d^2)*var_wt)-(var_d*var_wt)) -> all_summary

# confidence intervals -----
all_summary %>% 
  mutate(se_lb_m = sqrt(var_lb_m/n_trans), sd_lb_m = sqrt(var_lb_m), cv = se_lb_m/lb_m, 
         cv_sd = sd_lb_m / lb_m) %>% 
  select(year, lb_m, var_lb_m, se_lb_m, sd_lb_m, cv, cv_sd) -> biomass_calc

biomass_calc %>%  # multiple ways to get one sided 90% confidence intervals
  mutate(l90_se = lb_m - (1.28*se_lb_m), 
         l90_sd = lb_m - (1.28*sd_lb_m), 
         l90_log_se = lb_m * exp(-1.28*sqrt(log(1+cv^2))), 
         l90_log_sd = lb_m * exp(-1.28*sqrt(log(1+cv_sd^2))),
         P_se = (1-((lb_m - l90_se)/lb_m)), 
         P_sd = (1-((lb_m - l90_sd)/lb_m)),
         P_log_se = (1-((lb_m - l90_log_se)/lb_m)), 
         P_log_sd = (1-((lb_m - l90_log_sd)/lb_m))) -> c_inter_all# one-sided 90% large sample size about 1.28
write.csv(c_inter_all, './results/confidence_intervals_current_113_60.csv')






