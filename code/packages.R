# packages to load for sea cucumber exploatory analysis
# Katie Palof
# katie.palof@alaska.gov
# 2018-2-13

# load -----
library(tidyverse)
library(reshape2)
library(scales)
library(extrafont)
#library(lubridate)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))