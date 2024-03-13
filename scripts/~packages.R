# File to load all packages to replicate the paper 

if (!require("pacman")) install.packages("pacman")

pacman::p_load(DescTools, tidyr, dplyr, readxl, ggplot2, lubridate, zoo, stringr,
               xtable, stargazer, stats, Hmisc, plm, BMS, Cairo, reshape2, 
              writexl, fastDummies, extrafont)
