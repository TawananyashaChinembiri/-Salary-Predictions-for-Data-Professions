library(tidyverse)

data = read.csv('Salary Prediction of Data Professions.csv')

data %>% dplyr::group_by(sex) %>% 
  dplyr::count(sex)

