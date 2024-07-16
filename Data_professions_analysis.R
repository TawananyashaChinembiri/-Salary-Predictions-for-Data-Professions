library(tidyverse)

data = read.csv('Salary Prediction of Data Professions.csv')

# Bar graph showing the distribution of gender in the data industry 

sex_count <- data %>% 
  dplyr::count(SEX)

sex_plot <- ggplot(data = sex_count, aes(x = SEX, y = n)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'black') +
  labs (
    title = 'Gender distribution',
    subtitle = 'Bar graph showing the distribution of Genderd in the Data industry',
    x = 'Gender',
    y = 'Number of Employees'
    
  )

ggsave('gender plot.png', plot = sex_plot, height = 4, width = 6)

# distribution of age groups in the data industry

age_filtered <- data %>% 
  dplyr::filter(!is.na(AGE))

age_distribution <- ggplot(data = age_filtered, aes(x = AGE)) +
  geom_histogram(bins = 10, fill= 'blue', color = 'black') +
  labs (
    title = 'Age distribution',
    subtitle = 'Bar graph showing the distribution of different age groups in the data industry',
    x = 'Age', 
    y = 'Number of Data personnel'
  )
ggsave('Age distribution.png', plot = age_distribution, height = 4, width = 6)


# distribution of different designations in the data industry

designation_count <- data %>% 
  dplyr::count(DESIGNATION)

designation_dist <- ggplot(data = designation_count, aes(x = DESIGNATION, y = n)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'black') +
  labs(
    title = 'Designation distribution', 
    subtitle = 'Baar chart showing the distribution of Designations in the data industry',
    x = 'Designaitons',
    y = 'Number of Data employees'
  )
ggsave('Designation Distribution.png', plot = designation_dist, height = 4, width = 6)

# distribution of leaves used by the data professionals

