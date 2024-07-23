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

leaves_plot <- ggplot(data = data, aes(x = LEAVES.USED)) +
  geom_histogram(bins = 5, fill= 'blue', color = 'black') +
  labs(
    title = 'Distribution of Leaves',
    subtitle = 'Histogram showing the distribution of Leaves for Data Professional',
    x = 'Leave DAYS',
    y = 'Number of Employees'
    
  )

ggsave('Leaves plot.png', plot = leaves_plot, height = 4, width = 6)


# distribution of units in the data industry
units_count <- data %>% 
  dplyr::count(UNIT)

units_plot <- ggplot(data = units_count, aes(x = UNIT, y = n)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'black') +
  labs (
    title = 'Units distribution',
    subtitle = 'Bar graph showing the distribution of Units in the Data Industry',
    x = 'UNITS',
    y = 'Number of Employees'
  )
ggsave('Units distribution.png', plot = units_plot, height = 4, width = 6)

# distribution of Salaries in the data industry 
salary_plot <- ggplot(data = data, aes(x = SALARY)) +
  geom_histogram(bins = 5 , fill = 'blue', color = 'black') +
  labs (
    title = 'Salary distibution',
    subtitle = 'Bar chart showing the distribution of Salaries in the data industry',
    x = 'SALARY' ,
    y = 'Number of employees'
  )
ggsave('Salary plot.png', plot = salary_plot, height = 4, width = 6)

# distribution of Data professions rating
rating_count <- data %>% 
  dplyr::count(RATINGS)

rating_plot <- ggplot(data = rating_count, aes(x = RATINGS, y = n)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'white') +
  labs(
    title = 'Ratings Distribution', 
    subtitle = 'Histograms showing the distribution of Data Professions rating', 
    x = 'Rating', 
    y = 'Number of Data Professions'
  )
ggsave('Rating plot.png', plot = rating_plot, height = 4, width = 6)

# distribution of Past experiences of the Data Professionals
exp_count <-data %>% 
  dplyr::count(PAST.EXP)

exp_plot <- ggplot(data = exp_count, aes(x = PAST.EXP,  y = n)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'white') +
  labs (
    title = 'Experience distribution',
    subtitle = 'Bar graph showing the distribution of Data professionals Industry',
    x = 'Past Experience',
    y = 'Number of Employees'
  )

ggsave('Experience Plot.png', plot = exp_plot, height = 4, width = 6)

# The deeper analysis on Data Professionals information

# The designations that are paid more

summarised_data <- data %>% 
  dplyr::group_by(DESIGNATION) %>% 
  dplyr::summarise(mean_salary = mean(SALARY))

designation_based_salary <- ggplot(data = summarised_data, aes(x = DESIGNATION, y = mean_salary)) +
  geom_bar(stat = 'identity', fill = 'blue', color = 'white') +
  labs(
    title = 'DESIGNATION BASED SALARIES',
    subtitle = 'Bar graph showing the mean salaries of different designations',
    x = 'DESIGNATONS',
    y = 'Mean Salaries'
  )
ggsave('Designations mean salaries.png', plot = designation_based_salary, height =4, width = 6)


# which age groups are paid more
age_summaried_data <- data %>% 
  dplyr::group_by(AGE) %>% 
  dplyr::summarise(age_mean_salary = mean(SALARY))

age_based_salary <- ggplot(data = age_summaried_data, aes(x = AGE,y = age_mean_salary)) +
  geom_histogram(stat = 'identity', fill = 'blue', color = 'black') +
  labs(
    title ='AGE BASED MEAN SALARIES' ,
    subtitle = 'Bar grapg showing the mean salaries of different age groups',
    x= 'AGE GROUPS',
    y = 'Mean Salaries'
  )
ggsave('Age based salary.png', plot = age_based_salary, height = 4, width = 6)


# which past experience is paid more

exp_avg_pay <- data %>% 
  dplyr::group_by(PAST.EXP) %>% 
  dplyr::summarise(avg_salary = mean(SALARY))

exp_based_salaries <- ggplot(data = exp_avg_pay, aes(x = PAST.EXP, y = avg_salary)) +
  geom_bar(stat = 'identity', fill = 'darkblue', color = 'lightblue') +
  labs(
    title = 'EXPERIENCE BASED SALARIES',
    subtitle = 'Bar graph showing the Average Baesd Mean Salaries',
    x = 'EXPERIENCE',
    y = 'MEAN SALARIES'
  )
ggsave('Experience based mean Salaries.png', plot = exp_based_salaries, height = 4, width = 6)

# which units are paid more

unit_based_salaries <- data %>% 
  dplyr::group_by(UNIT) %>% 
  dplyr::summarise(avg_unit_salary = mean(SALARY))

unit_based_salary_plot <- ggplot(data = unit_based_salaries, aes(x = UNIT, y = avg_unit_salary)) +
  geom_bar(stat = 'identity', fill= 'darkblue', color = 'black') +
  labs(
    title = 'UNIT BASED SALARIES',
    subtitle = 'Bar graph shoWing the average salaries for different UNITS',
    x = 'UNIT',
    y = 'AVERAGE SALARY'
  )
ggsave('Unit based salary.png', plot = unit_based_salary_plot, height = 4, width = 6)