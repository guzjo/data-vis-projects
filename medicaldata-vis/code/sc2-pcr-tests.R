# Script to visualize a dataset containing information 
# about pcr tests for sars-cov-2 in a hospital.


# Install packages if needed
# install.packages("pacman")


# Call packages
library("pacman")


# load (and install) packages using pacman
p_load("medicaldata", "tidyverse")


# medicaldata package
# This is a data package with 17 medical datasets for 
# teaching Reproducible Medical Research with R
# https://higgi13425.github.io/medicaldata/


# let's explore medicaldata package
data(package = "medicaldata")


# the dataset to be analyzed is called covid_testing
# add the dataset to an object
covid_data <- covid_testing


# Since there is a lot of data, we select only the columns that interest us
filtered_data <- covid_data %>% 
  select(gender,
         age,
         result)


# From the selected columns, we filter the data, 
# so that we only have tests with positive results, 
# and people with age between 0 and 100 years.
filtered_data_2 <- filtered_data %>% 
  filter(result == "positive",
         age > 0 & age < 100)


# Our objective will be to visualize the age distribution of people with 
# positive pcr tests using a double density plot.

# We'll create two objects containing the information 
# of the positive tests of the people by gender
male_data <- filtered_data_2 %>% 
  filter(gender == "male")

female_data <- filtered_data_2 %>% 
  filter(gender == "female")


# Make base density plot
base_density_plot <- ggplot() +
  geom_density(data = female_data,     # first plot female data
               mapping = aes(x = age,
                             y = ..density..),  # make density for age data
               fill = "darkred",
               alpha = 0.5) +                # density color and transparency
  geom_label(aes(x = 50, y = 0.015,          # add label for data
                 label = "Female data"),
             color = "darkred")

base_density_plot # check plot

# join male data and customize plot
base_density_plot_2 <- base_density_plot +
  geom_density(data = male_data,           # add male data
               mapping = aes(x = age,
                             y = - ..density..), 
               fill = "darkblue", 
               alpha = 0.5) +
  geom_label(aes(x = 50, y = -0.015,
                 label = "Male data"),
             color = "darkblue") +
  scale_x_continuous(limits = c(0, 100),        # customize x axis
                     breaks = seq(from = 0,
                                  to = 100,
                                  by = 10)) +
  scale_y_continuous(labels = abs)             # make absolute values for y axis

base_density_plot_2 # check plot

# customize plot
final_density_plot <- base_density_plot_2 +
  labs(title = "Age of people with positive PCR tests to SARS-CoV-2",
       subtitle = "Tests conducted at Children's Hospital of Pennsylvania (CHOP) in 2020",
       caption = "Elaborated by Josue G \nData source: package {medicaldata} \nAvailable at: https://github.com/higgi13425/medicaldata",
       x = "Age of people",
       y = "Density") +           # ADD TITLE, SUBTITLE, AXIS NAMES
  theme_classic() +               # use theme_classic
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 15),
        axis.text = element_text(size = 13))  # Customize all text 

final_density_plot # check plot
 

# save plot
ggsave( plot = final_density_plot,           
        filename = "../figures/positive_pcr_ddensity.png",  
        width = 10,               
        height = 8,
        dpi = 300)             











