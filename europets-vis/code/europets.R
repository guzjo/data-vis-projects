
# libraries ---------------------------------------------------------------

library("pacman")

p_load("vroom","tidyverse", "scales")




# load data ---------------------------------------------------------------

europets <- vroom("../testdata/europets.csv")



# transform data ----------------------------------------------------------

transformed <- europets %>% 
  pivot_longer(cols = -Country,
               names_to = "pet",
               values_to = "population")

pet_group <- rep(c(1:6), times = 28)
country_group <- rep(c(1:28), each = 6)
grouped_data <- cbind(transformed, country_group, pet_group)


transformed2 <- grouped_data %>%
  group_by(country_group) %>% 
  arrange(-population) %>% 
  unite("ctry_pet", country_group, pet_group, sep = "_", remove = F) %>% 
    data.frame() %>% 
    mutate(ctry_pet = factor(ctry_pet, levels = ctry_pet)) %>% 
  drop_na()



# required objects --------------------------------------------------------

col_pal.v <- c( "Aquatic_Pets" = "#2E86AB",
                    "Bird_Population" = "#A23B72",
                    "Cat_Population" = "#F18F01",
                    "Dog_Population" = "#C73E1D",
                    "Reptile_Population" = "#069E2D",
                    "SmallMammal_Population" = "#3B1F2B")
  

# pyramid plot ------------------------------------------------------------
max_y <- max(transformed2$population) # get max value
min_y <- 0

y_values <- seq(from = min_y,
                to = max_y,
                by = 1000000)  # get values for breaks in scale_y_continuous

y_labels <- y_values %>% comma() # transform values for labels in scale_y_continuous


plot1 <- transformed2 %>% 
  ggplot(aes(x = ctry_pet, 
             y = population,
             fill = pet)) + 
  geom_col() +
  scale_y_continuous(limits = c(min_y, max_y),
                     breaks = y_values,
                     labels = y_labels) +
  coord_flip()



plot1


plot2 <- plot1 +
  facet_wrap(~pet, scales = "free_y") +
  scale_x_discrete(breaks = transformed2$ctry_pet,
                   labels = transformed2$Country) +
  scale_fill_manual(values = col_pal.v) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



plot2






