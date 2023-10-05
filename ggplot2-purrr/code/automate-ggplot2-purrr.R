# From CS post


# We are going to visualize relationships for different numeric variables of the mpg data set 
# which features fuel economy data of popular car models for 
# different years, manufacturers, and car types. In this tutorial, 
# we are using only data from 2008.


# We are going to visualize relationships 
# for different numeric variables of the mpg dataset 
# which features fuel economy data of popular car models for 
# different years, manufacturers, and car types. In this tutorial, we are using only data from 2008.

# For some data-wrangling steps, 
# we make use of the {dplyr} package. 
# To visualize the data, we use the packages {ggplot2} and {patchwork}. 
# We will also use some functions of other packages, 
# namely {tidyr}, {stringr}, {prismatic} and {ggforce} which we address via the namespace.


# load packages -----------------------------------------------------------

library("pacman")     ## for load multiple packages in one function

p_load("ggplot2",     ## for plotting
       "purrr",       ## for iterative tasks
       "dplyr",       ## for data wrangling
       "glue",        ## to combine variable names into text
       "ggsci",       ## nice themes for plots
       "patchwork")   ## for multi-panel plots



# Customize general plots style -------------------------------------------
## customize plot style
theme_set(theme_minimal(base_size = 18, 
                        base_family = "Anybody"))

theme_update(
  # axis.title.x = element_text(margin = margin(12, 0, 0, 0), color = "grey30"),
  # axis.title.y = element_text(margin = margin(0, 12, 0, 0), color = "grey30"),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(color = "grey45", fill = NA, linewidth = 1.5),
  # panel.spacing = unit(.9, "lines"),
  # strip.text = element_text(size = rel(1)),
  plot.title = element_text(face = "bold", hjust = .5),
  # plot.title.position = "plot"
)



# load dataset ------------------------------------------------------------


## adjust data set
mpg <-
  ggplot2::mpg  %>%
  filter(year == 2008) %>% 
  mutate(manufacturer = stringr::str_to_title(manufacturer))

# Convert string to upper case, lower case, title case, or sentence case
# str_to_title() converts to title case, where only the first letter of each word is capitalized.
# https://stringr.tidyverse.org/reference/case.html



# data wrangling and plots ------------------------------------------------

# Let’s visualize the 2008 car fuel data and explore the relationship 
# of displacement and highway miles per gallon per manufacturer.

p1 <- 
  ggplot(data = mpg, 
         mapping = aes(x = hwy, 
                       y = displ)) +
  scale_x_continuous(breaks = 2:8*5) +
  labs(x = "Highway miles per gallon", 
       y = "Displacement in litres")

p1 +
  geom_point(aes(color = manufacturer), 
             alpha = .5, 
             size = 3)
  


# Two issues arise here:
# too many categories: the use of color encoding is not useful 
# given the large number of manufacturers (a usual recommended limit of categorical colors is 5-8)
# too many data points: the number of observations, 
# especially with identical value combinations, leads to* overplotting and color mixing


# A common solution to circumvent these issues are small multiples:

p1 + 
  geom_point(alpha = .5, size = 2) +
  facet_wrap(~ manufacturer, ncol = 3)


# While it solves the mentioned issues, 
# the resulting small multiple is likely too dense to effectively communicate the relationships
# for each manufacturer. Due to the large number of manufacturers, each plot also becomes rather small.

# To focus on a single manufacturer, we may decide to create a plot for a subset of the data. 
# To allow for comparison, we also plot all other car models as smaller, grey circles.

p1 +
  ## filter for manufacturer of interest
  geom_point(data = filter(mpg, manufacturer == "Audi"), 
             color = "#007cb1", alpha = .5, size = 4) +
  ## add shaded points for other data
  geom_point(data = filter(mpg, manufacturer != "Audi"), 
             shape = 1, color = "grey45", size = 2) +
  ## add title manually
  ggtitle("Audi")

# To communicate the relationship for all manufacturers, 
# e.g. a dedicated section for each in a report or revealing the results step by step 
# in a presentation, we now need to repeat the same code 15 times
# and replace the filter conditions and title. Or we iterate the process.

# create the function
plot_manufacturer <- function(group) {
  # check if inpu is valid
  if (!group %in% mpg$manufacturer) 
    stop("Manufacturer not listed in the data set.")
  
  plot <- ggplot(mapping = aes(x = hwy,
                       y = displ)) +
    geom_point(data = filter(mpg, manufacturer
                             %in% group),
               color = "darkblue", alpha = 0.5, size = 4) +
    geom_point(data = filter(mpg, !manufacturer %in% group),
               shape = 1, color  = "grey45", size = 2) +
    scale_x_continuous(breaks = 2:8*5) +
    labs(x = "Highway gallons", y = "Displacement", 
         title = group)
  
  ggsave(plot = plot, 
         filename = glue("{group}.plot.png"), 
         width = 8, height = 8, dpi = 400, 
         bg = "white")
}


## run function for specific subsets
plot_manufacturer("Audi")
plot_manufacturer("Chevrolet")



# using purrr to automate plot creation
# The final step is simple: we use the map() function from the {purrr} package 
# to iterate over a vector of manufacturers and 
# pass the elements to our plot_manufacturer() function:

# extract group names into a vector 
groups <- unique(mpg$manufacturer)

# apply map function with the plot_manufacturer function for each group
map(groups, ~plot_manufacturer(group = .x))



# more complex
# Let’s consider a more complex visualization which consists of multiple plots. 
# As before, we first check the input and filter the data. 
# Afterwards, we create three plots and combine them with the help of the {patchwork} package.

marginal.plot_manufacturer <- function(group, save = FALSE) {
  
  ## check if input is valid
  if (!group %in% mpg$manufacturer) stop("Manufacturer not listed in the data set.")
  if (!is.logical(save)) stop("save should be either TRUE or FALSE.")
  
  ## filter data
  data <- filter(mpg, manufacturer %in% group)
  
  ## set x and y limits
  lims_x <- range(mpg$hwy)
  lims_y <- range(mpg$displ)
  
  ## do scatter plot
  main.scatter <- ggplot(data, aes(x = hwy,
                                   y = displ,
                                   color = class)) +
    geom_point(size = 4, alpha = 0.5) +
    scale_x_continuous(limits = lims_x, breaks = 2:8*5) +
    scale_y_continuous(limits = lims_y) +
    scale_color_d3(name = NULL) + 
    labs(x = "Highway miles per gallon", y = "Displacement") +
    theme(legend.position = "bottom")
  
  
  ## do boxplots
  right.bp <- ggplot(data, aes(x = manufacturer, y = displ)) +
    geom_boxplot(linewidth = 0.7, color = "grey45") +
    scale_y_continuous(limits = lims_y, guide = "none", name = NULL) +
    scale_x_discrete(guide = "none", name = NULL) +
    theme_void()
  
  top.bp <- ggplot(data, aes(x = hwy, y = manufacturer)) +
    geom_boxplot(linewidth = .7, color = "grey45") +
    scale_x_continuous(limits = lims_x, guide = "none", name = NULL) +
    scale_y_discrete(guide = "none", name = NULL) +
    theme_void()
  
  ## combine plots
  all.plots <- top.bp + plot_spacer() + main.scatter + right.bp + 
    plot_annotation(title = group) 
  # + 
  #   plot_layout(widths = c(1, .05), heights = c(.1, 1))

  # plot_spacer function:
  # https://patchwork.data-imaginist.com/articles/guides/layout.html
    
    
    
    
}



