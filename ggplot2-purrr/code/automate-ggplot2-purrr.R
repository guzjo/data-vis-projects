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

p_load("palmerpenguins",
       "ggplot2",     ## for plotting
       "ggforce",
       "purrr",       ## for iterative tasks
       "dplyr",       ## for data wrangling
       "glue",        ## to combine variable names into text
       "ggsci",       ## nice themes for plots
       "patchwork",   ## for multi-panel plots
       "prismatic")   ## idk



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
         filename = glue("../tmp-figures/{group}.plot.png"), 
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
    plot_annotation(title = group) + 
    plot_layout(widths = c(1, .05), heights = c(.1, 1))

  # plot_spacer function:
  # https://patchwork.data-imaginist.com/articles/guides/layout.html
  
  ## save multi-panel plot
  if(isTRUE(save)) {
    ggsave(all.plots,
           filename = glue("../tmp-figures/{group}.multiplot.png"),
           width = 6, height = 6)
  }
  
  return(all.plots)

  }

# Now we can apply the function to manufacturers line by line:
marginal.plot_manufacturer("Nissan", save = TRUE)


# Or we iterate over the vector of manufacturers with the {purrr} package.
map(groups, ~marginal.plot_manufacturer(.x, save = TRUE))


# If you wish to only save the visualizations but not plot them, use the walk() function:
walk(groups, ~marginal.plot_manufacturer(.x, save = TRUE))


# variables as inputs
# To wrap up, let’s consider the use case of exploring the data set. 
# We create a general function that works with any data set and two numeric variables. 
# Based on these three inputs, 
# the function generates a scatter plot with a linear fitting.


# In addition, we allow to define a variable to encode points 
# by color as well as to control the size and transparency of the points. 
# If the user passes a column for color encoding, 
# we either use (i) a categorical palette and linear fittings for each group 
# for qualitative variables or 
# (ii) a sequential palette with a single smoothing line for quantitative variables.


scatter.lm_plot <- function(data, 
                            var1, 
                            var2, 
                            pointsize = 2, 
                            transparency = .5, 
                            color = ""){
  
  ## check if inputs are valid
  if(!exists(substitute(data))) stop("data needs to be a dataframe")
  if(!is.data.frame(data)) stop("data needs to be a dataframe")
  if(!is.numeric(pull(data[var1]))) stop("Column var1 needs to be of type numeric, passed as string")
  if(!is.numeric(pull(data[var2]))) stop("Column var2 needs to be of type numeric, passed as string")
  if(!is.numeric(pointsize)) stop("pointsize needs to be of type numeric")
  if(!is.numeric(transparency)) stop("transparency needs to be of type numeric")
  if(color != "") { if(!color %in% names(data)) stop("Column color needs to be a column of data, passed as string") }
  
  
  g <- 
    ggplot(data, aes(x = !!sym(var1),
                     y = !!sym(var2))) +
    geom_point(aes(color = !!sym(color)),
               size = pointsize,
               alpha = transparency) +
    geom_smooth(aes(color = !!sym(color),
                    color = after_scale(prismatic::clr_darken(color, .3))),
                method = "lm", se = FALSE) +
    theme_minimal(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "top")
  
  if(color != ""){
    if(is.numeric(pull(data[color]))){
      g <- g + scale_color_viridis_c(direction = -1, end = 0.85) +
        guides(color = guide_colorbar(
          barwidth = unit(12, "lines"),
          barheight = unit(0.6, "lines"),
          title.position = "top"
        ))
    } else {
      g <- g + scale_color_brewer(palette = "Set2")
    }
  }
  
  return(g)
    
}

scatter.lm_plot(data = mpg,
                var1 = "displ",
                var2 = "hwy")


scatter.lm_plot_embraced <- function(data, 
                            var1, 
                            var2, 
                            color = NULL){
  
  v1 <- deparse(substitute(var1))
  v2 <- deparse(substitute(var2))
  v3 <- deparse(substitute(color))
  
  ## check if inputs are valid
  if(!exists(substitute(data))) stop("data needs to be a dataframe")
  if(!is.data.frame(data)) stop("data needs to be a dataframe")
  if(!v1 %in% names(data)) stop("Column var1 needs to be a column of data")
  if(!v2 %in% names(data)) stop("Column var2 needs to be a column of data")
  if(!is.numeric(pull(data[v1]))) stop("Column var1 needs to be of type numeric, passed as string")
  if(!is.numeric(pull(data[v2]))) stop("Column var2 needs to be of type numeric, passed as string")
  if(!v3 %in% c(names(data), "NULL")) stop("Column color needs to be a column of data")

  
  g <- 
    ggplot(data, aes(x = {{var1}},
                     y = {{var2}})) +
    geom_point(aes(color = {{color}}),
               alpha = 0.5) +
    geom_smooth(aes(color = {{color}}),
                method = "lm", se = FALSE) +
    theme_minimal(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "top")
  
  if(v3 != "NULL"){
    if(is.numeric(pull(data[v3]))){
      g <- g + scale_color_viridis_c(direction = -1, end = 0.85) +
        guides(color = guide_colorbar(
          barwidth = unit(12, "lines"),
          barheight = unit(0.6, "lines"),
          title.position = "top"
        ))
    } else {
      g <- g + scale_color_brewer(palette = "Set2")
    }
  }
  
  return(g)
  
}

scatter.lm_plot_embraced(data = mpg, var1 = displ, var2 = hwy, color = class)


# To iterate over the function, we have two options:

# we can fix one variable and pass the other as vector with map()
# we can pass two variables as vectors to var1 and var2 with map2()
# Let’s consider an example in which we vary both positional variables, 
# using the 2008 car fuel data:

map2(
  c("displ", "displ", "hwy"), 
  c("hwy", "cty", "cty"),
  ~scatter.lm_plot(
    data = mpg, 
    var1 = .x, 
    var2 = .y, 
    color = "cyl", 
    pointsize = 3.5
  )
)


# Now we are going to apply our function to a different data set, 
# the Palmer penguins to visualize x-y relationships per species. 
# We automatically iterate over all combinations of a set of chosen numeric variables: 
# we first generate a vector containing the column names of interest (names) 
# and then create a data frame with all possible combinations (names_set) 
# with the help of expand_grid() from the {tidyr} package.

## set up variables of interest
names <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
## ... and create all possible combinations
names_set <- tidyr::expand_grid(names, names)




# Using another mapping function from the {purrr} package called pmap(), 
# we can map over multiple arguments simultaneously:
pmap(
  names_set, 
  ~scatter.lm_plot(
    data = penguins, 
    var1 = .x, var2 = .y, color = "species"
  )
)


# the facet_matrix() functionality from the {ggforce} package. 
# Instead of passing variable names in the aesthetics, 
# we specify placeholders called .panel_x and .panel_y. 
# We then create our ggplot as usual by adding layers and scales.

# Finally, we specify the variables to use in the facet 
# inside the facet_matrix() component. 
# You can also specify specific layers for different areas (upper, diagonal, lower) 
# inside facet_matrix():

matrix_palmer <- ggplot(penguins, aes(x = .panel_x, 
                     y = .panel_y)) +
  geom_point(aes(color = species), alpha = 0.5) +
  geom_smooth(aes(color = species), method = "lm") +
  ggforce::geom_autodensity(aes(color = species, fill = after_scale(color)), alpha = 0.7) +
  scale_color_brewer(palette = "Set2", name = NULL) +
  ggforce::facet_matrix(vars(names), layer.lower = 2, layer.diag = 3)


ggsave(matrix_palmer,
       filename = glue("../tmp-figures/matrix_palmer.png"),
       width = 15, height = 10, dpi = 300, bg = "white")





