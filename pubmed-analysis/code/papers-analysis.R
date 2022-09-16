# Script to access ncbi's pubmed database to 
# search and the plot number of papers by year


# Install packages if needed
# install.packages("pacman")


# Call packages
library("pacman")


# load (and install) packages using pacman
p_load("rentrez", "tidyverse", "glue", "scales", "cowplot")


## database exploration
# entrez_dbs() # check databases avialable
# entrez_db_summary(db = "pubmed") # summary about pubmedbibliographic record
# entrez_db_searchable(db = "pubmed") # Searchable fields for database 'pubmed'

## general exploration
# how many articles exist with the word bioinformatics
# entrez_search(db = "pubmed",
#                              term = "bioinformatics")


# To begin the exploration of my favorite scientific areas 
# we must define a range of publication years.
# Create a vector containing year range
year <- 2000:2021


# Search terms and years of publication are stored in an object, 
# just as if you were searching the PubMed page 
bioinfo_search <- glue("bioinformatics AND {year}[PDAT]")
crispr_search <- glue("crispr AND {year}[PDAT]")
all_db_search <- glue("{year}[PDAT]") # The purpose of this object is to find the total number of items in the year range

  
# search data using rentrez and map functions (from purr package)
# using tibble format
pubmed_search_counts <- tibble(year = year,                      # tibble creation using previous objects
                               bioinfo_search = bioinfo_search,
                               crispr_search = crispr_search,
                               all_db_search = all_db_search) %>% 
  mutate(bioinfo_total = map_dbl(bioinfo_search,                # create new columns containing the data on the total number of publications per year for each area 
                                 ~entrez_search(db = "pubmed",
                                                term = .x)$count),
         crispr_total = map_dbl(crispr_search,
                                ~entrez_search(db = "pubmed",
                                               term = .x)$count),
         all_db_search = map_dbl(all_db_search,
                                 ~entrez_search(db = "pubmed",
                                                term = .x)$count)
         ) # The map functions transform their input by applying a function to each element of a list or atomic vector and returning an object of the same length as the input.

# visualize tibble data
pubmed_search_counts


# transform the data obtained from the pubmed search in order to have a convenient format and plot it on ggplot2
year_count_data <- pubmed_search_counts %>% 
  select(year, bioinfo_total, crispr_total, all_db_search) %>% 
  pivot_longer(-year) # using pivot_longer (from wider data to longer data)


# max, min and values for scale_y_continuous 
max_y <- max(year_count_data$value) # get max value 

min_y <- min(year_count_data$value) # get min value

y_values <- seq(from = min_y,
                to = max_y,
                by = 250000)  # get values for breaks in scale_y_continuous

y_labels <- y_values %>% comma() # transform values for labels in scale_y_continuous


# year count plot
year_count_plot <- year_count_data %>%
  ggplot(aes(x = year,
             y = value,
             group = name,
             color = name)
         ) + 
  geom_line() +
  scale_y_continuous(limits = c(min_y, max_y),  # add min, max and values for scale_y_continuous
                     breaks = y_values,
                     labels = y_labels) +
  scale_color_manual(name = NULL,               # customize plot
                     label = c("PubMed papers",
                               "Bioinformatics papers",
                               "CRISPR papers"),
                     values = c("darkgreen", "darkred", "darkblue"),
                     breaks = c("all_db_search", "bioinfo_total", "crispr_total")) +
  labs( title = "The popularity of my favorite areas of scientific research",
        subtitle = "Total of articles by area",
        x = "Publication Year",
        y = "Total number of papers",
        caption = "Elaborated by Josue G \n Graphic inspired by Pat Schloss \n https://riffomonas.org/code_club/2022-05-12-pubs-per-year") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 15),
        axis.text = element_text(size = 13))

year_count_plot # check plot



# get percentage data
percentage_data <- pubmed_search_counts %>%
  select(year, bioinfo_total, crispr_total, all_db_search) %>% 
  mutate(per_bioinfo = (bioinfo_total / all_db_search) * 100,       # get percentage values
         per_crispr = (crispr_total / all_db_search) * 100) %>% 
  select(year, starts_with("per")) %>%                              # select data for plot
  pivot_longer(-year)                                               # pivot data


# plot percentage data
percentage_plot <- percentage_data %>% 
  ggplot(aes(x = year,
             y = value,
             group = name,
             color = name)
         ) + 
  geom_line() +
  scale_y_log10(limits = c(NA, 100),                             # transform y values into log10 form
                breaks = c(0.01, 0.1, 1, 10, 100),
                labels = c("0.01", "0.1", "1", "10" , "100")) +
  scale_color_manual(name = NULL,                               # customize plot
                     label = c("Bioinformatics papers",
                               "CRISPR papers"),
                     values = c("darkred", "darkblue"),
                     breaks = c("per_bioinfo", "per_crispr")) +
  labs( title = "The popularity of my favorite areas of scientific research",
        subtitle = "Percentage of articles with respect to the total number of articles in the PubMed database",
        x = "Publication Year",
        y = "Percentage (%)",
        caption = "Elaborated by Josue G \n Graphic inspired by Pat Schloss \n https://riffomonas.org/code_club/2022-05-12-pubs-per-year") +
    theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 15),
        axis.text = element_text(size = 13))

percentage_plot  # check plot


# join plots using plot_grid from cowplot

year_perc_plot <- plot_grid(year_count_plot +                   
                              theme(plot.title = element_blank(),           # remove titles, subtitles, captions and legend
                                    plot.subtitle = element_blank(),
                                    plot.caption = element_blank(),
                                    legend.position = "none"),
                            percentage_plot +                               # remove titles, subtitles, captions and legend
                              theme(plot.title = element_blank(),
                                    plot.subtitle = element_blank(),
                                    plot.caption = element_blank(),
                                    legend.position = "none"),
                            labels = c("Total number of papers in PubMed",  # Add subtitles as labels 
                                       "Percentage of papers in PubMed"),
                            label_x = c(0.2, 0.2),                         # Customize labels
                            label_size = 15
                            )


legend <- get_legend(year_count_plot +
                       theme(legend.position = "bottom"))                  # Extract legend and adjust it to fit the 2 plots together.


title <- ggdraw() +
  draw_label("The popularity of my favorite areas of scientific research", # Add title as ggdraw object
             size = 18,                                                    # Customize labels
             x = 0.5,
             hjust = 0.5)


year_perc_plot_2 <- plot_grid(title,                                      # Join all objects 
                              year_perc_plot,
                              legend,
                              ncol = 1,                                   # ncol = 1 indicates that the objects should be on top of each other in one column 
                              rel_heights = c(0.5, 2, 0.5)) +               # rel_heights controls the relative size of each object
  theme(plot.background = element_rect(fill = "white",
                                       colour = NA))

year_perc_plot_2 # check plot


# save grid plot

ggsave(filename = "figures/bioinfo_vs_crispr.png",
       plot = year_perc_plot_2,
       width=20, 
       height=10,
       dpi = 300)



























