# Script to access ncbi's pubmed database to 
# search and the plot number of papers by year


# Install packages if needed
# install.packages("pacman")


# Call packages
library("pacman")


# load (and install) packages using pacman
p_load("rentrez",
       "tidyverse",
       "glue")


# databases
entrez_dbs()


# 
entrez_db_summary(db = "pubmed")


# 
entrez_db_searchable(db = "pubmed")


## general exploration
# term_search <- entrez_search(db = "pubmed",
#                              term = "bioinformatics")


# Create vector containing year range
year <- 1990:2021


# 
bioinfo_search <- glue("bioinformatics AND {year}[PDAT]")
crispr_search <- glue("crispr AND {year}[PDAT]")


# search data using rentrez and map functions (from purr package)
pubmed_search_counts <- tibble(year = year,
       bioinfo_search = bioinfo_search,
       crispr_search = crispr_search) %>% 
         mutate(bioinfo_total = map_dbl(bioinfo_search,
                                  ~entrez_search(db = "pubmed",
                                                 term = .x)$count),
                crispr_total = map_dbl(crispr_search,
                                       ~entrez_search(db = "pubmed",
                                                      term = .x)$count))


# visualize tibble data
pubmed_search_counts


# wrangle data
# using pivot_longer
# from wider data to longer data
graph_data <- pubmed_search_counts %>% 
  select(year, bioinfo_total, crispr_total) %>% 
  pivot_longer(-year)


# plot
graph_data %>%
  ggplot(aes(x = year,
             y = value,
             group = name,
             color = name)
         ) +
  geom_line()




