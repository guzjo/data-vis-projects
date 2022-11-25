# libraries ---------------------------------------------------------------

library("pacman")

p_load("tidyverse", "vroom", "scales", "cowplot")



# load data ---------------------------------------------------------------

prot_faa_lengths <- vroom("../testdata/prot-faa-report_02.csv")
# prot_fna_lengths <- vroom("test/results/reports/prot-fna-report_03.csv") contains cds sequences



# transform data -------------------------------------------------------------
faa_original_id <- word(prot_faa_lengths$id, sep = "\\_prot*")
# fna_original_id <- word(prot_fna_lengths$id, sep = "\\_cds*")

full_faa_lengths_df <- cbind(prot_faa_lengths, faa_original_id)
# full_fna_lengths_df <- cbind(prot_fna_lengths, fna_original_id)


# filter and summarise
faa_proteins_per_sequence_df <- full_faa_lengths_df %>% 
  select(faa_original_id, seq_length, type) %>% 
  group_by(faa_original_id, type) %>% 
  summarise(total_proteins = n())


#  order ids by protein number
faa_ordered <- faa_proteins_per_sequence_df %>%               # a partir del dataframe resumido
  arrange( total_proteins ) %>% 
  pull(faa_original_id)


# proteins per ID plot  -------------------------------------------------------------------
# define y axis values
max_y <- max(faa_proteins_per_sequence_df$total_proteins) # get max value 
min_y <- 0 # get min value

y_values <- seq(from = min_y,
                to = max_y,
                by = 10)  # get values for breaks in scale_y_continuous

y_labels <- y_values %>% comma() # transform values for labels in scale_y_continuous


faa_total_proteins_plot <- ggplot(faa_proteins_per_sequence_df, 
                                    aes(x = faa_original_id, 
                                        y = total_proteins,
                                        fill = type)) +
  scale_x_discrete(limits = faa_ordered) + 
  geom_col() +
  scale_y_continuous(limits = c(min_y, max_y),
                     breaks = y_values,
                     labels = y_labels) +
  scale_fill_manual(name = NULL,
                    values = "#FF9505") +
  theme_bw() +
  labs(title = "Total number of proteins in .fna and .faa files",
       x = "ID",
       y = "Total proteins",
       caption = "These data are the result of\nanalyzing 16 .faa files containing protein information\nfrom E. coli plasmids.") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        text = element_text(size = 14)) +
  coord_flip()

faa_total_proteins_plot



# heatmap plot ------------------------------------------------------------
# trated seq lengths for geom tile plot
taged_lengths <- full_faa_lengths_df %>% 
  mutate( tag = case_when( seq_length < 50 ~ "< 50",
                           seq_length < 100 ~ "50 - 100",
                           seq_length < 500 ~ "100 - 500",
                           seq_length < 1000 ~ "500 - 1,000",
                           seq_length < 2000 ~ "1,000 - 2,000",
                           seq_length >= 2000 ~ "> 2,000" ) ) %>% 
  mutate( order = case_when(seq_length < 50 ~ "1",
                            seq_length < 100 ~ "2",
                            seq_length < 500 ~ "3",
                            seq_length < 1000 ~ "4",
                            seq_length < 2000 ~ "5",
                            seq_length >= 2000 ~ "6"))

taged_lengths2 <- taged_lengths %>% 
  group_by(faa_original_id, tag, order) %>% 
  summarise(groups = n()) %>% 
  pivot_wider(data = .,
              id_cols = c(tag, order),
              names_from = faa_original_id,
              values_from = groups) %>% 
  arrange(desc(order)) %>% 
  select(-order)


# replace NAs with 0 in the table
taged_lengths2[is.na(taged_lengths2)] <- 0

# from wide to long
taged_lengths3 <- pivot_longer( data = taged_lengths2,
                        cols = -tag,
                        names_to = "id",
                        values_to = "seqs" )

# define y axis order
taged_lengths4 <- pivot_wider( data = taged_lengths3, 
                      id_cols = id, names_from = tag, values_from = seqs  ) 


# create an order for the print table
tmptable <- taged_lengths4

tmptable <- tmptable %>%
  select( -id )

rownames( tmptable ) <- taged_lengths4$id
     
order <- rowSums( tmptable ) %>% sort( decreasing = T ) %>% names( )


# define x axis label and order
orderx <- c( "< 50", "50 - 100", "100 - 500",
             "500 - 1,000", "1,000 - 2,000", "> 2,000" )
labelx <- c( "less than 50 aa", "50 to 100 aa", "100 to 500 aa", 
             "500 to 1K aa", "1k to 2k aa", "More than 2k aa")

# try a heatmap
lengths_heat <- ggplot( data = taged_lengths3 %>% filter( seqs > 0 ),
                        mapping = aes( x = tag,
                                       y = id,
                                       fill = seqs,
                                       label = seqs ) ) +
  geom_tile( color = "black" ) +
  geom_text( ) +
  scale_x_discrete( limits = rev( orderx ),
                    labels = labelx,
                    position = "top") +
  scale_y_discrete( limits = rev( order ) ) +
  scale_fill_gradient( low = "white",
                       high = "darkorange2" ) +
  labs( title = "Number of proteins present in .faa files grouped by length of aa sequence.",
        caption = "These data are the result of\nanalyzing 16 .faa files containing protein information\nfrom E. coli plasmids.",
        x = "seq length",
        fill = "seq length" ) +
  theme_light( ) +
  theme_void( ) +
  theme( axis.title.y = element_blank( ),
         axis.text.y = element_text( face = "bold" ),
         axis.text.x = element_text( angle = 90 ),
         axis.title.x = element_text( face = "bold" ),
         legend.position = "none" ,
         plot.title = element_text(hjust = 0.5),
         text = element_text(size = 14))

# vis
lengths_heat



# join plots --------------------------------------------------------------
# using plot_grid 
protein_plots <- plot_grid(faa_total_proteins_plot, lengths_heat,
                           labels = "AUTO") +
  theme(plot.background = element_rect(fill = "white")) 

# vis
protein_plots



# save relevant csv -------------------------------------------------------
write.csv(faa_proteins_per_sequence_df,
          "test/results/reports/proteins-per-id-report_04.csv", 
          row.names = FALSE, quote = FALSE)

write.csv(full_faa_lengths_df,
          "test/results/reports/faa-lengths-report_05.csv", 
          row.names = FALSE, quote = FALSE)

# write.csv(full_fna_lengths_df,
#           "test/results/reports/fna-lengths-report_06.csv", 
#           row.names = FALSE, quote = FALSE)



# save plots --------------------------------------------------------------

ggsave(filename = "prot-seq-analysis.png",
       plot = protein_plots,
       path = "../figures/",
       width=24, 
       height=12,
       dpi = 300)

