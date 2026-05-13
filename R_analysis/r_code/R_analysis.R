# RStudio user: Ben Hodgson (Student number: 202317270)
# Date last worked on:
# 7/5/2026

# Contents for navigation: 

# 1) Clean dataset and descriptive stats
# 2) Venn diagram
# 3) Colour scheme and plant labels
# 4) Pairwise Sorensen similarly index
# 5) Chi-squared
# 6) Residuals
# 7) Fisher's exact (make table)
# 8) Network
# 9) Matrices
# 10) Bubble plot

# Make these packages are their associated functions available for use in this 
# script: 
library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(vegan)
library(ggplot2)
library(showtext)
library(sysfonts)
library(tidyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(bipartite)
library(tibble)
library(ggalluvial)
library(ggVennDiagram)
library(VennDiagram)
library(grid)
library(lme4)
library(purrr)
library(ggrepel)
library(forcats)
library(pdftools)

# Add fonts for plot
font_add("Segoe UI", 
         regular = "C:/Windows/Fonts/segoeui.ttf",
         bold = "C:/Windows/Fonts/segoeuib.ttf",
         italic  = "C:/Windows/Fonts/segoeuii.ttf")
showtext_auto()

# Clear R's brain
rm(list = ls())

# Load dataset
dat_all_merged <- read_csv(
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/GitHub folder/inat_data/raw_data/dat_all_merged/dat_all.csv",
  show_col_types = FALSE
)


### ============================================================================
# (1) Clean dataset and descriptive stats
### ============================================================================


### -------------------------------------
# (1A) Clean dataset
### -------------------------------------

# Reorders fields columns 
# Keeps only interactions
# Remove family-level plant IDs ending in -aceae
# Create plant genus from genus/species-level plant names
# Combine bee subspecies
# Remove records without a license
# Classify honeybees as honey or wild
# Remove unused columns

dat <- dat_all_merged %>%
  filter(!is.na(license), license != "") %>%
  select(
    -source_file,
    field_pollination_interaction,
    field_name_of_associated_plant,
    everything()
  ) %>%
  mutate(
    field_pollination_interaction =
      str_squish(field_pollination_interaction),
    
    field_name_of_associated_plant =
      str_squish(field_name_of_associated_plant)
  ) %>%
  filter(
    !is.na(field_pollination_interaction),
    str_to_lower(field_pollination_interaction) != "no"
  ) %>%
  filter(
    !is.na(field_name_of_associated_plant),
    field_name_of_associated_plant != "",
    !str_detect(
      str_to_lower(field_name_of_associated_plant),
      "aceae$"
    )
  ) %>%
  mutate(
    plant = word(field_name_of_associated_plant, 1),
    bee = scientific_name
  ) %>%
  mutate(
    bee = str_squish(bee),
    plant = str_squish(plant),
    
    bee = ifelse(
      str_detect(str_to_lower(bee), "apis\\s+mellifera"),
      "Apis mellifera",
      bee
    ),
    
    bee = ifelse(
      str_detect(str_to_lower(bee), "bombus\\s+terrestris"),
      "Bombus terrestris",
      bee
    ),
    
    is_honeybee = bee == "Apis mellifera",
    
    bee_type = ifelse(
      is_honeybee,
      "honey",
      "wild"
    ),
    
    bee_genus = word(bee, 1)
  ) %>%
  select(
    city,
    year,
    plant,
    bee,
    is_honeybee,
    bee_type,
    bee_genus
  )

# Save cleaned, analysis ready dataset
write_csv(
  dat,
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/GitHub folder/inat_data/processed_data/cleaned_dat.csv"
)


### -------------------------------------
# (1B) Descriptive stats
### -------------------------------------

# Bee frequency
bee_frequency <- dat %>% count(bee, sort = TRUE)

# Bee genera frequency
bee_genera <- dat %>% count(bee_genus, sort = TRUE)

# Plant genus frequency
plant_frequency <- dat %>% count(plant, sort = TRUE)

# Interaction frequencies
interaction_frequency <- dat %>% count(bee, plant, sort = TRUE)

# Bee species richness
bee_richness <- dat %>%
  summarise(richness = n_distinct(bee))

# Bee genus richness
bee_genus_richness <- dat %>%
  summarise(richness = n_distinct(bee_genus))

# Plant genus richness
plant_richness <- dat %>%
  summarise(richness = n_distinct(plant))

# Wild bee species richness by city
wild_bee_richness_city <- dat %>%
  filter(bee != "Apis mellifera") %>%
  group_by(city) %>%
  summarise(
    wild_bee_richness = n_distinct(bee),
    .groups = "drop"
  ) %>%
  arrange(desc(wild_bee_richness))

# Plant genus richness by city
plant_richness_city <- dat %>%
  group_by(city) %>%
  summarise(
    plant_genus_richness = n_distinct(plant),
    .groups = "drop"
  ) %>%
  mutate(city = str_to_title(city)) %>%
  arrange(desc(plant_genus_richness))

# Unique interactions
unique_interactions <- dat %>%
  summarise(n_unique = n_distinct(bee, plant))

# Total interactions by city
interactions_by_city <- dat %>%
  count(city, name = "n_interactions") %>%
  arrange(desc(n_interactions))

# Summary
summary_city <- dat %>%
  group_by(city) %>%
  summarise(
    total_interactions = n(),
    unique_bee_species = n_distinct(bee),
    unique_plant_genera = n_distinct(plant),
    honeybee_obs = sum(is_honeybee),
    wild_bee_obs = sum(!is_honeybee),
    .groups = "drop"
  )

# Sample size tables
sample_size_city_year <- dat %>%
  group_by(city, year) %>%
  summarise(
    total_interactions = n(),
    honeybee_obs = sum(is_honeybee),
    wild_bee_obs = sum(!is_honeybee),
    unique_bee_species = n_distinct(bee),
    unique_wild_bee_species = n_distinct(bee[!is_honeybee]),
    unique_plant_genera = n_distinct(plant),
    .groups = "drop"
  )

sample_size_city <- dat %>%
  group_by(city) %>%
  summarise(
    total_interactions = n(),
    honeybee_obs = sum(is_honeybee),
    wild_bee_obs = sum(!is_honeybee),
    unique_bee_species = n_distinct(bee),
    unique_wild_bee_species = n_distinct(bee[!is_honeybee]),
    unique_plant_genera = n_distinct(plant),
    .groups = "drop"
  )

city_bee_stats <- dat %>%
  group_by(city) %>%
  summarise(
    total_bee_records = n(),
    honeybee_records = sum(bee == "Apis mellifera"),
    wild_bee_records = sum(bee != "Apis mellifera"),
    wild_bee_richness = n_distinct(bee[bee != "Apis mellifera"]),
    plant_genus_richness = n_distinct(plant),
    honeybee_relative_frequency_percent = round((honeybee_records / total_bee_records) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(total_bee_records))

city_bee_stats

# Export as excel workbook
write_xlsx(
  list(
    "city_bee_stats" = city_bee_stats,
    "Summary by city" = summary_city,
    "City-year sample sizes" = sample_size_city_year,
    "City sample sizes" = sample_size_city,
    "Bee richness" = bee_frequency,
    "Plant richness" = plant_frequency,
    "Interaction frequencies" = interaction_frequency
  ),
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/data_summary_outputs.xlsx"
)


### -------------------------------------
# (1C) Top 10 wild bees + Apis mellifera
### -------------------------------------

# For analysis only keep top 10 bees
top10_wild_bees <- dat %>%
  filter(bee != "Apis mellifera") %>%
  count(bee, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(bee)

top10_wild_bees

# Filter dataset to Apis mellifera + top 10 wild bees
dat_top_bees <- dat %>%
  filter(bee == "Apis mellifera" | bee %in% top10_wild_bees)

# Checks
n_distinct(dat$bee)
n_distinct(dat_top_bees$bee)

dat_top_bees %>%
  count(bee, sort = TRUE)


### -------------------------------------
# (1D) Main figures/analyses: Top 20 plants + Other
### -------------------------------------

# Internal names for the two distinct "Other" categories
other_main <- "Other_main"
other_appendix <- "Other_appendix"

# Top 20 associated plant genera among Apis mellifera + top 10 wild bees
top20_plants <- dat_top_bees %>%
  count(plant, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(plant)

# Main analysis dataset:
# top 20 genera retained individually;
# all remaining genera pooled into the main "Other" category
dat_main_plants <- dat_top_bees %>%
  mutate(
    plant = ifelse(
      plant %in% top20_plants,
      plant,
      other_main
    )
  )

# Quick check
dat_main_plants %>%
  count(plant, sort = TRUE)

n_distinct(dat_main_plants$plant)


### -------------------------------------
# (1E) Appendix figures: All plants except singletons/doubletons + Other
### -------------------------------------

# Rare plant genera for appendix-level figures only
rare_plants_appendix <- dat_top_bees %>%
  count(plant) %>%
  filter(n <= 2) %>%
  pull(plant)

# Appendix dataset:
# all plant genera retained except singletons/doubletons;
# rare genera pooled into a distinct appendix "Other" category
dat_appendix_plants <- dat_top_bees %>%
  mutate(
    plant = ifelse(
      plant %in% rare_plants_appendix,
      other_appendix,
      plant
    )
  )

# Quick check
dat_appendix_plants %>%
  count(plant, sort = TRUE)

n_distinct(dat_appendix_plants$plant)


### -------------------------------------
# (1F) Final grouping checks
### -------------------------------------

# Check main Other and appendix Other are distinct
main_other_check <- dat_main_plants %>%
  filter(plant == other_main) %>%
  summarise(
    other_category = other_main,
    n_interactions = n()
  )

appendix_other_check <- dat_appendix_plants %>%
  filter(plant == other_appendix) %>%
  summarise(
    other_category = other_appendix,
    n_interactions = n()
  )

main_other_check
appendix_other_check

# Check original plant genera pooled into each Other category
dat_top_bees %>%
  filter(!plant %in% top20_plants) %>%
  summarise(
    main_other_interactions = n(),
    main_other_original_genera = n_distinct(plant)
  )

dat_top_bees %>%
  filter(plant %in% rare_plants_appendix) %>%
  summarise(
    appendix_other_interactions = n(),
    appendix_other_original_genera = n_distinct(plant)
  )

# Percent of original cleaned records retained by top 10 wild bees + Apis mellifera
top_bees_retention <- dat %>%
  summarise(
    total_cleaned_interactions = n(),
    retained_top_bee_interactions = sum(
      bee == "Apis mellifera" | bee %in% top10_wild_bees
    ),
    percent_retained = round(
      retained_top_bee_interactions / total_cleaned_interactions * 100,
      2
    )
  )

top_bees_retention

# Main dataset: unique interactions after grouping into top 20 + Other_main
dat_main_plants %>%
  summarise(
    n_plant_groups = n_distinct(plant),
    unique_grouped_interactions = n_distinct(bee, plant)
  )

# Appendix dataset: unique interactions after grouping singletons/doubletons into Other_appendix
dat_appendix_plants %>%
  summarise(
    n_plant_groups = n_distinct(plant),
    unique_grouped_interactions = n_distinct(bee, plant)
  )


### ============================================================================
# (2) Venn Diagram
### ============================================================================

# Note ALL plant genera

# Plants visited by Apis mellifera
plants_apis <- dat %>%
  filter(bee == "Apis mellifera") %>%
  pull(plant) %>%
  unique()

# Plants visited by wild bees
plants_wild <- dat %>%
  filter(bee != "Apis mellifera") %>%
  pull(plant) %>%
  unique()

# Values for Venn diagram
area_apis <- length(plants_apis)
area_wild <- length(plants_wild)
overlap_plants <- length(intersect(plants_apis, plants_wild))

area_apis
area_wild
overlap_plants

png(
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/venn_diagram.png",
  width = 5,
  height = 3.5,
  units = "in",
  res = 600,
  type = "cairo"
)

# Change positioning of 'Wild Bees' label?
draw.pairwise.venn(
  area1 = area_apis,
  area2 = area_wild,
  cross.area = overlap_plants,
  category = c("Apis mellifera", "Wild Bees"),
  scaled = TRUE,
  fill = c("#F2A900", "#2E7D32"),
  alpha = c(0.75, 0.75),
  lwd = 0.6,
  col = "black",
  cex = 5,
  cat.cex = 5,
  fontface = "plain",
  cat.fontfamily = "Segoe UI",
  cat.fontface = c("italic", "plain"),
  cat.pos = c(-20, 20),
  cat.dist = c(0.05, 0.05)
)

dev.off()


### ============================================================================
# (3) Colour scheme & Plant labels
### ============================================================================

# Bee colours
bee_cols <- c(
  "Apis mellifera" = "#F2A900", # "#FBB12B" or just orange
  "Bombus pascuorum" = "#2E7D32", # Wild Bees = "#83C802", # "#FD8469", # "#5C90CA", # "#B5A9E4"
  "Anthophora plumipes" = "#6A5ACD",
  "Bombus pratorum" = "#E91E63",
  "Bombus terrestris" = "#009688",
  "Bombus lapidarius" = "#FF5722",
  "Osmia bicornis" = "#795548",
  "Andrena cineraria" = "#607D8B",
  "Bombus hypnorum" = "#8BC34A",
  "Andrena haemorrhoa" = "#9C27B0",
  "Bombus hortorum" = "#3F51B5" 
)

# bee order 
bee_order <- c(
  "Apis mellifera", 
  top10_wild_bees
)

# Plant labels for plots, in italics bar Other
make_plant_labels <- function(plant_order) {
  ifelse(
    plant_order %in% c(other_main, other_appendix),
    "Other",
    paste0("italic('", plant_order, "')")
  )
}

make_bee_labels <- function(x) {
  paste0("italic('", x, "')")
}

### ============================================================================
# (4) Sorensen similarity index
### ============================================================================

# Build presence/absence matrix: bee x plant
pa_mat <- dat %>%
  distinct(bee, plant) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = plant,
    values_from = present,
    values_fill = 0
  ) %>%
  column_to_rownames("bee") %>%
  as.matrix()

# Apis plant-use vector
apis_vec <- pa_mat["Apis mellifera", ]

# Calculate Sørensen similarity only
similarity_results <- lapply(top10_wild_bees, function(sp) {
  
  wild_vec <- pa_mat[sp, ]
  
  a <- sum(apis_vec == 1 & wild_vec == 1)  # shared plants
  b <- sum(apis_vec == 1 & wild_vec == 0)  # Apis only
  c <- sum(apis_vec == 0 & wild_vec == 1)  # wild bee only
  
  sorensen <- (2 * a) / ((2 * a) + b + c)
  
  data.frame(
    wild_bee = sp,
    shared_plants = a,
    apis_only_plants = b,
    wild_only_plants = c,
    sorensen_similarity = sorensen
  )
}) %>%
  bind_rows() %>%
  arrange(desc(sorensen_similarity))

similarity_results

# Two-column plot
similarity_plot <- ggplot(similarity_results,
  aes(sorensen_similarity, fct_reorder(wild_bee, sorensen_similarity), fill = wild_bee)) +
  geom_col(width = 0.65, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.2f", sorensen_similarity)), hjust = -0.15, size = 18, family = "Segoe UI") +
  scale_x_continuous(limits = c(0, 0.60), breaks = seq(0, 0.6, 0.1), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = bee_cols) +
  labs(x = expression("Floral resource-use overlap with " * italic("Apis mellifera") * " (Sørensen index)"),y = NULL) +
  theme_classic(base_family = "Segoe UI") +
  theme(
    axis.text.y = element_text(face = "italic", colour = "black", size = 50),
    axis.text.x = element_text(colour = "black", size = 50),
    axis.title.x = element_text(size = 50, margin = margin(t = 10)),
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 10)
  )

# View
similarity_plot


# One-column plot with two-line x-axis title
similarity_plot_onecol <- ggplot(similarity_results,
  aes(sorensen_similarity, fct_reorder(wild_bee, sorensen_similarity), fill = wild_bee)) +
  geom_col(width = 0.65, colour = "black", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.2f", sorensen_similarity)), hjust = -0.15, size = 18, family = "Segoe UI") +
  scale_x_continuous(limits = c(0, 0.60), breaks = seq(0, 0.6, 0.1), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = bee_cols) +
  labs(x = expression(atop("Floral resource-use overlap with " * italic("Apis mellifera"),"(Sørensen index)")),y = NULL) +
  theme_classic(base_family = "Segoe UI") +
  theme(
    axis.text.y = element_text(face = "italic", colour = "black", size = 50),
    axis.text.x = element_text(colour = "black", size = 50),
    axis.title.x = element_text(size = 42, margin = margin(t = 12)),
    legend.position = "none",
    plot.margin = margin(10, 20, 45, 10)
  )

# View
similarity_plot_onecol


# Save as images
ggsave(
  filename = "similarity_plot_two_column_width.png",
  plot = similarity_plot,
  path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
  width = 7.09,
  height = 4.5,
  units = "in",
  dpi = 600
)

ggsave(
  filename = "similarity_plot_one_column_width.png",
  plot = similarity_plot_onecol,
  path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
  width = 3.54,
  height = 4.8,
  units = "in",
  dpi = 600
)

### ============================================================================
# (5) Chi-square
### ============================================================================


### -------------------------------------
# (5A) X2 - Top 20 plants (MAIN)
### -------------------------------------

# main result
mat_main <- dat_main_plants %>%
  count(plant, bee) %>%
  pivot_wider(names_from = bee, values_from = n, values_fill = 0) %>%
  column_to_rownames("plant") %>%
  as.matrix()

chi_main <- chisq.test(mat_main, simulate.p.value = TRUE)
chi_main
# X-squared = 1074.3, df = NA, p-value = 0.0004998

set.seed(123)
chi_main <- chisq.test(mat_main, simulate.p.value = TRUE, B = 10000)
chi_main 
# X-squared = 1006.6, df = NA, p-value = 9.999e-05

### -------------------------------------
# (5B) X2 - All plants (APPENDIX)
### -------------------------------------

# appendix result 
mat_all <- dat_appendix_plants %>%
  count(plant, bee) %>%
  pivot_wider(names_from = bee, values_from = n, values_fill = 0) %>%
  column_to_rownames("plant") %>%
  as.matrix()

set.seed(123)
chi_all <- chisq.test(mat_all, simulate.p.value = TRUE, B = 10000)
chi_all
# X-squared = 1331.5, df = NA, p-value = 9.999e-05

### ============================================================================
# (6) Residuals
### ============================================================================


### -------------------------------------
# (6A) Residuals — Top 20 plants (MAIN)
### -------------------------------------

res_main <- chi_main$stdres

res_df_main <- as.data.frame(res_main) %>%
  mutate(plant = rownames(.)) %>%
  pivot_longer(-plant, names_to = "bee", values_to = "std_residual")

# Bee order by similarity to Apis mellifera
bee_order_residual <- similarity_results %>%
  arrange(desc(sorensen_similarity)) %>%
  pull(wild_bee)

bee_order_residual <- c(
  "Apis mellifera",
  bee_order_residual
)

# Order plants by Apis mellifera residuals
plant_order_main <- res_df_main %>%
  filter(bee == "Apis mellifera") %>%
  arrange(desc(std_residual)) %>%
  pull(plant)

# plot
residual_plot_main <- ggplot(
  res_df_main %>%
    mutate(
      bee = factor(bee, levels = bee_order_residual),
      plant = factor(plant, levels = plant_order_main)
    ),
  aes(bee, plant, fill = std_residual)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_y_discrete(labels = parse(text = make_plant_labels(plant_order_main))) +
  scale_x_discrete(labels = parse(text = make_bee_labels(bee_order_residual))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-12, 12), oob = scales::squish, name = "Standardised residual") +
  theme_classic(base_family = "Segoe UI") +
  labs(x = "Bee species", y = "Plant genus", fill = "Standardised\nresidual") +
  theme(
    text = element_text(size = 50),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "italic", size = 50, margin = margin(t = 6)), 
    axis.text.y = element_text(size = 50, margin = margin(r = 6)),
    axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(size = 60, face = "bold",margin = margin(r = 12)), 
    legend.title = element_text(size = 50, hjust = 0.5),
    legend.text = element_text(size = 50, hjust = 0.5)
  )

# view
residual_plot_main

# save
ggsave(
  filename = "residual_heatmap_main.png",
  plot = residual_plot_main,
  path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
  width = 7.1,
  height = 5,   # adjust if needed
  units = "in",
  dpi = 600
)


### -------------------------------------
# (6B) Residuals — All plants (APPENDIX)
### -------------------------------------

res_all <- chi_all$stdres

# reorder
res_df_all <- as.data.frame(res_all) %>%
  mutate(plant = rownames(.)) %>%
  pivot_longer(-plant, names_to = "bee", values_to = "std_residual")

# Order appendix plants by Apis mellifera residuals
plant_order_all <- res_df_all %>%
  filter(bee == "Apis mellifera") %>%
  arrange(desc(std_residual)) %>%
  pull(plant)

# plot
residual_plot_all <- ggplot(
  res_df_all %>%
    mutate(bee = factor(bee, levels = bee_order_residual), plant = factor(plant, levels = plant_order_all)),
  aes(bee, plant, fill = std_residual)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_y_discrete(labels = parse(text = make_plant_labels(plant_order_all))) +
  scale_x_discrete(labels = parse(text = make_bee_labels(bee_order_residual))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limits = c(-12, 12), oob = scales::squish, name = "Standardised residual") +
  theme_classic(base_family = "Segoe UI") +
  labs(x = "Bee species", y = "Plant genus") +
  theme(
    text = element_text(size = 50),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,  face = "italic", size = 50, margin = margin(t = 6)),
    axis.text.y = element_text(size = 50),
    axis.title.x = element_text(size = 60, face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(size = 60, face = "bold", margin = margin(r = 20)),
    legend.title = element_text(size = 50, hjust = 0.5),
    legend.text = element_text(size = 50, hjust = 0.5)
  )

# view
residual_plot_all

# save
ggsave(
  filename = "residual_heatmap_all.png",
  plot = residual_plot_all,
  path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
  width = 7.1,
  height = 8,   # taller for readability
  units = "in",
  dpi = 600
)

### -------------------------------------
# (6C) Residual tables for checking/export
### -------------------------------------

# Folder for outputs
output_dir <- "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis"

# Display labels for Other categories
main_other_label <- "Other"
appendix_other_label <- "Other"

# Main residual table: long format, ordered by bee species and residual strength
residual_table_main_long <- res_df_main %>%
  mutate(
    plant_display = ifelse(plant == other_main, main_other_label, plant),
    bee = factor(bee, levels = bee_order_residual),
    std_residual = round(std_residual, 3)
  ) %>%
  arrange(bee, desc(std_residual)) %>%
  select(
    Bee = bee,
    Plant = plant_display,
    `Standardised residual` = std_residual
  )

# Appendix residual table: long format, ordered by bee species and residual strength
residual_table_all_long <- res_df_all %>%
  mutate(
    plant_display = ifelse(plant == other_appendix, appendix_other_label, plant),
    bee = factor(bee, levels = bee_order_residual),
    std_residual = round(std_residual, 3)
  ) %>%
  arrange(bee, desc(std_residual)) %>%
  select(
    Bee = bee,
    Plant = plant_display,
    `Standardised residual` = std_residual
  )

# Main residual table: wide format, ordered like the main heatmap
residual_table_main_wide <- res_df_main %>%
  mutate(
    plant = factor(plant, levels = plant_order_main),
    bee = factor(bee, levels = bee_order_residual),
    plant_display = ifelse(as.character(plant) == other_main, main_other_label, as.character(plant)),
    std_residual = round(std_residual, 3)
  ) %>%
  select(plant, plant_display, bee, std_residual) %>%
  arrange(plant) %>%
  select(-plant) %>%
  pivot_wider(
    names_from = bee,
    values_from = std_residual
  ) %>%
  rename(Plant = plant_display) %>%
  select(Plant, all_of(bee_order_residual))

# Appendix residual table: wide format, ordered like the appendix heatmap
residual_table_all_wide <- res_df_all %>%
  mutate(
    plant = factor(plant, levels = plant_order_all),
    bee = factor(bee, levels = bee_order_residual),
    plant_display = ifelse(as.character(plant) == other_appendix, appendix_other_label, as.character(plant)),
    std_residual = round(std_residual, 3)
  ) %>%
  select(plant, plant_display, bee, std_residual) %>%
  arrange(plant) %>%
  select(-plant) %>%
  pivot_wider(
    names_from = bee,
    values_from = std_residual
  ) %>%
  rename(Plant = plant_display) %>%
  select(Plant, all_of(bee_order_residual))

# Strongest main residuals, ordered by absolute residual strength
strongest_residuals_main <- res_df_main %>%
  mutate(
    plant = ifelse(plant == other_main, main_other_label, plant),
    abs_residual = abs(std_residual),
    std_residual = round(std_residual, 3),
    abs_residual = round(abs_residual, 3)
  ) %>%
  arrange(desc(abs_residual)) %>%
  select(
    Bee = bee,
    Plant = plant,
    `Standardised residual` = std_residual,
    `Absolute residual` = abs_residual
  )

# Strongest appendix residuals, ordered by absolute residual strength
strongest_residuals_all <- res_df_all %>%
  mutate(
    plant = ifelse(plant == other_appendix, appendix_other_label, plant),
    abs_residual = abs(std_residual),
    std_residual = round(std_residual, 3),
    abs_residual = round(abs_residual, 3)
  ) %>%
  arrange(desc(abs_residual)) %>%
  select(
    Bee = bee,
    Plant = plant,
    `Standardised residual` = std_residual,
    `Absolute residual` = abs_residual
  )


# Export to Excel workbook
write_xlsx(
  list(
    "Main residuals wide" = residual_table_main_wide,
    "Appendix residuals wide" = residual_table_all_wide,
    "Main residuals long" = residual_table_main_long,
    "Appendix residuals long" = residual_table_all_long,
    "Strongest main residuals" = strongest_residuals_main,
    "Strongest appendix residuals" = strongest_residuals_all
  ),
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/residual_tables.xlsx"
)


### ============================================================================
# (7) Pairwise Fisher's exact tests
### ============================================================================

# Apis mellifera vs each top 10 wild bee

wild_species <- setdiff(colnames(mat_main), "Apis mellifera")

set.seed(123)

pairwise_results <- lapply(wild_species, function(sp) {
  
  sub_mat <- mat_main[, c("Apis mellifera", sp)]
  sub_mat <- sub_mat[rowSums(sub_mat) > 0, , drop = FALSE]
  
  test <- fisher.test(
    sub_mat,
    simulate.p.value = TRUE,
    B = 10000
  )
  
  data.frame(
    species = sp,
    p_value = test$p.value
  )
}) %>%
  bind_rows() %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH"),
    significance = ifelse(p_adj < 0.05, "Significant", "Not significant")
  ) %>%
  arrange(p_adj)

pairwise_results

# Table of results
fisher_table <- pairwise_results %>%
  mutate(
    species = gsub("_", " ", species),
    p_value = ifelse(p_value < 0.001, "< 0.001", round(p_value, 5)),
    p_adj = ifelse(p_adj < 0.001, "< 0.001", round(p_adj, 5))
  ) %>%
  rename(
    Species = species,
    `p-value` = p_value,
    `Adjusted p-value (BH)` = p_adj
  ) %>%
  select(Species, `p-value`, `Adjusted p-value (BH)`)


### ============================================================================
# (8) Network visualisation
### ============================================================================


### -------------------------------------
# (8A) Prep
### -------------------------------------

# Network dataset for networks the same as main analysis dataset
dat_network <- dat_main_plants

# Build plant x bee interaction matrix
net_web <- dat_main_plants %>%
  count(plant, bee, name = "edge.weight") %>%
  pivot_wider(
    names_from = bee,
    values_from = edge.weight,
    values_fill = 0
  ) %>%
  column_to_rownames("plant") %>%
  as.matrix()

# Order plants by total interactions
plant_order <- order(rowSums(net_web), decreasing = TRUE)
net_web_weight <- net_web[plant_order, ]

# Move "Other" to bottom
if (other_main %in% rownames(net_web_weight)) {
  net_web_weight <- net_web_weight[
    c(setdiff(rownames(net_web_weight), other_main), other_main),
  ]
}

# Order bees
net_web_weight <- net_web_weight[
  ,
  order(colSums(net_web_weight), decreasing = TRUE)
]

# Save network order for matrices and bubble plot ONLY
# This does not change the network
plant_order_network <- rownames(net_web_weight)
bee_order_network <- colnames(net_web_weight)

# Create plotting copy only:
# keep net_web_weight unchanged for analysis/checks,
# but display Other_main as "Other" in the network figure
net_web_weight_plot <- net_web_weight

rownames(net_web_weight_plot)[
  rownames(net_web_weight_plot) == other_main
] <- "Other"

### -------------------------------------
# (8B - i) Bees LEFT, plants RIGHT
### -------------------------------------

# Do NOT transpose
net_web_plot <- net_web_weight_plot

# Plant colours (left)
lower_color <- rep("black", nrow(net_web_plot))
names(lower_color) <- rownames(net_web_plot)
if ("Other" %in% names(lower_color)) {
  lower_color["Other"] <- "grey80"
}

# Bee colours (right)
higher_color <- bee_cols[colnames(net_web_plot)]

# Wild bees white, Apis mellifera stays coloured
higher_color[names(higher_color) != "Apis mellifera"] <- "transparent"

# Checks
dim(net_web_weight)
rownames(net_web_weight)
colnames(net_web_weight)
n_distinct(dat_network$plant)
n_distinct(dat_network$bee)

# Save as PDF
pdf(
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/network_2.pdf",
  width = 4,
  height = 8
)

par(
  family = "SegoeUI",
  mar = c(0, 0, 0, 0),   # remove ALL margins
  xaxs = "i",            # no x padding
  yaxs = "i",            # no y padding
  bg = "white"
)

plotweb(
  net_web_plot,
  horizontal = TRUE,
  lower_color = lower_color,
  higher_color = higher_color,
  link_color = "higher",
  link_alpha = 1,
  curved_links = FALSE,
  lower_italic = TRUE,
  higher_italic = TRUE,
  text_size = 0.75,
  spacing = 0.5
)

dev.off()

# convert pdfs to images
pdf_convert(
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/network_2.pdf",
  filenames = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/network_2_highres.png",
  dpi = 1200
)


### -------------------------------------
# (8B - ii) Plants LEFT, Bees RIGHT
### -------------------------------------

# Transpose for plotweb
net_web_plot <- t(net_web_weight_plot)

# Bee colours (left)
lower_color <- bee_cols[rownames(net_web_plot)]

# Apis mellifera white, wild bees stay coloured
lower_color["Apis mellifera"] <- "transparent"

# Plant colours (right)
higher_color <- rep("black", ncol(net_web_plot))
names(higher_color) <- colnames(net_web_plot)
if ("Other" %in% names(higher_color)) {
  higher_color["Other"] <- "grey80"
}

# Save as PDF
pdf(
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/network_1.pdf",
  width = 4,
  height = 8
)

par(
  family = "SegoeUI",
  mar = c(0, 0, 0, 0),   
  xaxs = "i",           
  yaxs = "i",            
  bg = "white"
)

plotweb(
  net_web_plot,
  horizontal = TRUE,
  lower_color = lower_color,
  higher_color = higher_color,
  link_color = "lower",
  link_alpha = 1,
  curved_links = FALSE,
  lower_italic = TRUE,
  higher_italic = TRUE,
  text_size = 0.75,
  spacing = 0.5
)

dev.off()

# convert pdfs to images to edit in powerpoint
pdf_convert(
  "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/network_1.pdf",
  filenames = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis/network_1_highres.png",
  dpi = 1200
)


### ============================================================================
# (9) Matrices
### ============================================================================


### -------------------------------------
# (9A) Overall
### -------------------------------------

# Build presence/absence matrix
pa_matrix <- dat_appendix_plants %>%
  distinct(plant, bee) %>%
  mutate(presence = 1) %>%
  pivot_wider(
    names_from = bee,
    values_from = presence,
    values_fill = 0
  ) %>%
  column_to_rownames("plant") %>%
  as.matrix()

# Use network order first, then add extra dat_appendix_plants genera after
all_matrix_plants <- rownames(pa_matrix)

main_plants_no_other <- setdiff(plant_order_network, other_main)

extra_plants <- setdiff(
  all_matrix_plants,
  c(main_plants_no_other, other_appendix)
)

extra_plants_ordered <- dat_appendix_plants %>%
  filter(
    plant %in% extra_plants,
    plant != other_appendix
  ) %>%
  count(plant, sort = TRUE) %>%
  pull(plant)

plant_order_overall <- c(
  main_plants_no_other,
  extra_plants_ordered,
  other_appendix
)

plant_order_overall <- plant_order_overall[
  plant_order_overall %in% all_matrix_plants
]

bee_order_overall <- bee_order_network

pa_matrix <- pa_matrix[
  plant_order_overall,
  bee_order_overall,
  drop = FALSE
]

# Convert to long format
pa_df <- as.data.frame(pa_matrix) %>%
  mutate(plant = rownames(.)) %>%
  pivot_longer(
    cols = -plant,
    names_to = "bee",
    values_to = "presence"
  ) %>%
  mutate(
    plant = factor(plant, levels = plant_order_overall),
    bee = factor(bee, levels = bee_order_overall)
  )

pa_df <- pa_df %>%
  mutate(
    fill_type = case_when(
      presence == 0 ~ "absent",
      bee == "Apis mellifera" ~ "honeybee",
      TRUE ~ "wild"
    )
  )

# make matrix plot
pa_plot <- ggplot(pa_df, aes(x = plant, y = bee, fill = fill_type)) +
  geom_tile(colour = "black", linewidth = 0.35) +
  scale_x_discrete(labels = parse(text = make_plant_labels(plant_order_overall))) +
  scale_y_discrete(labels = parse(text = make_bee_labels(bee_order_overall))) +
  scale_fill_manual(
    values = c(
      "absent" = "transparent",
      "wild" = "#2E7D32", 
      "honeybee" = "#F2A900"
    )
  ) +
  coord_equal() +
  theme_classic(base_family = "Segoe UI") +
  theme(
    axis.text.x = element_text(size = 50, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 50),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none"
  )

# view
pa_plot

# save
ggsave(
  filename = "matrix_overall.png",
  plot = pa_plot,
  path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
  width = 27,
  height = 13,
  units = "cm",
  dpi = 600
)


### -------------------------------------
# (9B) Cities 
### -------------------------------------

make_pa_plot_same_layout <- function(df, city_name,
                                     plant_order = plant_order_overall,
                                     bee_order = bee_order_overall) {
  
  # Build city presence/absence data, but keep ALL overall plants/bees
  pa_df_city <- df %>%
    distinct(plant, bee) %>%
    mutate(presence = 1) %>%
    complete(
      plant = plant_order,
      bee = bee_order,
      fill = list(presence = 0)
    ) %>%
    mutate(
      plant = factor(plant, levels = plant_order),   # same x layout
      bee = factor(bee, levels = bee_order),         # same y layout
      fill_type = case_when(
        presence == 0 ~ "absent",
        bee == "Apis mellifera" ~ "honeybee",
        TRUE ~ "wild"
      )
    )
  
  ggplot(pa_df_city, aes(x = plant, y = bee, fill = fill_type)) +
    geom_tile(colour = "black", linewidth = 0.35) +
    scale_x_discrete(labels = parse(text = make_plant_labels(plant_order))) +
    scale_y_discrete(labels = parse(text = make_bee_labels(bee_order))) +
    scale_fill_manual(
      values = c(
        "absent" = "transparent",
        "wild" = "#2E7D32",
        "honeybee" = "#F2A900"
      )
    ) +
    coord_equal() +
    theme_classic(base_family = "Segoe UI") +
    labs(title = str_to_title(city_name)) +
    theme(
      axis.text.x = element_text(size = 50, angle = 90, hjust = 1),
      axis.text.y = element_text(size = 50),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.position = "none", 
      plot.title = element_text(
        size = 60,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 20)
      )
    )
}

city_list <- split(dat_appendix_plants, dat_appendix_plants$city)

plots <- lapply(names(city_list), function(city) {
  make_pa_plot_same_layout(city_list[[city]], city)
})

names(plots) <- names(city_list)

for (city in names(plots)) {
  ggsave(
    paste0("matrix_", city, "_same_layout.png"),
    plot = plots[[city]],
    path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
    width = 27,
    height = 13,
    units = "cm",
    dpi = 600
  )
}


### ============================================================================
# (10) Bubble plot
### ============================================================================

bubble_data <- dat_appendix_plants %>%
  mutate(
    bee_type = ifelse(
      bee == "Apis mellifera",
      "Honeybee",
      "Wild bee"
    )
  ) %>%
  count(plant, bee, bee_type) %>%
  mutate(
    plant = factor(plant, levels = plant_order_overall),
    bee = factor(bee, levels = bee_order_overall)
  )

bubble_plot <- ggplot(bubble_data, aes(x = plant, y = bee)) +
  geom_point(aes(size = n, fill = bee_type), shape = 21, colour = "black", alpha = 0.85) +
  scale_size(range = c(1.5, 10)) +
  scale_fill_manual(values = c("Honeybee" = "#F2A900", "Wild bee" = "#2E7D32")) +
  scale_x_discrete(
    labels = parse(text = make_plant_labels(plant_order_overall)),
    expand = expansion(mult = c(0.035, 0.02))
  ) +
  scale_y_discrete(
    labels = parse(text = make_bee_labels(bee_order_overall)),
    expand = expansion(mult = c(0.07, 0.05))
  ) +
  coord_cartesian() +
  theme_minimal(base_family = "Segoe UI") +
  labs(x = "Plant genus", y = "Bee species") +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.25),
    axis.text.x = element_text(size  = 50, angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(size  = 50, colour = "black"),
    axis.title = element_text(size  = 60, face = "bold", colour = "black"),
    legend.position = "none"
  )

# check
bubble_plot

# save as image
ggsave(
  filename = "bubble_plot_A4_landscape.png",
  plot = bubble_plot,
  path = "C:/Users/770551/OneDrive - hull.ac.uk/Aa. Dissertation/Data and Analysis",
  width = 27,
  height = 13,
  units = "cm",
  dpi = 600
)
