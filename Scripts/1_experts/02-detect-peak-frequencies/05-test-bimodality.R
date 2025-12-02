rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence")

library(ggplot2)
library(mclust)
library(purrr)
library(tidyr)
library(dplyr)

## ============================================================================================================ ##
## Test correlation between our measured peak frequencies and those reported by Mikula


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/1_experts/peak_frequency_detections/xeno-canto/all_birds_peak_frequencies.csv")

#--------------
## Test bimodality ##
#--------------

# helper
analyze_species <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 3) return(tibble(n_records = n, n_modes = NA, mean1 = NA, mean2 = NA, BIC_diff = NA))
  
  m1 <- Mclust(x, G = 1, verbose = FALSE)
  m2 <- Mclust(x, G = 2, verbose = FALSE)
  
  # get scalar BIC difference
  bic_diff <- max(m2$BIC, na.rm = TRUE) - max(m1$BIC, na.rm = TRUE)
  
  if (is.finite(bic_diff) && bic_diff > 5 && length(m2$parameters$mean) == 2) {
    tibble(
      n_records = n,
      n_modes = 2,
      mean1 = m2$parameters$mean[1],
      mean2 = m2$parameters$mean[2],
      BIC_diff = bic_diff
    )
  } else {
    tibble(
      n_records = n,
      n_modes = 1,
      mean1 = mean(x),
      mean2 = NA_real_,
      BIC_diff = bic_diff
    )
  }
}

bimodal_results <- dat %>%
  group_by(full_name) %>%
  summarise(result = list(analyze_species(peak_freq_hz)), .groups = "drop") %>%
  unnest(result) %>%
  mutate(bimodal = n_modes == 2 & BIC_diff > 5)


write.csv2(bimodal_results, "Data/1_experts/peak_frequency_detections/xeno-canto/bimodality_all_birds.csv", row.names = FALSE)


bimodal_results$full_name <- gsub(" ", "_", bimodal_results$full_name)

cooccurences <- read.csv2("Data/1_experts/cooccurence_pairs.csv")

positive_pairs <- cooccurences %>%
  filter(type == "positive") %>%
  select(sp1_name, sp2_name)

unique_species <- positive_pairs %>%
  unlist() %>%          # convert to a vector
  unique() 


bimodal_results$positive_cooccurrence <- bimodal_results$full_name %in% unique_species

chisq_test <- chisq.test(table(bimodal_results$bimodal, bimodal_results$positive_cooccurrence))
chisq_test

df_plot <- bimodal_results %>%
  group_by(positive_cooccurrence, bimodal) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(positive_cooccurrence) %>%
  mutate(prop = n / sum(n))

# Plot
ggplot(df_plot, aes(x = positive_cooccurrence, y = prop, fill = bimodal)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "In positive Co-Occurrence",
    y = "Proportion of species",
    fill = "Bimodal"
  ) +
  theme_minimal()





expert_detections <- read.csv2("Data/1_experts/detections_freile_gelis_2021_2022_birds_dummy_pivot_reordered.csv")

plot_categories <- read.csv("Data/plot_categories.csv")
plot_categories <- plot_categories[, c("Plot_ID", "Label")]

expert_detections <- expert_detections %>%
  left_join(plot_categories, by = c("plot" = "Plot_ID"))

# Assuming your merged dataframe has columns: plot, species, Label (plot category)
# Step 1: Convert from wide 0/1 per species per plot to long format
df_long <- expert_detections %>%
  pivot_longer(
    cols = 3:336,  # adjust to match your species columns
    names_to = "species",
    values_to = "presence"
  ) %>%
  filter(presence > 0)  # keep only presences

# Step 2: Count presence per species per label
df_summary <- df_long %>%
  group_by(species, Label) %>%
  dplyr::summarize(count = n(), .groups = "drop")

df_max <- df_summary %>%
  group_by(species) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE) %>% 
  ungroup()


bimodal_results$full_name <- gsub(" ", "_", bimodal_results$full_name)

bimodal_results <- left_join(bimodal_results, df_max, by = c("full_name" = "species"))


write.csv2(bimodal_results, "Data/1_experts/peak_frequency_detections/xeno-canto/bimodality_all_birds.csv", row.names = FALSE)

bimodal_results$Label <- factor(
  bimodal_results$Label,
  levels = c("A_P", "A_C", "F_P", 
             "F_C", "F_P_RegI",  "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old")  # adjust according to your gradient
)

table(bimodal_results$bimodal, bimodal_results$Label)

chisq_test <- chisq.test(table(bimodal_results$bimodal, bimodal_results$Label))
chisq_test


df_long <- as.data.frame(table(bimodal_results$bimodal, bimodal_results$Label)) %>%
  rename(bimodal = Var1, Label = Var2, count = Freq) %>%
  group_by(Label) %>%
  mutate(prop = count / sum(count))

ggplot(df_long, aes(x = Label, y = prop, fill = bimodal)) +
  geom_bar(stat = "identity") +
  ylab("Proportion of species") +
  theme_minimal()


#--------------
## Set factor levels of 'Label' to reflect the recovery gradient ##
#--------------

cooccurence_pairs_categories$Label <- factor(
  cooccurence_pairs_categories$Label,
  levels = c("A_P", "A_C", "F_P", 
             "F_C", "F_P_RegI",  "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old"
  )
)

# Spalten vereinheitlichen (Unterstriche)
bimodal_results <- bimodal_results %>%
  mutate(sp_name = gsub(" ", "_", full_name))

# Alle Art-Label-Kombinationen extrahieren
sp_gradient <- cooccurence_pairs_categories %>%
  dplyr::select(plot, Label, sp1_name, sp2_name) %>%
  tidyr::pivot_longer(cols = c(sp1_name, sp2_name), names_to = "col", values_to = "sp_name") %>%
  dplyr::select(-col) %>%
  distinct()

# Merge mit bimodal_results
bimodal_gradient <- bimodal_results %>%
  left_join(sp_gradient, by = "sp_name")


library(dplyr)
library(ggplot2)

bimodal_ratio <- bimodal_gradient %>%
  group_by(Label) %>%
  summarise(
    n_bimodal = sum(bimodal, na.rm = TRUE),
    n_total = n(),
    prop_bimodal = n_bimodal / n_total
  )

ggplot(bimodal_ratio, aes(x = Label, y = prop_bimodal)) +
  geom_col(fill = "steelblue") +
  ylim(0,1) +
  xlab("Gradient Category") +
  ylab("Proportion of Bimodal Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(dplyr)

pairs <- cooccurence_pairs_categories %>%
  mutate(sp1_name = tolower(sp1_name),
         sp2_name = tolower(sp2_name))

bimodal_results <- bimodal_results %>%
  mutate(sp_name = gsub(" ", "_", tolower(full_name)))

# Bimodalität für beide Arten anhängen
pairs <- pairs %>%
  dplyr::left_join(bimodal_results %>% dplyr::select(sp_name, bimodal),
                   by = c("sp1_name" = "sp_name")) %>%
  dplyr::rename(bimodal_sp1 = bimodal) %>%
  dplyr::left_join(bimodal_results %>% dplyr::select(sp_name, bimodal),
                   by = c("sp2_name" = "sp_name")) %>%
  dplyr::rename(bimodal_sp2 = bimodal)

pairs <- pairs %>%
  mutate(any_bimodal = bimodal_sp1 | bimodal_sp2)

table(pairs$type, pairs$any_bimodal)

chisq.test(matrix(c(28, 47, 243, 428), nrow = 2,
                  dimnames = list(CoOccurrence = c("negative", "positive"),
                                  Bimodal = c("FALSE","TRUE"))))

glm_pos <- glm(I(type == "positive") ~ any_bimodal, data = pairs, family = binomial)
summary(glm_pos)

ggplot(pairs, aes(x = type, y = peak_distance)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  xlab("Co-Occurrence Type") +
  ylab("Peak Frequency Distance (Hz)") +
  theme_minimal()


# Gruppieren nach Gradient-Kategorie
bimodal_ratio <- bimodal_gradient %>%
  group_by(Label) %>%
  summarise(
    n_bimodal = sum(bimodal, na.rm = TRUE),
    n_total = n(),
    prop_bimodal = n_bimodal / n_total
  ) %>%
  arrange(Label)

library(ggplot2)

ggplot(bimodal_ratio, aes(x = Label, y = prop_bimodal)) +
  geom_col(fill = "steelblue") +
  ylim(0,1) +
  xlab("Gradient-Kategorie") +
  ylab("Anteil bimodaler Arten") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

glm_bimodal <- glm(bimodal ~ Label, data = bimodal_gradient, family = binomial)
summary(glm_bimodal)
anova(glm_bimodal, test = "Chisq")

peak_data <- dat

intra_var <- peak_data %>%
  mutate(sp_name = gsub(" ", "_", tolower(full_name))) %>%
  group_by(sp_name) %>%
  summarise(
    n_records = n(),
    mean_peak = mean(peak_freq_hz, na.rm = TRUE),
    var_peak = var(peak_freq_hz, na.rm = TRUE)
  ) %>%
  left_join(dplyr::select(bimodal_results, sp_name, bimodal), by = "sp_name")


library(ggplot2)

ggplot(intra_var, aes(x = bimodal, y = var_peak)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  xlab("Bimodalität") +
  ylab("Varianz der Peak-Frequenzen (Hz^2)") +
  theme_minimal()

# Falls die Varianz normal verteilt ist
t.test(var_peak ~ bimodal, data = intra_var)

# Oder robuster: Wilcoxon-Test
wilcox.test(var_peak ~ bimodal, data = intra_var)