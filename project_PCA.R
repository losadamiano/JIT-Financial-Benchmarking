library(readxl)

# Load the dataset containing AIDA data, which includes 
# a dummy variable representing the production start timing 
# (1 if started immediately, 0 if started at a later stage)
dataset <- read_excel("data_finale.xlsx")


library(tidyverse)
## Calculate missing indices and add them to the dataset 

dataset <- dataset %>% 
  mutate(current_ratio = attivo_circolante / debiti_breve ,
         quick_ratio = ( attivo_circolante - tot_rimanenze  ) / debiti_breve ,
         immobiliz_su_tot_attivo = tot_immobilizzazioni / ( tot_immobilizzazioni +
                                                              attivo_circolante  ))
library(FactoMineR)

## Remove variables not needed for the PCA

dataset_pca <- dataset %>% 
  select(codice_fiscale, ricavi_vendite, ROA, ROS, EBITDAMargin, CCN, PFN, utile_netto,
         current_ratio, quick_ratio, immobiliz_su_tot_attivo )

# VARIOUS CHECKS
# Check for NA values
sapply(dataset_pca, function(x) sum(is.na(x)))

# Check for Inf values
sapply(dataset_pca, function(x) sum(is.infinite(x)))


library(dplyr)

# Set the fiscal code (codice_fiscale) as row names so it doesn't affect 
# the PCA calculation
dataset_finale <- dataset_pca %>% 
  group_by(codice_fiscale) %>% 
  column_to_rownames("codice_fiscale")

# Once the dataset is ready, perform the PCA and verify the outputs 
pca_dati <- PCA(dataset_finale, scale.unit = T, graph = F)
plot(pca_dati)

# Most points are clustered around the origin (0,0),
# suggesting that many companies are similar across the main indicators.
# The plot does not show a clear separation into visible clusters.

pca_dati$eig

library(factoextra) # Ensure this package is loaded

# Correlation Circle Plot (Variables Factor Map)
fviz_pca_var(pca_dati,
             col.var = "cos2",       # Color variables based on their quality of representation (cos2)
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Color palette for quality
             repel = TRUE,           # Avoid overlapping variable labels
             ggtheme = theme_minimal() # Use a minimalist graphic theme
)

# The scree plot shows that the first 3 principal components are 
# the most significant, capturing the majority of the useful variance,
# indicated by the pronounced "elbow" between the third and fourth components.

pca_dati$var$cor
pca_dati$eig
pca_dati$var$coord
var_coord <- as.data.frame(pca_dati$var$coord)
#install.packages("writexl")
library(writexl)
write_xlsx(var_coord, path = "var_coord.xlsx")
# Select the first 3 principal components and load the table to Excel
write_xlsx(var_coord[1:3], path = "var_coord2.xlsx")
# The table shows the correlation between components and indices, appearing 
# in descending order of correlation. Furthermore, the first component is 
# almost entirely positively correlated. 
# High values (positive or negative): Indicate a strong association between the variable and the component.
# Sign (+ or -): Indicates the direction of the relationship. Variables with the same 
# sign on loadings contribute similarly to the component, while those with opposite signs contribute in opposite directions.
## Verify on Excel via the table if rotation is necessary

### Perform rotation 
#install.packages("psych") 
library(psych)            
### Rotate
# Extract variable coordinates for the first 3 principal components
# 'coord' represents the loadings (correlation coefficients between original variables and components)
# Choose the number of columns corresponding to the components to be rotated (nfactors = 3)
# Rotation is performed on the original data table used for PCA
# Apply the principal() function to the extracted loadings
# nfactors must match the number of extracted columns
rc <- principal(dataset_finale, nfactors = 3, rotate = 'varimax', scores = TRUE)

# Print rotation results
print(rc)

## These are the points in the rotated variables plane 
rc$scores
data_scores <-  as.data.frame( rc$scores )
plot(data_scores$RC1, data_scores$RC2, col= "white", xlab = "RC1", ylab= "RC2")
text(data_scores$RC1, data_scores$RC2, rownames(data_scores), cex = 0.6)
abline(h=0, lty=2)
abline(v=0, lty=2)

# The plot identifies the position of each company in the component space 
# through the rotation.
# This graph shows how our original variables (vectors starting from the origin) 
# align with the new axes (the rotated principal components). 
# The rotation aims to "simplify" the loading structure, 
# clarifying which variables contribute strongly to each component. 
# In a good rotation result, variable vectors should align 
# as much as possible with one of the axes.

# Create a database with all principal components
df <- pca_dati$ind$coord 
df <- as.data.frame(df)

# Reload the dataset with non-numbered fiscal codes for years,
# so that codes referring to the same company are identical
dataset_codici_non_numerati <- read_excel("dataset_codici_non_numerati.xlsx")
dataset_codici_non_numerati <- dataset_codici_non_numerati %>% 
  mutate(current_ratio = attivo_circolante / debiti_breve ,
         quick_ratio = ( attivo_circolante - tot_rimanenze  ) / debiti_breve ,
         immobiliz_su_tot_attivo = tot_immobilizzazioni / ( tot_immobilizzazioni +
                                                              attivo_circolante  ))


# Insert the first two principal components into the dataset
dataset_codici_non_numerati$pc1 <- df$Dim.1
dataset_codici_non_numerati$pc2 <- df$Dim.2


# Calculate year-to-year variations of the principal components.
# Only the first 2 principal components are selected as they are 
# the most explanatory. They will be used for plotting.
# Pivot the database to a wide format (separate columns for 2022 and 2023 
# for each index and principal component)
finale_wide <- dataset_codici_non_numerati %>%
  pivot_wider(
    id_cols = c(codice_fiscale),
    names_from = anno,
    values_from = c(ricavi_vendite, ROA, ROS, EBITDAMargin, CCN, PFN, utile_netto,
                    current_ratio, quick_ratio, immobiliz_su_tot_attivo, pc1, pc2)
  )

# Calculate Delta variations for the first 2 principal components
finale_wide <- finale_wide %>%
  mutate(Delta_pc1 = pc1_2023 - pc1_2022,
         Delta_pc2 = pc2_2023 - pc2_2022,
  )

# Create a database with fiscal codes and principal components only
finale_pca <- finale_wide %>%
  select(codice_fiscale, pc1_2022, pc2_2022, pc1_2023, pc2_2023) %>%
  pivot_longer(
    cols = starts_with("pc"),
    names_to = c("Dim", "Anno"),
    names_sep = "_",
    values_to = "Valore"
  ) %>%
  pivot_wider(names_from = Dim, values_from = Valore)

#install.packages("ggplot2")
library(ggplot2)
library(grid)

# Plot showing movement of principal components for each specific fiscal code using arrows
print(
  ggplot(finale_pca, aes(x = pc1, y = pc2, group = codice_fiscale)) +
    geom_point(aes(color = Anno)) +
    geom_line(arrow = arrow(length = unit(0.2, "cm")), alpha = 0.6) +
    theme_minimal() +
    labs(title = "Company movement between 2022 and 2023 in the PCA plane",
         x = "Principal Component 1",
         y = "Principal Component 2")
)

# This graph visualizes the trajectory of each individual company in the plane 
# of the first two principal components between 2022 and 2023.
# Each arrow represents a specific company, with the starting point 
# indicating its 2022 position (blue) and the arrowhead indicating its 2023 position (red).

# Select only variables that actually improved over time
aziende_migliorate <- finale_wide %>%  
  filter(Delta_pc1 > 0, Delta_pc2 > 0)

# Mean for each index of the Improved Group for 2022
aziende_migliorate %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Mean for each index of the Entire Sample for 2022
finale_wide %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Mean for each index of the Improved Group for 2023
aziende_migliorate %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))

# Mean for each index of the Entire Sample for 2023
finale_wide %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))


# Calculate distance in the PCA space
aziende_migliorate <- aziende_migliorate %>%
  mutate(Distanza = sqrt(Delta_pc1^2 + Delta_pc2^2)
  )
# Add fiscal codes
aziende_migliorate_1 <- aziende_migliorate %>%
  left_join(finale_wide %>% select(codice_fiscale), by = "codice_fiscale")


# Prepare the dataset in long format
aziende_migliorate_long <- aziende_migliorate_1 %>%
  select(codice_fiscale, pc1_2022, pc2_2022, pc1_2023, pc2_2023) %>%
  pivot_longer(cols = starts_with("pc"),
               names_to = c("PC", "Anno"),
               names_sep = "_",
               values_to = "Valore") %>%
  pivot_wider(names_from = PC, values_from = Valore)

# Sort by fiscal code to draw lines correctly
aziende_migliorate_long <- aziende_migliorate_long %>%
  arrange(codice_fiscale)

# Plot
ggplot(aziende_migliorate_long , aes(x = pc1, y = pc2, group = codice_fiscale)) +
  geom_point(aes(color = Anno)) +
  geom_line(arrow = arrow(length = unit(0.2, "cm")), alpha = 0.5, color = "grey40") +
  labs(
    title = "Evolution of improved companies between 2022 and 2023 in the PCA plane",
    x = "Principal Component 1",
    y = "Principal Component 2"
  )

# This graph shows only the improved companies; arrows are all 
# pointing right (increase in PC1) and upward (increase in PC2).


# Now analyze if immediate production start affects index variations


# Split indices by year in the dataset where codes are not numbered by year
finale_produzione_subito <- dataset_codici_non_numerati %>%
  pivot_wider(
    id_cols = c(codice_fiscale),
    names_from = anno,
    values_from = c(ricavi_vendite, ROA, ROS, EBITDAMargin, CCN, PFN, utile_netto,
                    current_ratio, quick_ratio, immobiliz_su_tot_attivo, pc1, pc2, prod_subito)
  )


# Create two datasets: one for "just-in-time" companies and another for 
# companies that started production later
produzione_subito <- finale_produzione_subito %>% 
  filter(prod_subito_2022 == 1, prod_subito_2023 == 1)

produzione_dopo <- finale_produzione_subito %>% 
  filter(prod_subito_2022 == 0, prod_subito_2023 == 0)

# Mean for each index of the "just-in-time" group for 2022
produzione_subito %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Mean for each index of the "delayed" group for 2022
produzione_dopo %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Mean for each index of the "just-in-time" group for 2023
produzione_subito %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))

# Mean for each index of the "delayed" group for 2023
produzione_dopo %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))

# Boxplots to visualize distribution changes over years 
# for "just-in-time" and "delayed" companies
Distanza = sqrt(finale_wide$Delta_pc1^2 + finale_wide$Delta_pc2^2)


ggplot(finale_produzione_subito, aes(x = as.factor(prod_subito_2022), y = Distanza, fill = as.factor(prod_subito_2022))) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distance in the PCA plane between 2022 and 2023 by production start",
    x = "Production Start",
    y = "Distance (PCA Displacement)",
    fill = "Production Start"
  ) +
  theme(legend.position = "none")

# The plots show that the 50th percentile and the median distribution are lower 
# for "just-in-time" companies, meaning their distance (displacement 
# across years in the variable space) is smaller. They may have maintained 
# a more consistent position in the multi-dimensional space of indices.
# Conversely, "Group 0" companies have a higher median, which could indicate greater evolution, 
# growth, decline, or a more pronounced variation in their measured characteristics.

# "Group 1" companies thus show greater stability over time in the space of the first 
# 2 principal components compared to "Group 0" companies.