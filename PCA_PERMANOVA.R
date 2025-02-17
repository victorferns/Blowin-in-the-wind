# Load the required packages
library(ggplot2)
library(dplyr)

# Load the data (adjust the file path as needed)
dados <- read.csv("/Users/Lab_Evol_SL/Documents/alunos/victor/CAMADAS /id.ambientais.csv")

# Perform PCA, excluding the 'codigo_localidade' and 'categoria' columns
pca_data <- dados %>%
  select(-codigo_localidade, -categoria) %>%
  scale()  # Standardize the data

# Run PCA
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Create a data frame with PCA results and adjust categories
pca_df <- data.frame(pca_result$x, categoria = dados$categoria) %>%
  mutate(categoria = recode(categoria,
                            "BNs" = "Brejos Nordestinos",
                            "CEP" = "Pernambuco Center"))

# Plot the PCA results
pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = categoria, shape = categoria)) +
  geom_point(size = 5) +
  labs(
    x = "PC1",                         # X-axis label
    y = "PC2",                         # Y-axis label
    color = "Sub-region:",             # Legend title (colors)
    shape = "Sub-region:") +           # Legend title (shapes)
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80", size = 0.25),  # Major grid lines
        panel.grid.minor = element_line(color = "gray80", size = 0.25),  # Minor grid lines
        axis.line = element_blank(),                                    # Remove axis lines
        axis.ticks = element_blank(),                                   # Remove axis ticks
        panel.border = element_blank(),                                 # Remove panel border
        plot.background = element_rect(fill = "white"),                 # White background
        legend.position = "top",                                        # Place legend at the top
        legend.title = element_text(size = 12, face = "bold"),          # Style legend title
        legend.text = element_text(size = 10)) +                        # Style legend text
  scale_color_manual(values = c("lightgreen", "#1f78b4")) +             # Define point colors
  scale_shape_manual(values = c(16, 16))                                # Set point shapes to circles

# Display the plot
print(pca_plot)

# Save the plot as a TIFF file with 300 dpi in the desired folder
ggsave("/Users/Lab_Evol_SL/Documents/alunos/victor/pca_plot.tif", plot = pca_plot, dpi = 300, width = 8, height = 6, units = "in", device = "tiff")

# Load the required package
library(vegan)

# Check the existing groups (categories)
print(unique(dados$categoria))  # Ensure the categories are correct

# Create a distance matrix (e.g., using Euclidean distance)
dist_matrix <- dist(pca_data)  # The matrix should be based on standardized data

# Run PERMANOVA using the distance matrix and the groupings
permanova_result <- adonis2(dist_matrix ~ categoria, data = dados, permutations = 999)

# Display the results
print(permanova_result)

