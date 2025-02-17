# Load packages
library(vegan)
library(ggplot2)

# Load CSV data
data <- read.csv("/Users/Lab_Evol_SL/Documents/alunos/victor/analise/matriz.csv")

# Check the structure of the data
str(data)

# Extract the category (group) column and presence/absence data
group <- factor(data$subregion)  # 'category' column as a factor
local <- data$local  # Extract the 'local' column
presence_absence_data <- data[, -(1:2)]  # Remove category and local columns

# Ensure the data is in matrix form
presence_absence_data <- as.matrix(presence_absence_data)

# Perform NMDS using the Jaccard index
nmds <- metaMDS(presence_absence_data, distance = "jaccard", k = 2, trymax = 9999999)

# Check the NMDS result
nmds

# Extract NMDS coordinates
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$sub_region <- group  # Add the group (category) as 'sub_region'
nmds_scores$local <- local  # Add the 'local' column to the scores

# Modify group values
nmds_scores$sub_region <- factor(nmds_scores$sub_region, 
                                 levels = c("BNs", "CEP"), 
                                 labels = c("Brejos Nordestinos", "Pernambuco Center"))

# Perform K-means clustering to divide into 2 groups
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(nmds_scores[, c("NMDS1", "NMDS2")], centers = 2)

# Add the K-means group column
nmds_scores$kmeans_group <- factor(kmeans_result$cluster, labels = c("Group 1", "Group 2"))

# Add the stress value to the plot
stress_value <- round(nmds$stress, 3)

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = sub_region, shape = kmeans_group)) +
  geom_point(size = 5, position = position_jitter(width = 0.05, height = 0.05)) +  # Removed 'alpha' to eliminate transparency
  scale_color_manual(values = c("Brejos Nordestinos" = "lightgreen", "Pernambuco Center" = "#1f78b4"), name = "Sub-regions:") +  # Define colors and label for sub-regions
  scale_shape_manual(values = c(17, 16), name = "Clusteres:", labels = c("Cluster 1", "Cluster 2")) +  # Circle (16) and triangle (17)
  labs(title = "", x = "NMDS1", y = "NMDS2") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste(), 
           hjust = 1.1, vjust = 1.5, size = 4, color = "black") +
  theme(legend.position = "top", 
        legend.title = element_text(face = "bold"),  # Legend titles in bold
        legend.text = element_text(size = 12),  # Increase legend text size
        axis.title = element_text(size = 12),  # Increase axis title size
        axis.text = element_text(size = 12),  # Increase axis text size
        panel.grid.major = element_line(color = "gray90"),  # Retain grid lines
        panel.grid.minor = element_line(color = "gray90"),  # Retain minor grid lines
        panel.background = element_rect(fill = "white", color = NA),  # White background with no border
        plot.background = element_rect(fill = "white", color = NA))  # White background without border


# Save the plot in PNG format with 300 DPI
ggsave("/Users/Lab_Evol_SL/Documents/alunos/victor/analise/nmds_plot.tiff", 
       dpi = 300, width = 10, height = 8, units = "in", device = "tiff")

# Run ANOSIM for K-means clusters
anosim_kmeans <- anosim(presence_absence_data, nmds_scores$kmeans_group, 
                        distance = "jaccard", permutations = 99999)
print(anosim_kmeans)

# Run ANOSIM for sub-region clusters
anosim_subregion <- anosim(presence_absence_data, nmds_scores$sub_region, 
                           distance = "jaccard", permutations = 99999)
print(anosim_subregion)
